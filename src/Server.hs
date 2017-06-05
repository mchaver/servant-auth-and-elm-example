{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Server where

import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.Text (Text)
import qualified Data.Time as Time
import GHC.Generics
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Auth.Server
import Servant.Auth.Server.Internal.ConfigTypes (SameSite (AnySite))
import Servant.Auth.Server.SetCookieOrphan ()
import System.Random

import Network.Wai.Application.Static
import WaiAppStatic.Types (unsafeToPiece)

data User =
  User
    { userId       :: Int
    , userEmail    :: Text 
    , userPassword :: Text
    } deriving (Eq, Read, Show, Generic)

instance ToJSON User where
  toJSON (User _id _email _password) =
    object
      [ "id"       .= _id
      , "email"    .= _email
      , "password" .= _password
      ]
instance FromJSON User where
  parseJSON = withObject "User" $ \o ->
    User <$> o .: "id"
         <*> o .: "email"
         <*> o .: "password"

instance ToJWT User
instance FromJWT User

data Login = 
  Login 
    { username :: Text
    , password :: Text
    } deriving (Eq, Read, Show, Generic)

instance ToJSON Login where
  toJSON (Login _username _password) =
    object
      [ "username" .= _username
      , "password" .= _password
      ]
      
instance FromJSON Login where
  parseJSON = withObject "Login" $ \o ->
    Login <$> o .: "username"
          <*> o .: "password"

type Protected = 
       "die"      :> "roll" :> Get '[JSON] Int
  :<|> "loggedin" :> Get '[JSON] User

type Unprotected = 
       "login" :> ReqBody '[JSON] Login
               :> PostNoContent '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                                  , Header "Set-Cookie" SetCookie]
                                                    NoContent)
  :<|> Raw

type API auths = (Auth auths User :> Protected) :<|> Unprotected

server :: CookieSettings -> JWTSettings -> Server (API auths)
server cs jwts = protected :<|> unprotected cs jwts

-- | 'Protected' will be protected by 'auths', which we still have to specify.
protected :: AuthResult User -> Server Protected
-- If we get an "Authenticated v", we can trust the information in v, since
-- it was signed by a key we trust.
protected (Authenticated _user) = (liftIO $ randomRIO (1, 6)) :<|> (return $ User 1 "test@test.com" "")
-- Otherwise, we return a 401.
protected _ = throwAll err401

staticFiles :: [(FilePath, ByteString)]
staticFiles =
  [ ("index.html", $(embedFile "static/index.html"))
  ]

unprotected :: CookieSettings -> JWTSettings -> Server Unprotected
unprotected cs jwts = 
  loginH :<|> staticH
  where
    loginH              = checkCreds cs jwts
    -- staticH             = serveDirectoryFileServer staticDir　
    staticH             = serveDirectoryWith $ set
      where
        set = (defaultWebAppSettings $ error "unused") { ssLookupFile = ssLookupFile embedded, ssIndices = map unsafeToPiece ["index.html"] }
        embedded = embeddedSettings staticFiles

checkCreds :: CookieSettings
           -> JWTSettings
           -> Login
           -> Handler (Headers '[ Header "Set-Cookie" SetCookie
                                , Header "Set-Cookie" SetCookie]
                               NoContent)
checkCreds cookieSettings jwtSettings (Login loginUserIdent loginUserPassword) = do
  let mUser = 
        case loginUserIdent == "test@test.com" && loginUserPassword == "password" of
          True -> Just $ User 1 "test@test.com" "test"
          _    -> Nothing
  case mUser of
    Nothing -> throwError err401
    Just usr -> do
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings usr
      case mApplyCookies of
        Nothing     -> throwError err401
        Just applyCookies -> do 
          return $ applyCookies NoContent

mainWithCookies :: IO ()
mainWithCookies = do
  -- We *also* need a key to sign the cookies
  myKey <- generateKey
  now <- Time.getCurrentTime  
  -- Adding some configurations. 'Cookie' requires, in addition to
  -- CookieSettings, JWTSettings (for signing), so everything is just as before
  let jwtCfg = defaultJWTSettings myKey
      cookieSettings =
        defaultCookieSettings
          { cookiePath = Just "/"
          , cookieExpires = Just now
              { Time.utctDay = Time.addDays 30 (Time.utctDay now) }
          , xsrfCookieName = "XSRF-TOKEN"
          , xsrfHeaderName = "X-XSRF-TOKEN"
          -- , xsrfCookiePath = Just "/"
          , cookieIsSecure = Servant.Auth.Server.NotSecure
          , cookieSameSite = AnySite
          }
      cfg = cookieSettings :. jwtCfg :. EmptyContext
      --- Here is the actual change
      api = Proxy :: Proxy (API '[Cookie])
  run 3000 $ serveWithContext api cfg (server cookieSettings jwtCfg)
