import CsrfCookie

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode exposing (..)

import Html exposing (br, button, div, form, input, label, text, Html)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import Http exposing (send, expectStringResponse, Request, Body, request, header)
import HttpBuilder as HttpB

import Task


main : Program Never Model Msg
main =
  Html.program { init = init, subscriptions = subscriptions, view = view, update = update }

type alias Model =
  { loggedin       : Bool
  , username       : String
  , password       : String
  , passwordAgain  : String
  , rollDieMessage : String
  }

type alias User =
  { id    : Int
  , email : String
  }
  
userDecoder : Decoder User
userDecoder =
  Json.Decode.Pipeline.decode 
    User
      |> Json.Decode.Pipeline.required "id" (Json.Decode.int)
      |> Json.Decode.Pipeline.required "email" (Json.Decode.string)      

type alias Login =
  { username : String
  , password : String
  }

init : (Model, Cmd Msg)
init = 
  let
    request = HttpB.get "/loggedin" |> HttpB.withExpect (Http.expectJson userDecoder)
  in
    (model, sendWithCsrfToken CheckLoggedInStatus request)

model : Model
model =
  Model False "" "" "" ""


-- UPDATE

type Msg
    = CheckLoggedInStatus (Result Http.Error User)
    -- update model (login form)
    | Username      String
    | Password      String
    | PasswordAgain String
    -- request a login with the data from the form
    | LoginRequest Model
    | LoginResponse (Result Http.Error NoContent)
    | RollDieRequest
    | RollDieResponse (Result Http.Error Int)
    | Logout
    | Error String
    | UnauthorizedError
    | NullMsg
    
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    CheckLoggedInStatus (Ok result) ->
      Debug.log ("CheckLoggedInStatus OK")
      ({ model | loggedin = True } , Cmd.none)
    
    CheckLoggedInStatus (Err err) ->
      Debug.log ("CheckLoggedInStatus Err")
      Debug.log (toString err)
      ({ model | loggedin = False } , Cmd.none)
          
    Username username ->
      ({ model | username = username }, Cmd.none)

    Password password ->
      ({ model | password = password }, Cmd.none)

    PasswordAgain password ->
      ({ model | passwordAgain = password }, Cmd.none)
    
    LoginRequest model ->
      let
        request = postNoContent "/login" (Http.jsonBody (loginEncode (Login model.username model.password)))
      in
        (model, Http.send LoginResponse request)
        
    LoginResponse (Ok result) ->
      Debug.log ("LoginResponse OK")
      (model, Cmd.none)
    
    LoginResponse (Err err) ->
      Debug.log "LoginResponse error"
      Debug.log (toString err)
      (model, Cmd.none)
      
    RollDieRequest ->
      let
        request = HttpB.get "/die/roll" |> HttpB.withExpect (Http.expectJson Json.Decode.int)
      in
        (model, sendWithCsrfToken RollDieResponse request)

    RollDieResponse (Ok result) ->
      Debug.log "TestAuthResponse Ok"
      Debug.log (toString result)
      ({model | rollDieMessage = ("The result of the die roll is: " ++ toString result) }, Cmd.none)
      
    RollDieResponse (Err err) ->
      Debug.log "TestAuthResponse Error"
      Debug.log (toString err)
      ({model | rollDieMessage = ("You are not logged in. You cannot roll the die.") }, Cmd.none)
    
    Logout ->
      (model, Task.attempt (\r -> NullMsg) (CsrfCookie.deleteCookie))
    
    Error s ->
      Debug.log "Error "
      Debug.log s
      (model, Cmd.none)
    
    UnauthorizedError ->
      Debug.log "UnauthorizedError"
      (model, Cmd.none)
    
    NullMsg -> 
      (model, Cmd.none)


type alias NoContent = ()

noContentDecoder : Decoder NoContent
noContentDecoder = decode ()

decodeAlways : Decoder ()
decodeAlways = Json.Decode.value |> andThen (\_ -> Json.Decode.succeed ()) 

postNoContent : String -> Body -> Request NoContent
postNoContent url body =
  request
    { method = "POST"
    , headers = []
    , url = url
    , body = body
    , expect = 
        expectStringResponse 
          (\res ->
            if String.isEmpty res.body then
              Ok ()
            else
              Err "Expected the response body to be empty"
          )
    , timeout = Nothing
    , withCredentials = False
    }

view : Model -> Html Msg
view model =
  let
    loginStatusTitle =
      [ div [] [ text (if model.loggedin then "You are currently logged in" else "Please log in") ] ]
    loginForm =
      if not model.loggedin
        then 
          [ Html.form [] 
            [ label [] [ text "Username:" ]
            , br [] []
            , input [ type_ "text", placeholder "Username", onInput Username ] []
            , br [] []
            , label [] [ text "Password:"]
            , br [] []
            , input [ type_ "password", placeholder "Password", onInput Password ] []
            , br [] []
            , label [] [ text "Password Again:"]
            , br [] []
            , input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
            , br [] []
            , button [ onClick (LoginRequest model) ] [ text "Login" ]
            ]
          , viewValidation model
          ]
        else
          []
    loginOnlyButton =
      [  button [ onClick (RollDieRequest) ] [ text "Roll Die (Require login)" ] ]
    rollDieMessage =
      if model.rollDieMessage /= ""
        then 
          [ div [] [ text model.rollDieMessage ]]
        else
          []
    logoutButton = 
      [ button [ onClick (Logout)] [ text "Logout"]]
  in 
    div [] (loginStatusTitle ++ loginForm ++ loginOnlyButton ++ rollDieMessage ++ logoutButton)

viewValidation : Model -> Html msg
viewValidation model =
  let
    (color, message) =
      if model.username /= "" && model.password /= "" && model.password == model.passwordAgain then
        ("green", "OK")
      else        
        ("red", "No fields may be empty and passwords must match.")
  in
    div [ style [("color", color)] ] [ text message ]


loginEncode : Login -> Json.Encode.Value
loginEncode l = 
  object 
    [ ("username", Json.Encode.string l.username)
    , ("password", Json.Encode.string l.password)
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


sendOnAuthError :
    Msg
    -> (a -> Msg)
    -> HttpB.RequestBuilder a
    -> Cmd Msg
sendOnAuthError onAuthError tag =
  sendWithCsrfToken (handleHttpError (Just onAuthError) tag Nothing)

handleHttpError :
    Maybe Msg
    -> (a -> Msg)
    -> Maybe (Http.Error -> Msg)
    -> Result Http.Error a
    -> Msg
handleHttpError handleAuthError tag tagE r =
  case r of
    Ok a ->
      tag a

    Err e ->
      case Debug.log "error" e of
        Http.BadStatus resp ->
          if resp.status.code == 401 then
            case handleAuthError of
              Nothing ->
                UnauthorizedError
              Just handler ->
                handler
          else
            Maybe.withDefault showError tagE e
        _ ->
          Maybe.withDefault showError tagE e

showError : a -> Msg
showError =
  Error << toString

sendWithCsrfToken :
    (Result Http.Error a -> msg)
    -> HttpB.RequestBuilder a
    -> Cmd msg
sendWithCsrfToken handler req =
  CsrfCookie.csrfCookie ()
    |> Task.map Just
    |> Task.onError (always (Task.succeed Nothing))
    |> Task.andThen
        (\mcsrf ->
          req
            |> 
              (case mcsrf of
                Nothing ->
                  Debug.log "mcsrf is nothing"
                  identity
                Just csrf ->
                  Debug.log "mcsrf is something"
                  HttpB.withHeader "X-XSRF-TOKEN" csrf
               )
            |> HttpB.toTask
        )
    |> Task.attempt handler
