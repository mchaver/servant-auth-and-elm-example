import Distribution.Simple
import System.Directory (doesDirectoryExist, setCurrentDirectory)
import System.Process
import System.IO (hPutStrLn, stderr)
import System.IO.Error (catchIOError, isDoesNotExistError)

toolExists :: FilePath -> IO Bool
toolExists str = 
  (createProcess (proc str []) { std_out = CreatePipe
                               , std_err = CreatePipe
                               } >> return True)
    `catchIOError`
      (\e -> if isDoesNotExistError e then return False else return True)

main :: IO ()
main = do
  elmMakeExists <- toolExists "elm-make"
  if elmMakeExists
    then do
      elmExists <- doesDirectoryExist "elm"
      if elmExists
        then do 
          setCurrentDirectory "elm"

          hPutStrLn stderr ""
          hPutStrLn stderr "Building elm/src/Main.elm"
          hPutStrLn stderr "=========================\n"
          rawSystem "elm-make" ["src/Main.elm", "--output=../static/index.html"]
          
          setCurrentDirectory ".."
          
          hPutStrLn stderr ""
          hPutStrLn stderr "Compiling servant-auth-and-elm-example"
          hPutStrLn stderr "======================================\n"
          defaultMain
          hPutStrLn stderr "servant-auth-and-elm-example build is complete."
        else do 
          hPutStrLn stderr "Unable to find the elm "
    else do
      hPutStrLn stderr "Unable to complete servant-auth-and-elm-example build.\n Setup.hs cannot find 'elm-make'. Make sure it is installed in your system and available on PATH."
