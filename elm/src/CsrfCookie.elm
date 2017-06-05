module CsrfCookie exposing (..)

-- import Native.Cookie
import Native.CsrfCookie
import Task

csrfCookie : () -> Task.Task () String
csrfCookie =
  Native.CsrfCookie.csrfCookie

deleteCookie : Task.Task () ()
deleteCookie =
  Native.CsrfCookie.deleteCookie ()
