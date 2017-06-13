module Main exposing (..)

import App
import Navigation
import Types exposing (..)


main : Program Flags Model Msg
main =
    Navigation.programWithFlags UrlChange
        { init = App.init
        , view = App.view
        , update = App.update
        , subscriptions = App.subscriptions
        }
