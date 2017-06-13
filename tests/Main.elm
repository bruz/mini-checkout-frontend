port module Main exposing (..)

import VendorsTests
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    run emit VendorsTests.all


port emit : ( String, Value ) -> Cmd msg
