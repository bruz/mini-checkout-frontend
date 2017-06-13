module FormViews exposing (..)

import Form exposing (Form)
import Form.Input as Input
import Html exposing (Attribute, Html, div, a, h2, li, table, tbody, td, th, tr, thead, text)
import Html.Attributes exposing (class, classList, disabled, href, placeholder, style, type_, value)
import Html.Events exposing (onWithOptions)
import Json.Decode as Decode
import Types exposing (..)


textField : Form.FieldState () String -> String -> Bool -> Html Form.Msg
textField state placeholderText isDisabled =
    div [ classList [ ( "form-group", True ), errorClass state.liveError ] ]
        [ Input.textInput state
            [ disabled isDisabled
            , classList [ ( "form-control", True ) ]
            , placeholder placeholderText
            , value (Maybe.withDefault "" state.value)
            ]
        ]


dateField : Form.FieldState () String -> Html Form.Msg
dateField state =
    div [ classList [ ( "form-group", True ), errorClass state.liveError ] ]
        [ Input.textInput state
            [ classList [ ( "form-control", True ) ]
            , value (Maybe.withDefault "" state.value)
            , type_ "date"
            ]
        ]


radioField : String -> String -> Form.FieldState () String -> Bool -> Html Form.Msg
radioField fieldValue label state isDisabled =
    div [ classList [ ( "radio-inline", True ), errorClass state.liveError ] ]
        [ Input.radioInput fieldValue
            state
            [ disabled isDisabled
            , classList []
            , value fieldValue
            ]
        , text label
        ]


checkboxField : Form.FieldState () Bool -> Bool -> Html Form.Msg
checkboxField state isDisabled =
    div [ classList [ ( "form-group", True ), errorClass state.liveError ] ]
        [ Input.checkboxInput state
            [ disabled isDisabled
            ]
        ]


selectField : Form.FieldState () String -> List ( String, String ) -> Bool -> Html Form.Msg
selectField state choices isDisabled =
    div [ classList [ ( "form-group", True ), errorClass state.liveError ] ]
        [ Input.selectInput choices
            state
            [ disabled isDisabled
            , class "form-control"
            ]
        ]


errorClass : Maybe a -> ( String, Bool )
errorClass hasError =
    case hasError of
        Nothing ->
            ( "has-error", False )

        Just _ ->
            ( "has-error", True )


onSubmitForm : msg -> Attribute msg
onSubmitForm msg =
    onWithOptions "submit" { stopPropagation = True, preventDefault = True } (Decode.succeed msg)


alert : Maybe String -> Html Msg
alert maybeError =
    case maybeError of
        Nothing ->
            div [] []

        Just error ->
            div [ class "alert alert-danger" ] [ text error ]
