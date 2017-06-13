module Vendors exposing (..)

import Bootstrap.Buttons exposing (..)
import Bootstrap.Forms exposing (..)
import Bootstrap.Grid exposing (..)
import Types exposing (..)
import Form exposing (Form)
import FormViews exposing (..)
import Html exposing (Attribute, Html, div, a, h2, li, table, tbody, td, th, tr, thead, text)
import Html.Attributes exposing (class, classList, disabled, href, placeholder, style, type_, value)
import Html.Events exposing (onInput, onClick, onWithOptions)


view : Model -> Html Msg
view model =
    column [ Medium Twelve ]
        [ alert model.error
        , case ( model.vendorsLoadStatus, model.error ) of
            ( NotLoaded, Nothing ) ->
                div [] [ text "Loading vendors..." ]

            ( NotLoaded, Just _ ) ->
                div [] []

            ( Loaded, _ ) ->
                div []
                    [ h2 [] [ text "Vendors" ]
                    , container
                        [ row
                            [ Html.map NewVendorFormMsg (formView model) ]
                        , row
                            [ vendorTable model.vendorRows ]
                        ]
                    ]
        ]


formView : Model -> Html Form.Msg
formView model =
    let
        name =
            Form.getFieldAsString "name" model.newVendorForm

        number =
            Form.getFieldAsString "number" model.newVendorForm
    in
        case model.newVendorRecordState of
            Saving ->
                form FormInline
                    []
                    [ textField name "Name" True
                    , text " "
                    , textField number "Number" True
                    , text " "
                    , btn BtnPrimary [] [] [ disabled True ] [ text "Adding..." ]
                    ]

            _ ->
                form FormInline
                    [ onSubmitForm Form.Submit ]
                    [ textField name "Name" False
                    , text " "
                    , textField number "Number" False
                    , text " "
                    , btn BtnPrimary [] [] [ type_ "submit" ] [ text "Add" ]
                    ]


vendorTable : List VendorRow -> Html Msg
vendorTable vendorRows =
    let
        rows =
            List.map vendorRowView vendorRows

        headerStyle =
            style [ ( "width", "30%" ) ]
    in
        table [ class "table table-striped" ]
            [ thead []
                [ tr []
                    [ th [ headerStyle ] [ text "Name" ]
                    , th [ headerStyle ] [ text "Number" ]
                    , th [ headerStyle ] [ text "Active?" ]
                    , th [] []
                    ]
                ]
            , tbody [] rows
            ]


vendorRowView : VendorRow -> Html Msg
vendorRowView { vendorId, form, state } =
    let
        name =
            Form.getFieldAsString "name" form

        number =
            Form.getFieldAsString "number" form

        active =
            Form.getFieldAsBool "active" form

        activeText =
            case active.value of
                Nothing ->
                    ""

                Just value ->
                    if value then
                        "✓"
                    else
                        ""
    in
        case state of
            Viewing ->
                tr []
                    [ td []
                        [ text (Maybe.withDefault "" name.value) ]
                    , td []
                        [ text (Maybe.withDefault "" number.value) ]
                    , td []
                        [ text activeText ]
                    , td []
                        [ btn BtnDefault [ BtnExtraSmall ] [] [ onClick (VendorRowEdit vendorId) ] [ text "Edit" ] ]
                    ]

            Editing ->
                Html.map (VendorRowFormMsg vendorId) (vendorRowEditing name number active)

            Saving ->
                Html.map (VendorRowFormMsg vendorId) (vendorRowSaving name number active)


vendorRowEditing : Form.FieldState () String -> Form.FieldState () String -> Form.FieldState () Bool -> Html Form.Msg
vendorRowEditing name number active =
    tr []
        [ td []
            [ textField name "Name" False ]
        , td []
            [ textField number "Number" False ]
        , td []
            [ checkboxField active False ]
        , td []
            [ btn BtnPrimary [ BtnSmall ] [] [ onClick Form.Submit ] [ text "Save" ] ]
        ]


vendorRowSaving : Form.FieldState () String -> Form.FieldState () String -> Form.FieldState () Bool -> Html Form.Msg
vendorRowSaving name number active =
    tr []
        [ td []
            [ textField name "Name" True ]
        , td []
            [ textField number "Number" True ]
        , td []
            [ checkboxField active True ]
        , td []
            [ btn BtnPrimary [ BtnSmall ] [] [ disabled True ] [ text "Saving..." ] ]
        ]
