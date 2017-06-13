module Purchases exposing (..)

import Bootstrap.Buttons exposing (..)
import Bootstrap.Forms exposing (..)
import Bootstrap.Grid exposing (..)
import Date.Format
import Form exposing (Form)
import FormViews exposing (..)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Locale)
import Html exposing (Attribute, Html, button, div, a, h2, label, li, table, tbody, td, th, tr, thead, text)
import Html.Attributes exposing (class, classList, colspan, disabled, href, placeholder, style, type_, value)
import Html.Events exposing (onClick)
import Types exposing (..)


locale : Locale
locale =
    { decimals = 2
    , thousandSeparator = ","
    , decimalSeparator = "."
    }



-- VIEW


view : Model -> Html Msg
view model =
    column [ Medium Twelve ]
        [ alert model.error
        , case ( model.purchasesLoadStatus, model.vendorsLoadStatus, model.error ) of
            ( NotLoaded, _, Nothing ) ->
                div [] [ text "Loading purchases..." ]

            ( _, NotLoaded, Nothing ) ->
                div [] [ text "Loading purchases..." ]

            ( NotLoaded, _, Just _ ) ->
                div [] []

            ( _, NotLoaded, Just _ ) ->
                div [] []

            ( Loaded, Loaded, _ ) ->
                div []
                    [ h2 [] [ text "Add New Purchase" ]
                    , container
                        [ row
                            [ Html.map NewPurchaseFormMsg (formView model.newPurchaseRecordState model.newPurchaseForm model.vendorRows) ]
                        ]
                    , h2 [] [ text "Purchases" ]
                    , container
                        [ row
                            [ purchaseTable model.purchaseRows model.vendorRows ]
                        ]
                    ]
        ]


formView : RecordState -> Form () a -> List VendorRow -> Html Form.Msg
formView state purchaseForm vendorRows =
    let
        ( disabled, buttonText ) =
            case state of
                Saving ->
                    ( True, "Saving..." )

                _ ->
                    ( False, "Save" )

        paymentMethod =
            Form.getFieldAsString "paymentMethod" purchaseForm

        vendorPairs =
            List.map (\v -> ( (toString v.vendorId), v.name )) vendorRows

        vendorChoices =
            ( "", "Please select" ) :: vendorPairs

        tableStyles =
            style [ ( "margin-bottom", "5px" ) ]

        buttonStyles =
            style
                [ ( "margin-bottom", "20px" )
                , ( "cursor", "pointer" )
                , ( "display", "block" )
                ]
    in
        form FormDefault
            [ onSubmitForm Form.Submit ]
            [ div
                [ class "form-group" ]
                [ label [ class "col-sm-2 control-label" ] [ text "Payment Method" ]
                , radioField "cash" "Cash" paymentMethod disabled
                , radioField "check" "Check" paymentMethod disabled
                , radioField "credit" "Credit Card" paymentMethod disabled
                ]
            , div
                [ class "todo-list" ]
                [ table [ class "table table-striped table-bordered", tableStyles ]
                    [ thead []
                        [ tr []
                            [ th [] [ text "Vendor" ]
                            , th [] [ text "Amount" ]
                            , th [] []
                            ]
                        ]
                    , tbody [] <|
                        List.map
                            (itemView purchaseForm disabled vendorChoices)
                            (Form.getListIndexes "lineItems" purchaseForm)
                    ]
                , a
                    [ class "add"
                    , onClick (Form.Append "lineItems")
                    , buttonStyles
                    ]
                    [ text "Add Vendor" ]
                ]
            , btn BtnPrimary [] [] [ type_ "submit" ] [ text buttonText ]
            ]


itemView : Form () a -> Bool -> List ( String, String ) -> Int -> Html Form.Msg
itemView form disabled vendorChoices i =
    let
        vendorId =
            Form.getFieldAsString ("lineItems." ++ (toString i) ++ ".vendorId") form

        price =
            Form.getFieldAsString ("lineItems." ++ (toString i) ++ ".price") form
    in
        tr
            []
            [ td []
                [ selectField vendorId vendorChoices disabled ]
            , td []
                [ textField price "Price" disabled ]
            , td []
                [ a
                    [ class "btn btn-primary btn-sm"
                    , onClick (Form.RemoveItem "lineItems" i)
                    ]
                    [ text "Remove" ]
                ]
            ]


purchaseTable : List PurchaseRow -> List VendorRow -> Html Msg
purchaseTable purchaseRows vendorRows =
    let
        rows =
            List.map (tableRow vendorRows) purchaseRows

        headerStyle =
            style [ ( "width", "30%" ) ]
    in
        table [ class "table table-striped" ]
            [ thead []
                [ tr []
                    [ th [ headerStyle ] [ text "Purchased At" ]
                    , th [ headerStyle ] [ text "Total Paid" ]
                    , th [ headerStyle ] [ text "Payment Method" ]
                    , th [] []
                    ]
                ]
            , tbody [] rows
            ]


tableRow : List VendorRow -> PurchaseRow -> Html Msg
tableRow vendorRows purchaseRow =
    case purchaseRow.state of
        Viewing ->
            let
                total =
                    format locale (totalPaid purchaseRow)
            in
                tr []
                    [ td [] [ text (Date.Format.format "%m/%d/%Y at %l:%M %p" purchaseRow.time) ]
                    , td [] [ text ("$" ++ total) ]
                    , td [] [ text purchaseRow.paymentMethod ]
                    , td []
                        [ btn BtnDefault [ BtnExtraSmall ] [] [ onClick (PurchaseRowEdit purchaseRow.purchaseId) ] [ text "Edit" ] ]
                    ]

        _ ->
            tr []
                [ td [ colspan 3 ]
                    [ Html.map (PurchaseRowMsgForm purchaseRow.purchaseId) (formView purchaseRow.state purchaseRow.form vendorRows)
                    ]
                ]


totalPaid : PurchaseRow -> Float
totalPaid purchaseRow =
    let
        prices =
            List.map (\li -> li.price) purchaseRow.lineItems
    in
        List.sum prices
