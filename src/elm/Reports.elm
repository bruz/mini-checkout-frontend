module Reports exposing (..)

import Bootstrap.Forms exposing (..)
import Bootstrap.Grid exposing (..)
import Date
import Date.Format
import Dict exposing (Dict)
import Form exposing (Form)
import FormViews exposing (..)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Locale)
import Html exposing (Html, div, h2, table, tbody, td, th, tr, thead, text)
import Html.Attributes exposing (class, colspan, style, type_)
import Html.Events exposing (onClick)
import List.Extra
import Types exposing (..)


locale : Locale
locale =
    { decimals = 2
    , thousandSeparator = ","
    , decimalSeparator = "."
    }


view : Model -> Html Msg
view model =
    column [ Medium Twelve ]
        [ case ( model.purchasesLoadStatus, model.vendorsLoadStatus, model.error ) of
            ( NotLoaded, _, Nothing ) ->
                div [] [ text "Loading sales totals..." ]

            ( _, NotLoaded, Nothing ) ->
                div [] [ text "Loading sales totals..." ]

            ( NotLoaded, _, Just _ ) ->
                alert (Just "Loading sales totals failed")

            ( _, NotLoaded, Just _ ) ->
                alert (Just "Loading sales totals failed")

            ( Loaded, Loaded, _ ) ->
                div []
                    [ h2 [] [ text "Sales Totals" ]
                    , container
                        [ row
                            [ Html.map DateRangeForm (formView model)
                            , salesTable model
                            ]
                        ]
                    ]
        ]


formView : Model -> Html Form.Msg
formView model =
    let
        start =
            Form.getFieldAsString "start" model.dateRangeForm

        end =
            Form.getFieldAsString "end" model.dateRangeForm
    in
        form FormInline
            [ onSubmitForm Form.Submit ]
            [ dateField start
            , text " to "
            , dateField end
            ]


salesTable : Model -> Html Msg
salesTable model =
    case ( model.vendorsLoadStatus, model.purchasesLoadStatus ) of
        ( Loaded, Loaded ) ->
            let
                rows =
                    List.map (salesRow model.reportVendorStates model) (salesTotals model)
            in
                table [ class "table table-striped" ]
                    [ thead []
                        [ tr []
                            [ th [] [ text "Vendor" ]
                            , th [] [ text "Total Sales" ]
                            ]
                        ]
                    , tbody [] rows
                    ]

        _ ->
            div [] [ text "Loading vendors..." ]


salesRow : Dict Int ExpandState -> Model -> SalesTotal -> Html Msg
salesRow reportVendorStates model salesTotal =
    let
        total =
            format locale salesTotal.total

        state =
            Dict.get salesTotal.vendorId reportVendorStates

        rowStyles =
            style [ ( "cursor", "pointer" ) ]

        toggleStyles =
            style [ ( "margin-right", "10px" ) ]
    in
        if state == Just Expanded then
            tr [ onClick (ToggleReportVendorState salesTotal.vendorId), rowStyles ]
                [ td [ colspan 2 ]
                    [ div [ style [ ( "margin-top", "8px" ) ] ]
                        [ div [ toggleStyles, class "glyphicon glyphicon-chevron-down" ] []
                        , text salesTotal.vendorName
                        ]
                    , (vendorSalesTable model salesTotal.vendorId)
                    ]
                ]
        else
            tr [ onClick (ToggleReportVendorState salesTotal.vendorId), rowStyles ]
                [ td []
                    [ div [ class "glyphicon glyphicon-chevron-right", toggleStyles ] []
                    , text salesTotal.vendorName
                    ]
                , td []
                    [ text ("$" ++ total) ]
                ]


vendorSalesTable : Model -> Int -> Html Msg
vendorSalesTable model vendorId =
    let
        hasVendor lineItems =
            List.filter (\l -> l.vendorId == vendorId) lineItems
                |> List.isEmpty
                |> not

        ( startResult, endResult ) =
            (startAndEndDates model.dateRangeForm)

        filteredByVendor =
            List.filter (\r -> (hasVendor r.lineItems)) model.purchaseRows

        filtered =
            (vendorsInRange model filteredByVendor)

        rows =
            List.map (vendorSalesRow vendorId) filtered
    in
        table [ class "table table-striped table-bordered" ]
            [ thead []
                [ tr []
                    [ th [] [ text "Purchased At" ]
                    , th [] [ text "Total Paid" ]
                    , th [] [ text "Payment Method" ]
                    ]
                ]
            , tbody [] rows
            ]


vendorSalesRow : Int -> PurchaseRow -> Html Msg
vendorSalesRow vendorId purchaseRow =
    let
        total =
            List.filter (\l -> l.vendorId == vendorId) purchaseRow.lineItems
                |> List.map .price
                |> List.sum
    in
        tr []
            [ td [] [ text (Date.Format.format "%m/%d/%Y at %l:%M %p" purchaseRow.time) ]
            , td [] [ text ("$" ++ (toString total)) ]
            , td [] [ text purchaseRow.paymentMethod ]
            ]


salesTotals : Model -> List SalesTotal
salesTotals model =
    let
        lineItems =
            List.concatMap .lineItems (vendorsInRange model model.purchaseRows)

        vendorIds =
            List.Extra.uniqueBy .vendorId lineItems
                |> List.map .vendorId
    in
        List.map (vendorSalesTotal lineItems model) vendorIds


vendorsInRange : Model -> List PurchaseRow -> List PurchaseRow
vendorsInRange model purchaseRows =
    let
        ( startResult, endResult ) =
            (startAndEndDates model.dateRangeForm)
    in
        case ( startResult, endResult ) of
            ( Ok start, Ok end ) ->
                List.filter (\r -> (Date.toTime r.time) > (Date.toTime start) && (Date.toTime r.time) < (Date.toTime end)) purchaseRows

            ( Ok start, _ ) ->
                List.filter (\r -> (Date.toTime r.time) > (Date.toTime start)) purchaseRows

            ( _, Ok end ) ->
                List.filter (\r -> (Date.toTime r.time) < (Date.toTime end)) purchaseRows

            ( _, _ ) ->
                purchaseRows


startAndEndDates : Form () DateRange -> ( Result String Date.Date, Result String Date.Date )
startAndEndDates form =
    let
        startResult =
            Form.getFieldAsString "start" form
                |> .value
                |> Maybe.withDefault ""
                |> Date.fromString

        endResult =
            Form.getFieldAsString "end" form
                |> .value
                |> Maybe.withDefault ""
                |> Date.fromString
    in
        ( startResult, endResult )


vendorSalesTotal : List LineItem -> Model -> Int -> SalesTotal
vendorSalesTotal lineItems model vendorId =
    let
        vendorLineItems =
            List.filter (\i -> i.vendorId == vendorId) lineItems

        total =
            List.foldl (\i -> \s -> s + i.price) 0.0 vendorLineItems

        vendorName =
            List.filter (\v -> v.vendorId == vendorId) model.vendorRows
                |> List.map .name
                |> List.head
                |> Maybe.withDefault "Unknown"
    in
        { vendorId = vendorId
        , vendorName = vendorName
        , total = total
        }
