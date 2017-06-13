module App exposing (..)

import Api
import Bootstrap.Navbar exposing (..)
import Dict exposing (Dict)
import Types exposing (..)
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (..)
import Html exposing (Attribute, Html, div, a, h2, li, table, tbody, td, th, tr, thead, text)
import Html.Attributes exposing (class, classList, disabled, href, placeholder, style, type_, value)
import Http
import Navigation
import Purchases
import Reports
import Vendors


-- MODEL


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        tab =
            locationToTab location
    in
        ( initialModel flags.apiBaseUrl location, fetchForTab flags.apiBaseUrl tab )


initialModel : String -> Navigation.Location -> Model
initialModel apiBaseUrl location =
    { apiBaseUrl = apiBaseUrl
    , tab = locationToTab location
    , vendorsLoadStatus = NotLoaded
    , vendorRows = []
    , newVendorForm = Form.initial [] validateNewVendorForm
    , newVendorRecordState = Editing
    , purchasesLoadStatus = NotLoaded
    , purchaseRows = []
    , newPurchaseForm = Form.initial [ initialNewPurchase ] validateNewPurchaseForm
    , newPurchaseRecordState = Editing
    , dateRangeForm = Form.initial [] validateDateRange
    , reportVendorStates = Dict.empty
    , error = Nothing
    }


initialNewPurchase : ( String, Field.Field )
initialNewPurchase =
    ( "lineItems", Field.list [ Field.group [] ] )


locationToTab : Navigation.Location -> Tab
locationToTab location =
    if location.hash == vendorsTabHash then
        VendorsTab
    else if location.hash == reportTabHash then
        ReportTab
    else
        PurchasesTab


purchasesTabHash : String
purchasesTabHash =
    "#purchases"


vendorsTabHash : String
vendorsTabHash =
    "#vendors"


reportTabHash : String
reportTabHash =
    "#report"


validateNewVendorForm : Validation () NewVendor
validateNewVendorForm =
    map2 NewVendor
        (field "name" string |> andThen nonEmpty)
        (field "number" string |> andThen nonEmpty)


validateVendorRow : Validation () Vendor
validateVendorRow =
    map4 Vendor
        (field "id" int |> andThen (minInt 1))
        (field "name" string |> andThen nonEmpty)
        (field "number" string |> andThen nonEmpty)
        (field "active" bool)


validateLineItem : Validation () LineItem
validateLineItem =
    map2 LineItem
        (field "vendorId" int |> andThen (minInt 1))
        (field "price" float |> andThen (minFloat 0.01))


validateNewPurchaseForm : Validation () NewPurchase
validateNewPurchaseForm =
    map2 NewPurchase
        (field "lineItems" (list validateLineItem))
        (field "paymentMethod" string |> andThen nonEmpty)


validatePurchaseRow : Validation () Purchase
validatePurchaseRow =
    map4 Purchase
        (field "id" int |> andThen (minInt 1))
        (field "lineItems" (list validateLineItem))
        (field "paymentMethod" string |> andThen nonEmpty)
        (field "time" date)


validateDateRange : Validation () DateRange
validateDateRange =
    map2 DateRange
        (field "start" date)
        (field "end" date)


buildVendorRow : Vendor -> VendorRow
buildVendorRow vendor =
    let
        fields =
            [ ( "id", Field.string (toString vendor.id) )
            , ( "name", Field.string vendor.name )
            , ( "number", Field.string vendor.number )
            , ( "active", Field.bool vendor.active )
            ]
    in
        { vendorId = vendor.id
        , name = vendor.name
        , form = Form.initial fields validateVendorRow
        , state = Viewing
        }


buildPurchaseRow : Purchase -> PurchaseRow
buildPurchaseRow purchase =
    let
        buildLineItem lineItem =
            Field.group
                [ ( "vendorId", Field.string (toString lineItem.vendorId) )
                , ( "price", Field.string (toString lineItem.price) )
                ]

        fields =
            [ ( "id", Field.string (toString purchase.id) )
            , ( "paymentMethod", Field.string purchase.paymentMethod )
            , ( "time", Field.string (toString purchase.time) )
            , ( "lineItems"
              , (Field.list (List.map buildLineItem purchase.lineItems))
              )
            ]
    in
        { purchaseId = purchase.id
        , paymentMethod = purchase.paymentMethod
        , lineItems = purchase.lineItems
        , time = purchase.time
        , form = Form.initial fields validatePurchaseRow
        , state = Viewing
        }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        UrlChange location ->
            let
                newTab =
                    locationToTab location
            in
                ( { model | tab = newTab }, fetchForTab model.apiBaseUrl newTab )

        FetchVendorsComplete result ->
            case result of
                Err _ ->
                    ( { model | error = Just "Loading vendors failed" }, Cmd.none )

                Ok vendors ->
                    let
                        sortedVendors =
                            sorted vendors

                        reportVendorStates =
                            List.map (\v -> ( v.id, Collapsed )) sortedVendors
                                |> Dict.fromList
                    in
                        ( { model
                            | vendorsLoadStatus = Loaded
                            , vendorRows = (List.map buildVendorRow sortedVendors)
                            , reportVendorStates = reportVendorStates
                            , error = Nothing
                          }
                        , Cmd.none
                        )

        NewVendorFormMsg formMsg ->
            case ( formMsg, Form.getOutput model.newVendorForm ) of
                ( Form.Submit, Just request ) ->
                    ( { model | newVendorRecordState = Saving }, createVendor model.apiBaseUrl request )

                _ ->
                    ( { model | newVendorForm = Form.update validateNewVendorForm formMsg model.newVendorForm }, Cmd.none )

        NewVendorSaveComplete result ->
            case result of
                Err _ ->
                    ( { model
                        | newVendorRecordState = Editing
                        , error = Just "Adding vendor failed"
                      }
                    , Cmd.none
                    )

                Ok vendor ->
                    let
                        ( updatedModel, cmd ) =
                            update (NewVendorFormMsg (Form.Reset [])) model

                        row =
                            buildVendorRow vendor
                    in
                        ( { updatedModel
                            | newVendorRecordState = Editing
                            , vendorRows = row :: model.vendorRows
                            , error = Nothing
                          }
                        , cmd
                        )

        VendorRowEdit id ->
            let
                updateEntry t =
                    if t.vendorId == id then
                        { t | state = Editing }
                    else
                        t
            in
                ( { model | vendorRows = List.map updateEntry model.vendorRows }, Cmd.none )

        VendorRowFormMsg id formMsg ->
            let
                maybeVendorRow =
                    List.filter (\v -> v.vendorId == id) model.vendorRows
                        |> List.head

                maybeRequest =
                    Maybe.map .form maybeVendorRow
                        |> Maybe.map Form.getOutput
            in
                case ( formMsg, maybeVendorRow, maybeRequest ) of
                    ( Form.Submit, Just vendorRow, Just (Just vendor) ) ->
                        let
                            updateEntry v =
                                if v.vendorId == id then
                                    { v | state = Saving }
                                else
                                    v
                        in
                            ( { model | vendorRows = List.map updateEntry model.vendorRows }
                            , saveVendor model.apiBaseUrl id vendor
                            )

                    _ ->
                        let
                            updateEntry v =
                                if v.vendorId == id then
                                    { v | form = Form.update validateVendorRow formMsg v.form }
                                else
                                    v
                        in
                            ( { model | vendorRows = List.map updateEntry model.vendorRows }, Cmd.none )

        VendorRowSaveComplete id result ->
            let
                updateEntry newState v =
                    if v.vendorId == id then
                        { v | state = newState }
                    else
                        v
            in
                case result of
                    Err _ ->
                        ( { model
                            | vendorRows = List.map (updateEntry Editing) model.vendorRows
                            , error = Just "Updating vendor failed"
                          }
                        , Cmd.none
                        )

                    Ok vendor ->
                        ( { model | vendorRows = List.map (updateEntry Viewing) model.vendorRows }, Cmd.none )

        FetchPurchasesComplete result ->
            case result of
                Err _ ->
                    ( { model | error = Just "Loading purchases failed" }, Cmd.none )

                Ok purchaseRows ->
                    ( { model
                        | purchasesLoadStatus = Loaded
                        , purchaseRows = (List.map buildPurchaseRow purchaseRows)
                        , error = Nothing
                      }
                    , Cmd.none
                    )

        NewPurchaseFormMsg formMsg ->
            case ( formMsg, Form.getOutput model.newPurchaseForm ) of
                ( Form.Submit, Just request ) ->
                    ( { model | newPurchaseRecordState = Saving }, createPurchase model.apiBaseUrl request )

                _ ->
                    ( { model | newPurchaseForm = Form.update validateNewPurchaseForm formMsg model.newPurchaseForm }, Cmd.none )

        NewPurchaseSaveComplete result ->
            case result of
                Err _ ->
                    ( { model
                        | newPurchaseRecordState = Editing
                        , error = Just "Saving purchase failed"
                      }
                    , Cmd.none
                    )

                Ok purchase ->
                    let
                        ( updatedModel, cmd ) =
                            update (NewPurchaseFormMsg (Form.Reset [ initialNewPurchase ])) model

                        row =
                            buildPurchaseRow purchase
                    in
                        ( { updatedModel
                            | newPurchaseRecordState = Editing
                            , purchaseRows = row :: model.purchaseRows
                            , error = Nothing
                          }
                        , cmd
                        )

        PurchaseRowEdit id ->
            let
                updateEntry r =
                    if r.purchaseId == id then
                        { r | state = Editing }
                    else
                        r
            in
                ( { model | purchaseRows = List.map updateEntry model.purchaseRows }, Cmd.none )

        PurchaseRowMsgForm id formMsg ->
            let
                maybePurchaseRow =
                    List.filter (\v -> v.purchaseId == id) model.purchaseRows
                        |> List.head

                maybeRequest =
                    Maybe.map .form maybePurchaseRow
                        |> Maybe.map Form.getOutput
            in
                case ( formMsg, maybeRequest ) of
                    ( Form.Submit, Just (Just purchase) ) ->
                        let
                            updateEntry v =
                                if v.purchaseId == id then
                                    { v | state = Saving }
                                else
                                    v
                        in
                            ( { model | purchaseRows = List.map updateEntry model.purchaseRows }
                            , savePurchase model.apiBaseUrl id purchase
                            )

                    _ ->
                        let
                            updateEntry v =
                                if v.purchaseId == id then
                                    { v
                                        | form =
                                            Form.update validatePurchaseRow formMsg v.form
                                                |> Form.update validatePurchaseRow Form.Validate
                                    }
                                else
                                    v
                        in
                            ( { model | purchaseRows = List.map updateEntry model.purchaseRows }, Cmd.none )

        PurchaseRowSaveComplete id result ->
            case result of
                Err _ ->
                    let
                        updateEntry p =
                            if p.purchaseId == id then
                                { p | state = Editing }
                            else
                                p
                    in
                        ( { model
                            | purchaseRows = List.map updateEntry model.purchaseRows
                            , error = Just "Updating purchase failed"
                          }
                        , Cmd.none
                        )

                Ok purchase ->
                    let
                        updateEntry p =
                            if p.purchaseId == id then
                                buildPurchaseRow purchase
                            else
                                p
                    in
                        ( { model | purchaseRows = List.map updateEntry model.purchaseRows }, Cmd.none )

        DateRangeForm formMsg ->
            ( { model | dateRangeForm = Form.update validateDateRange formMsg model.dateRangeForm }, Cmd.none )

        ToggleReportVendorState vendorId ->
            let
                newState =
                    if (Dict.get vendorId model.reportVendorStates) == Just Collapsed then
                        Expanded
                    else
                        Collapsed
            in
                ( { model | reportVendorStates = Dict.insert vendorId newState model.reportVendorStates }, Cmd.none )


fetchForTab : String -> Tab -> Cmd Msg
fetchForTab apiBaseUrl tab =
    case tab of
        VendorsTab ->
            fetchVendors apiBaseUrl

        PurchasesTab ->
            Cmd.batch [ (fetchPurchases apiBaseUrl), (fetchVendors apiBaseUrl) ]

        ReportTab ->
            Cmd.batch [ (fetchPurchases apiBaseUrl), (fetchVendors apiBaseUrl) ]


sortKey : Vendor -> String
sortKey vendor =
    let
        activeString =
            if vendor.active then
                "0"
            else
                "1"
    in
        activeString ++ (String.toLower vendor.name)


sorted : List Vendor -> List Vendor
sorted vendors =
    List.sortBy sortKey vendors



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ navbar DefaultNavbar
            []
            [ navbarHeader []
                [ navbarBrand []
                    [ text "Mini Checkout"
                    ]
                ]
            , navbarCollapse []
                [ navbarList (NavbarNav)
                    NavbarLeft
                    []
                    [ menuItem (model.tab == PurchasesTab) purchasesTabHash "Purchases"
                    , menuItem (model.tab == VendorsTab) vendorsTabHash "Vendors"
                    , menuItem (model.tab == ReportTab) reportTabHash "Report"
                    ]
                ]
            ]
        , tabView model
        ]


menuItem : Bool -> String -> String -> Html Msg
menuItem active link title =
    let
        activeClass =
            if active then
                "active"
            else
                ""
    in
        li [ class activeClass ]
            [ a [ href link ] [ Html.text title ]
            ]


tabView : Model -> Html Msg
tabView model =
    case model.tab of
        VendorsTab ->
            Vendors.view model

        PurchasesTab ->
            Purchases.view model

        ReportTab ->
            Reports.view model



-- API


fetchVendors : String -> Cmd Msg
fetchVendors apiBaseUrl =
    Http.send FetchVendorsComplete (Api.fetchVendors apiBaseUrl)


createVendor : String -> NewVendor -> Cmd Msg
createVendor apiBaseUrl request =
    Http.send NewVendorSaveComplete (Api.createVendor apiBaseUrl request)


saveVendor : String -> Int -> Vendor -> Cmd Msg
saveVendor apiBaseUrl id vendor =
    Http.send (VendorRowSaveComplete id) (Api.updateVendor apiBaseUrl vendor)


fetchPurchases : String -> Cmd Msg
fetchPurchases apiBaseUrl =
    Http.send FetchPurchasesComplete (Api.fetchPurchases apiBaseUrl)


createPurchase : String -> NewPurchase -> Cmd Msg
createPurchase apiBaseUrl request =
    Http.send NewPurchaseSaveComplete (Api.createPurchase apiBaseUrl request)


savePurchase : String -> Int -> Purchase -> Cmd Msg
savePurchase apiBaseUrl id request =
    Http.send (PurchaseRowSaveComplete id) (Api.updatePurchase apiBaseUrl request)



-- SUBSCIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
