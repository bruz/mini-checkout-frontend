module Api exposing (..)

import Http
import Types exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Decode.Extra as ExtraDecode


jsonContentHeader : List Http.Header
jsonContentHeader =
    [ Http.header "Content-Type" "application/json"
    ]


vendorDecoder : Decode.Decoder Vendor
vendorDecoder =
    Decode.map4 Vendor
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "number" Decode.string)
        (Decode.field "active" Decode.bool)


vendorListDecoder : Decode.Decoder (List Vendor)
vendorListDecoder =
    Decode.list vendorDecoder


lineItemDecoder : Decode.Decoder LineItem
lineItemDecoder =
    Decode.map2 LineItem
        (Decode.field "vendor_id" Decode.int)
        (Decode.field "price" Decode.float)


purchaseDecoder : Decode.Decoder Purchase
purchaseDecoder =
    Decode.map4 Purchase
        (Decode.field "id" Decode.int)
        (Decode.field "line_items" (Decode.list lineItemDecoder))
        (Decode.field "payment_method" Decode.string)
        (Decode.field "time" ExtraDecode.date)


purchaseListDecoder : Decode.Decoder (List Purchase)
purchaseListDecoder =
    Decode.list purchaseDecoder


fetchVendors : String -> Http.Request (List Vendor)
fetchVendors baseUrl =
    let
        url =
            baseUrl ++ "vendors"
    in
        Http.get url vendorListDecoder


newVendorEncoder : NewVendor -> Encode.Value
newVendorEncoder vendor =
    Encode.object
        [ ( "name", Encode.string vendor.name )
        , ( "number", Encode.string vendor.number )
        ]


createVendor : String -> NewVendor -> Http.Request Vendor
createVendor baseUrl vendor =
    let
        url =
            baseUrl ++ "vendors"

        body =
            Http.jsonBody (newVendorEncoder vendor)
    in
        Http.request
            { method = "POST"
            , headers = jsonContentHeader
            , url = url
            , body = body
            , expect = Http.expectJson vendorDecoder
            , timeout = Nothing
            , withCredentials = False
            }


updateVendorEncoder : Vendor -> Encode.Value
updateVendorEncoder vendor =
    Encode.object
        [ ( "name", Encode.string vendor.name )
        , ( "number", Encode.string vendor.number )
        , ( "active", Encode.bool vendor.active )
        ]


updateVendor : String -> Vendor -> Http.Request Vendor
updateVendor baseUrl vendor =
    let
        url =
            baseUrl ++ "vendors" ++ "/" ++ (toString vendor.id)

        body =
            Http.jsonBody (updateVendorEncoder vendor)
    in
        Http.request
            { method = "PATCH"
            , headers = jsonContentHeader
            , url = url
            , body = body
            , expect = Http.expectJson vendorDecoder
            , timeout = Nothing
            , withCredentials = False
            }


fetchPurchases : String -> Http.Request (List Purchase)
fetchPurchases baseUrl =
    let
        url =
            baseUrl ++ "purchases"
    in
        Http.get url purchaseListDecoder


newPurchaseEncoder : NewPurchase -> Encode.Value
newPurchaseEncoder purchase =
    Encode.object
        [ ( "payment_method", Encode.string purchase.paymentMethod )
        , ( "line_items", Encode.list (List.map lineItemEncoder purchase.lineItems) )
        ]


updatePurchaseEncoder : Purchase -> Encode.Value
updatePurchaseEncoder purchase =
    Encode.object
        [ ( "id", Encode.int purchase.id )
        , ( "payment_method", Encode.string purchase.paymentMethod )
        , ( "line_items", Encode.list (List.map lineItemEncoder purchase.lineItems) )
        ]


lineItemEncoder : LineItem -> Encode.Value
lineItemEncoder lineItem =
    Encode.object
        [ ( "vendor_id", Encode.int lineItem.vendorId )
        , ( "price", Encode.float lineItem.price )
        ]


createPurchase : String -> NewPurchase -> Http.Request Purchase
createPurchase baseUrl purchase =
    let
        url =
            baseUrl ++ "purchases"

        body =
            Http.jsonBody (newPurchaseEncoder purchase)
    in
        Http.request
            { method = "POST"
            , headers = jsonContentHeader
            , url = url
            , body = body
            , expect = Http.expectJson purchaseDecoder
            , timeout = Nothing
            , withCredentials = False
            }


updatePurchase : String -> Purchase -> Http.Request Purchase
updatePurchase baseUrl purchase =
    let
        url =
            baseUrl ++ "purchases" ++ "/" ++ (toString purchase.id)

        body =
            Http.jsonBody (updatePurchaseEncoder purchase)
    in
        Http.request
            { method = "PATCH"
            , headers = jsonContentHeader
            , url = url
            , body = body
            , expect = Http.expectJson purchaseDecoder
            , timeout = Nothing
            , withCredentials = False
            }
