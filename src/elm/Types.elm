module Types exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)
import Form exposing (Form)
import Http
import Navigation


type alias Flags =
    { apiBaseUrl : String
    }


type alias Model =
    { apiBaseUrl : String
    , tab : Tab
    , vendorsLoadStatus : LoadStatus
    , vendorRows : List VendorRow
    , newVendorForm : Form () NewVendor
    , newVendorRecordState : RecordState
    , purchasesLoadStatus : LoadStatus
    , purchaseRows : List PurchaseRow
    , newPurchaseForm : Form () NewPurchase
    , newPurchaseRecordState : RecordState
    , dateRangeForm : Form () DateRange
    , reportVendorStates : Dict Int ExpandState
    , error : Maybe String
    }


type Tab
    = PurchasesTab
    | VendorsTab
    | ReportTab


type LoadStatus
    = NotLoaded
    | Loaded


type RecordState
    = Viewing
    | Editing
    | Saving


type ExpandState
    = Collapsed
    | Expanded


type alias NewPurchase =
    { lineItems : List LineItem
    , paymentMethod : String
    }


type alias Purchase =
    { id : Int
    , lineItems : List LineItem
    , paymentMethod : String
    , time : Date
    }


type alias PurchaseRow =
    { purchaseId : Int
    , paymentMethod : String
    , time : Date
    , lineItems : List LineItem
    , form : Form () Purchase
    , state : RecordState
    }


type alias LineItem =
    { vendorId : Int
    , price : Float
    }


type alias NewVendor =
    { name : String
    , number : String
    }


type alias Vendor =
    { id : Int
    , name : String
    , number : String
    , active : Bool
    }


type alias VendorRow =
    { vendorId : Int
    , name : String
    , form : Form () Vendor
    , state : RecordState
    }


type alias DateRange =
    { start : Date
    , end : Date
    }


type alias SalesTotal =
    { vendorId : Int
    , vendorName : String
    , total : Float
    }


type Msg
    = UrlChange Navigation.Location
    | FetchVendorsComplete (Result Http.Error (List Vendor))
    | NewVendorFormMsg Form.Msg
    | NewVendorSaveComplete (Result Http.Error Vendor)
    | VendorRowEdit Int
    | VendorRowFormMsg Int Form.Msg
    | VendorRowSaveComplete Int (Result Http.Error Vendor)
    | FetchPurchasesComplete (Result Http.Error (List Purchase))
    | NewPurchaseFormMsg Form.Msg
    | NewPurchaseSaveComplete (Result Http.Error Purchase)
    | PurchaseRowEdit Int
    | PurchaseRowMsgForm Int Form.Msg
    | PurchaseRowSaveComplete Int (Result Http.Error Purchase)
    | DateRangeForm Form.Msg
    | ToggleReportVendorState Int
