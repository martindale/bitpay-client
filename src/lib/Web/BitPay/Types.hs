{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.BitPay.Types where


import qualified Data.Aeson           as Ae
import           Data.Data            (Data, Typeable)
import qualified Data.Text            as T
import qualified Data.Time            as Time
import           Control.Applicative
import           GHC.Generics         (Generic)

--------------------------------------------------------------------------------

data InvoiceStatus
   = InvoiceStatusNew
   | InvoiceStatusPaid
   | InvoiceStatusConfirmed
   | InvoiceStatusComplete
   | InvoiceStatusExpired
   | InvoiceStatusInvalid
   deriving (Eq, Ord, Bounded, Enum, Show, Read, Data, Typeable, Generic)

instance Ae.FromJSON InvoiceStatus where
  parseJSON = Ae.withText "InvoiceStatus" $ \t -> case t of
    "new"       -> return InvoiceStatusNew
    "paid"      -> return InvoiceStatusPaid
    "confirmed" -> return InvoiceStatusConfirmed
    "complete"  -> return InvoiceStatusComplete
    "expired"   -> return InvoiceStatusExpired
    "invalid"   -> return InvoiceStatusInvalid
    _           -> fail "FromJSON InvoiceStatus: unexpected"

--------------------------------------------------------------------------------

data InvoiceError = InvoiceError
  { _invoiceErrorType :: !T.Text
  , _invoiceErrorMessage :: !T.Text
  } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)


instance Ae.FromJSON InvoiceError where
  parseJSON = Ae.withObject "InvoiceError" $ \o0 -> do
    v1 <- o0 Ae..: "error"
    flip (Ae.withObject "InvoiceError") v1 $ \o1 ->
      InvoiceError <$> o1 Ae..: "type"
                   <*> o1 Ae..: "message"

--------------------------------------------------------------------------------

data CurrencyInfo = CurrencyInfo
  { _currencyInfoCode :: !Currency
  , _currencyInfoSymbol :: !T.Text
  , _currencyInfoPrecision :: !Int
  , _currencyInfoExchangePctFee :: !Int
  , _currencyInfoPayoutEnabled :: !Bool
  , _currencyInfoName :: !T.Text
  , _currencyInfoPlural :: !T.Text
  , _currencyInfoAlts :: !T.Text
  , _currencyInfoPayoutFields :: ![T.Text]
  } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Ae.FromJSON CurrencyInfo where
  parseJSON = Ae.withObject "CurrencyInfo" $ \o0 -> do
    CurrencyInfo <$> o0 Ae..: "code"
                 <*> o0 Ae..: "symbol"
                 <*> o0 Ae..: "precision"
                 <*> o0 Ae..: "exchangePctFee"
                 <*> o0 Ae..: "payoutEnabled"
                 <*> o0 Ae..: "name"
                 <*> o0 Ae..: "plural"
                 <*> o0 Ae..: "alts"
                 <*> o0 Ae..: "payoutFields"

--------------------------------------------------------------------------------

data Currency
  = AED | AFN | ALL | AMD | ANG | AOA | ARS | AUD | AWG | AZN | BAM | BBD | BDT
  | BGN | BHD | BIF | BMD | BND | BOB | BRL | BSD | BTC | BTN | BWP | BYR | BZD
  | CAD | CDF | CHF | CLF | CLP | CNY | COP | CRC | CVE | CZK | DJF | DKK | DOP
  | DZD | EEK | EGP | ETB | EUR | FJD | FKP | GBP | GEL | GHS | GIP | GMD | GNF
  | GTQ | GYD | HKD | HNL | HRK | HTG | HUF | IDR | ILS | INR | IQD | ISK | JEP
  | JMD | JOD | JPY | KES | KGS | KHR | KMF | KRW | KWD | KYD | KZT | LAK | LBP
  | LKR | LRD | LSL | LTL | LVL | LYD | MAD | MDL | MGA | MKD | MMK | MNT | MOP
  | MRO | MUR | MVR | MWK | MXN | MYR | MZN | NAD | NGN | NIO | NOK | NPR | NZD
  | OMR | PAB | PEN | PGK | PHP | PKR | PLN | PYG | QAR | RON | RSD | RUB | RWF
  | SAR | SBD | SCR | SDG | SEK | SGD | SHP | SLL | SOS | SRD | STD | SVC | SYP
  | SZL | THB | TJS | TMT | TND | TOP | TRY | TTD | TWD | TZS | UAH | UGX | USD
  | UYU | UZS | VEF | VND | VUV | WST | XAF | XAG | XAU | XCD | XOF | XPF | YER
  | ZAR | ZMW | ZWL
  deriving (Eq, Ord, Bounded, Enum, Show, Read, Data, Typeable, Generic)

instance Ae.FromJSON Currency where
  parseJSON = Ae.withText "Currency" $ \t -> case reads (T.unpack t) of
    [(x,"")] -> return x
    _ -> fail "FromJSON Currency: unexpected"

instance Ae.ToJSON Currency where
  toJSON = Ae.toJSON . show

--------------------------------------------------------------------------------

data TransactionSpeed
  = TransactionSpeedHigh
  | TransactionSpeedMedium
  | TransactionSpeedLow
  deriving (Eq, Ord, Bounded, Enum, Show, Read, Data, Typeable, Generic)

instance Ae.ToJSON TransactionSpeed where
  toJSON x = Ae.toJSON $ case x of
    TransactionSpeedHigh   -> "high" :: T.Text
    TransactionSpeedMedium -> "medium"
    TransactionSpeedLow    -> "low"

--------------------------------------------------------------------------------

data InvoiceCreateOptions = InvoiceCreateOptions
  { _invoiceCreateOptionsOptionsOptsPrice :: !Double
  , _invoiceCreateOptionsOptionsOptsCurrency :: !Currency
  , _invoiceCreateOptionsOptionsOptsPosData :: !(Maybe T.Text)
  , _invoiceCreateOptionsOptionsOptsNotificationURL :: !(Maybe T.Text)
  , _invoiceCreateOptionsOptionsOptsTransactionSpeed :: !(Maybe TransactionSpeed)
  , _invoiceCreateOptionsOptionsOptsFullNotifications :: !(Maybe Bool)
  , _invoiceCreateOptionsOptionsOptsNotificationEmail :: !(Maybe T.Text)
  , _invoiceCreateOptionsOptionsOptsRedirectURL :: !(Maybe T.Text)
  , _invoiceCreateOptionsOptionsOptsOrderId :: !(Maybe T.Text)
  , _invoiceCreateOptionsOptionsOptsItemDesc :: !(Maybe T.Text)
  , _invoiceCreateOptionsOptionsOptsItemCode :: !(Maybe T.Text)
  , _invoiceCreateOptionsOptionsOptsPhysical :: !(Maybe Bool)
  , _invoiceCreateOptionsOptionsOptsBuyerName :: !(Maybe T.Text)
  , _invoiceCreateOptionsOptionsOptsBuyerAddress1 :: !(Maybe T.Text)
  , _invoiceCreateOptionsOptionsOptsBuyerAddress2 :: !(Maybe T.Text)
  , _invoiceCreateOptionsOptionsOptsBuyerCity :: !(Maybe T.Text)
  , _invoiceCreateOptionsOptionsOptsBuyerState :: !(Maybe T.Text)
  , _invoiceCreateOptionsOptionsOptsBuyerZip :: !(Maybe T.Text)
  , _invoiceCreateOptionsOptionsOptsBuyerCountry :: !(Maybe T.Text)
  , _invoiceCreateOptionsOptionsOptsBuyerEmail :: !(Maybe T.Text)
  , _invoiceCreateOptionsOptionsOptsBuyerPhone :: !(Maybe T.Text)
  } deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Ae.ToJSON InvoiceCreateOptions where
  toJSON ir = Ae.object
    [ "price"             Ae..= _invoiceCreateOptionsOptionsOptsPrice ir
    , "currency"          Ae..= _invoiceCreateOptionsOptionsOptsCurrency ir
    , "posData"           Ae..= _invoiceCreateOptionsOptionsOptsPosData ir
    , "notificationURL"   Ae..= _invoiceCreateOptionsOptionsOptsNotificationURL ir
    , "transactionSpeed"  Ae..= _invoiceCreateOptionsOptionsOptsTransactionSpeed ir
    , "fullNotifications" Ae..= _invoiceCreateOptionsOptionsOptsFullNotifications ir
    , "notificationEmail" Ae..= _invoiceCreateOptionsOptionsOptsNotificationEmail ir
    , "redirectURL"       Ae..= _invoiceCreateOptionsOptionsOptsRedirectURL ir
    , "orderID"           Ae..= _invoiceCreateOptionsOptionsOptsOrderId ir
    , "itemDesc"          Ae..= _invoiceCreateOptionsOptionsOptsItemDesc ir
    , "itemCode"          Ae..= _invoiceCreateOptionsOptionsOptsItemCode ir
    , "physical"          Ae..= _invoiceCreateOptionsOptionsOptsPhysical ir
    , "buyerName"         Ae..= _invoiceCreateOptionsOptionsOptsBuyerName ir
    , "buyerAddress1"     Ae..= _invoiceCreateOptionsOptionsOptsBuyerAddress1 ir
    , "buyerAddress2"     Ae..= _invoiceCreateOptionsOptionsOptsBuyerAddress2 ir
    , "buyerCity"         Ae..= _invoiceCreateOptionsOptionsOptsBuyerCity ir
    , "buyerState"        Ae..= _invoiceCreateOptionsOptionsOptsBuyerState ir
    , "buyerZip"          Ae..= _invoiceCreateOptionsOptionsOptsBuyerZip ir
    , "buyerCountry"      Ae..= _invoiceCreateOptionsOptionsOptsBuyerCountry ir
    , "buyerEmail"        Ae..= _invoiceCreateOptionsOptionsOptsBuyerEmail ir
    , "buyerPhone"        Ae..= _invoiceCreateOptionsOptionsOptsBuyerPhone ir
    ]

defaultInvoiceCreateOptions :: Double -> Currency -> InvoiceCreateOptions
defaultInvoiceCreateOptions price currency = InvoiceCreateOptions
    { _invoiceCreateOptionsOptionsOptsPrice             = price
    , _invoiceCreateOptionsOptionsOptsCurrency          = currency
    , _invoiceCreateOptionsOptionsOptsPosData           = Nothing
    , _invoiceCreateOptionsOptionsOptsNotificationURL   = Nothing
    , _invoiceCreateOptionsOptionsOptsTransactionSpeed  = Nothing
    , _invoiceCreateOptionsOptionsOptsFullNotifications = Nothing
    , _invoiceCreateOptionsOptionsOptsNotificationEmail = Nothing
    , _invoiceCreateOptionsOptionsOptsRedirectURL       = Nothing
    , _invoiceCreateOptionsOptionsOptsOrderId           = Nothing
    , _invoiceCreateOptionsOptionsOptsItemDesc          = Nothing
    , _invoiceCreateOptionsOptionsOptsItemCode          = Nothing
    , _invoiceCreateOptionsOptionsOptsPhysical          = Nothing
    , _invoiceCreateOptionsOptionsOptsBuyerName         = Nothing
    , _invoiceCreateOptionsOptionsOptsBuyerAddress1     = Nothing
    , _invoiceCreateOptionsOptionsOptsBuyerAddress2     = Nothing
    , _invoiceCreateOptionsOptionsOptsBuyerCity         = Nothing
    , _invoiceCreateOptionsOptionsOptsBuyerState        = Nothing
    , _invoiceCreateOptionsOptionsOptsBuyerZip          = Nothing
    , _invoiceCreateOptionsOptionsOptsBuyerCountry      = Nothing
    , _invoiceCreateOptionsOptionsOptsBuyerEmail        = Nothing
    , _invoiceCreateOptionsOptionsOptsBuyerPhone        = Nothing
    }

--------------------------------------------------------------------------------

data Invoice = Invoice
  { _invoiceId :: !T.Text
  , _invoiceURL :: !T.Text
  , _invoicePosData :: !T.Text
  , _invoiceStatus :: !InvoiceStatus
  , _invoicePrice :: !Double
  , _invoiceCurrency :: !Currency
  , _invoiceBTCPrice :: !Double
  , _invoiceInvoiceTime :: !Time.UTCTime
  , _invoiceExpirationTime :: !Time.UTCTime
  , _invoiceCurrentTime :: !Time.UTCTime
  } deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Ae.FromJSON Invoice where
  parseJSON = Ae.withObject "Invoice" $ \o ->
    Invoice <$> o Ae..: "id"
            <*> o Ae..: "url"
            <*> o Ae..: "posData"
            <*> o Ae..: "status"
            <*> o Ae..: "price"
            <*> o Ae..: "currency"
            <*> o Ae..: "btcPrice"
            <*> o Ae..: "invoiceTime"
            <*> o Ae..: "expirationTime"
            <*> o Ae..: "currentTime"


ensure :: String -> Bool -> Either String ()
ensure s False = Left ("Ensure that: " ++ s)
ensure _ True  = Right ()
