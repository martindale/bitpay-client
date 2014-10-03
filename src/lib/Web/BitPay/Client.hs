{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}

module Web.BitPay.Client where

import qualified Control.Exception as Ex
import           Control.Lens
import qualified Control.Monad.Catch as Cx
import qualified Data.Aeson as Ae
import qualified Data.Aeson.Lens as Ae
import qualified Data.Binary as Bin
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16.Lazy as B16L
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import           Data.Monoid
import qualified Data.Text.Encoding as T
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as Time
import           Data.Typeable (Typeable)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4
import qualified Network.Haskoin.Crypto as Haskoin
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTPT
import qualified Web.BitPay.Types as BitPay

--------------------------------------------------------------------------------

listCurrencies :: Request [BitPay.CurrencyInfo]
listCurrencies = RequestCurrenciesList $ \resp -> do
    obj <- Ae.decode $ HTTP.responseBody resp
    val <- (obj :: Ae.Value) ^? Ae.key "data"
    fromJSON' val

createInvoice :: BitPay.InvoiceCreateOptions -> Request BitPay.Invoice
createInvoice a = RequestInvoiceCreate a $ \resp -> do
    Ae.decode $ HTTP.responseBody resp

--------------------------------------------------------------------------------

data Request a
  = RequestInvoiceCreate
       !BitPay.InvoiceCreateOptions
       (HTTP.Response BSL.ByteString -> Maybe a)
  | RequestCurrenciesList
       (HTTP.Response BSL.ByteString -> Maybe a)
  deriving (Functor)

--------------------------------------------------------------------------------

-- | Exception thrown in case the 'Response' received from BitPay.com isn't
-- recognized, which might mean the BitPay.com servers are misbehaving or
-- that the API has changed and this library doesn't support it yet.
data BadResponse = BadResponse !(HTTP.Response BSL.ByteString)
  deriving (Eq, Show, Typeable)

instance Ex.Exception BadResponse


--------------------------------------------------------------------------------
-- Internal stuff

data RequestMethod = GET | POST

requestRaw :: Request a -> (RequestMethod, BS.ByteString, Ae.Object)
requestRaw x = case x of
    RequestInvoiceCreate a _ -> (POST, "/invoices", toObject a)
    RequestCurrenciesList _ -> (GET, "/currencies", HM.empty)
  where
    toObject :: Ae.ToJSON a => a -> Ae.Object
    toObject a = let Ae.Object a' = Ae.toJSON a in a'

requestCont :: Request a -> HTTP.Response BSL.ByteString -> Maybe a
requestCont x = case x of
    RequestInvoiceCreate _ k -> k
    RequestCurrenciesList k -> k

--------------------------------------------------------------------------------

data UnsignedRequest a
   = UnsignedRequestGET BS.ByteString -- ^ Full URL
   | UnsignedRequestPOST BS.ByteString BS.ByteString -- ^ Full URL, body.

mkUnsignedRequest
  :: BS.ByteString   -- ^ URL prefix (e.g., "https://test.bitpay.com")
  -> Request a
  -> UnsignedRequest a
mkUnsignedRequest prefix req = case method of
    GET ->
       let bsQS = HTTPT.renderSimpleQuery True $ simpleQueryFromObject hmBody
       in UnsignedRequestGET (baseURL <> bsQS)
    POST ->
       let bsBody = BSL.toStrict $ Ae.encode hmBody
       in UnsignedRequestPOST baseURL bsBody
  where
    (method, baseURL, hmBody) =
       let (a, pathInfo, c) = requestRaw req
       in (a, prefix <> pathInfo, c)

--------------------------------------------------------------------------------

data SignedRequest a
   = SignedRequestGET Haskoin.PubKey Haskoin.Signature BS.ByteString
     -- ^ Public key, signature, full URL.
   | SignedRequestPOST Haskoin.PubKey Haskoin.Signature BS.ByteString BS.ByteString
     -- ^ Public key, signature, full URL, body.

mkSignedRequest
  :: Haskoin.PrvKey
  -> BS.ByteString   -- ^ URL prefix (e.g., "https://test.bitpay.com")
  -> Request a
  -> IO (SignedRequest a)
mkSignedRequest prvKey prefix req = do
    (method, baseURL, hmBody) <- getRequestDetails
    case method of
       GET -> do
          let bsQs = HTTPT.renderSimpleQuery True $ simpleQueryFromObject hmBody
              fullURL = baseURL <> bsQs
              sig = Haskoin.detSignMsg (Haskoin.hash256 fullURL) prvKey
          return $ SignedRequestGET pubKey sig fullURL
       POST -> do
          let bsBody = BSL.toStrict $ Ae.encode hmBody
              toHash = baseURL <> bsBody
              sig = Haskoin.detSignMsg (Haskoin.hash256 toHash) prvKey
          return $ SignedRequestPOST pubKey sig baseURL bsBody
  where
    pubKey = Haskoin.derivePubKey prvKey
    mkGuid = fmap (T.decodeUtf8 . UUID.toASCIIBytes) UUIDV4.nextRandom
    mkNonce = fmap (ceiling . Time.utcTimeToPOSIXSeconds) Time.getCurrentTime
    getRequestDetails = do
      nonce <- mkNonce
      let (method, pathInfo, hmBody) = requestRaw req
          baseURL = prefix <> pathInfo
          hmBody' = HM.insert "nonce" (Ae.toJSON (nonce :: Integer)) hmBody
      case method of
         GET -> return (method, baseURL, hmBody')
         POST -> do
            guid <- mkGuid
            return (method, baseURL, HM.insert "guid" (Ae.toJSON guid) hmBody')


--------------------------------------------------------------------------------
-- Running UnsignedRequest and SignedRequest

runUnsignedRequest
  :: BS.ByteString   -- ^ URL prefix (e.g., "https://test.bitpay.com")
  -> HTTP.Manager
  -> Request a
  -> IO a
runUnsignedRequest prefix mgr req = do
    let ureq = mkUnsignedRequest prefix req
    httpReq <- httpRequestFromUnsignedRequest ureq
    runHTTPRequest mgr httpReq (requestCont req)

runSignedRequest
  :: Haskoin.PrvKey
  -> BS.ByteString   -- ^ URL prefix (e.g., "https://test.bitpay.com")
  -> HTTP.Manager
  -> Request a
  -> IO a
runSignedRequest prvKey prefix mgr req = do
    sreq <- mkSignedRequest prvKey prefix req
    httpReq <- httpRequestFromSignedRequest sreq
    runHTTPRequest mgr httpReq (requestCont req)

runHTTPRequest
  :: HTTP.Manager
  -> HTTP.Request
  -> (HTTP.Response BSL.ByteString -> Maybe a)
  -> IO a
runHTTPRequest mgr httpReq k = do
    resp <- HTTP.httpLbs httpReq mgr
    case k resp of
       Nothing -> Cx.throwM (BadResponse resp)
       Just a  -> return a

--------------------------------------------------------------------------------
-- Building HTTP requests

httpRequestFromUnsignedRequest
  :: Cx.MonadThrow m => UnsignedRequest a -> m HTTP.Request
httpRequestFromUnsignedRequest ureq = case ureq of
    UnsignedRequestGET url -> mkBasicHTTPRequest GET url
    UnsignedRequestPOST url body -> do
       httpReq <- mkBasicHTTPRequest POST url
       return $ httpReq { HTTP.requestBody = HTTP.RequestBodyBS body }

httpRequestFromSignedRequest
  :: Cx.MonadThrow m => SignedRequest a -> m HTTP.Request
httpRequestFromSignedRequest sreq = case sreq of
    SignedRequestGET pubKey sig url -> do
       mkSignedHTTPRequest GET url sig pubKey
    SignedRequestPOST pubKey sig url body -> do
       httpReq <- mkSignedHTTPRequest POST url sig pubKey
       return $ httpReq { HTTP.requestBody = HTTP.RequestBodyBS body }


mkSignedHTTPRequest
  :: Cx.MonadThrow m
  => RequestMethod
  -> BS.ByteString
  -> Haskoin.Signature
  -> Haskoin.PubKey
  -> m HTTP.Request
mkSignedHTTPRequest method url sig pubKey = do
  req <- mkBasicHTTPRequest method url
  return $ req
     { HTTP.requestHeaders
         = ("x-signature", BSL.toStrict $ B16L.encode $ Bin.encode sig)
         : ("x-pubkey", BSL.toStrict $ B16L.encode $ Bin.encode pubKey)
         : HTTP.requestHeaders req
     }

mkBasicHTTPRequest
  :: Cx.MonadThrow m
  => RequestMethod
  -> BS.ByteString
  -> m HTTP.Request
mkBasicHTTPRequest method url = do
  req <- HTTP.parseUrl $ B8.unpack url
  return $ req
     { HTTP.method = case method of { GET -> "GET"; POST -> "POST" }
     , HTTP.requestHeaders
         = [ ("Content-Type", "application/json")
           , ("X-Accept-Version", "2.0.0") ]
     }

--------------------------------------------------------------------------------
-- Misc

simpleQueryFromObject :: Ae.Object -> HTTPT.SimpleQuery
simpleQueryFromObject hm =
    map (\(k,v) -> (T.encodeUtf8 k, BSL.toStrict (Ae.encode v)))
        (HM.toList hm)

fromJSON' :: Ae.FromJSON a => Ae.Value -> Maybe a
fromJSON' val = case Ae.fromJSON val of
   Ae.Success a -> Just a
   _ -> Nothing
