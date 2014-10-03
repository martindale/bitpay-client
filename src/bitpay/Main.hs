{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Network.Haskoin.Crypto as Haskoin
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Web.BitPay.Client as BitPay
import qualified Web.BitPay.Types as BitPay
import           Numeric (readHex)

--------------------------------------------------------------------------------

main :: IO ()
main = do
    [(prvKeyRaw,"")] <- fmap readHex getContents
    let Just prvKey = Haskoin.makePrvKey prvKeyRaw

    HTTP.withManager HTTP.tlsManagerSettings $ \mgr -> do
       let apiRoot = "https://test.bitpay.com"
           runUnsignedRequest = BitPay.runUnsignedRequest apiRoot mgr
           runSignedRequest = BitPay.runSignedRequest prvKey apiRoot mgr

       -- Running an unsigned request (works!)
       print =<< runUnsignedRequest BitPay.listCurrencies

       -- Running n signed request (doesn't quite work! missing tokens)
       let invoiceOpts = BitPay.defaultInvoiceCreateOptions 1.2 BitPay.EUR
       print =<< runSignedRequest (BitPay.createInvoice invoiceOpts)
