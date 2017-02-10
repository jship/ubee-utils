{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ( (<>) )
import Data.Function ( (&) )
import Network.HTTP.Simple
import Network.HTTP.Types.Status ( Status, ok200, statusCode, statusMessage )
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.ByteString ( ByteString )
import Options.Generic ( Generic, ParseRecord, getRecord )

baseRequest :: Request
baseRequest = defaultRequest
            & setRequestMethod "POST"
            & setRequestHost "192.168.0.1"
            & setRequestBasicAuth "user" "user"

userResetRequest :: Request
userResetRequest = baseRequest
                 & setRequestPath "/ToolNavigation.asp"
                 & setRequestBodyURLEncoded [ ("FactoryDefaultConfirm", "0")
                                            , ("RestoreDefaultsNo", "0x00")
                                            , ("UserResetYes", "0x01")
                                            ]

restoreWirelessDefaultsRequest :: Request
restoreWirelessDefaultsRequest = baseRequest
                               & setRequestPath "/goform/wifiRadio"
                               & setRequestBodyURLEncoded [ ("WirelessMacAddress", "0")
                                                          , ("Band", "2")
                                                          , ("NMode", "1")
                                                          , ("NSupReq", "0")
                                                          , ("NBandwidth", "20")
                                                          , ("ChannelNumber", "0")
                                                          , ("RegulatoryMode", "0")
                                                          , ("ObssCoexistence", "1")
                                                          , ("STBCTx", "0")
                                                          , ("restoreWirelessDefaults", "1")
                                                          , ("commitwlanRadio", "1")
                                                          , ("scanActions", "0")
                                                          ]

setPrimaryNetworkRequest :: String -> String -> Request
setPrimaryNetworkRequest theSsid thePw = baseRequest
                                       & setRequestPath "/goform/wifiPrimaryNetwork"
                                       & setRequestBodyURLEncoded [ ("ServiceSetIdentifier", S8.pack theSsid)
                                                                  , ("ClosedNetwork", "0")
                                                                  , ("ApIsolate", "0")
                                                                  , ("WpaPskAuth", "0")
                                                                  , ("Wpa2PskAuth", "1")
                                                                  , ("WpaEncryption", "2")
                                                                  , ("WpaPreSharedKey", S8.pack thePw)
                                                                  , ("ShowWpaKey", "0x01")
                                                                  , ("WpaRekeyInterval", "0")
                                                                  , ("GenerateWepKeys", "0")
                                                                  , ("WepKeysGenerated", "0")
                                                                  , ("commitwlanPrimaryNetwork", "1")
                                                                  , ("AutoSecurity", "2")
                                                                  , ("WpsName", "UbeeAP")
                                                                  , ("CfgDevPin", "53117090")
                                                                  , ("cfgAction", "0")
                                                                  , ("wpsActions", "0")
                                                                  , ("WpsStaPin", "")
                                                                  , ("WpsAuthStaMac", "")
                                                                  ]

data Command = Reset
             | Restore
             | Set { ssid :: String, pw :: String }
             deriving ( Generic, Show )

instance ParseRecord Command

sendRequest :: Request -> (Status -> IO ()) -> IO () -> IO ()
sendRequest request onFailure onSuccess = do
  status <- getResponseStatus <$> httpLBS userResetRequest
  if status /= ok200
    then onFailure status
    else onSuccess

logError :: Status -> IO ()
logError status = putStrLn $ "[ERROR]: Router reset request failed: " <> show httpCode <> " - " <> S8.unpack httpMsg where
  httpCode = statusCode status
  httpMsg = statusMessage status

parseCommand :: IO Command
parseCommand = getRecord "CLI for Ubee routers"

runCommand :: Command -> IO ()
runCommand Reset = sendRequest userResetRequest logError (pure ())
runCommand Restore = sendRequest restoreWirelessDefaultsRequest logError (pure ())
runCommand (Set theSsid thePw) = sendRequest restoreWirelessDefaultsRequest logError (pure ())

main :: IO ()
main =  parseCommand >>= runCommand
