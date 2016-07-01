{-# LANGUAGE NoImplicitPrelude #-}

module CherryTop
    ( main
    , withBlinkStick
    , getBlinkStick
    , getBlinkSticks
    , isBlinkStick
    , vendorId
    , productId
    , getSerialNumber
    , setColor
    ) where

import Data.Bool ((&&))
import Data.Eq ((==))

import qualified Control.Applicative as Applicative
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Bool as Bool
import qualified Data.ByteString as Bytes
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.Word as Word
import qualified System.Environment as Environment
import qualified System.IO as IO
import qualified System.USB as USB
import qualified Text.Read as Read


main :: IO.IO ()
main = do
    [a, b, c, d, e, f] <- Environment.getArgs
    let serialNumber = Text.pack a
    let channel = Read.read b
    let index = Read.read c
    let red = Read.read d
    let green = Read.read e
    let blue = Read.read f

    context <- USB.newCtx
    USB.setDebug context USB.PrintWarnings

    Monad.void (withBlinkStick context serialNumber (\ blinkStick -> do
        setColor blinkStick channel index red green blue))


withBlinkStick :: USB.Ctx -> Text.Text -> (USB.DeviceHandle -> IO.IO a) -> IO.IO (Maybe.Maybe a)
withBlinkStick context serialNumber action = do
    let acquire = getBlinkStick context serialNumber
    let release maybeHandle = do
            case maybeHandle of
                Maybe.Nothing -> do
                    Applicative.pure ()
                Maybe.Just handle -> do
                    USB.closeDevice handle
    let perform maybeHandle = do
            case maybeHandle of
                Maybe.Nothing -> do
                    Applicative.pure Maybe.Nothing
                Maybe.Just handle -> do
                    result <- action handle
                    Applicative.pure (Maybe.Just result)

    Exception.bracket acquire release perform


getBlinkStick :: USB.Ctx -> Text.Text -> IO.IO (Maybe.Maybe USB.DeviceHandle)
getBlinkStick context serialNumber = do
    let step maybeHandle blinkStick = do
            case maybeHandle of
                Maybe.Just _handle -> do
                    Applicative.pure maybeHandle
                Maybe.Nothing -> do
                    handle <- USB.openDevice blinkStick
                    sn <- getSerialNumber handle
                    if sn == Maybe.Just serialNumber
                    then do
                        Applicative.pure (Maybe.Just handle)
                    else do
                        USB.closeDevice handle
                        Applicative.pure Maybe.Nothing

    blinkSticks <- getBlinkSticks context
    Vector.foldM step Maybe.Nothing blinkSticks


getBlinkSticks :: USB.Ctx -> IO.IO (Vector.Vector USB.Device)
getBlinkSticks context = do
    let predicate device = do
            description <- USB.getDeviceDesc device
            Applicative.pure (isBlinkStick description)

    devices <- USB.getDevices context
    Vector.filterM predicate devices


isBlinkStick :: USB.DeviceDesc -> Bool.Bool
isBlinkStick description = do
    USB.deviceVendorId description == vendorId &&
        USB.deviceProductId description == productId


vendorId :: Word.Word16
vendorId = 0x20a0


productId :: Word.Word16
productId = 0x41e5


getSerialNumber :: USB.DeviceHandle -> IO.IO (Maybe.Maybe Text.Text)
getSerialNumber handle = do
    let device = USB.getDevice handle
    description <- USB.getDeviceDesc device

    case USB.deviceSerialNumberStrIx description of
        Maybe.Nothing -> do
            Applicative.pure Maybe.Nothing
        Maybe.Just index -> do
            serialNumber <- USB.getStrDescFirstLang handle index 12
            Applicative.pure (Maybe.Just serialNumber)


setColor :: USB.DeviceHandle -> Word.Word8 -> Word.Word8 -> Word.Word8 -> Word.Word8 -> Word.Word8 -> IO.IO ()
setColor handle channel index red green blue = do
    let setup = USB.ControlSetup
            { USB.controlSetupRequestType = USB.Class
            , USB.controlSetupRecipient = USB.ToDevice
            , USB.controlSetupRequest = 0x09
            , USB.controlSetupValue = 0x0005
            , USB.controlSetupIndex = 0x0000
            }
    let input = Bytes.pack [0x01, channel, index, red, green, blue]
    let timeout = USB.noTimeout

    USB.writeControlExact handle setup input timeout
