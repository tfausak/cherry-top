{-# LANGUAGE NoImplicitPrelude #-}

-- | Cherry Top controls <https://www.blinkstick.com BlinkStick>s.
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
import Data.Monoid ((<>))

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
import qualified Text.Show as Show


main
    :: IO.IO ()
main = do
    context <- USB.newCtx
    USB.setDebug context USB.PrintWarnings

    args <- Environment.getArgs
    case args of

        [] -> do
            blinkSticks <- getBlinkSticks context
            Monad.forM_ blinkSticks (\ blinkStick -> do
                USB.withDeviceHandle blinkStick (\ handle -> do
                    serialNumber <- getSerialNumber handle
                    IO.print serialNumber))

        [a, b, c, d, e, f] -> do
            let serialNumber = Text.pack a
            let channel = Read.read b
            let index = Read.read c
            let red = Read.read d
            let green = Read.read e
            let blue = Read.read f
            Monad.void (withBlinkStick context serialNumber (\ blinkStick -> do
                setColor blinkStick channel index red green blue))

        _ -> do
            Monad.fail ("Unexpected arguments: " <> Show.show args)


-- | Gets a BlinkStick by its serial number and performs some action with it.
-- Automatically closes the device's handle.
--
-- Returns 'Maybe.Nothing' if the BlinkStick could not be found. Otherwise
-- returns 'Maybe.Just' the result of performing the action with the
-- BlinkStick.
--
-- >>> context <- USB.newCtx
-- >>> let serialNumber = Text.pack "BS000000-0.0"
-- >>> let turnOff blinkStick = setColor blinkStick 0 0 0 0 0
-- >>> withBlinkStick context serialNumber turnOff
-- Nothing
withBlinkStick
    :: USB.Ctx
    -> Text.Text -- ^ serial number
    -> (USB.DeviceHandle -> IO.IO a)
    -> IO.IO (Maybe.Maybe a)
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


-- | Gets a BlinkStick by its serial number.
--
-- This returns a 'USB.DeviceHandle' instead of a 'USB.Device' because reading
-- the device's serial number requires a handle. Make sure that you close the
-- handle with 'USB.closeDevice' when you are done with it. Alternatively, use
-- 'withBlinkStick'.
--
-- >>> context <- USB.newCtx
-- >>> let serialNumber = Text.pack "BS000000-0.0"
-- >>> getBlinkStick context serialNumber
-- Nothing
getBlinkStick
    :: USB.Ctx
    -> Text.Text -- ^ serial number
    -> IO.IO (Maybe.Maybe USB.DeviceHandle)
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


-- | Gets all of the USB devices that are BlinkSticks.
--
-- >>> context <- USB.newCtx
-- >>> getBlinkSticks context
-- ...
getBlinkSticks
    :: USB.Ctx
    -> IO.IO (Vector.Vector USB.Device)
getBlinkSticks context = do
    let predicate device = do
            description <- USB.getDeviceDesc device
            Applicative.pure (isBlinkStick description)

    devices <- USB.getDevices context
    Vector.filterM predicate devices


-- | Determines if a USB device is a BlinkStick.
--
-- >>> let notABlinkStick = USB.DeviceDesc (0, 0, 0, 0) 0 0 0 0 0 0 (0, 0, 0, 0) Maybe.Nothing Maybe.Nothing Maybe.Nothing 0
-- >>> isBlinkStick notABlinkStick
-- False
--
-- >>> let aBlinkStick = USB.DeviceDesc (0, 0, 0, 0) 0 0 0 0 vendorId productId (0, 0, 0, 0) Maybe.Nothing Maybe.Nothing Maybe.Nothing 0
-- >>> isBlinkStick aBlinkStick
-- True
isBlinkStick
    :: USB.DeviceDesc
    -> Bool.Bool
isBlinkStick description = do
    USB.deviceVendorId description == vendorId &&
        USB.deviceProductId description == productId


-- | BlinkStick's vendor ID.
--
-- >>> vendorId
-- 8352
vendorId
    :: Word.Word16
vendorId = do
    0x20a0


-- | BlinkStick's product ID.
--
-- >>> productId
-- 16869
productId
    :: Word.Word16
productId = do
    0x41e5


-- | Gets a BlinkStick's serial number.
--
-- The returned serial number will follow the format @"BSxxxxxx-m.n"@. For
-- example: @"BS012345-3.0"@. The @BS@ part is a literal that stands for
-- "BlinkStick". The next 6 characters (@xxxxxx@) are a 0-padded integer like
-- @012345@. The final part (@m@ and @n@) is the version number, like @3.0@.
getSerialNumber
    :: USB.DeviceHandle
    -> IO.IO (Maybe.Maybe Text.Text)
getSerialNumber handle = do
    let device = USB.getDevice handle
    description <- USB.getDeviceDesc device

    case USB.deviceSerialNumberStrIx description of
        Maybe.Nothing -> do
            Applicative.pure Maybe.Nothing
        Maybe.Just index -> do
            serialNumber <- USB.getStrDescFirstLang handle index 12
            Applicative.pure (Maybe.Just serialNumber)


-- | Sets a BlinkStick's color.
--
-- The channel and index are usually both @0@. With a BlinkStick Nano, index
-- @0@ is the "top" LED and @1@ is the "bottom".
--
-- The red, green, and blue values can range from @0@ (dark) to @255@ (light).
-- So @0 0 0@ is black, @0 255 0@ is green, and @255 255 255@ is white.
setColor
    :: USB.DeviceHandle
    -> Word.Word8 -- ^ channel
    -> Word.Word8 -- ^ index
    -> Word.Word8 -- ^ red
    -> Word.Word8 -- ^ green
    -> Word.Word8 -- ^ blue
    -> IO.IO ()
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
