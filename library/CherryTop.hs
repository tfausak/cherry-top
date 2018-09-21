-- | Cherry Top controls <https://www.blinkstick.com BlinkStick>s. It is a thin
-- wrapper around "System.USB".
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
  )
where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.ByteString as Bytes
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.Word as Word
import qualified System.Environment as Environment
import qualified System.USB as USB
import qualified Text.Read as Read


-- | With 0 args, prints out the serial number for each connected BlinkStick.
--
-- > $ cherry-top
-- > Just "BS012345-3.0"
--
-- With 6 args, sets the color of one LED on one BlinkStick.
--
-- > # cherry-top serial-number channel index red green blue
-- > $ cherry-top BS012345-3.0  0       1     2   3     4
--
-- Any other number of args is an error.
main :: IO ()
main = do
  context <- USB.newCtx
  USB.setDebug context USB.PrintWarnings

  args <- Environment.getArgs
  case args of

    [] -> do
      blinkSticks <- getBlinkSticks context
      Monad.forM_ blinkSticks
        $ \blinkStick -> USB.withDeviceHandle blinkStick $ \handle -> do
            serialNumber <- getSerialNumber handle
            print serialNumber

    [a, b, c, d, e, f] -> do
      let serialNumber = Text.pack a
      channel <- readWord8 b
      index <- readWord8 c
      red <- readWord8 d
      green <- readWord8 e
      blue <- readWord8 f
      Monad.void $ withBlinkStick context serialNumber $ \blinkStick ->
        setColor blinkStick channel index red green blue

    _ -> fail $ "Unexpected arguments: " <> show args


readWord8 :: Monad m => String -> m Word.Word8
readWord8 = either fail pure . Read.readEither


-- | Gets a BlinkStick by its serial number and performs some action with it.
-- Automatically closes the device's handle.
--
-- Returns 'Maybe.Nothing' if the BlinkStick could not be found. Otherwise
-- returns 'Maybe.Just' the result of performing the action with the
-- BlinkStick.
--
-- >>> context <- USB.newCtx
-- >>> let serialNumber = Text.pack "BS012345-3.0"
-- >>> let turnOff blinkStick = setColor blinkStick 0 0 0 0 0
-- >>> withBlinkStick context serialNumber turnOff
-- Nothing
withBlinkStick
  :: USB.Ctx
  -> Text.Text -- ^ serial number
  -> (USB.DeviceHandle -> IO a)
  -> IO (Maybe a)
withBlinkStick context serialNumber action = Exception.bracket
  (getBlinkStick context serialNumber)
  (maybe (pure ()) USB.closeDevice)
  (maybe (pure Nothing) (fmap Just . action))


-- | Gets a BlinkStick by its serial number.
--
-- This returns a 'USB.DeviceHandle' instead of a 'USB.Device' because reading
-- the device's serial number requires a handle. Make sure that you close the
-- handle with 'USB.closeDevice' when you are done with it. Alternatively, use
-- 'withBlinkStick'.
--
-- >>> context <- USB.newCtx
-- >>> let serialNumber = Text.pack "BS012345-3.0"
-- >>> getBlinkStick context serialNumber
-- Nothing
getBlinkStick
  :: USB.Ctx
  -> Text.Text -- ^ serial number
  -> IO (Maybe USB.DeviceHandle)
getBlinkStick context serialNumber = do
  blinkSticks <- getBlinkSticks context
  Vector.foldM (getBlinkStickStep serialNumber) Nothing blinkSticks


getBlinkStickStep
  :: Text.Text
  -> Maybe USB.DeviceHandle
  -> USB.Device
  -> IO (Maybe USB.DeviceHandle)
getBlinkStickStep serialNumber maybeHandle blinkStick = case maybeHandle of
  Just _ -> pure maybeHandle
  Nothing -> do
    handle <- USB.openDevice blinkStick
    sn <- getSerialNumber handle
    if sn == Just serialNumber
      then pure $ Just handle
      else do
        USB.closeDevice handle
        pure Nothing


-- | Gets all of the USB devices that are BlinkSticks.
--
-- >>> context <- USB.newCtx
-- >>> getBlinkSticks context
-- ...
getBlinkSticks :: USB.Ctx -> IO (Vector.Vector USB.Device)
getBlinkSticks context = do
  devices <- USB.getDevices context
  Vector.filterM (fmap isBlinkStick . USB.getDeviceDesc) devices


-- | Determines if a USB device is a BlinkStick.
--
-- >>> let notABlinkStick = USB.DeviceDesc (0, 0, 0, 0) 0 0 0 0 0 0 (0, 0, 0, 0) Nothing Nothing Nothing 0
-- >>> isBlinkStick notABlinkStick
-- False
--
-- >>> let aBlinkStick = USB.DeviceDesc (0, 0, 0, 0) 0 0 0 0 vendorId productId (0, 0, 0, 0) Nothing Nothing Nothing 0
-- >>> isBlinkStick aBlinkStick
-- True
isBlinkStick :: USB.DeviceDesc -> Bool
isBlinkStick description =
  (USB.deviceVendorId description == vendorId)
    && (USB.deviceProductId description == productId)


-- | BlinkStick's vendor ID.
--
-- >>> vendorId
-- 8352
vendorId :: Word.Word16
vendorId = 0x20a0


-- | BlinkStick's product ID.
--
-- >>> productId
-- 16869
productId :: Word.Word16
productId = 0x41e5


-- | Gets a BlinkStick's serial number.
--
-- The returned serial number will follow the format @"BSxxxxxx-m.n"@. For
-- example: @"BS012345-3.0"@. The @BS@ part is a literal that stands for
-- BlinkStick. The next 6 characters (@xxxxxx@) are a 0-padded integer like
-- @012345@. The final part (@m@ and @n@) is the version number, like @3.0@.
getSerialNumber :: USB.DeviceHandle -> IO (Maybe Text.Text)
getSerialNumber handle = do
  description <- USB.getDeviceDesc $ USB.getDevice handle
  case USB.deviceSerialNumberStrIx description of
    Nothing -> pure Nothing
    Just index -> Just <$> USB.getStrDescFirstLang handle index 12


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
  -> IO ()
setColor handle channel index red green blue = do
  let
    setup = USB.ControlSetup
      { USB.controlSetupRequestType = USB.Class
      , USB.controlSetupRecipient = USB.ToDevice
      , USB.controlSetupRequest = 0x09
      , USB.controlSetupValue = 0x0005
      , USB.controlSetupIndex = 0x0000
      }
    input = Bytes.pack [0x01, channel, index, red, green, blue]
    timeout = USB.noTimeout
  USB.writeControlExact handle setup input timeout
