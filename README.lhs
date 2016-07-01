# [Cherry Top][]

Cherry Top controls [BlinkStick][]s.

Example usage as an executable:

``` shell
# cherry-top SERIAL-NUMBER CHANNEL INDEX RED GREEN BLUE
$ cherry-top BS000000-0.0 0x00 0x00 0x00 0x00 0x00
```

Example usage as a library:

<!--
This code is necessary for the example to work, but it isn't necessary for a
reader to figure out what's going on. Putting it inside a comment hides it from
the latter but not from the former.
``` .haskell
import qualified CherryTop
import qualified Control.Monad
import qualified Data.Text
import qualified System.USB
main :: IO ()
main = do
```
-->

``` .haskell
  context <- System.USB.newCtx

  -- Print out every BlinkStick's serial number.
  blinkSticks <- CherryTop.getBlinkSticks context
  Control.Monad.forM_ blinkSticks (\ blinkStick -> do
    System.USB.withDeviceHandle blinkStick (\ handle -> do
      serialNumber <- CherryTop.getSerialNumber handle
      print serialNumber))

  -- Find a BlinkStick by its serial number and turn the LED off.
  Control.Monad.void (do
    let serialNumber = Data.Text.pack "BS000000-0.0"
    CherryTop.withBlinkStick context serialNumber (\ blinkStick -> do
      let channel = 0x00
      let index = 0x00
      let red = 0x00
      let green = 0x00
      let blue = 0x00
      CherryTop.setColor blinkStick channel index red green blue))
```

[Cherry Top]: https://github.com/tfausak/cherry-top
[BlinkStick]: https://www.blinkstick.com
