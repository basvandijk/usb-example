module Main where

-- from base:
import System.IO
import System.Exit
import Data.Functor
import Data.List
import Control.Monad
import Text.Printf

-- from bytestring:
import qualified Data.ByteString as B ( ByteString, length, unpack )

-- from vector:
import           Data.Vector      ( (!) )
import qualified Data.Vector as V ( toList )

-- from usb:
import System.USB

main :: IO ()
main = do
  -- Initialization:
  ctx <- newCtx
  setDebug ctx PrintInfo

  -- Device retrieval:
  dev <- if ctx `hasCapability` HasHotplug
         then waitForMyDevice ctx
         else findMyDevice ctx

  -- Device usage:
  doSomethingWithDevice dev

waitForMyDevice :: Ctx -> IO Device
waitForMyDevice ctx = do
  putStrLn "Waiting for device attachment..."
  (dev, _event) <- waitForFirstHotplugEvent
                     ctx
                     deviceArrived
                     enumerate
                     (Just myVendorId)
                     (Just myProductId)
                     Nothing -- match any device class
  return dev

-- Enumeratie all devices and find the right one.
findMyDevice :: Ctx -> IO Device
findMyDevice ctx = do
    devs <- V.toList <$> getDevices ctx
    deviceDescs <- mapM getDeviceDesc devs
    case fmap fst $ find (isMyMouse . snd) $ zip devs deviceDescs of
      Nothing  -> hPutStrLn stderr "Mouse not found" >> exitFailure
      Just dev -> return dev
  where
    isMyMouse :: DeviceDesc -> Bool
    isMyMouse devDesc =  deviceVendorId  devDesc == myVendorId
                      && deviceProductId devDesc == myProductId

myVendorId :: VendorId
myVendorId = 0x045e

myProductId :: ProductId
myProductId = 0x0040

doSomethingWithDevice :: Device -> IO ()
doSomethingWithDevice dev = do
  putStrLn "Opening device..."
  withDeviceHandle dev $ \devHndl -> do

    putStrLn "Detaching kernel driver..."
    withDetachedKernelDriver devHndl 0 $ do

      putStrLn "Claiming interface..."
      withClaimedInterface devHndl 0 $ do

        -- Inspecting descriptors:
        config0 <- getConfigDesc dev 0
        let interface0 = configInterfaces config0 ! 0
            alternate0 = interface0 ! 0
            endpoint1  = interfaceEndpoints alternate0 ! 0
            mps        = maxPacketSize $ endpointMaxPacketSize endpoint1

            nrOfBytesToRead = 20 * mps

            timeout = 5000

        -- Performing I/O:
        _ <- printf "Reading %i bytes during a maximum of %i ms...\n"
                    nrOfBytesToRead timeout

        (bs, status) <- readInterrupt devHndl
                                      (endpointAddress endpoint1)
                                      nrOfBytesToRead
                                      timeout

        when (status == TimedOut) $ putStrLn "Reading timed out!"
        _ <- printf "Read %i bytes:\n" $ B.length bs
        printBytes bs

printBytes :: B.ByteString -> IO ()
printBytes = putStrLn . intercalate " " . map show . B.unpack
