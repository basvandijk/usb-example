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

  -- Enumerating devices & finding the right device:
  devs <- V.toList <$> getDevices ctx
  deviceDescs <- mapM getDeviceDesc devs
  case fmap fst $ find (isMyMouse . snd) $ zip devs deviceDescs of
    Nothing -> hPutStrLn stderr "Mouse not found" >> exitFailure
    Just dev ->

      -- Opening the device:
      withDeviceHandle dev $ \devHndl ->
        withDetachedKernelDriver devHndl 0 $
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

isMyMouse :: DeviceDesc -> Bool
isMyMouse devDesc =  deviceVendorId  devDesc == 0x045e
                  && deviceProductId devDesc == 0x0040

printBytes :: B.ByteString -> IO ()
printBytes = putStrLn . intercalate " " . map show . B.unpack
