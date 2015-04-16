module Main where

-- from base:
import Control.Exception
import Control.Concurrent.MVar
import System.IO
import System.Environment
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
  [vendorIdStr, productIdStr] <- getArgs
  let vendorId  = read vendorIdStr
      productId = read productIdStr

  -- Initialization:
  ctx <- newCtx
  setDebug ctx PrintDebug

  -- Device retrieval:
  dev <- if ctx `hasCapability` HasHotplug
         then waitForMyDevice ctx vendorId productId
         else findMyDevice ctx vendorId productId

  -- Device usage:
  doSomethingWithDevice dev

waitForMyDevice :: Ctx -> VendorId -> ProductId -> IO Device
waitForMyDevice ctx vendorId productId = do
  putStrLn "Waiting for device attachment..."
  mv <- newEmptyMVar
  mask_ $ do
    h <- registerHotplugCallback ctx
                                 deviceArrived
                                 enumerate
                                 (Just vendorId)
                                 (Just productId)
                                 Nothing
                                 (\dev event ->
                                    tryPutMVar mv (dev, event) $>
                                      DeregisterThisCallback)
    void $ mkWeakMVar mv $ deregisterHotplugCallback h
  (dev, _event) <- takeMVar mv
  return dev

-- Enumeratie all devices and find the right one.
findMyDevice :: Ctx -> VendorId -> ProductId -> IO Device
findMyDevice ctx vendorId productId = do
    devs <- V.toList <$> getDevices ctx
    deviceDescs <- mapM getDeviceDesc devs
    case fmap fst $ find (match . snd) $ zip devs deviceDescs of
      Nothing  -> hPutStrLn stderr "Mouse not found" >> exitFailure
      Just dev -> return dev
  where
    match :: DeviceDesc -> Bool
    match devDesc =  deviceVendorId  devDesc == vendorId
                  && deviceProductId devDesc == productId

doSomethingWithDevice :: Device -> IO ()
doSomethingWithDevice dev = do
  putStrLn $ unlines $ deviceInfo dev

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
            timeout    = 5000

        printf "maxPacketSize = %i\n" mps

        putStrLn "Creating transfer..."
        readTrans <- newReadTransfer
                       InterruptTransfer
                       devHndl
                       (endpointAddress endpoint1)
                       0
                       timeout

        -- Performing I/O:
        let n = 3 :: Int
        forM_ [0..n-1] $ \i -> do
          let size = (2^i) * mps

          _ <- printf "(%i/%i) reading %i bytes during a maximum of %i ms...\n"
                      (i+1) n size timeout

          setReadTransferSize readTrans size

          (bs, status) <- performReadTransfer readTrans

          when (status == TimedOut) $ putStrLn "Reading timed out!"
          _ <- printf "Read %i bytes:\n" $ B.length bs
          printBytes bs

deviceInfo :: Device -> [String]
deviceInfo dev =
  [ printf "deviceSpeed:   %s" (maybe "-" show $ deviceSpeed dev)
  , printf "busNumber:     %s" (show $ busNumber dev)
  , printf "portNumber:    %s" (show $ portNumber dev)
  , printf "portNumbers:   %s" (maybe "-" (show . V.toList) $
                                  portNumbers dev 7)
  , printf "deviceAddress: %s" (show $ deviceAddress dev)
  ]

printBytes :: B.ByteString -> IO ()
printBytes = putStrLn . intercalate " " . map (printf "0x%02x") . B.unpack
