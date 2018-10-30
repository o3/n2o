{-# LANGUAGE LambdaCase, RecordWildCards #-}
{- | Naive implementation of HTTP server
TODO:
* graceful exit
-}
module Network.N2O.Web.Http ( runServer ) where

import Control.Concurrent
import Control.Exception (catch, finally, SomeException(..), bracket, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString
import Network.N2O.Types
import Network.N2O.Core
import Network.N2O.Web.WebSockets
import Prelude hiding (takeWhile)
import Data.Attoparsec.ByteString hiding (try)
import Data.CaseInsensitive
import qualified Network.WebSockets as WS

data HttpConf = HttpConf

data Resp = Resp
  { respCode :: Int
  , respHead :: [Header]
  , respBody :: BS.ByteString
  } deriving (Show)

mkResp = Resp { respCode = 200, respHead = [], respBody = BS.empty }

runServer :: String -> Int -> Context f a -> IO ()
runServer host port cx =
  withSocketsDo $ do
    addr <- resolve host (show port)
    bracket (open addr) close (acceptConnections HttpConf cx)
  where
    resolve host port = do
      let hints = defaultHints {addrSocketType = Stream, addrFlags = [AI_PASSIVE]}
      addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
      return addr
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      bind sock (addrAddress addr)
      listen sock 10
      return sock

acceptConnections :: HttpConf -> Context f a -> Socket -> IO ()
acceptConnections conf cx sock = do
  (handle, host_addr) <- accept sock
  forkIO (catch
            (talk conf cx handle host_addr `finally` close handle)
            (\e@(SomeException _) -> print e))
  acceptConnections conf cx sock

talk :: HttpConf -> Context f a -> Socket -> SockAddr -> IO ()
talk conf cx sock addr = do
  bs <- recv sock 4096
  let either = parseReq bs
  case either of
    Left resp -> sendResp conf sock resp
    Right req ->
      if needUpgrade req
        then do
          pending <- mkPending WS.defaultConnectionOptions sock req
          wsApp cx {cxReq = req} pending
        else fileResp (preparePath $ C.unpack $ reqPath req) (sendResp conf sock)
  where
    preparePath ('/':path) = path
    preparePath path = path

status = \case
  200 -> "OK"
  400 -> "Bad Request"
  404 -> "Not Found"
  500 -> "Internal Server Eror"
  _   -> ""

calcLen resp@Resp{..} = (C.pack "Content-Length", C.pack $ show $ BS.length respBody) : respHead

sendResp :: HttpConf -> Socket -> Resp -> IO ()
sendResp conf sock resp@Resp {..} = do
  let headers = fmap (\(k, v) -> k <> C.pack ": " <> v <> C.pack "\r\n") (calcLen resp)
      cmd = C.pack "HTTP/1.1 " <> C.pack (show respCode) <> C.pack " " <> C.pack (status respCode) <> C.pack "\r\n"
      x = cmd : headers
      y = x ++ [C.pack "\r\n", respBody]
  send sock (mconcat y)
  return ()

fileResp :: FilePath -> (Resp -> IO ()) -> IO ()
fileResp path respond = do
  res <- try (BS.readFile path)
  let (status, content) = case res of
                            Left e@(SomeException _) -> (404, C.pack "File Not Found")
                            Right content -> (200, content)
  respond $ mkResp {respCode = status, respBody = content}

parseReq :: BS.ByteString -> Either Resp Req
parseReq bs = case parseOnly reqParser bs of
                Left _ -> Left $ mkResp {respCode = 400}
                Right req -> Right req

needUpgrade :: Req -> Bool
needUpgrade req = case getHeader (C.pack "upgrade") (reqHead req) of
                    Nothing -> False
                    Just h -> (mk $ snd h) == (mk $ C.pack "websocket")

isKeepAlive :: Req -> Bool
isKeepAlive req = case getHeader (C.pack "connection") (reqHead req) of
                    Nothing -> False
                    Just h -> (mk $ snd h) == (mk $ C.pack "keep-alive")

getHeader :: BS.ByteString -> [Header] -> Maybe Header
getHeader _ [] = Nothing
getHeader k (h:hs)
  | (mk k) == (mk $ fst h) = Just h
  | otherwise = getHeader k hs

crlf = (||) <$> (==10) <*> (==13)
isSpace = (== 32)

reqParser :: Parser Req
reqParser = do
    cmd <- takeWhile1 $ not.isSpace
    skipWhile isSpace
    path <- takeWhile1 $ not.isSpace
    skipWhile isSpace
    ver <- takeWhile1 $ not.crlf
    skipWhile crlf
    headers <- many' headerParser
    skipWhile crlf
    takeByteString
    endOfInput
    return $ mkReq {reqMeth = cmd, reqPath = path, reqVers = ver, reqHead = headers }

headerParser :: Parser (BS.ByteString, BS.ByteString)
headerParser = do
    name <- takeWhile1 (\b -> b /= 58 && b /= 10 && b /= 13)
    skip (== 58)
    skipWhile isSpace
    val <- takeWhile1 $ not.crlf
    skipWhile crlf
    return (name, val)
