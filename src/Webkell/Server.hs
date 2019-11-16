{-# LANGUAGE TypeApplications #-}

module Webkell.Server where

-- base
import qualified Data.List as List

-- bytestring
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Char8      as ASCII
import qualified Data.ByteString.Lazy.Char8 as LASCII
import qualified Data.ByteString.Builder    as BSB

-- network
import           Network.Socket                 ( Socket )
import qualified Network.Socket.ByteString      as Socket
import qualified Network.Socket.ByteString.Lazy as LSocket


-- network-simple
import qualified Network.Simple.TCP as NS


port :: String
port = "8000"

server :: (Socket -> IO ()) -> IO a
server f = do
    putStrLn $ "Launching Server on localhost:" <> port
    NS.serve NS.HostAny port  $ \(socket, _socketAddress) ->
        f socket

helloResponse_byteString :: BS.ByteString
helloResponse_byteString =
    asciiLines
        [ "HTTP/1.1 200 OK"
        , "Content-Type: text/plain; charset-us-ascii"
        , "Content-Length: 7"
        , ""
        , "Hello!\n"
        ]

asciiLines :: [String] -> BS.ByteString
asciiLines xs = ASCII.pack (List.intercalate "\r\n" xs)

sayHello :: Socket -> IO ()
sayHello socket =
    Socket.sendAll socket helloResponse_byteString


data Message startLine =
    Message startLine [HeaderField] (Maybe MessageBody)

data Request = Request RequestLine [HeaderField] (Maybe MessageBody)

data Response = Response StatusLine [HeaderField] (Maybe MessageBody)


-- for Request
data RequestLine =
    RequestLine Method RequestTarget HttpVersion
newtype Method = Method BS.ByteString
newtype RequestTarget = ReqeustTarget BS.ByteString


-- for Response
data StatusCode = StatusCode Digit Digit Digit
data StatusLine = StatusLine HttpVersion StatusCode ReasonPhrase
newtype ReasonPhrase = ReasonPhrase BS.ByteString


data HeaderField = HeaderField FieldName FieldValue
newtype FieldName = FieldName BS.ByteString
newtype FieldValue = FieldValue BS.ByteString

newtype MessageBody = MessageBody LBS.ByteString

data HttpVersion = HttpVersion Digit Digit

-- we can improve I think.
data Digit =
    D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9

encodeResponse :: Response -> BSB.Builder
encodeResponse (Response statusLine headerFields bodyMaybe) =
    encodeStatusLine statusLine
    <> encodeHeaderFieldList headerFields
    <> BSB.string7 "\r\n"
    <> foldMap @Maybe @BSB.Builder @MessageBody encodeMessageBody bodyMaybe

encodeHeaderFieldList :: [HeaderField] -> BSB.Builder
encodeHeaderFieldList headerFields = foldMap (\x -> encodeHeaderField x <> BSB.string8 "\r\n") headerFields

encodeStatusLine :: StatusLine -> BSB.Builder
encodeStatusLine (StatusLine httpVersion statusCode reasonPhrase) =
    encodeHttpVersion httpVersion
    <> BSB.string7 " "
    <> encodeStatusCode statusCode
    <> BSB.string7 " "
    <> encodeReasonPhrase reasonPhrase
    <> BSB.string7 "\r\n"

encodeStatusCode :: StatusCode -> BSB.Builder
encodeStatusCode (StatusCode x y z) =
    encodeDigit x <> encodeDigit y <> encodeDigit z

encodeReasonPhrase :: ReasonPhrase -> BSB.Builder
encodeReasonPhrase (ReasonPhrase x) = BSB.byteString x

encodeHttpVersion :: HttpVersion -> BSB.Builder
encodeHttpVersion (HttpVersion x y) =
    BSB.string7 "HTTP/"
    <> encodeDigit x
    <> BSB.string7 "."
    <> encodeDigit y

digitChar :: Digit  -> Char
digitChar D0 = '0'
digitChar D1 = '1'
digitChar D2 = '2'
digitChar D3 = '3'
digitChar D4 = '4'
digitChar D5 = '5'
digitChar D6 = '6'
digitChar D7 = '7'
digitChar D8 = '8'
digitChar D9 = '9'

encodeDigit :: Digit -> BSB.Builder
encodeDigit d = BSB.string7 [digitChar d]


encodeHeaderField :: HeaderField -> BSB.Builder
encodeHeaderField
    (HeaderField (FieldName x) (FieldValue y)) =
        BSB.byteString x
        <> BSB.string7 ": "
        <> BSB.byteString y

encodeMessageBody :: MessageBody -> BSB.Builder
encodeMessageBody (MessageBody x) = BSB.lazyByteString x
