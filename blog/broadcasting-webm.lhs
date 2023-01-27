---
title: Broadcasting a WebM stream using MSE
date: 2023-01-27
tags:
  - haskell
  - blog
---

This post demonstrates a motivating use-case for my new [ebml] library:
an audio broadcasting server for WebSocket clients.
In three parts, I present:

- The key features of this use-case.
- The characteristics of a WebM stream.
- A client/server demo implementation.

This is the demo's architecture diagram where the code presented in this post implements the pink components:

```mermaid
graph LR
  subgraph Broadcast Station
    MIC[MC] -->|PCM| ADC
    SND[VLC] -->|PCM| ADC
   ADC[gstreamer encoder] -->|WebM| SRV(Haskell Server):::impl
  end
  subgraph Listeners
    SRV -->|WebSocket| U1(JS MediaSource):::impl -->|PCM| O1(Web Player)
    SRV -->|WebSocket| U2(JS MediaSource):::impl -->|PCM| O2(Web Player)
    SRV -->|WebSocket| U3(JS MediaSource):::impl -->|PCM| O3(Web Player)
  end
  classDef impl fill:#f9f,stroke:#333,stroke-width:2px
```

> This document is a literate haskell file.

---

## Key Features

This section introduces the context and the key features of this demo.

After experiencing the pain of working with native libraries for my [animation-fractal][af] project, libraries such as SDL or Vulkan,
I wanted to give the [Web API][web-api] a try for implementing multi-media applications.
Modern web browser implements a sizable set of features for such applications, and it looks like a great platform to work with.


### Overview

I set myself the following goals:

- Avoid service oriented architecture ([SOA]) and rely as much as possible on hypermedia. Learn more about this strategy in the [HATEOAS — An Alternative Explanation][hateoas].
- Use a single uniform HTTP API to handle all the requests.

Therefore, the key features of this demo are:

- The server controls the state and the clients act like dumb terminals.
- The application is self-contained and it can be served using a standard HTTP ingress.

As a first step, I am using WebSocket connections to handle dynamic interactions.
While this approach has acceptable performance, I am also looking forward using the
[WebTransport] API, a new API to replace WebSocket with HTTP/3 transport.
I believe this is going to be a great match for such embedded system.


### Use-Cases

Concretely, this demo can be used for:

- Adding audio to the [NoVNC] client, a JavaScript library to render a remote desktop within a modern browser.
- Implementing a voice chat system.

You can find alternative solutions presented on this [MDN page][mdn-stream], most notably:

- WebRTC: the main standard which relies on the SCTP protocol.
- RTSP: Real Time Streaming Protocol.
- DASH.js, HLS and IceCast: custom media formats that can be served over HTTP.
- Media Source Extensions (MSE): which allows JavaScript to generate media streams for playback.

I decided to use MSE because it looked like the most straight forward API to use with WebSockets.
The next section describes the rationals for using the WebM format.

## WebM Stream

Before diving into the characteristics of a WebM stream, let's see why using such format is necessary.


### Playing live audio

There are two options to play audio from JavaScript:

- [Web Audio API][web-audio-api]: a powerful API for controlling audio. It enables creating custom audio processing graph, for example to synthesize sound or play raw data.
- [Media Source API][media-source-api]: previously known as MSE, this API leverage the existing `<audio>` and `<video>` media element.

I initially used the first option with the [pcm-player] library.
This worked out of the box without any issues.
However this technique has a major drawback: it uses a lot of bandwidth.
Considering a stereo stream sampled at 44.1 kHZ, serving five clients requires: $5 * 2 * 44100 / 1024^2 = 0.42 MiB/sec$.

To reduce the footprint, the audio data must be compressed using a codec like [vorbis], or [opus].
Such codecs are designed to be contained inside a container format such as [ogg] or [WebM].
While it is possible to a use a custom decoder before the pcm-player, it is quite a bit of work for the Web Audio API.
At that point, it may be easier to use the native Media Source API, which already expects such encoded data.


### Container formats supported by MSE

In an early experiment, I used the [ogg] container because it is a simpler format that can be produced by the browser MediaRecorder.
It looked promising because the resulting buffers are valid frames starting with the correct `OggS` header.
Unfortunately, I learned the hard way that MSE can't playback this format.

According to the [Byte Stream Format Registry][mse-byte-stream-format-registry], MSE supports:

```mermaid
flowchart TB
    subgraph MPEG
        mpeg[audio/mpeg\naudio/aac]
    end
    subgraph MPEG-2
        MP2[audio/mp2t\nvideo/mp2t]
    end
    subgraph MP4[ISO BMFF]
       mp4[audio/mp4\nvideo/mp4]
    end
    subgraph WebM
        webm[audio/webm\nvideo/webm]
    end
```

I decided to use WebM because it is the most recent and, as the name suggests, it is optimized for the web.

### WebM format

WebM is based on the [Matroska] container, which implements the Extensible Binary Meta Language ([EBML]). The layout is surprisingly simple, each element are defined as:

```raw
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    | Element ID    | Element Size  | Element Data ...              |
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
```

The element ID and size are using a variable length encoding, similar to UTF-8.
Then the specification provides a [schema][ebml-schema] that defines how to interpret the data based on the element ID.
In the case of a WebM stream, some elements' size are unknown, and a look-ahead decoder must be used.
Thanks to the [binary] library, this was relatively easy to implement.

The main challenge is to properly align the data when serving a new client.
The [WebM Byte Stream Format][byte-stream-format-webm] describes the layout expected by MSE:

- Initialization segments must be provided before the first media segment. They contains the EBML header, and the beginning of the first segment.
- A media segment must start with a cluster element. Such segment contains the codec data such as the [opus] frames.

Therefore, a new client must be provided with the stream initialization segments along with the beginning of the most recent media segment.
And this is precisely the goal of the [StreamReader] of my [ebml] library.
The next section presents how that works in practice.

## Demo

This section introduces a standalone application that broadcasts a local audio stream to web clients through a WebSocket.


### Extension and packages

The program is written for GHC2021, here are the necessary extra extensions:

```haskell
-- small syntactic change
{-# LANGUAGE BlockArguments, OverloadedStrings, OverloadedRecordDot #-}
-- DataKinds is required by Servant API definition
{-# LANGUAGE DataKinds #-}
-- QuasiQuote is required for multi-line string with [s| ... |]
{-# LANGUAGE QuasiQuotes #-}
```

Here are the build dependencies:

```haskell
-- rio, text and bytestring, to extend the default Prelude
import RIO
import RIO.Process qualified as RIO
import Data.Text qualified as Text
import Data.ByteString qualified as BS

-- string-qq to embed the multiline javascript client
import Data.String.QQ (s)

-- servant and websocket to serve the http api
import Servant qualified
import Servant ((:-), (:>), Get, OctetStream, Context(EmptyContext), NamedRoutes)
import Servant.HTML.Lucid (HTML)
import Servant.API.WebSocket (WebSocket)
import Network.WebSockets qualified as WS
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai qualified as Wai

-- lucid to write html
import Lucid qualified

-- ebml, the new library to decode WebM stream
import Codec.EBML qualified as EBML
```

### Broadcast server

The server is defined as a broadcast channel of (raw data, frame):

```haskell
newtype BroadcastServer = BroadcastServer (TChan (ByteString, Maybe EBML.StreamFrame))

newBroadcastServer :: IO BroadcastServer
newBroadcastServer = BroadcastServer <$> newBroadcastTChanIO

broadcast :: BroadcastServer -> (ByteString, Maybe EBML.StreamFrame) -> IO ()
broadcast (BroadcastServer chan) buf = atomically $ writeTChan chan buf
```

Using gstreamer to record and encode the local audio, here is the thread that decodes
the WebM stream:

```haskell
audioStreamer :: BroadcastServer -> IO ()
audioStreamer srv = RIO.withProcessWait_ cmdProc (readProcess EBML.newStreamReader)
  where
    cmdProc = RIO.setStderr RIO.createPipe cmd
    cmd = "gst-launch-1.0 pulsesrc ! audioconvert ! opusenc ! webmmux ! fdsink fd=2"

    readProcess sr process = do
        buf <- BS.hGet (RIO.getStderr process) 512
        case EBML.feedReader buf sr of
            Left err -> error ("Stream failed: " <> show err)
            Right (mFrame, newSR) -> do
                broadcast srv (buf, mFrame)
                readProcess newSR process
```

### Broadcast client

A client is defined as a broadcast channel reader:

```haskell
newtype BroadcastClient = BroadcastClient (TChan (ByteString, Maybe EBML.StreamFrame))

newBroadcastClient :: BroadcastServer -> IO BroadcastClient
newBroadcastClient (BroadcastServer srv) = BroadcastClient <$> atomically (dupTChan srv)

recv :: BroadcastClient -> IO (ByteString, Maybe EBML.StreamFrame)
recv (BroadcastClient client) = atomically $ readTChan client
```

Here is the thread that serves a WebSocket client, waiting for a new media segment
to be received before forwarding the raw stream:

```haskell
audioClient :: BroadcastClient -> WS.Connection -> IO ()
audioClient srv client = sendHeader >> sendStream
  where
    sendHeader = do
        (_, mFrame) <- recv srv
        case mFrame of
            Just frame -> sendBuffer (frame.initialization <> frame.media)
            Nothing -> sendHeader

    sendStream = forever do
        (buf, _) <- recv srv
        sendBuffer buf

    sendBuffer = WS.sendBinaryData client
```

### Servant API

The API data defines three routes:

- `index` to serve the html body.
- `fav` to serve a favicon and avoid the spurious 404 error.
- `ws` to handle the WebSocket client.

```haskell
data API mode = API
    { index :: mode :- Get '[HTML] (Lucid.Html ())
    , fav   :: mode :- "favicon.ico" :> Get '[OctetStream] ByteString
    , ws    :: mode :- "ws" :> WebSocket
    }
    deriving (Generic)
```

Here is the server implementation:

```haskell
app :: BroadcastServer -> Wai.Application
app srv = Servant.serveWithContext (Proxy @(NamedRoutes API)) EmptyContext API
    { index = pure $ Lucid.doctypehtml_ do
        Lucid.head_ do
            Lucid.title_ "Broadcasing WebM demo"
        Lucid.body_ do
            "Welcome!"
            Lucid.with Lucid.ul_ [Lucid.id_ "log"] mempty
            Lucid.script_ jsScript
    , fav = pure ""
    , ws = \client -> liftIO $ WS.withPingThread client 30 (pure ()) do
        broadcastClient <- newBroadcastClient srv
        audioClient broadcastClient client
    }
```

Finally, the main entry-point creates the broadcast channel, and starts both the
audio streamer and the HTTP api servant threads:

```haskell
main :: IO ()
main = do
    srv <- newBroadcastServer
    race_ (audioStreamer srv) do
        putStrLn "Serving to http://localhost:8000"
        Warp.run 8000 (app srv)
```

### JavaScript client

:::{.hidden}
The JavaScript code is inlined as a multi-line string.

```haskell
jsClient :: Text
jsClient = [s|
```
:::

The welcome page contains the following script to handle the stream connection and
feed the media source buffer. This is a minimal, standalone implementation embedded in the
server code.

Here is the transport implementation:

```javascript
// Create the websocket:
const connect = () => {
  const skt = new WebSocket("ws://" + window.location.host + "/ws")
  skt.binaryType = "arraybuffer"
  skt.onopen = log("ws open")
  skt.onmessage = event => playBuffer(new Uint8Array(event.data))
  skt.onclose = reconnect("ws closed")
  skt.onerror = reconnect("ws error")
  log("Connecting")(skt)
}
const reconnect = msg => ev => {
  log(msg)(ev)
  setTimeout(connect, 1e3)
}

// Implement the buffering logic:
const buffer = { chunks: [], dst: null }
const playBuffer = arr => {
  if (buffer.dst && !buffer.dst.updating) {
    flushChunks()
    buffer.dst.appendBuffer(arr)
  } else {
    buffer.chunks.push(arr)
  }
}
const flushChunks = () => {
  if (buffer.chunks.length > 0) {
    log("Flusing chunks")(buffer.chunks.length)
    buffer.dst.appendBuffer(concatBuffers(buffer.chunks))
    buffer.chunks = []
  }
}
```

Here is the MSE implementation:

```javascript
// Create the audio element:
const listen = () => {
  const audio = document.createElement("audio")
  audio.autoplay = true
  audio.onerror = log("audio error")
  audio.onplay = connect

  // Create the media source.
  const mediaSource = new MediaSource()
  mediaSource.onsourceopen = () => {
    // Create the source buffer.
    buffer.dst = mediaSource.addSourceBuffer("audio/webm; codecs=opus")
    buffer.dst.mode = "sequence"
    log("Media source created")(buffer.dst)
    buffer.dst.onerror = log("source buffer error")

    // If the buffer ends, flush the cached chunks.
    buffer.dst.onupdateend = () => flushChunks()

    // Try to start playing the audio.
    audio.play().then(() => {}, askAutoPlay(audio))
  }

  // Attach the media source to the audio.
  audio.src = URL.createObjectURL(mediaSource)
}
```

And here are some helper functions:

```javascript
// Log message to the body.
const log = msg => obj => {
  console.log(msg, obj)
  const item = document.createElement('li')
  item.appendChild(document.createTextNode(msg + ": " + JSON.stringify(obj)))
  document.getElementById("log").prepend(item)
}

// Merge a list of array.
const concatBuffers = xs => {
  const size = xs.reduce((acc, x) => acc + x.length, 0)
  const res = new Uint8Array(size)
  let pos = 0
  xs.forEach(x => {
    res.set(x, pos)
    pos += x.length
  })
  return res
}

// Workaround for https://github.com/w3c/autoplay/issues/13
const askAutoPlay = audio => () => {
  alert(`AutoPlay is not available, please click anywhere to start listening. Setup auto-play with:
    - Press Ctrl-I
    - Click on the Permissions tab
    - Unmark the Audoplay 'Use Default' checkbox
    - Select the 'Allow Audio' toggle
  `)
  document.onclick = () => { audio.play(); }
}

listen()
```

:::{.hidden}
A small helper to remove `markdown-unlit` metadata:

```haskell
|]

jsScript :: Text
jsScript = cleanup jsClient
  where
    cleanup = Text.unlines . filter removeMarkdownMarker . Text.lines
    removeMarkdownMarker = not . Text.isPrefixOf "#line "
```
:::

Once the service is running, you can use the `qpwgraph` tool to configure the I/O,
for example using such graph:

![qpwgraph-gstreamer](../static/qpwgraph-gstreamer.png)

> Run the application with:
> nix develop .#gstreamer --command ghcid --command "ghci -XGHC2021 -pgmL markdown-unlit -optL haskell -optL javascript" --test=:main broadcasting-webm.lhs

## Conclusion

It wasn't obvious that this implementation would work in practice.
I had to study the EBML format and use trial and errors as a means of understanding MSE.
In the end, I am happy with the results, Haskell once again proved its efficiency:
the service runs steadily and its overhead is barely noticeable.

The [Web API][web-api] offers a fully featured application environment,
and using the different APIs is a great learning opportunity.
I am looking forward the new JavaScript and Web-Assembly backend
that are coming with GHC 9.6 to better leverage this environment.

Cheers!

[af]: https://gitlab.com/TristanCacqueray/animation-fractal
[ebml]: https://hackage.haskell.org/package/ebml
[web-api]: https://developer.mozilla.org/en-US/docs/Web/API
[SOA]: https://en.wikipedia.org/wiki/Service-oriented_architecture
[hateoas]: https://htmx.org/essays/hateoas/
[WebTransport]: https://developer.mozilla.org/en-US/docs/Web/API/WebTransport_API
[NoVNC]: https://novnc.com
[mdn-stream]: https://developer.mozilla.org/en-US/docs/Web/Guide/Audio_and_video_delivery/Live_streaming_web_audio_and_video
[web-audio-api]: https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API
[media-source-api]: https://developer.mozilla.org/en-US/docs/Web/API/Media_Source_Extensions_API
[pcm-player]: https://github.com/samirkumardas/pcm-player/
[vorbis]: https://xiph.org/vorbis/
[opus]: https://opus-codec.org/
[ogg]: https://xiph.org/ogg/
[WebM]: https://www.webmproject.org/
[mse-byte-stream-format-registry]: https://w3c.github.io/mse-byte-stream-format-registry/
[Matroska]: https://www.matroska.org/
[ebml]: https://github.com/ietf-wg-cellar/ebml-specification/blob/master/specification.markdown
[ebml-schema]: https://github.com/ietf-wg-cellar/matroska-specification/blob/master/ebml_matroska.xml
[binary]: https://hackage.haskell.org/package/binary
[byte-stream-format-webm]: https://w3c.github.io/mse-byte-stream-format-webm/
[StreamReader]: https://hackage.haskell.org/package/ebml-0.1.0.0/docs/Codec-EBML.html#g:3
