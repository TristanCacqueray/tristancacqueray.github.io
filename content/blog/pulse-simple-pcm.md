---
title: Synchronizing Sound PCM with Image FPS
date: 2022-12-21
tags:
  - haskell
  - blog
---

This post introduces a couple of techniques I used to synchronize audio contents with the output of my [animation-fractal][af] project.
In two parts, I present:

- How to load audio data into Pulse-Code Modulation (PCM).
- An audio player to play the audio along the video's Frames Per Second (FPS).

The goal of this synchronization is to ensure that the video is related to the audio,
as demonstrated in this new demo:

:::{.flex .items-center .justify-center}
<iframe width="888" height="500" src="https://www.youtube.com/embed/ouoys1PABwA" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
:::
---

## Normalize the audio data

First, we need to decode the audio data into a raw format suitable for real-time playback.
The main requirement is to pick a fixed sample rate so that the data can be divided into per frame chunk.
In this case the video runs at 60 frames per second, so we can use the 44100 sampling frequency.
The resulting audio chunk size is: $44100/60 = 735$ samples.

Using the [typed-process] library to run ffmpeg, we decode a given file with this function:

```haskell
type Samples = VectorS.Vector Float

decodeFile :: FilePath -> IO Samples
decodeFile fname = do
    let args = ["-i", fname]    -- input
            <> ["-ac", "1"]     -- convert to mono
            <> ["-ar", "44100"] -- sample at 44100 (fit for 60 fps)
            <> ["-f", "f32le"]  -- use float
            <> ["-"]            -- output
    pcmBuf <- readProcessStdout_ $ proc "ffmpeg" args
    let (wordPtr, wordSZ) = toForeignPtr0 (from pcmBuf)
        pcmPtr = castForeignPtr wordPtr :: ForeignPtr Float
        pcmSZ = wordSZ `div` sizeOf (0 :: Float)
        samples = VectorS.unsafeFromForeignPtr0 pcmPtr pcmSZ
    pure samples
```

This function relies on [toForeignPtr0] from the [bytestring] library
to access the underlying memory. Then we convert it into a `Vector` using
[unsafeFromForeignPtr0].

[typed-process]: https://hackage.haskell.org/package/typed-process-0.2.10.1/docs/System-Process-Typed.html
[bytestring]: https://hackage.haskell.org/package/bytestring
[vector]: https://hackage.haskell.org/package/vector
[toForeignPtr0]: https://hackage.haskell.org/package/bytestring-0.11.3.1/docs/Data-ByteString-Internal.html#v:toForeignPtr
[unsafeFromForeignPtr0]: https://hackage.haskell.org/package/vector-0.13.0.0/docs/Data-Vector-Storable.html#v:unsafeFromForeignPtr0


## Playback through pipewire

Using the [pulse-simple] library, we send the samples to the speaker with this function:

```haskell
import Sound.Pulse.Simple qualified as PS

playSample :: Samples -> PS.Simple -> IO ()
playSample samples client = PS.simpleWriteRaw client bs
  where
    (floatPtr, floatSZ) = VectorS.unsafeToForeignPtr0 samples
    wordPtr = castForeignPtr floatPtr
    wordSZ = floatSZ * 4
    bs = fromForeignPtr0 wordPtr wordSZ
```

This function does the opposite operation from *decodeFile* to convert the data back
into a `ByteString`.

[pulse-simple] is a synchronous API: the *simpleWriteRaw* call blocks until the samples are completely consumed.
It's also amazing to see that the library has been published 10 years ago (2012), and it still compiles today!

[pulse-simple]: https://hackage.haskell.org/package/pulse-simple


## Audio player thread

We use a dedicated thread to handle the synchronous audio connection
with a channel to receive the samples to be played:

```haskell
clientThread :: TChan (Maybe Samples) -> IO ()
clientThread chan = do
    -- the pulse client reference.
    mClientV <- newTVarIO Nothing
    let startClient = do
            readTVarIO mClientV >>= \case
                -- the client is already started
                Just client -> pure client
                Nothing -> do
                    client <- newClient
                    atomically $ writeTVar mClientV (Just client)
                    pure client

        stopClient = do
            readTVarIO mClientV >>= \case
                Nothing -> pure ()
                Just client -> do
                    PS.simpleDrain client
                    PS.simpleFree client
                    atomically $ writeTVar mClientV Nothing

        run = forever do
            atomically (readTChan chan) >>= \case
                Nothing -> stopClient
                Just samples -> playSample samples =<< startClient

    run `finally` stopClient
  where
    name = "animation-fractal"
    newClient = PS.simpleNew Nothing name PS.Play Nothing "pulse-pipe" spec Nothing Nothing
    spec = PS.SampleSpec (PS.F32 PS.LittleEndian) 44100 1
```

## Synchronize the audio with the video

Finally, we submit the audio samples by calling this function in the video render loop:

```haskell
playAudioFrame :: Frame -> Samples -> TChan (Maybe Samples) -> IO ()
playAudioFrame (Frame position) samples chan =
    when (even position) do
        let frameSize = 44100 `div` 30
            startingPos = fromIntegral position `div` 2
            chunk = VectorS.slice (startingPos * frameSize) frameSize samples
        atomically $ writeTChan chan (Just chunk)
```

Playing 60 audio chunks per second may be too fast, resulting in some audible clips.
Thus this function only submits 30 chunks per second instead.


## Conclusion

Similarly to the previous post [[massiv-vulkan-capture]], we used the Haskell `Foreign`
capabilities to efficiently manipulate raw data. While this initial implementation does
not prevent under/over-run, it already performs quite well: we can jump to arbitrary
location and the audio is played in sync with the video.

Haskell offers efficient low-level interfaces that are available from a high level of abstraction.

[af]: https://gitlab.com/TristanCacqueray/animation-fractal
