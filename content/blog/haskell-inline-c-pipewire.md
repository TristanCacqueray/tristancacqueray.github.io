---
title: Haskell inline-c Demo For Pipewire
date: 2024-06-09
tags: [haskell, ffi, pipewire]
---

This post shows how I created a high level, proof of concept, Haskell binding for the [libpipewire][libpipewire] using [inline-c][inline-c].
The goal is to demonstrate how to implement a Pipewire client with Haskell.

:::{.flex .items-center .justify-center}
<img src="/static/pipewire-hs.svg" width=370 />
:::

:::{.hidden}
![logo](static/pipewire-hs.svg)
:::
---

This post introduces the initial implementation of the [pipewire.hs][pipewire.hs] library,
which I hope will provide a new foundation for handling multimedia with Haskell.
In four parts I present:

- Introduction to Pipewire.
- How to play a tone with C and the libpipewire.
- The Haskell Foreign Function Interface (FFI).
- Future work and conclusion.

> Note that this is also a learning exercise for me as I'm not familiar with Pipewire or the Haskell FFI.
> I welcome your feedback and I would appreciate the opportunity to fix any errors contained in this post.


## Context

[Pipewire][pipewire] is a multimedia framework that can be used to capture and play audio, midi and video streams with minimal latency.
Pipewire provides an unified system to process multimedia in real-time and can replace services like [pulseaudio][pulseaudio] and [jack][jack].
It was created by Wim Taymans, one of the main developers of the popular [gstreamer][gstreamer] framework.
Pipewire is now enabled by default on most Linux systems.

Pipewire can be used for a wide range of multimedia applications, such as:

- Media Player.
- Digital Audio Workstation (DAW).
- MIDI Sequencer.
- Screen casting and live streaming.

I recently started to learn more about how this system works and it looked like a great fit for my [[animation-fractal]] project as Pipewire can be used to:

- Replace the pulse-simple player with a more powerful client to playback sound file input.
- Capture external modulation inputs.
- Stream video output.

However, I was not familiar with using the Haskell Foreign Function Interface (FFI) to interact with this system,
and I was not even sure if Haskell was a practical language for such tasks.
Thus, I started the [pipewire.hs][pipewire.hs] project to learn these new skills.

This post shows how I implemented a Haskell binding for the libpipewire client
library to demonstrate a minimal media player.


## Playing a tone with the libpipewire

The libpipewire is a C library, and it provides many examples that describe how to use the API.
The [tutorial4.c][tutorial4] in particular explains how to play a tone using the following procedure:

[tutorial4]: https://docs.pipewire.org/tutorial4_8c-example.html

- Initialize a client context.
- Create a stream and setup its events listener.
- Configure and connect the stream.
- Run the main loop.

The implementation looks like this:

```c
#include <pipewire/pipewire.h>

// The events listener
static const struct pw_stream_events stream_events = {
	PW_VERSION_STREAM_EVENTS,
	.process = on_process,
};

void main() {
  struct pw_main_loop *loop;
  struct pw_stream *stream;

  // initialization is omitted...

  pw_stream_add_listener(stream, hooks, stream_events, stream);
  pw_stream_connect(stream, ...);
  pw_main_loop_run(loop);

  // cleanup is omitted...
}
```

The tone is played through the process event callback by performing the following actions:

- Acquiring a buffer.
- Computing the buffer size in term of samples.
- Writing the sample values.
- Queuing the buffer.

The implementation looks like this:

```c
// The process event callback:
static void on_process(struct pw_stream* stream) {
  struct pw_buffer *buf;
  int stride, size;

  buf = pw_stream_dequeue_buffer(stream);

  stride = sizeof(int16_t) * DEFAULT_CHANNELS
  size = buf->buffer->datas[0].maxsize / stride;

  // writting samples to the buffer is omitted...

  pw_stream_queue_buffer(stream, buf);
}
```

The following sections show how to use the inline-c library to implement this example with Haskell.

## Haskell FFI

Haskell's FFI is used to call functions from other languages and for other languages to call Haskell functions.
It primarily supports the C language application binary interface (ABI) and it is enabled with the `foreign` keyword.
Implementing a FFI usually requires wrappers to convert the higher level domain into primitive types and memory pointers.
These wrappers are often referred to as *bindings*.

The following sections leverage the inline-c library to seamlessly call C libraries without having to use `foreign` declarations.

> I should note that the C ABI is not ideal for higher level languages. There is some work in progress to improve this status quo.
> For example, the [crABI][crABI] offers interoperability between high-level programming languages that have safe data types.
> Vanessa McHale also suggests that new designs could be "fruitful" in this article: [Functional Compilers That Stand Toe-to-toe With C's Object Files: a Manifesto](http://blog.vmchale.com/article/ffi).

[crABI]: https://github.com/rust-lang/rfcs/pull/3470

### Opaque foreign structure

Inline-c leverages quasi-quotations to include C code directly in Haskell declarations.
To handle custom types, such as the `struct pw_*`, inline-c provides a context to configure the desired mapping between C and Haskell.
The context needs to be defined in a dedicated module like this:

```haskell
module Pipewire.CContext where

import Data.Map.Strict qualified as Map
import Language.C.Inline.Context (Context (..))
import Language.C.Types (TypeSpecifier(Struct))

data MainLoopStruct
data StreamStruct
data StreamEventsStruct
data BufferStruct

pwContext :: Context
pwContext =
    mempty
        { ctxTypesTable =
            Map.fromList
                [ (Struct "pw_main_loop",     [t|MainLoopStruct|])
                , (Struct "pw_stream",        [t|StreamStruct|])
                , (Struct "pw_stream_events", [t|StreamEventsStruct|])
                , (Struct "pw_buffer",        [t|BufferStruct|])
                ]
        }
```

Then this context can be used like this:

```haskell
module Pipewire where

import Control.Exception (bracket, finally)
import Foreign (Ptr)
import Foreign qualified
import Language.C.Inline qualified as C

import Pipewire.CContext

C.context (C.baseCtx <> pwContext)
C.include "<pipewire/pipewire.h>"
```

### Haskell Foreign Ptr

To handle foreign data, the Haskell base library provides a `Ptr` data to represent a typed memory pointer.
Haskell also provides a `Storable` type class than can be used to read from the pointer and write data.
Though, for this initial proof of concept, I did not implement any storable instances, instead I treated all the foreign data as opaque and I provided a dedicated API to read and write the necessary data.
This is why the structure definitions in the context above don't provide any constructors.
We can use newtype wrappers to hide this implementation detail like this:

```haskell
newtype MainLoop = MainLoop (Ptr MainLoopStruct)
newtype Stream = Stream (Ptr StreamStruct)
newtype StreamEvents = StreamEvents (Ptr StreamEventsStruct)
newtype Buffer = Buffer (Ptr BufferStruct)
```

### Initializing the main loop

To initialize the loop, we can call C functions using the `C.exp` quasi-quote:

```haskell
-- | Create the pw_main_loop.
pw_main_loop_new :: IO MainLoop
pw_main_loop_new =
    MainLoop <$> dieOnNull "pw_main_loop_new"
        [C.exp| struct pw_main_loop*{pw_main_loop_new(NULL)} |]

-- | Helper to abort on null pointer
dieOnNull :: String -> IO (Ptr a) -> IO (Ptr a)
dieOnNull src action = do
    ptr <- action
    if ptr == Foreign.nullPtr
        then ioError $ userError $ src <> " returned NULL"
        else pure ptr
```

Thanks to the context, inline-c is able to infer the Haskell type from the return value.
We can capture Haskell variables using the `$(c-type variable-name)` syntax, for example
to destroy the loop:

```haskell
-- | Destroy the pw_main_loop.
pw_main_loop_destroy :: MainLoop -> IO ()
pw_main_loop_destroy (MainLoop mainLoop) =
    [C.exp| void{pw_main_loop_destroy($(struct pw_main_loop* mainLoop))} |]
```

Then we can provide an idiomatic wrapper using `bracket`:

```haskell
-- | Create and destroy the pw_main_loop after use.
withMainLoop :: (MainLoop -> IO a) -> IO a
withMainLoop = bracket pw_main_loop_new pw_main_loop_destroy
```

### Allocating the events listener

Most of the libpipewire structs are created by the library and they are given as pointers.
Though, some structs, such as the `pw_stream_events`, need to be created by the library user.
We can do this with the `allocaBytes` helper:

```haskell
-- | Create the StreamEvents
withStreamEvents :: (StreamEvents -> IO a) -> IO a
withStreamEvents cb = Foreign.allocaBytes
    (fromIntegral [C.pure| size_t {sizeof (struct pw_stream_events)} |])
    \ptr -> cb (StreamEvents ptr)
```

This uses the inline-c `C.pure` quasi-quote to get the size of the structure using the C `sizeof` function.

### Registering the event callback

For the process event callback, we need to setup a pointer to create a `FunPtr` to make a Haskell function callable from foreign code.
The inline-c library provides the `C.mkFunPtr` helper for that:

```haskell
type ProcessHandler = IO ()

withProcessHandler :: ProcessHandler -> StreamEvents -> IO a -> IO a
withProcessHandler processHandler (StreamEvents streamEvents) cb = do
    processPtr <- $(C.mkFunPtr [t|Ptr () -> IO ()|]) processWrapper
    [C.block| void{
        struct pw_stream_events* pw_events = $(struct pw_stream_events* streamEvents);
        pw_events->version = PW_VERSION_STREAM_EVENTS;
        pw_events->process = $(void (*processPtr)(void*));
    }|]
    cb `finally` Foreign.freeHaskellFunPtr processPtr
  where
    -- ignore the user data pointer as the handler closure captures it
    processWrapper _data = processHandler
```

This uses the inline-c `C.block` quasi-quote to write the necessary fields in the StreamEvent structure.

With the above techniques, we can cover most of the libpipewire API.
The next sections show more examples of how that works for this demo.

### Reading the buffer size

To implement the process callback, we need to know how many samples are requested.
We can integrate the C snippet to do that like this:

```haskell
newtype Channels = Channels Int

-- | Return the requested samples buffer size.
audioFrames :: Channels -> Buffer -> IO Int
audioFrames (Channels (fromIntegral -> chans)) (Buffer pwBuffer) =
    fromIntegral
        <$> [C.block| int{
                struct pw_buffer* buf = $(struct pw_buffer* pwBuffer);
                int stride = sizeof(int16_t) * $(int chans);
                int n_frames = buf->buffer->datas[0].maxsize / stride;
                if (buf->requested)
                    n_frames = SPA_MIN(b->requested, n_frames);
                return n_frames;
            }|]
```

Being able to seamlessly use C code in Haskell is a great capability to accelerate developments.
Without this, we would have had to implement storable instances for each individual structure, in this case for `pw_buffer`, `spa_buffer` and `spa_data`.
It can be quite laborious to handle all these details, when all we need is a small portion of the API.
Therefor, inline-c lets us use complex systems by focusing on the higher level details.

To further illustrate this capability, here is the binding to configure the stream:

```haskell
connectAudioStream :: Channels -> Stream -> IO ()
connectAudioStream (Channels (fromIntegral -> chans)) (Stream pwStream) = do
    [C.block| void{
        const struct spa_pod *params[1];
        uint8_t buffer[1024];
        struct spa_pod_builder b = SPA_POD_BUILDER_INIT(buffer, sizeof(buffer));

        params[0] = spa_format_audio_raw_build(&b, SPA_PARAM_EnumFormat,
            &SPA_AUDIO_INFO_RAW_INIT(
                .format = SPA_AUDIO_FORMAT_S16,
                .channels = $(int chans),
                .rate = 44100));

        pw_stream_connect($(struct pw_stream* pwStream),
            PW_DIRECTION_OUTPUT,
            PW_ID_ANY,
            PW_STREAM_FLAG_MAP_BUFFERS | PW_STREAM_FLAG_RT_PROCESS,
            params, 1);
    }|]
```

This functions provides a simplified wrapper to call `pw_stream_connect` with the
attributes that are necessary for playing audio. In particular, we are able to
define the audio format using the existing spa macro facilities, without having to
handle all the necessary interfaces up front.

This proved to be helpful to make rapid progress on this proof of concept.

The next section shows another example of this capability.


### Reading the link state

Similar to the buffer size, another interesting API to implement is the link state.
It is defined in C like this:

```c
enum pw_link_state {
    PW_LINK_STATE_ERROR = -2,
    PW_LINK_STATE_UNLINKED = -1,
    PW_LINK_STATE_INIT = 0,
    PW_LINK_STATE_NEGOTIATING = 1,
    // ...
}

struct pw_link_info {
    uint32_t id;
    enum pw_link_state state;
    // an error reason if the state is error
    const char *error;
    struct spa_pod *format;
    // ...
}
```

To bind the enum values, we can write a `Enum.hsc` file like this:

```haskell
{-# LANGUAGE CPP, PatternSynonyms #-}
module Pipewire.Enum where

import Foreign (Storable)

#include <pipewire/pipewire.h>

newtype LinkState = LinkState Int
  deriving newtype (Eq, Storable, Show)

-- /* awk '/PW_LINK_STATE/ { print "pattern " $1 " = (#const " $1 ")" }' pipewire/*.h */
pattern PW_LINK_STATE_ERROR = LinkState (#const PW_LINK_STATE_ERROR)
pattern PW_LINK_STATE_UNLINKED = LinkState (#const PW_LINK_STATE_UNLINKED)
pattern PW_LINK_STATE_INIT = LinkState (#const PW_LINK_STATE_INIT)
pattern PW_LINK_STATE_NEGOTIATING = LinkState (#const PW_LINK_STATE_NEGOTIATING)
```

This uses the CPP language extension to get the link state constants as Haskell values.
Instead of creating an enum (sum type) in Haskell, we can keep the original integer representation,
and use the PatternSynonyms language extension to constraint the available values.

That way we can write an idiomatic `getLinkState` like this:

```haskell
getLinkState :: LinkInfo -> IO (Either Text LinkState)
getLinkState (LinkInfo linkInfo) = do
    state <-
        LinkState . fromIntegral
            <$> [C.exp| int{$(struct pw_link_info* linkInfo)->state} |]
    case state of
        PW_LINK_STATE_ERROR -> do
            errC <- [C.exp| const char*{$(struct pw_link_info* linkInfo)->error} |]
            err <- peekCString errC
            pure $ Left err
        _ -> pure $ Right state
```

> The `peekCString` can be found in this [text\#599](https://github.com/haskell/text/pull/599) PR.

This takes care of reading the error message when the link state is error.

### Writing audio samples

Inline-c also provides facilities to capture Haskell vectors.
By adding the `vecCtx` context, we can use the `$vec-` syntax to write the audio samples to the process buffer like this:

```haskell
import Data.Vector.Storable qualified as SV

-- | Write the frame
writeAudioFrame :: Buffer -> SV.Vector Float -> IO ()
writeAudioFrame (Buffer pwBuffer) samples = do
    [C.block| void {
      struct pw_buffer* b = $(struct pw_buffer* pwBuffer);
      float *src = $vec-ptr:(float *csamples);
      int16_t *dst = b->buffer->datas[0].data;
      for (int i = 0; i < $vec-len:samples; i++)
        dst[i] = src[i] * 32767.0; // scale f32 to i16
    }|]
  where
    -- It is safe to convert from Float to CFloat
    csamples = SV.unsafeCoerceVector samples
```

And with this last example, we covered everything that is needed to implement a media player in Haskell with the libpipewire.
Checkout the [Tutorial4.hs](https://github.com/TristanCacqueray/pipewire.hs/blob/main/pipewire/examples/Tutorial4.hs)
source code for the full demo.

## Conclusion

### Status of the library

This post demonstrated how I used the inline-c library to quickly create a prototype binding for the libpipewire.
To confirm that Haskell can be used effectively to interact with a Pipewire service,
I covered enough of the C API to implement the following examples:

- `pw-link` to list/create/delete links: [PwLink.hs][pw-link]
- `video-src` to draw a video stream: [VideoSrc.hs][video-src]
- `pw-play` to play an audio media file: [PwPlay.hs][pw-play]

The `video-src` revealed an extra challenge with regards to the resources allocation.
In some situations, it is necessary to create resources during a callback.
Such resources need to outlive the callback execution and we can't use the *withResource* pattern.
So I think we need to integrate the [resourcet][resourcet] allocation system to provide a safer API.

I am now confident Pipewire can be used efficiently with Haskell.

### Exploring alternatives

With regards to the actual binding, I would like to evaluate the other available options beside
[inline-c][inline-c]. In particular, I would like to compare the compilation speed, runtime overhead and
usage complexity of the following alternatives:

- [CApiFFI][capi-ffi], a language extension to write foreign declaration.
- [hsc2hs][hsc2hs], a tool provided by Haskell to embed C definitions using a source pre-processor.
- [c2hs][c2hs], a tool to generate interface.
- A custom code generator, like it is done for [vulkan][vulkan] or [dear-imgui][dear-imgui].

### Learning experience

I wrote a few bindings over the years, and each time I found that they were great learning experiences
to discover different APIs design and implementation. Here are some examples:

- [purescript-gjs][purescript-gjs]: GNOME Javascript for Purescript.
- [re-patternfly][re-patternfly]: ReScript binding for Patternfly.
- [dhall-ansible][dhall-ansible]: Ansible definitions with Dhall.

For Pipewire I was even able to make a minor contribution to improve the C API:
[pipewire!2029](https://gitlab.freedesktop.org/pipewire/pipewire/-/merge_requests/2029/diffs).

I am looking forward to developing the [pipewire.hs][pipewire.hs] library,
please let me know if you are interested.
Thanks for your time!

[libpipewire]: https://docs.pipewire.org/page_api.html
[pipewire.hs]: https://github.com/TristanCacqueray/pipewire.hs#readme
[pipewire]: https://pipewire.org/
[pulseaudio]: https://www.freedesktop.org/wiki/Software/PulseAudio/
[jack]: https://jackaudio.org/
[gstreamer]: https://gstreamer.freedesktop.org/
[pw-play]: https://github.com/TristanCacqueray/pipewire.hs/blob/main/pipewire/examples/PwPlay.hs
[pw-link]: https://github.com/TristanCacqueray/pipewire.hs/blob/main/pipewire/examples/PwLink.hs
[video-src]: https://github.com/TristanCacqueray/pipewire.hs/blob/main/pipewire/examples/VideoSrc.hs
[inline-c]: https://hackage.haskell.org/package/inline-c
[resourcet]: https://hackage.haskell.org/package/resourcet-1.3.0/docs/Control-Monad-Trans-Resource.html
[capi-ffi]: https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/ffi.html#the-capi-calling-convention
[hsc2hs]: https://downloads.haskell.org/ghc/latest/docs/users_guide/utils.html#writing-haskell-interfaces-to-c-code-hsc2hs
[c2hs]: https://github.com/haskell/c2hs/wiki/User-Guide
[vulkan]: https://github.com/expipiplus1/vulkan/tree/main/generate-new
[dear-imgui]: https://github.com/haskell-game/dear-imgui.hs/blob/main/generator/DearImGui/Generator.hs
[purescript-gjs]: https://github.com/purescript-gjs/purescript-gjs
[dhall-ansible]: https://github.com/TristanCacqueray/dhall-ansible
[re-patternfly]: https://github.com/softwarefactory-project/re-patternfly
