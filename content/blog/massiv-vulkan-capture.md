---
title: Capturing Vulkan Framebuffer with Massiv
date: 2022-12-13
tags:
  - blog
  - haskell
  - vulkan
---

This post shows how to efficiently capture and save a Vulkan framebuffer to an image file.


:::{.flex .items-center .justify-center}
![af-screenshot-demo](../static/af-screenshot-demo.jpg)
:::

## Context

In my [animation-fractal][af] project, the rendering loop runs at 60 frame per seconds.
Thus, we have 16.6 millisecond to create a single 1600x1200 frame.

The goal is to implement a record feature so that the display can be exported
at the highest quality, without relying on an external capture tool.


## Copy memory from GPU to CPU

As demonstrated in this [screenshot.cpp][screenshot.cpp] example, we need to create
an image buffer that can be accessed from the CPU using `VMA.MEMORY_USAGE_GPU_TO_CPU` usage flag.
Here is the image representation data type:

```haskell
data ScreenshotImage = ScreenshotImage
    { info :: VMA.AllocationInfo
    , image :: Vk.Image
    , layout :: Vk.SubresourceLayout
    , dim :: (Word32, Word32)
    }
```

Then we can transfer the framebuffer data from the GPU to the CPU with these commands:

- Set the image to `IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL`.
- Set the framebuffer to `IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL`.
- Call `cmdCopyImage`.
- Reset the framebuffer to `IMAGE_LAYOUT_GENERAL`.

This step takes about 1 millisecond.

[screenshot.cpp]: https://github.com/SaschaWillems/Vulkan/blob/ed406e61a69a9fb5616e087c99291eb27ba2b9a9/examples/screenshot/screenshot.cpp#L239


## Decode the memory into a Massiv array

After the copy we have a memory region that contains the pixels data.
Though, the layout is defined by the GPU requirements and we need to take into account
any extra paddings as well as the row pitch.
Moreover, we need to copy the raw data as fast as possible so that the memory can be re-used to
capture the next frame.

The Haskell base library provides a few capabilities to deal with such task through the
`Foreign` modules. In this case, we need the following two functions:

```haskell
-- | Advances the given address by the given offset in bytes.
Foreign.Ptr.plusPtr :: Ptr a -> Int -> Ptr b

-- | Read a value from the given memory location.
Foreign.Storable.peek :: Ptr a -> IO a
```

Thanks to the [generateArrayS][generateArrayS] function, we can copy the data by
re-arranging each pixel into the correct layout using this implementation:

```haskell
type MassivPixel = Pixel (Alpha RGB) Word8
type MassivImage = Array S Ix2 MassivPixel

readImageMassiv :: ScreenshotImage -> IO MassivImage
readImageMassiv si =
    generateArrayS
        (Sz $ fromIntegral (snd si.dim) :. fromIntegral (fst si.dim))
        getPixel
  where
    getPixel :: Ix2 -> IO MassivPixel
    getPixel (y :. x) = liftIO $! peek (pixelAddr x y)

    pixelAddr :: Int -> Int -> Ptr MassivPixel
    pixelAddr x y =
        plusPtr
            (VMA.mappedData si.info)
            ( fromIntegral si.layout.offset
                + (y * fromIntegral si.layout.rowPitch)
                + (x * sizeOf (0 :: Word32))
            )
```

I'm not sure why the `Sz` parameter appears to be inversed, e.g. (height :. width),
but that gives the correct result, otherwise the image would be rotated by 90 degrees.

This step takes about 8 milliseconds.


## Encoding the final image file

The [massiv-io][massiv-io] library provides encoder for most image formats, based on the
standard [JuicyPixels][JuicyPixels] implementations.
The format which requires the less computation seems to be BMP, and here is the function to
save the image to disk:

```haskell
writeImageMassiv :: FilePath -> MassivImage -> IO ()
writeImageMassiv = MIO.writeArray MIO.BMP MIO.def
```

This step takes about 7 milliseconds. Though, that can be done in a dedicated thread outside of the rendering loop.


## Conclusion

Thanks to the Haskell `Foreign` capabilities we saw how to interpret raw data.
Then using the [massiv][massiv] library we implemented an image decoder.
In 8 milliseconds we copy the framebuffer to a cpu memory region and we decode the data into a massiv array.
This array can then be encoded to an image format in a final step.

Note that I also tried the [Codec.Picture.withImage][withImage] function provided by [JuicyPixels][JuicyPixels]
library, but the performance was a couple order of magnitude slower than the massiv [generateArrayS][generateArrayS].
Please let me know if there is a faster way to do this task.

Finally, here is the commit that introduced the screenshot record feature
  [animation-fractal fb4522c1][fb4522c1].


[af]: https://gitlab.com/TristanCacqueray/animation-fractal
[massiv]: https://hackage.haskell.org/package/massiv-1.0.2.0/docs/Data-Massiv-Array.html
[massiv-io]: https://hackage.haskell.org/package/massiv-io-1.0.0.1/docs/Data-Massiv-Array-IO.html
[JuicyPixels]: https://hackage.haskell.org/package/JuicyPixels
[generateArrayS]: https://hackage.haskell.org/package/massiv-1.0.2.0/docs/Data-Massiv-Array-Mutable.html#v:generateArrayS
[withImage]: https://hackage.haskell.org/package/JuicyPixels-3.3.8/docs/Codec-Picture.html#v:withImage
[fb4522c1]: https://gitlab.com/TristanCacqueray/animation-fractal/-/commit/fb4522c124e97722b93ea90e010dc65fa2701097
