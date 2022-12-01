Sound Engineering
=================

Here are my notes from [Dan Worrall](https://www.youtube.com/channel/UCQnz5mUTDkGwKvIQ9uyqiXw) tutorials.

## Dither [video](https://www.youtube.com/watch?v=2iDrbgfPjPY)

- Rule 1: Use 16 bit dither when creating a 16 bit file
- Rule 2: Always use dither when creating a 24 bit file

- Safebet: encode wav at 16 bit with dither, and then encode mp3

## Drum loudness

Levels:

- Mute everything but the drums, and set level to -23 LUFS-S  (average signal level).
- Dialup the speaker to hear it loud.
- Unmute the other elements, starts at 0 and dialup until it feels right.
- Tweak with EQ/Compressor, but don't touch the anchor level.

Dynamics:

- Setup bus group and slap compressor.
- Use high ratio until something nice happen, then dial down.
- Use saturation and distortion.

Methods, iterate, each settings depends on the others:

- 1. start with the volume faders
- 2. attack the biggest problem next
- 3. work quickly, get to step3 as soon as possible

# ReaComp

- Threshold -30db
- RMS size to 0
- Attack  < 100ms               :  3   ms
- Release > 30 ~ 50ms < 300ms   : 70   ms
- Knee ~ 2db (no weird knee)    :  2.1 dB

Chain two, first one in Feedback mode (and infinit ratio). Link threshold.
Adjust make-up gain on the second one

## Emphasis and de emphasis EQ

- Duplicate a track, reverse phase, observe the sound is canceled.
- Add ReaEQ and observe the sound is still canceled.
- Add another ReaEQ, set gain on the first one, and oposite loss on the second one, observe the sound is still canceled.
- Insert an effect in between.

Select parameter modulation for EQ frequency, use Link from parameter, and pick the freq of the other EQ.
Try with Slick EQ, and confirm with the null test.

## Voxengo SPAN

Settings for master:

- Block Size to 8192
- Smoothing 1/3 Oct
- Avg Time 6000
- Add underlay for MID using same setting
- Do Mono check (and avoid underlay is exceeding).

## Head room

Headroom: level at -18dBFS

Bass:
- HP until it is affected, e.g. 84Hz
- Maybe Boost 4dB at >1kHz

Kick:
- Bell -10dB for bass room, e.g. 320Hz
- Boost +6dB high, e.g. 1.91 kHz

## Plugins

Here are some free plugins to investigate:

Compressors:
- rough rider https://www.audiodamage.com/pages/free-downloads
- klanghelm https://klanghelm.com/contents/products/DC1A.php

Amp:
- https://www.voxengo.com/product/tubeamp/

Limiter:
- Limiter №6 https://vladgsound.wordpress.com/plugins/limiter6/

Reverb:
- Valhalla SuperMassive https://valhalladsp.com/shop/reverb/valhalla-supermassive/

Sources: https://bedroomproducersblog.com/
