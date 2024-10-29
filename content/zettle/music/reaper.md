# Reaper

## Shortcuts like op1

```
KEY 9 32 40073 0		 # Main : Ctrl+Space : OVERRIDE DEFAULT : Transport: Play/pause
KEY 176 7 40157 0		 # Main : MIDI Chan 1 CC 7 : Markers: Insert marker at current position
KEY 1 77 40157 0		 # Main : M : OVERRIDE DEFAULT : Markers: Insert marker at current position
KEY 176 15 40172 0		 # Main : MIDI Chan 1 CC 15 : Markers: Go to previous marker/project start
KEY 0 91 40172 0		 # Main : [ : OVERRIDE DEFAULT : Markers: Go to previous marker/project start
KEY 176 16 40173 0		 # Main : MIDI Chan 1 CC 16 : Markers: Go to next marker/project end
KEY 0 93 40173 0		 # Main : ] : OVERRIDE DEFAULT : Markers: Go to next marker/project end
KEY 1 81 40625 0		 # Main : Q : Time selection: Set start point
KEY 1 69 40626 0		 # Main : E : OVERRIDE DEFAULT : Time selection: Set end point
KEY 9 32807 41042 0		 # Main : Ctrl+Right : OVERRIDE DEFAULT : Move edit cursor forward one measure
KEY 9 32805 41043 0		 # Main : Ctrl+Left : OVERRIDE DEFAULT : Move edit cursor back one measure
```

## Solo in front

Click `Options` and then `Solo in front`



## Settings

Avoid dangling project and ensure audio stems are saved in the right place:
- Project -> Prompt to save on new project [x]

Better default send:
- Project -> Track/Send Defaults -> Send gain `-18db`
                                 -> Send midi by default [ ]

Assign midi controller, press <kbd>?</kbd> to see action list
- `:>` Transport play (not play/stop because that requires the button to stay pressed)
- `[]` Transport stop
- `<-` Markers: Go to previous marker/project start
- `->` Markers: Go to next marker/project end
- `C-<left>` Move edit cursor back one measure
- `C-<right>` Move edit cursor forward one measure

> Use `Find shortcut` to find the action of an existing key

Disable auto fading on cut:

- Project -> Item Fade Defaults -> Untick all the "Fade-in/fade-out"

Navigate arrange view with middle click:

- Editing Behavior -> Mouse Modifier:
  - Context: Arrange view
  - middle drag
  - Default action set to `Hand scroll`

Center zoom on mouse position (instead of play cursor):
- Appearance -> Zoom/Scroll/Offset -> Horizontal zoom center: `Mouse cursor`

Untick to disable `Tiny fade in on playback start`

Bind (e.g. to `z`) `Toggle solo for selected tracks`, to quickly solo the current track, whatever the current view.

## Match item or track loudness with SWS

- select items
- show actions list
- SWS/BR: Normalize loudness of selected items/tracks
- pick items (the clip) or tracks (the mixer)
- set lufs to -14 for spotify

## Find BPM

- insert tempo marker in the timeline <kbd>shift</kbd><kbd>c</kbd>, for example at position 5
- in TCP, change tracks timebase to `time`
- change source properties: check `ignore project tempo`
- ctrl drag the mark to match transient

## Move items on one axis only: <kbd>ctrl</kbd><kbd>shift</kbd>

## Midi note too early

When recording midi sequence through extenal synth with the miniFuse,
the audio latency is negative (recording starts a few ms after the audio play first midi).

In `Preferences` -> `Audio` -> `Recording`, untick `Use audio driver reported latency`, then adjust recording manually.

## Free impulse response

For ReaVerb, use [Big Gee's Lexicon 480L](https://grantnelson.co/article/1/lexicon-480l-free-impulse-responses), like Large Room, or Small Plate (which is bigger :)

## Automatic Gain Riding

- Use loudness meter plugin to write LUFS-M env
- Move automation to track envelop
- Tweaks envelop params to inverse

## Master FX by Kenny Gioia

- ReaEQ    (e.g. shelft +1db)
- JS 1175  (e.g. -10db threshold, 3db gain, 140 atk, 180 release)
- ReaXComp (e.g. ratio 4/1, check program dependent release, link the thresholdm -10db threshold)
- ReaLimit
- JS Loudness Meter (to check LUFS-I for max -8)
