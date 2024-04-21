---
title: Using Dhall To Animate Fractal
date: 2024-04-21
tags: [haskell, blog]
---

This post presents the new [animation-fractal][af] configuration schema I used to produce the following
[19th Century Piano Fractal][playlist] playlist:

:::{.hidden}
![cover](/static/af-piano-cover.png)
:::

[af]: https://gitlab.com/TristanCacqueray/animation-fractal
[playlist]: https://www.youtube.com/playlist?list=PLOA1Lq_GM0VvsnCyvoBjul7rYEBAAkN_s

:::{.flex .items-center .justify-center}
<iframe width="560" height="315" src="https://www.youtube.com/embed/videoseries?si=ZU-gDRVkXxuW6lKh&amp;list=PLOA1Lq_GM0VvsnCyvoBjul7rYEBAAkN_s" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>
:::

---

In three parts, I describe how [[animation-fractal]] (AF) demos are defined, the new [Dhall][dhall] schemas, and its Haskell embedding.

[dhall]: https://dhall-lang.org/

## Context

AF demos are defined with the following elements:

- Variables: the shader inputs passed as uniform values.
- Inputs: modulations and how they modify the variables.
- Media: external files providing modulation sources.

Here is the underlying data type:

```haskell
data Scene = Scene
  { name :: String,
    variables :: [Variable],
    inputs :: [Input],
    medias :: [FilePath]
  }

data Variable = Variable
  { name :: String,
    controller :: Controller
  }

data Controller
  = SliderFloat {min :: Double, max :: Double}
  | ColorPicker
  | Toggle

data Input = Input
  { source :: Source,
    modulation :: Modulation
  }

data Source
  = Clock
  | Audio {freq :: Double}
  | Midi {track :: String}

data Modulation = Modulation
  { speed :: Double,
    variable :: String
  }
```

> Notice how data types can be divided into two groups:
>
> - Records such as `Scene` are the product of multiple types.
> - Enums such as `Controller` are the sum of multiple types.

Here is an example demo defined in Haskell:

```haskell
demo :: Scene
demo =
  Scene
    { name = "microChop",
      variables =
        [ Variable "iTime" (SliderFloat 0 256),
          Variable "iColor" ColorPicker,
          Variable "iMotion" (SliderFloat 0 256)
        ],
      inputs =
        [ Input Clock (Modulation 1 "iTime"),
          Input (Audio 440) (Modulation 42 "iColor"),
          Input (Midi "piano") (Modulation 60 "iMotion")
        ],
      medias = ["mazurka-op63.mp3", "mazurka-op63.mid"]
    }
```

> You can checkout the full data type in the [AnimationFractal.Scene module][scene-hs] and
> how it was used in the [Demo.Mandelbrot][mandelbrot-hs].

[scene-hs]: https://gitlab.com/TristanCacqueray/animation-fractal/-/blob/34cd830b0713a2ac703e63bac43fc3b93903c15c/src/AnimationFractal/Scene.hs
[mandelbrot-hs]: https://gitlab.com/TristanCacqueray/animation-fractal/-/blob/34cd830b0713a2ac703e63bac43fc3b93903c15c/src/Demo/Mandelbrot.hs

This implementation had two major flaws:

- Adding a new demo required rebuilding the source code, and,
- The scene data type is tailored for the runtime and all the fields need to be provided up-front, resulting in some repetitions.

In the next sections, I show how I fixed these flaws.

## Use case for dhall

As I already explained in the [podenv documentation][podenv-doc], I like [Dhall][dhall] because it simplifies the runtime configuration logic.
Having a programmable configuration language lets you define a cleaner API for the user config.

For example, with a static language like YAML, the configuration could be implemented like this:

```yaml
- scene:
    name: microChop
    variables:
      - name: iTime
        controller: slider
      - name: iColor
        controller: color
    inputs:
      - source: {kind: clock}
        modulation: {speed: 1, variable: iTime}

- scene:
    parent: microChop
    inputs:
      - source: {kind: midi, track: piano}
        modulation: {speed: 42, variable: iColor}
    medias:
      - mazurka-op63.mp3
      - mazurka-op63.mid
```

Then, the runtime configuration logic would be in charge of resolving the parent inheritance and
merging the configuration values.
The media list definition could also be improved using ad hoc interpolation, for example with:

```yaml
- env:
    compo: "mazurka-op63"

- demo:
    medias:
      - "{{ env.compo }}.mp3"
      - "{{ env.compo }}.mid"
```

It is tempting to provide this type a schema, as it appears simple to use,
but I think that results in a bad user experience. It relies on a custom
runtime logic to resolve the final scene definition, and the user has no other choices
but to execute the application to interpret the configuration.

In the next section, I explain how I used Dhall to implement AF configuration.

## Dhall schemas

Here is the Scene data type from the above defined in a Dhall package:

```haskell
-- | package.dhall
let Modulation =
      { Type = { speed : Double, variable : Text }
      , default.speed = 60.0
      }

let Source =
      < Clock
      | Audio : { freq : Double }
      | Midi : { track : Text }
      >

let Input =
      { Type = { source : Source, modulation : Modulation.Type }
      , default = {=}
      }

let Controller =
      < SliderFloat : { min : Double, max : Double }
      | ColorPicker
      | Toggle
      >

let Variable =
      { Type = { name : Text, controller : Controller }
      , default = {=} }

let Scene =
      { Type =
          { name : Text
          , variables : List Variable.Type
          , inputs : List Input.Type
          , medias : List Text
          }
      , default = { medias = [] : List Text, inputs = [] : List Input.Type }
      }

in  { Modulation, Source, Input, Controller, Variable, Scene }
```

Here is an example usage:

```haskell
let AF = ./package.dhall

in  AF.Scene::{
    , name = "microChop"
    , variables =
      [ AF.Variable::{
        , name = "iTime"
        , controller = AF.Controller.SliderFloat { min = 0.0, max = 256.0 }
        }
      , AF.Variable::{ name = "iColor", controller = AF.Controller.ColorPicker }
      ]
    , inputs =
      [ AF.Input::{
        , source = AF.Source.Clock
        , modulation = AF.Modulation::{ speed = 1.0, variable = "iTime" }
        }
      , AF.Input::{
        , source = AF.Source.Audio { freq = 440.0 }
        , modulation = AF.Modulation::{ speed = 42.0, variable = "iTime" }
        }
      ]
    , medias = [ "mazurka-op63.mp3", "mazurka-op63.mid" ]
    }
```

The user can customize the schema using the language construct,
for example, using this function:

```haskell
let mkMedias = \(name : Text) -> ["${name}.mp3", "${name}.mid"]
```

## Embedding Dhall in Haskell

Haskell data types can be generated from the Dhall schemas.
Here I used intermediary types, prefixed with `DH`, to provide a layer between the configuration value
and the runtime value, which contains more information.
Since I gradually introduced this new configuration format, it's also useful to have
a dedicated end user data type while the full API is not directly available.

```haskell
import Dhall qualified
import Dhall.TH

Dhall.TH.makeHaskellTypes
    [ SingleConstructor "DHModulation" "MkModulation" "(./package.dhall).Modulation.Type"
    , MultipleConstructors "DHSource" "(./package.dhall).Source"
    , SingleConstructor "DHInput" "MkInput" "(./package.dhall).Input.Type"
    , MultipleConstructors "DHController" "(./package.dhall).Controller"
    , SingleConstructor "DHVariable" "MkVariable" "(./package.dhall).Variable.Type"
    , SingleConstructor "DHScene" "MkScene" "(./package.dhall).Scene.Type"
    ]
```

> Records are generated using the `SingleConstructor` param and enums use the `MultipleConsuctors` param.

Here is the conversion function between the configuration and the runtime types:

```haskell
mkScene :: DHScene -> AF.Scene
mkScene scene =
  AF.Scene
    { name = scene.name,
      variables = map mkVariable scene.variables,
      inputs = map mkInput scene.inputs,
      medias = scene.medias
    }
  where
    mkInput :: DHInput -> AF.Input
    mkInput input =
      AF.Input
        { source = mkSource input.source,
          modulation = mkModulation input.modulation
        }

    mkSource :: DHSource -> AF.Source
    mkSource = \case
      Clock -> AF.Clock
      Audio {freq} -> AF.Audio freq []
      Midi {track} -> AF.Midi track

    mkVariable :: DHVariable -> AF.Variable
    mkModulation :: DHModulation -> AF.Modulation

loadConfig :: FilePath -> IO Scene
loadConfig fp = mkScene <$> Dhall.input Dhall.auto (from fp)
```

> I omitted some details, you can checkout the full configuration logic in the [AnimationFractal.Config module][config-hs].

[config-hs]: https://gitlab.com/TristanCacqueray/animation-fractal/-/blob/main/src/AnimationFractal/Config.hs

[podenv-doc]: https://github.com/podenv/podenv/blob/main/docs/discussions/dhall-configuration.md


## Conclusion

This new configuration helps streamline AF usage and it let me create the many new demos of the
[19th Century Piano Fractal][playlist] playlist. You can check all the definitions in the [demo directory][demo-dir].

[demo-dir]: https://gitlab.com/TristanCacqueray/animation-fractal/-/tree/main/demo

I hope you'll enjoy this, thanks for your time!
