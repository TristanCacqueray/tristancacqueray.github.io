---
title: Arbitrary precision float controller
date: 2022-12-02
tags: [haskell, blog]
---

This is a follow-up to [[dear-imgui]].
This post demonstrates how to implement a dear-imgui widget to manipulate arbitrary floating point values.

> This document is a literate haskell file

---

```haskell
-- Quality of life syntaxic sugar
{-# LANGUAGE OverloadedStrings, ImportQualifiedPost, NamedFieldPuns, BlockArguments, LambdaCase #-}

-- RankNTypes enables using Lens as function argument
{-# LANGUAGE RankNTypes #-}

-- To replace base with rio
{-# LANGUAGE NoImplicitPrelude #-}

import RIO
import Data.Text (pack)

import DearImGui qualified
import Data.StateVar qualified as StateVar

-- Necessary import to create the window and render the GUI
import Control.Monad.Managed (runManaged, managed, managed_)
import DearImGui.OpenGL3 qualified as DGL3
import DearImGui.SDL qualified as DSDL
import DearImGui.SDL.OpenGL qualified as DGL
import Graphics.GL qualified as GL
import SDL qualified
```

## Context

The goal of this implementation is to manipulate and adjust dynamic values,
such as a coordinate position which can be any of these values:

- 9001.0
- 0.001
- -2.0

It is not practical to use a regular slider or drag controller because we don't know
the range and we don't want arbitrary bounds.
Instead, we are going to use a [Scientific][scientific]
representation to manipulate the exponent independently from the coefficient.

```haskell
import Data.Scientific (Scientific)
import Data.Scientific qualified as Scientific
```


## Scientific lens

Here are the lenses to pull appart our scientific value:

```haskell
exp10L :: Lens' Scientific Int
exp10L = lens getv setv
  where
    getv :: Scientific -> Int
    getv = Scientific.base10Exponent
    setv :: Scientific -> Int -> Scientific
    setv s c = Scientific.scientific (Scientific.coefficient s) c

coefL :: Lens' Scientific Int
coefL = lens getv setv
  where
    -- This should be Integer, but dear-imgui works with Int.
    getv :: Scientific -> Int
    getv = fromInteger . Scientific.coefficient
    setv :: Scientific -> Int -> Scientific
    setv s c = Scientific.scientific (toInteger c) (Scientific.base10Exponent s)
```

These lenses let us view and modify a scientific value:

```haskell
example :: Scientific
example = 9001.42

demo :: [String]
demo = [
    show (coefL  `view` example),     -- 900142
    show (exp10L `view` example),     -- -2
    show ((coefL  `set` 42) example), -- 0.42
    show ((exp10L `set` 42) example)  -- 9.00142e47
  ]
```

> Note that `lens`, `view` and `set` are provided by [rio][rio]. If you are not familiar with lens, checkout the [lens-tutorial][lens-tutorial].


## StateVar Lens

The dear-imgui Haskell bindings expects [StateVar][statevar] references,
so that the GUI can read and write our value.
We'll use this helper function to create one StateVar for each of our lens:

```haskell
-- note: this function definition requires the RankNTypes extension
makeStateLens :: TVar a -> Lens' a b -> StateVar.StateVar b
makeStateLens value valueLens = StateVar.makeStateVar getv setv
  where
    -- getv :: IO b
    getv = view valueLens <$> readTVarIO value
    -- setv :: b -> IO ()
    setv = atomically . modifyTVar' value . set valueLens
```

Here is an example StateVar usage:

```haskell
stateDemo :: IO String
stateDemo = do
  -- create the coef StateVar
  exampleVar <- newTVarIO example
  let coefVar = makeStateLens exampleVar coefL

  -- update the coef
  coefVar StateVar.$= 42

  -- Return "0.42"
  show <$> readTVarIO exampleVar
```

## The float controller

We'll keep both the internal scientific reprensentation and the final floating output:

```haskell
data FloatController = FloatController {
  value :: TVar Scientific.Scientific,
  output :: TVar Float
}

newFloatController :: Float -> STM FloatController
newFloatController initialValue =
  FloatController
    <$> newTVar (Scientific.fromFloatDigits initialValue)
    <*> newTVar initialValue
```

And here is how we can draw the controller:

```haskell
drawFloatController :: FloatController -> IO ()
drawFloatController FloatController{value, output} = do
  let
    coefVar = makeStateLens value coefL
    expVar = makeStateLens value exp10L

  -- we clamp the exponent between -38 and 38.
  whenM (DearImGui.dragInt "exp" expVar 1 (-38) 38) updateOutput
  whenM (DearImGui.dragInt "coef" coefVar 1 minBound maxBound) updateOutput
  DearImGui.text . mappend "Final value: " =<< pack . show <$> readTVarIO output
 where
  -- on change, we convert the scientific value to the float output.
  updateOutput = atomically do
    scientificValue <- readTVar value
    writeTVar output (Scientific.toRealFloat scientificValue)
```

… which looks like this:

<video loop autoplay>
  <source src="../static/float-controller-1.webm" type="video/webm" />
</video><br /><br />

In the next section we'll improve this implementation so that
changing the exponent does not affect the final value.

## Dynamic float controller

We would like the exponent to only define the range,
adjusting the coefficient automatically.
Here is the final version:

```haskell
drawFloatController' :: FloatController -> IO ()
drawFloatController' FloatController{value, output} = do
  let
    coefVar = makeStateLens value coefL
    expVar = makeStateLens value exp10L

  -- We pack all the elements tightly on a single line
  DearImGui.setNextItemWidth 40
  prevExp <- StateVar.get expVar
  whenM (DearImGui.dragInt "##exp" expVar 1 (-38) 38) do
    -- When the exponent value changes, we adjust the coefficient.
    newExp <- StateVar.get expVar
    let adjust coef
          | -- The exp decrease, so we adjust the coef
            newExp < prevExp = coef * 10
          | -- The exp increase and the coef can be reduced
            abs coef >= 10 = coef `div` 10
          | -- otherwise, we don't touch the coef
            otherwise = coef
    coefVar StateVar.$~! adjust

  DearImGui.sameLine
  DearImGui.setNextItemWidth 140
  whenM (DearImGui.dragInt "##coef" coefVar 1 minBound maxBound) $ atomically do
    -- When the coefficient changes, we update the final float value.
    scientificValue <- readTVar value
    writeTVar output (Scientific.toRealFloat scientificValue)
  DearImGui.sameLine

  DearImGui.text . mappend "value: " =<< pack . show <$> readTVarIO output
```

… which looks like this:

<video loop autoplay>
  <source src="../static/float-controller.webm" type="video/webm" />
</video><br /><br />

## Run the GUI

Finally, the initializing code borrowed from the dear-imgui README:

```haskell
main :: IO ()
main = runSimpleApp do
  -- print example
  logInfo (displayShow demo)
  logInfo . displayShow =<< liftIO stateDemo

  floatController <- atomically (newFloatController 1)
  -- For this demo we'll draw the interface fullscreen:
  let doDrawGUI = DearImGui.withFullscreen (drawFloatController' floatController)

  SDL.initializeAll
  liftIO $ runManaged do
    window <- do
      let title = "Hello, Dear ImGui!"
      let config = SDL.defaultWindow { SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL }
      managed $ bracket (SDL.createWindow title config) SDL.destroyWindow
    glContext <- managed $ bracket (SDL.glCreateContext window) SDL.glDeleteContext
    _ <- managed $ bracket DearImGui.createContext DearImGui.destroyContext
    _ <- managed_ $ bracket_ (DGL.sdl2InitForOpenGL window glContext) DSDL.sdl2Shutdown
    _ <- managed_ $ bracket_ DGL3.openGL3Init DGL3.openGL3Shutdown
    liftIO $ mainLoop window doDrawGUI


mainLoop :: SDL.Window -> IO () -> IO ()
mainLoop window doDrawGUI = unlessQuit do
  DGL3.openGL3NewFrame
  DSDL.sdl2NewFrame
  DearImGui.newFrame

  -- Build the GUI
  doDrawGUI

  GL.glClear GL.GL_COLOR_BUFFER_BIT
  DearImGui.render
  DGL3.openGL3RenderDrawData =<< DearImGui.getDrawData
  SDL.glSwapWindow window
  mainLoop window doDrawGUI

  where
    unlessQuit action = do
      shouldQuit <- checkEvents
      if shouldQuit then pure () else action
    checkEvents = do
      DSDL.pollEventWithImGui >>= \case
        Nothing ->
          return False
        Just event ->
          (isQuit event ||) <$> checkEvents
    isQuit event =
      SDL.eventPayload event == SDL.QuitEvent
```

> To evaluate the file:
> nixGLIntel nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/da60f2dc9c95692804fa6575fa467e659de5031b.tar.gz -p ghcid -p 'haskellPackages.ghcWithPackages (p: [p.markdown-unlit (haskell.lib.overrideCabal p.dear-imgui { patches = [(fetchpatch {url = "https://patch-diff.githubusercontent.com/raw/TristanCacqueray/dear-imgui.hs/pull/1.patch"; sha256 = "sha256-SKiaS30dPe2xJZ1Th47upWlfxmONWaQnlm8v1DmOF8c=";})];}) p.text p.rio p.OpenGLRaw])' --command 'ghcid -W --test=:main --command "ghci -pgmL markdown-unlit" float-controller.lhs'

## Conclusion

Using the scientific and lens package we implemented a controller to manipulate floating values.


[scientific]: https://hackage.haskell.org/package/scientific-0.3.7.0/docs/Data-Scientific.html#t:Scientific
[rio]: https://hackage.haskell.org/package/rio-0.1.22.0/docs/RIO.html
[lens-tutorial]: https://hackage.haskell.org/package/lens-tutorial-1.0.4/docs/Control-Lens-Tutorial.html
[statevar]: https://hackage.haskell.org/package/StateVar-1.2.2/docs/Data-StateVar.html
