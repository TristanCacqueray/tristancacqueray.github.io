# Varying Modulation

This post shows how to use the [varying](https://hackage.haskell.org/package/varying)
library to create modulations. The goal is to define a value that varies over time,
for example to react smoothly to an external event.

<img src="./static/varying-modulation.svg" width="600" alt="Varying Modulation Plot">

> This document is a literate haskell file

---

```haskell
-- Extensions to create a newtype:
{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
-- Quality of life syntaxic sugar
{-# LANGUAGE ImportQualifiedPost, NamedFieldPuns #-}

import System.Environment (withArgs)
import Control.Concurrent.STM (STM, TVar, atomically, newTVar, readTVar, writeTVar)
import Control.Monad (forM_, forever)

-- https://hackage.haskell.org/package/varying
import Control.Varying.Core qualified as Varying
import Control.Varying.Tween qualified as Varying

-- For the final plot, using https://diagrams.github.io
import Diagrams.Prelude (V2, (&~))
import Diagrams.Backend.SVG.CmdLine (mainWith, B)
-- and https://hackage.haskell.org/package/plots
import Plots (Axis, r2Axis, linePlot, key)
```

A varying [VarT](https://hackage.haskell.org/package/varying-0.8.1.0/docs/Control-Varying-Core.html#t:VarT)
is defined by a time and value type.
In this example we'll use a new type for milli seconds:

```haskell
newtype MSec = MSec Float
    deriving newtype (Num, Show, Real, Ord, Eq, Fractional, Enum)

-- Simplify the VarT definition
type VaryingVar = Varying.VarT STM MSec Float
```

And we'll use this data type to represent a modulation:

```haskell
data Modulation = Modulation
  { name :: String
  , source :: TVar VaryingVar
  , lastPoll :: TVar MSec
  }
```

So here is an example sin modulation, where the VaryingVar is implemented manually:

```haskell
modSIN :: STM Modulation
modSIN = do
  lastPoll <- newTVar 0
  let sinVar = Varying.VarT $ \(MSec dt) -> do
        MSec lastDt <- readTVar lastPoll
        pure (sin ((lastDt + dt) / 1000), sinVar)
  source <- newTVar sinVar
  pure $ Modulation "sin" source lastPoll
```

The varying library enables defining [Tween](https://hackage.haskell.org/package/varying-0.8.1.0/docs/Control-Varying-Tween.html)
to generate intermediate samples, which is great for dynamic animation:

```haskell
-- Simplify the Tween definition
type Tween = Varying.TweenT MSec Float STM Float
```

Tweens let us describe more complex modulation using the do notation,
here is an example of an attack/decay envelop:

```haskell
tweenAD :: Float -> MSec -> MSec -> Tween
tweenAD velocity attack decay = do
    -- raise to velocity value during the attack
    x <- Varying.tween Varying.easeInExpo 0 velocity attack
    -- then slowly go back to 0 during the decay
    Varying.tween Varying.easeOutCubic x 0 decay
```

And we use an helper function to create a 'Modulation' from a 'Tween',
with a resting value of 0.

```haskell
newTweenModulation :: String -> Tween -> STM Modulation
newTweenModulation name tween =
   Modulation name <$> newTVar var <*> newTVar 0
 where
   var = Varying.tweenStream tween 0
```

Which we can use to create the envelop modulation,
here with 500ms attack and 1sec decay:

```haskell
modAD :: STM Modulation
modAD = newTweenModulation "adEnv" (tweenAD 1 500 1000)
```

Here is another example of a periodic modulation:

```haskell
modPulse :: STM Modulation
modPulse = newTweenModulation "pulse" $ forever $ do
    -- raise to 0.5 in 300ms
    Varying.tween_ Varying.easeOutExpo 0 0.5 300
    -- lower to 0 in 600ms
    Varying.tween_ Varying.easeOutExpo 0.5 0 600
```

Now, given a modulation and the current time, we can compute
it's value:

```haskell
pollModulation :: MSec -> Modulation -> STM Float
pollModulation now Modulation{source, lastPoll} = do
  src <- readTVar source
  pre <- readTVar lastPoll
  (value, newSource) <- Varying.runVarT src (now - pre)
  writeTVar lastPoll now
  writeTVar source newSource
  pure value
```

Finally, the diagram rendering code:

```haskell
-- Sample a modulation
runModulation :: Int -> Modulation -> STM [(Float, Float)]
runModulation count modulation = reverse <$> go 0 []
  where
    step :: MSec
    step = 2000 / fromIntegral count
    go n acc
      | n >= count = pure acc
      | otherwise = do
          let now@(MSec nowF) = fromIntegral n * step
          v <- pollModulation now modulation
          go (n + 1) ((nowF, v) : acc)

-- Create the plot
mkPlotMods :: [STM Modulation] -> STM (Axis B V2 Float)
mkPlotMods mkMods = do
  mods <- sequence mkMods
  lines <- traverse (runModulation 40) mods
  pure $ r2Axis &~ do
    forM_ (zip mods lines) $ \(mod, line) -> do
      linePlot line $ key (name mod)

main :: IO ()
main = do
  plot <- atomically (mkPlotMods [modSIN, modAD, modPulse])
  withArgs ["-o", "static/varying-modulation.svg", "-w 400"] $ mainWith plot
```


> To evaluate the file:
> nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/da60f2dc9c95692804fa6575fa467e659de5031b.tar.gz -p ghcid -p "haskellPackages.ghcWithPackages (p: [p.markdown-unlit p.plots p.varying p.diagrams-lib p.diagrams-svg])" --command 'ghcid --test=:main --command "ghci -pgmL markdown-unlit" varying-modulation.lhs'
