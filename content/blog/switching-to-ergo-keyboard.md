---
title: Switching To An Ergonomic Keyboard
date: 2023-10-28
tags: [blog]
---

This post is a live journal to document my switch to using an ergonomic keyboard.
I am writing it as suggested by a colleague, and I hope that you'll find this entertaining and useful.

## Making a decision

To put this into context: I have been using various keyboards to write copious amounts of code for almost 30 years now.
I used Emacs, then Vim, and now mostly vanilla Emacs with the default key bindings.
My favorite keyboard layout is the 75%, but after relocating back from South Korea, the keyboards I bought all broke right after the warranty was over.
That was really frustrating because I was not able to fix them and I don't want to produce waste.

Thus, last year I switched back to using my laptop keyboard and I developed a bad habit of reaching the default arrow keys using my pinky.
It's worth noting that I don't use fancy navigation or browser plugins and that I rely on the arrows key for reading.
After a while, my right hand became painful and I worried that I could get a serious injury if I kept going like that.

A couple of weeks ago, a friend told me about his switch to a split keyboard and that sparked my curiosity.
After spending half a day checking what I could get, I settled on the Moonlander.
There are lots of option, and it's difficult to make a choice, and since I am not familiar with these systems, the Moonlander looked like the easiest option since it comes pre-built.
I just wished they had a version without any LEDs.


## Waiting

After making the purchase, my curiosity grew over the days, and I was wondering if I made the right decision.
Thankfully, ZSA, the producer of the Moonlander, has really good support and they provide great resources to get started.

Then I read the following resources:

* [Designing a 36-key custom keyboard layout](https://peterxjang.com/blog/designing-a-36-key-custom-keyboard-layout.html)
* [How do you use layers on your keyboard?](https://lobste.rs/s/2ps6iq/how_do_you_use_layers_on_your_keyboard)

… and these made me wonder if I shouldn't have picked the [atreus](https://shop.keyboard.io/products/keyboardio-atreus).

Anyways, yesterday, on a Friday morning I got the "out for delivery" notification which prompt me to prepare the layout.
I didn't want to waste time tinkering my own system, and I simply replicated the layout technomancy generously shared [here](https://atreus.technomancy.us/cheat.pdf).
Using the online configurator, I made this config: https://configure.zsa.io/moonlander/layouts/J5QlB/latest/0

* The base layer:
![ergo-init-layer-base](media/ergo-init-layer-base.png)

* The fun layer:
![ergo-init-layer-fun](media/ergo-init-layer-fun.png)

* The upper layer:
![ergo-init-layer-upper](media/ergo-init-layer-upper.png)



## Unboxing

On Friday evening, eight days after making the order, I got the keyboard.
I flashed the initial layout I prepared and quickly realized that the learning curves was going to be too steep.
This was just too much of a change and I couldn't see how I would be able to get used to such a different setup.
Thus, I made the following adjustments to get something usable more quickly:

* add back the numbers row.
* assign the tab key where it used to be.
* assign a key to turn off the leds.
* remove the unassigned key-cap to help me place my fingers correctly.

After re-flashing the firmware, I got something a bit more usable, but it still felt like using an alien device or an advanced technology:

![ergo-unbox](../static/ergo-unbox.jpg)

My first task was to start Emacs, by typing alt-f2 and "emacs". That was quite a challenge since the function keys are in the upper layer.
This was a big relief because it felt like something I can get used to.
After this first success, it was already getting late and I call it a day.


## First contact

On Saturday, I got some practice by writing this post, my goal was to get used to:

* the ortho layout, in particular the `c` key location.
* the `enter`, `space`, `ctrl` and `alt` location.

Writing prose was surprisingly easy to get used to, however, editing is a very different beast.
I realized that I don't even know what are the keys to make basic operations such as selecting, copy/pasting or undoing.
These are so fundamental that I am relying on muscle memory to hit the right keys.
So I had to manually perform these actions on my previous keyboard to take note of the keys I was hitting.

On one hand it is irritating having to slow down to recall new hand motions, but on the other hand, it is satisfying to see this process getting continuously faster as I am using the keyboard more and more.
I might never reach the proficiency I had before, but it feels like I should achieve a satisfying speed in a couple of weeks.

I am not doing this switch to become faster anyway, it's an investment for my well-being, and I can already feels that it is paying off:
my wrists and fingers barely moved as I am writing this post.


## Let the fun begin

On Sunday, I focused my practice on using the fun layer.
This layer systems is a neat feature that lets you remap keys, similarly to what the shift key does.
This is what really lets you reduce hand motions.

There are 4 options to switch layers, and for now I'm using the `TT` mode, which stands for *tap* *toggle*.
I can access the fun layer by holding the key down, or I can toggle the layer by tapping the key.
Similar to the caps lock, the keyboard has LEDs to indicate which layer is currently activated.
I think it would be easier without the toggle, but since the navigation is on the fun layer, it can be useful to keep it active by tapping the key.

It quickly became natural to hold the fun key to access arrows keys and symbols such as `#` or `*`.
My goal is to eventually get rid of the numbers row, and using a different modifiers for such symbols is not as difficult as I thought it would be.
On the contrary, it is actually easier because using symbols now requires less motion.

I have also realized that the layout I was using didn't have the `%` key.
Perhaps there is a combo to send this symbol, but since I'm aiming for a 44keys layout, and the one I copied was a 42keys layout, I had a spare key to use for `%`.
I am not sure it's a good idea to do such change as I believe the default layout is well thought out.
But I guess it's fine to try changing one key at a time and see if it works better.

I forgot to mention yesterday that while ergonomics was the primary reason for using such a keyboard,
I also enjoy the fact that this keyboard is designed for long term usage with a durable design.
It looks like this one will not end up in the trash and I will be able to repair failures that will happen in the future.


## Getting to work

To be continued...
