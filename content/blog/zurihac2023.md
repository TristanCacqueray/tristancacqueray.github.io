---
title: ZuriHac 2023 trip report
date: 2023-06-13
tags:
  - haskell
  - blog
---

The Haskell community gathered on the [OST][ost] campus for the [ZuriHac][zh2023], a three days hackathon, packed with all sort of events.
This time I had the chance to be there in person and this post is my trip report.

## Context

I am a proud member of this community. Whenever I can, I try to improve the Haskell ecosystem.
For example, I write about the language, create bug reports, update packages to be compatible with the latest GHC releases, and upload new libraries to Hackage.
Therefore I was looking forward to meeting the folks I met online.

Well, I wasn't sure what to expect, so I took some time off my summer vacation to attend the event.
Unfortunately, my initial trip was already tight, thus I arrived at the event jet-lagged and sleep deprived.

In the next sections I describe some of my ZuriHac highlights.


## Verse keynote by Simon Peyton Jones

The very first presentation was delivered by one of the Haskell original authors.
He showed us a new language called Verse that he has been working on at Epic Games.

Simon is such a great speaker, I already watched many of his presentations and I was curious to see what it felt like to be part of the audience.
Indeed, he strives to make his presentation as interactive as possible by engaging with the public in real time.

Thus, when one of the slides introduced a new symbol `Ǝ`, I was able to ask why this syntax was necessary.
Thereafter I had a great discussion with him where he clarified what is this syntax.
Moreover, a week earlier I helped him configure his Emacs text editor and we had a good laugh about it.


## Security Advisories Workshop

The ZuriHac is composed of a mix of presentations and workshops.
As part of my volunteer work with the Haskell Security Response Team (SRT), we spent some time kick-starting the new security-advisories database.
It was great to be able to sit down in a quiet room.
There we managed to help an attendee make a contribution [PR#39](https://github.com/haskell/security-advisories/pull/39) to our hsec-tools project.


## Hallway Track

I was not in the mood to attend the other talks or workshops so I spent most of my time in the hallway.
The Haskell community has been very welcoming, and it was great to be able to thank the folks that helped me out over the past few years.

I made a few demos of [[introducing-butler|butler]], my multiplayer virtual operating system, as well as [[introducing-animation-fractal|animation-fractal]], an engine to create live visuals.
There I got great feedback and new improvement ideas.
In particular, someone generously showed me with a pen and paper how I could implement a filter for my fractal inputs.
I am now looking forward to implementing this technique in a new library.


## Lakeside

The event happened in Rapperswil which is next to a lake, and I have to say, it is a truly wonderful place.
I was stunned by the scenery and the atmosphere, thus I sat on the grass and took a dip in the water whenever I could.

In the evening, when the campus had to close, Haskellers continued the event on the lakeside.
For example, using a pen and paper again, we looked at bezier curves equations to find singularities.

The last evening, I had a lot of fun discussing this `Ǝ` syntax with category theorists.
Now my understanding is that, according to first order logic, there are two types of variables: universals and existentials.
Therefore Verse use this special syntax to bring into scope existential variables, otherwise a variable is universal by default.
Though, Haskell doesn't use this syntax because we can rely on the scope position to infer if a variable is existential.


## Conclusion

I wished I had more energy to attend the other presentations and to make a custom demo for the project presentations.
Overall I am more than happy to have attended this ZuriHac, and I'm looking forward to coming again in the future.

Cheers!

[ost]: https://www.ost.ch/en/university-of-applied-sciences/campus/rapperswil-jona-campus
[zh2023]: https://zfoh.ch/zurihac2023/
