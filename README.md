# Bacalao
Somewhat fishy live cod(e) extensions to SuperCollider.

![Image of Atlantic cod (Bacalao in Spanish)](HelpSource/Classes/BacalaoLogo.png?raw=true)
([ref](https://commons.wikimedia.org/wiki/File:Atlantic_cod.jpg))

## What is it?
Bacalao is a live coding "domain specific language". You can use
Bacalao's shortened notation to quickly experiment with Node- and
especially Pattern-proxies (which it uses under the covers).

Bacalao was inspired by [TidalCycles](https://tidalcycles.org/), and
adopts a similar syntax for certain things, such as its phrase
definitions.  Similarly to Tidal, Bacalao allows patterns/phrases
(including timing) to be described in a compact notation.  It likewise
supports sub-phrases with repeats and holds (adjustments of note or
sub-phrase relative duration).  Unlike Tidal, it does not depend on
Haskell or any other language. It is coded entirely in SuperCollider
(sclang), building on top of **Event** patterns, **NodeProxy**
and **PatternProxy**. It (optionally) uses the wonderful
[VSTPlugin](https://git.iem.at/pd/vstplugin) extension to play VST
instruments natively inside SuperCollider, allowing their output
to be processed by SC server effects. It can also use the
[ChordSymbol](https://github.com/triss/ChordSymbol) Quark (if
installed) to allow chord notation in note patterns.

## Getting started
To install Bacalao as a Quark in SuperCollider (tested with
versions 3.10.2 and later), run the following line:

```supercollider
Quarks.install("https://github.com/totalgee/bacalao");
```

The **Bacalao** class documention is not up to date. To get started,
it's better to go through the [Bacalao_examples](Bacalao_examples.scd)
file, reading the comments and executing the code line by line. Even
there, however, not everything is documented yet.

There is also a cheatsheet ([part 1](Bacalao_cheatsheet_1.pdf),
[part 2](Bacalao_cheatsheet_2.pdf)).

There are a series of unit tests that should run successfully, so if
you're brave you can also look in those files for ideas or inspiration.

## Issues
Be warned that this is primarily Glen's live coding language and
playground, so it will evolve and change according to his whims, but
it may still be interesting or useful to others. It makes no promises
to be backward (or forward, or sideways) compatible. It's a set of
tools for making music on the fly, so -- swim free, live in the moment,
and let old code... be old code.

## Etymology
Bacalao (the Spanish word for
[cod](https://en.wikipedia.org/wiki/Atlantic_cod)), is simply
the words "live code" written using one-third of the letters (or, even
better: it's "live coding" compressed by nearly 75%)...and then
translated into Spanish to make it sound exotic (and salty!).  In the
translation process, unfortunately, it loses nearly all of those letter
savings. (This may or may not be a metaphor for the gains and
losses you experience using the language itself...) *Bacalao*
also uses two-thirds of the letters of *Barcelona*, which is where
it was spawned and hatched.

Bacalao (or Bakalao, Vacalao, or one of numerous other spellings) also
refers to a repetitive, aggressive
[electronic music style](https://es.wikipedia.org/wiki/Bakalao) that
emerged "Made in Spain" in the mid-1980s, but trust me, that is
purely coincidental...  It is *also* coincidental (or is it?) that the
Atlantic cod population was nearly wiped out around the same time
Bakalao music was reaching its prime. Fortunately, the *fish* (at
least) seem to be making a comeback...let's leave it at that.
