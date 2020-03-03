//////////////////////////////////////////////////////////////////////
// Examples to demonstrate Bacalao
//////////////////////////////////////////////////////////////////////

// First, create an instace for live coding...
b = Bacalao();
s.boot;

// Patterns can be defined using a special preprocessor syntax
// The syntax is: Event key (parameter) name followed by a quoted string
// (without any spaces between the key and the string)
degree"0 2 4 6".play  // Equivalent to: Pbind(\degree, Pseq([0,2,4,6]), \dur, 0.25).play

// Everything in quotes normally fits into a single "unit" of time.
// If tempo is 1, then this "bar" will last one second.
degree"0 4".play
degree"0 2 4 7".play
degree"0 1 2 3 4 5 6 7".play // all the above last one second in total

// You can change tempo
b.tempo = 120/60 // shortcut for b.clock.tempo = 1.5

// We define looping patterns using Bacalao's 'p' method.
// It needs a definition name (a handle), which can be anything,
// but typically a number or Symbol.
b.p(\a, degree"1 2 3 4")

// If you just want to play once without looping:
b.once(\a, degree"1 -2")

// You can make more complex patterns using a syntax similar to TidalCycles:
// [] square brackets make hierarchical sub-patterns
// *N makes a sub-pattern that repeats N times
b.p(\a, degree"[0 1] 2 3*2 4*3")

// Note that you can use abbreviations for many parameters,
// such as 'deg' instead of 'degree'

// You can extend (relative) durations using the @ notation
b.p(\a, deg"0@4 5 7")
// 0 is held four times longer than 5 or 7, which are held 1
// (the whole loop is then normalized to a duration of one bar)

// You can extend the overall pattern to last more bars
b.p(\a, deg"[0 2 4 7]@4")

// You can alternate values in the pattern (choose one each time through)
b.p(\a, deg"<0 1> 2 3 <4 5 7>")
// Note that currently alternation isn't too flexible,
// for example it doesn't properly support sub-sequences.

// You can also specify chords (parallel notes)
// by using commas instead of spaces between angle brackets
b.p(\a, deg"<0,1> 2 3 <4,5,7>")

// Set variables that can be used when parsing pattern strings
b.setParserVariable(\sd, 61)

b.p(\a, midinote"sd*4")

// Arrays can set as variables, then indexed using colon (e.g. "bd:0 bd:2" or (random) "bd:r")
b.setParserVariable(\bd, [47,48,59,60]);

// The parser variable Dictionary is pushed to the current Environment
// when you're working with Bacalao, so if you haven't switched Environment,
// you can just use the simpler notation:
~bd;

// When a variable has an Array, you can index it using "name:index" notation,
// or you can randomly choose one each time, with "name:r" notation.
b.once(\a, midinote"bd bd:3".trace)
b.once(\a, midinote"bd:1 bd:r".trace) // run various times, second note is random from ~bd

// Chord variables can be set using arrays inside the outer array
~chord = [ [0,3,6,9], [-1,0,2,3], [3,6,7,9] ];

b.once(\a, deg"chord chord:1");
b.once(\a, deg"0 chord:r") // run various times

// There are abbreviations for most common Event keys/parameters:
mn"60" // midinote
deg"0" // degree
oct"4" // octave
leg"1.1" // legato
slow"2" // stretch -- not much of an abbreviation, but more similar to TidalCycles notation
ins"\mySynth" // instrument (can also use \inst)

// Rests are specified using tilde (~)
deg"0 1 ~ 3".trace.play
// or you could just use Rest(), but that's longer!
deg"0 1 Rest() 3".trace.play

deg"0 ~@2 3".trace.play
freq"440 <~ 330 ~ 660> 550".play

// There is an alternate kind of pattern: character patterns.
// These represent each event with a single character.

b.p(\a, deg'012 349 ')
// There are default definitions for a-z, A-Z and 0-9, which are
// different based on what type of parameter you're setting, but
// hopefully are reasonable defaults.
// Normally, a-z are notes from middle C (or degree 0-25),
// A-Z are normally two octaves lower than their lower-case counterparts.
// 0-9 are normally the same as a-j.
b.p(\a, deg'abc dej ')
b.p(\a, deg'abcCdejJ')
b.p(\a, note'abcCdejJ')
b.p(\a, mn'Hello There ')

// You can provide bar divisions, usihng '|' or '/' characters.
// The characters in a bar are spread evenly in time,
// so if you have fewer notes they'll play slower to fit the bar.
b.p(\a, mn'hi|bacalao|lovers')
// For more regular spacing, pad to your desired division using spaces (rests).
// You can also use '_' to extend/hold a note for several steps.
b.p(\a, mn'h_i_|bacalao | lovers ')

// Alternatively, you may specify the number of events per bar.
// When you add bar dividers, rests will be added to pad (sync)
// until the next bar.
b.p(\a, mn'8@hi|bacalao|lovers')

// IF the number of events is more than you've specified per bar,
// then it will span several bars.
b.p(\a, mn'5@hi|bacalao|lovers')

// You can use variables to lookup your own custom char patterns.
~weird = (a: 200, b: 250, c: 300, d: 350, e: 400, f: 450);
b.p(\a,       freq'ab dcb d/ab  cd  /de f c d') // using default freqs
b.p(\a, freq~weird'ab dcb d/ab  cd  /de f c d') // using custom freqs

// You can modify the lookup while the pattern is running
~weird.d = 600
~weird.d = { [350,600].choose }

// Clear a running pattern (optional fadeout)
b.clear(\a, 4)

// If you have the ChordSymbol Quark installed, you can do further things:
// (install it from https://github.com/triss/ChordSymbol)
Quarks.install("https://github.com/triss/ChordSymbol");

//////////////////////////////////////////////////////////////////////
// PtimeChain
//////////////////////////////////////////////////////////////////////
b.tempo = 0.6;

// A new Event Pattern called PtimeChain allows you to chain Event Patterns
// (similar to Pchain), but using the timing information from each chained
// Pattern. This is similar to how TidalCycles works.
// Just as the <> operator may be used to instantiate Pchain, the
// operator << may be used to instantitate a PtimeChain pattern.

// This applies the amp 0.01 to the first two notes, and 0.1 to the second two.
// The pan of -1 applies to the first three notes, and 1 applies to the last.
(deg"0 1 2 3" << amp"0.01 0.1" << pan"-1@3 1").trace.play

// In other words, whereas Pchain always takes the next Event from each of its
// patterns, PtimeChain takes into consideration the timing of each chained
// pattern, and steps each one forward so Events from the same times are matched
// up. The final set of Event durations always comes from the left-most Pattern
// in the PtimeChain.

// If a parameter (key) only has a single value (without square brackets),
// there is not considered to be any duration information, so it will always
// return the next value (like Pchain).
(deg"0 1 2 3" << amp"Pexprand(0.01,0.2)").trace.play

// If you apply square brackets around the single element, it will have
// a duration, so the "next" value will be used for each whole pattern cycle.
(deg"0 1 2 3" << amp"[Pexprand(0.01,0.2)]").trace.play

// Note that you can also use variables in the patterns:
a = Pbrown(-7, 7, 2, inf);
(db"-12*8" << deg"a").trace.play

(
var amp = Pexprand(0.1, 0.8);
var note = Pwrand([-4,-2,0,2,4,7], [1,2,3,2,1,0.5].normalizeSum, inf);
(pan"[-0.5 0 0.5]*4@2" << note"note" <> amp"amp" <> leg"0.2").trace.play;
)

// There are some shortcut classes, for example Per() instead of Pexprand()
// for exponentially-distributed random numbers, and Pr() instead of Pwhite()
// for uniform random numbers.

//////////////////////////////////////////////////////////////////////
// Bacalao patterns (loops)
//////////////////////////////////////////////////////////////////////

// Bacalao patterns can work on any SynthDef instrument. Let's define one:

(
SynthDef(\ping, { arg out=0, freq=440, amp=0.1, pan=0.0, gate=1;
	var sig = SinOscFB.ar(freq * Rand(0.995, 1.005!2), ExpRand(0.2, 1.2)).mean;
	var env = EnvGen.kr(Env.asr(0.01, 1, 0.5), gate, doneAction: 2);
	Out.ar(out, Pan2.ar(sig * env, pan, amp));
}).add;
)

b.tempo = 2;

// Bacalao (like TidalCycles) is primarily designed to work with loops of note events.
// You define a looping (or non-looping) pattern like this:
b.p(\melody, deg"5 <2 ~ -2 ~> 4 <3 7 -1> 0 6 4 ~" <> leg"Pexprand(0.2,1)" <> inst"\ping")

// There are a few things you can do to modify patterns (not many so far),
// such as "degrading" it (replacing Events by Rests by a random fraction).
b.p(\melody, deg"5 <2 ~ -2 ~> 4 <3 7 -1> 0 6 4 ~".degrade(0.75) <> leg"0.3" <> oct"Pwhite(4,5)" <> inst"\ping")

b.p(\melody, deg"[0 0 4 4 5 5 4@2 3 3 2 2 1 1 0@2]@2" << inst"\ping")
b.p(\melody, deg"[0 0 4 4 5 5 4@2 3 3 2 2 1 1 0@2]@2".degrade << inst"\ping")
b.p(\melody, deg"[0 0 4 4 5 5 4@2 3 3 2 2 1 1 0@2]@2".perfectShuffle << inst"\ping")
b.p(\melody, deg"[0 0 4 4 5 5 4@2 3 3 2 2 1 1 0@2]@2".scramble << inst"\ping")
b.p(\melody, deg"[0 0 4 4 5 5 4@2 3 3 2 2 1 1 0@2]@2".rand << inst"\ping")
b.p(\melody, deg"[0 0 4 4 5 5 4@2 3 3 2 2 1 1 0@2]@2".reverse << inst"\ping")
b.p(\melody, deg"0 1 2 3".mirror << inst"\ping")
b.p(\melody, deg"0 1 2 3".mirror1 << inst"\ping")
b.p(\melody, deg"0 1 2 3".mirror2 << inst"\ping")
b.p(\melody, deg"0 1 2 3".mirror2 << inst"\ping")
b.p(\melody, deg"[0 0 4 4 5 5 4@2 3 3 2 2 1 1 0@2]@2".rotate(2) << inst"\ping", 1)
b.p(\melody, deg"[0 0 4 4 5 5 4@2 3 3 2 2 1 1 0@2]@2".rotate(-3) << inst"\ping", 1)
b.p(\melody, deg"[0 4 5 6 7@4]".pyramid << inst"\ping")
b.p(\melody, deg"[0 4 5 6 7@4]".pyramid(9) << inst"\ping")
b.p(\melody, deg"[0 4 5 6 7@4]".permute(2) << inst"\ping")
b.p(\melody, deg"[0 4 5 6 7@4]".permute(3) << inst"\ping")

// Instead of looping, you may also play a pattern through just once:
b.once(\melody, deg"5 2 4 -1 0 6 4 ~" <> leg"Pexprand(0.2,1)" <> inst"\ping")
b.once(\melody, deg"-1 5 4 4 0 2 ~ 6" <> leg"Pexprand(0.2,1)" <> oct"4" <> inst"\ping")

// The cycle duration of a looping pattern will be calculated if possible.
// However, you may specify a specific duration, which may either truncateç
// a longer pattern, or extend (with silence) a shorter one:
b.p(\melody, deg"[0 1 2 3]") // the full pattern
b.p(\melody, deg"[0 1 2 3]", 3) // extending it to three bars (one plus two of silence)

// The following pattern "should" last five bars, but here we
// truncate it to just two...
b.p(\melody, deg"[0 1 2 <3 4 5 6 7>]" <> inst"\ping") // full pattern of five bars
b.p(\melody, deg"[0 1 2 <3 4 5 6 7>]" <> inst"\ping", 2) // truncated to two bars
b.p(\melody, deg"[0 1 2 <3 4 5 6 7>]" <> inst"\ping", 2.625) // truncated to 2.625 bars (why not; nice for syncopation!)

// Remember you can extend pattern durations using the @ notation
(
b.p(\melody, deg"[0 1 2 3 4 5 6 7]@1.9" <> pan"-0.7" <> inst"\ping");
b.p(\melody2, deg"[0 1 2 3 4 5 6 7]@2" <> pan"0.7" <> inst"\ping");
)

// Instead of using two named slots, you can create multiple sources for
// a single Bacalao pattern instrument. To do so, you use an Association
// to specify the source index. (No Association means set the '0' source
// and remove all other sources.)
b.clear(\melody2);
b.p(\melody, deg"[0 1 2 3 4 5 6 7]@2" << pan"-0.7" << inst"\ping" << lag"0/8" << leg"0.25", 2);
b.p(\melody -> 1, deg"[0 1 2 3 4 5 6 7]@2".degrade(0.75) << pan"0.7" << inst"\ping" << lag"1/8" << leg"0.25", 2);

// IF you want to redefine the original without stopping the '1' slot, use explicity index '0'
b.p(\melody -> 0, deg"[3 1 0 6 7 5 4 2]@2" << pan"-0.7" << inst"\ping", 2);
b.p(\melody -> 1, deg"[3 0 7 4]@2" << oct"3" << pan"0.7" << inst"\ping", 2);

// There is also a Bjorklund helper pattern to produce masking effects:
b.p(\melody, deg"0 5 3 -1 2 2 9 8" << amp"1 0.1@7" << inst"\ping")
b.p(\melody, deg"0 5 3 -1 2 2 9 8" << amp"1 0.1@7" << inst"\ping" << mask"Pbjork(5,8)")
b.p(\melody, deg"0 5 3 -1 2 2 9 8" << amp"1 0.1@7" << inst"\ping" << mask"Pbjork(3,8,1)")
b.p(\melody, deg"[0 5 3 -1 2 2 9 8]*2" << amp"1 0.1@15" << mask"Pbjork(5,16)" << inst"\ping" )
b.p(\melody, deg"[0 5 3 -1 2 2 9 8]*2" << amp"1 0.1@15" << mask"Pbjork(7,16)" << inst"\ping" )

// You can adjust the volume of a pattern:
b.db(\melody, -6)

// You can clear (and fade out) patterns using clear (or free, which also removes
// VST instruments completely from the server...so normally you should use clear)
[\melody, \melody2].do(b.clear(_, 8))

//////////////////////////////////////////////////////////////////////
// Bacalao effects
//////////////////////////////////////////////////////////////////////

// You can apply effects to pattern instruments, and then pattern their parameters.
b.p(\melody, deg"[0 1 2 3 4 5 6 7]@2" << pan"Pbrown(-0.7,0.7,0.3)" <> (instrument: \ping, amp: 0.5));
b.fx(\melody -> 1, { arg in; BBandPass.ar((in * \overdrive.kr(1, 0.5)).softclip, \bpf.kr(400, 0.5), \bpq.kr(1, 0.5), 2)})
b.fx(\melody -> 2, { arg in; CombL.ar(in.reverse, 1, 0.375 * b.tempo, 2) }, 0.4);
b.fx(\melody -> 3, { arg in; JPverb.ar(in, 3) }, 0.3)

b.pset(\melody -> 100, overdrive"1@3 <15 1 4>" <> slow"2", 6)
b.pset(\melody -> 101, bpf"80 2000" <> bpq"0.5" <> slow"4", 4)
// Note there are new patterns (Psine, Psaw) to do time-based sine and saw waves
b.pset(\melody -> 101, bpf"Psine.exprange(16,-0.25,80,2000)" <> (dur: 1/8, bpq: 1))
b.pset(\melody -> 101, bpf"Psaw.exprange(4,0,80,2000)" <> (dur: 1/8, bpq: 1), 4)
b.pset(\melody -> 101, nil)
b.set(\melody, \bpf, 800); b.set(\melody, \bpq, 1)

b.clear(\melody, 8)

//////////////////////////////////////////////////////////////////////
// Baking pattern arrays
//////////////////////////////////////////////////////////////////////

// There is a very useful helper called Bake, which lets you evaluate any
// code, and puts the result into the clipboard, ready for pasting wherever
// you like.

Bake({((-5..5) ++ ('~' ! 4)).choose}!8) // run this line several times to see the output
Bake(Pbrown(-7,7,2,8))
// Now select the contents of the string after deg and paste the result there
b.p(\melody, deg"~ -5 ~ -4 ~ -2 3 ~" << inst"\ping", quant: 4)
b.p(\melody, deg"1 -5 5 ~ -1 -5 ~ 3" << inst"\ping", quant: 4)
b.p(\melody, deg"2 -4 5 -2 -1 ~ ~ 5" << inst"\ping", quant: 4)
b.p(\melody, deg"-4 -2 0 2 0 2 3 2".degrade(0.75) << inst"\ping", quant: 4)

b.clear(\melody)

//////////////////////////////////////////////////////////////////////
// VST instruments (using VSTPlugin Quark)
// To install it, copy it from: https://git.iem.at/pd/vstplugin/-/releases
// Extract the appropriate version and place it in your Extensions
// folder:
Platform.userExtensionDir.openOS;

//////////////////////////////////////////////////////////////////////
