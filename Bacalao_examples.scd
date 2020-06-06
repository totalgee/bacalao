//////////////////////////////////////////////////////////////////////
// Examples to demonstrate Bacalao
//////////////////////////////////////////////////////////////////////

// First, create an instace for live coding...
b = Bacalao().boot;

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
// You can also use bar lines to divide things up (often easier to understand)
b.p(\a, deg"0|2|4|7")

// You can alternate values in the pattern (choose one each time through)
b.p(\a, deg"<0 1> 2 3 <4 5 7>")
// Note that currently alternation isn't too flexible,
// for example it doesn't properly support sub-sequences.

// You can also specify chords (parallel notes)
// by using commas instead of spaces between angle brackets
b.p(\a, deg"<0,1> 2 3 <4,5,7>")

// You can specify sub-note repeats using the '*' notation,
// and/or duplication using '!'.
// Repeats occur within a single event, whereas duplication repeats events.
b.p(\a, deg"0*3 4!2")                 // or deg"[0 0 0] 4 4"
b.p(\a, deg"[0 2]*2 <4 7>!2")         // or deg"[[0 2] [0 2]] <4 7> <4 7>"
b.p(\a, deg"[0 <1 2>]*3!2 <4 8 7>!2") // or deg"[[0 <1 2>] [0 <1 2>] [0 <1 2>]] [[0 <1 2>] [0 <1 2>] [0 <1 2>]] <4 8 7> <4 8 7>"

// You can also use "Bjorklund" sequences, for repeats ('*') that
// use different durations, trying to keep them as evenly spaced
// in time as possible. Notation is (numEvents, numSteps, optionalOffset)
// (Bjorklund sequence notation may also be used with duplication ('!'),
// which is similar to repetition but different ;-)
b.p(\a, deg"0*(3,8) 4*(5,8,-1)" << leg"0.25")
b.p(\a, deg"[0*(3,8) -7 4*(5,8) 7]@2" << leg"0.25")
b.p(\a, deg"[0!(3,7) -7 4!(5,7) 7]@2" << leg"0.25")
b.p(\a, deg"1*(3,8) [2 3]*(2,3,1)" << leg"0.25")

// Set variables that can be used when parsing pattern strings
b.varSet(\sd, 61)

b.p(\a, midinote"sd*4")

// Arrays can set as variables, then indexed using colon (e.g. "bd:0 bd:2" or (random) "bd:r")
b.varSet(\bd, [47,48,59,60]);

// The parser variable Dictionary is pushed to the current Environment
// when you're working with Bacalao, so -- as long as you haven't switched
// Environment, you can just use the simpler notation:
~bd;

// When a variable has an Array, you can index it using "name:index" notation,
// or you can randomly choose one each time, with "name:r" notation.
b.once(\a, midinote"bd bd:3".trace)
b.once(\a, midinote"bd:1 bd:r".trace) // run various times, second note is random from ~bd

// Chord variables can be set using arrays inside the outer array
~chord = [ [0,3,6,9], [-1,0,2,3], [3,6,7,9] ];

b.once(\a, deg"chord chord:1");
b.once(\a, deg"0 chord:r") // run various times

// You can also define variables that are Dictionaries, to be used
// to lookup variables in different pattern definitions.
~dict1 = (bd: 48, sd: [50,51])
~dict2 = (bd: 60, sd: [67,72,79])
b.once(\a, mn~dict1"[bd sd:r]*2")
b.once(\a, mn~dict2"[bd sd:r]*2")

// There are abbreviations for most common Event keys/parameters:
mn"60" // midinote
deg"0" // degree
oct"4" // octave
leg"1.1" // legato
slow"2" // stretch -- not much of an abbreviation, but more similar to TidalCycles notation
ins"\mySynth" // instrument (can also use inst)

// Rests are specified using tilde (~)
deg"0 1 ~ 3".trace.play
// or you could just use Rest(), but that's longer!
deg"0 1 Rest() 3".trace.play

deg"0 ~@2 3".trace.play
freq"440 <~ 330 ~ 660> 550".play

// There is an alternate kind of pattern: character patterns (using '' instead of "").
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

// You can provide bar divisions, using '|'.
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
b.p(\a,       freq'ab dcb d|ab  cd  |de f c d') // using default freqs
b.p(\a, freq~weird'ab dcb d|ab  cd  |de f c d') // using custom freqs

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
// Be careful about naming conflicts with Environment variables, because
// if the variable name is located as a key in the current Environment,
// it will use that, instead of the local variable.
// e.g. If you had defined ~amps = 0.5, it would use that value instead
// of the local variable amps declared here.
var amps = Pexprand(0.1, 0.8);
var notes = Pwrand([-4,-2,0,2,4,7], [1,2,3,2,1,0.5].normalizeSum, inf);
(pan"[-0.5 0 0.5]*4@2" << note"notes" <> amp"amps" <> leg"0.2").trace.play;
)

// There are some shortcut classes, for example Per() instead of Pexprand()
// for exponentially-distributed random numbers, Pr() instead of Pwhite()
// for uniform random numbers, and Pr2() which returns values between +/-value.
// These helpers also take a random seed, so you can get deterministic
// "random" values.
Bake(Per(0.01,1, 1234, 8).round(0.01)) // exponential random from 0.1 to 1 (with seed)
Bake(Pr(0.5,1, 1357, 8).round(0.01)) // uniform random from 0.5 to 1 (with seed)
Bake(Pr2(0.5, nil, 8).round(0.01)) // uniform random bewteen -0.5 and 0.5, different each time)

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

// We can set a default value for the \melody track,
// so we don't have to keep adding the inst"\ping"
b.defSet(\melody, (instrument: \ping))

b.p(\melody, deg"[0 0 4 4 5 5 4@2 3 3 2 2 1 1 0@2]@2")
b.p(\melody, deg"[0 0 4 4 5 5 4@2 3 3 2 2 1 1 0@2]@2".degrade)
b.p(\melody, deg"[0 0 4 4 5 5 4@2 3 3 2 2 1 1 0@2]@2".perfectShuffle)
b.p(\melody, deg"[0 0 4 4 5 5 4@2 3 3 2 2 1 1 0@2]@2".scramble)
b.p(\melody, deg"[0 0 4 4 5 5 4@2 3 3 2 2 1 1 0@2]@2".rand)
b.p(\melody, deg"[0 0 4 4 5 5 4@2 3 3 2 2 1 1 0@2]@2".reverse)
b.p(\melody, deg"0 1 2 3".mirror)
b.p(\melody, deg"0 1 2 3".mirror1)
b.p(\melody, deg"0 1 2 3".mirror2)
b.p(\melody, deg"[0 0 4 4 5 5 4@2 3 3 2 2 1 1 0@2]@2".rotate(2), 1)
b.p(\melody, deg"[0 0 4 4 5 5 4@2 3 3 2 2 1 1 0@2]@2".rotate(-3), 1)
b.p(\melody, deg"[0 4 5 6 7@4]".pyramid)
b.p(\melody, deg"[0 4 5 6 7@4]".pyramid(9))
b.p(\melody, deg"[0 4 5 6 7@4]".permute(2))
b.p(\melody, deg"[0 4 5 6 7@4]".permute(3))

// Instead of looping, you may also play a pattern through just once:
b.once(\melody, deg"5 2 4 -1 0 6 4 ~" <> leg"Pexprand(0.2,1)")
b.once(\melody, deg"-1 5 4 4 0 2 ~ 6" <> leg"Pexprand(0.2,1)" <> oct"4")

// The cycle duration of a looping pattern will be calculated if possible.
// However, you may specify a specific duration, which may either truncate
// a longer pattern, or extend (with silence) a shorter one:
b.p(\melody, deg"[0 1 2 3]") // the full pattern
b.p(\melody, deg"[0 1 2 3]", 3) // extending it to three bars (one plus two of silence)

// The following pattern "should" last five bars, but here we
// truncate it to just two...
b.p(\melody, deg"[0 1 2 <3 4 5 6 7>]") // full pattern of five bars
b.p(\melody, deg"[0 1 2 <3 4 5 6 7>]", 2) // truncated to two bars
b.p(\melody, deg"[0 1 2 <3 4 5 6 7>]", 2.625) // truncated to 2.625 bars (why not; nice for syncopation!)

// Remember you can extend pattern durations using the @ notation
(
b.p(\melody, deg"[0 1 2 3 4 5 6 7]@1.9" <> pan"-0.7");
b.p(\melody2, deg"[0 1 2 3 4 5 6 7]@2" <> pan"0.7" <> inst"\ping");
)

// Instead of using two named slots, you can create multiple sources for
// a single Bacalao pattern instrument. To do so, you use an Association
// to specify the source index. (No Association means set the '0' source
// and remove all other sources.)
b.clear(\melody2);
b.p(\melody, deg"[0 1 2 3 4 5 6 7]@2" << pan"-0.7" << lag"0/8" << leg"0.25", 2);
b.p(\melody -> 1, deg"[0 1 2 3 4 5 6 7]@2".degrade(0.75) << pan"0.7" << lag"1/8" << leg"0.25", 2);

// If you want to redefine the original without stopping the '1' slot, use index '0' explicity
b.p(\melody -> 0, deg"[3 1 0 6 7 5 4 2]@2" << pan"-0.7", 2);
b.p(\melody -> 1, deg"[3 0 7 4]@2" << oct"3" << pan"0.7", 2);

// Besides the Bjorklund pattern notation "*(k,n,o)" or "!(k,n,o)",
// there is also a way to use Bjorklund patterns to produce masking effects:
b.p(\melody, deg"0 5 3 -1 2 2 9 8" << amp"1 0.1@7")
b.p(\melody, deg"0 5 3 -1 2 2 9 8" << amp"1 0.1@7" << mask"Pbjork(5,8)")
// mask"Pbjork(...)" can be replaced by PmaskBjork(...) -- longer, but with auto-completion
b.p(\melody, deg"0 5 3 -1 2 2 9 8" << amp"1 0.1@7" << PmaskBjork(3,8,1))
b.p(\melody, deg"[0 5 3 -1 2 2 9 8]*2" << amp"1 0.1@15" << mask"Pbjork(5,16)")
b.p(\melody, deg"[0 5 3 -1 2 2 9 8]*2" << amp"1 0.1@15" << PmaskBjork(7,16))

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
b.fx(\melody -> 1, { arg in; BBandPass.ar((in * \overdrive.kr(1, 0.5)).softclip, \bpf.kr(400, 0.5), \bpq.kr(1, 0.5), 1)})
b.fx(\melody -> 2, { arg in; CombL.ar(in.reverse, 1, 0.375 * b.tempo, 2) }, 0.4);
b.fx(\melody -> 3, { arg in; JPverb.ar(in, 3) }, 0.3)

b.pset(\melody -> 100, overdrive"1@3 <15 1 4>" <> slow"2", 6)
b.pset(\melody -> 101, bpf"80 2000" <> bpq"0.5" <> slow"4", 4)
// Note there are new patterns (Psine, Psaw) to do time-based sine and saw waves
b.pset(\melody -> 101, bpf"Psine.exprange(16,-0.25,80,2000)" <> (dur: 1/8, bpq: 1))
b.pset(\melody -> 101, bpf"Psaw.exprange(4,0,80,2000)" <> (dur: 1/8, bpq: 1), 4)
// Stop the "set" pattern
b.pset(\melody -> 101, nil)
// Set values (constant value, not a pattern)
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

b = Bacalao();
b.tempo = 90/60;

// Show which VST instruments are available on your system (you may
// specify an extra search path argument if desired).
b.vstPrintInstruments(onlyWithPresets: false)

// If you see some instruments listed in the Post window, choose one to load:
b.vstInit(\piano, "FM8")

// You need to wait for it to load, it may take a few seconds.
// This happens asynchronously, so if other patterns are playing,
// they should continue without glitches, but you can't access
// the language for a moment.

// Now use the VST to play something with its default configuration
b.p(\piano, deg"0 2 4 7 [6 5] [4 3] 2 -1" << PampRand(0.4,0.6))

// If you don't hear anything, or if you don't like the preset,
// you can modify it:
b.vst(\piano).editor
// Now the VST's window should be open (you'll need to bring it to the foreground)

// After making some changes, loading presets, etc., you can
// save a preset for quick loading in future:
b.vst(\piano).savePreset("piano")
// b.vst(\piano) gives you access to the VSTPluginController object:
VSTPluginController.help // or HelpBrowser.openHelpFor("VSTPluginController")

// In future, you can directly load it like this:
b.vstInit(\piano, "FM8", "piano")

// Or you can also read/change preset without changing the VST:
b.vstRead(\piano, "piano")

// If you want to know where it's saved that preset, there is a
// separate directory per plugin:
b.vstPresetDir(\piano).openOS

// To stop playing VST instruments, you should clear(), which will
// remove all patterns and effects, but will leave the VST Synth
// still running on an Ndef, so you can use it again.
b.clear(\piano, 4)

// Clear also turns off processing for the VST instrument, so it
// won't consume CPU. But it can be reactivated in future without
// caling vstInit()!
b.p(\piano, mn"48 50 55 60" << amp"0.6")

// If you call free(), it will perform a clear() but also remove
// the VST Synth.
b.free(\piano)

// Now, the \piano definition is totally empty. If we play something
// on it  now, it will use the 'default' SuperCollider Synth instead,
// no longer a VST.
b.once(\piano, note"0 1 2 3")


// If you have a VST instrument that responds to MIDI bank and
// program change, you can set them like this on load:
// (use an Association for (bank -> program), in this example
// we request bank 7 and program 3)
b.vstInit(\plink, "AAS Player", bankAndProgram: 7 -> 3)

b.p(\plink, deg"-7 -9 -5 ~" << PampRand(0.4,0.8))

b.vst(\plink).editor

// Change to a different bank and program:
b.vstBankProgram(\plink, 2 -> 20)

// You can apply SuperCollider effects (fx) to definitions,
// whether they be for VST instruments or regular SynthDefs.
b.fx(\plink -> 10, { arg in; CombL.ar(in, 2, 0.75 / b.tempo, 2) }, 0.4)

(
b.fx(\plink -> 10, { arg in;
	var drive = (in * LFDNoise3.kr(1).exprange(0.5,50)).distort;
	CombL.ar(BLowPass.ar(drive, SinOsc.kr(b.tempo / 4).exprange(80,1200), 1), 2, 0.75 / b.tempo, 2)
}, 0.5)
)

b.fx(\plink -> 20, { arg in; JPverb.ar(in, 4, 0.2) }, 0.3)


// You can also remove all VST patterns and definitions in one go, by calling:
b.vstFreeAll(5)
