//////////////////////////////////////////////////////////////////////
// Bacalao installation
//////////////////////////////////////////////////////////////////////
Quarks.install("https://github.com/totalgee/bacalao");
thisProcess.recompile; // recompile class library after installing

// If you already have it installed and want to make sure it's up-to-date,
// you can do this (if you prefer, just use Quarks.gui to update to the
// latest version visually)
(
Quarks.uninstall("bacalao");
Quarks.update("https://github.com/totalgee/bacalao");
Quarks.install("https://github.com/totalgee/bacalao");
)

//////////////////////////////////////////////////////////////////////
// Examples to demonstrate Bacalao
//////////////////////////////////////////////////////////////////////

// First, create an instance for live coding...
b = Bacalao().boot;

// Pattern basics
// ==============
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
b.tempo = 120/60 // shortcut for b.clock.tempo = 2

// Playing looping and one-time patterns
// =====================================
// We define looping patterns using Bacalao's 'p' method.
// It needs a track name (a handle), which can be anything,
// but typically a number (e.g. 1) or Symbol (e.g. \drum or 'bass').
b.p(\a, degree"1 2 3 4")

// Here, the timing is set so the unit of pattern time is a bar
// (on the default clock, there are four beats per bar, so this pattern
// will take two seconds to play through, at 120bpm or 2 beats per second).

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
b.p(\a, deg"[0 2*2 4*3 7]@4")
// You can also use bar lines to divide things up (often easier to understand)
b.p(\a, deg"0|2*2|4*3|7")


// Basics - chaining patterns in time
// ==================================
// If you're familiar with SuperCollider Events and Patterns, you'll
// know the Pchain composition pattern, normally abbreviated <>.
// Bacalao adds a PtimeChain pattern, abbreviated <<, which you will
// use to combine event patterns, taking into account durations.
// It works similarly to the # (aka |>) operator in TidalCycles, where the
// time "structure" comes from the left-most pattern.

b.p(\a, deg"0 2 4 7" << amp"0.4") // set all note amplitudes to 0.4
b.p(\a, deg"0 2 4 7" << (amp: 0.4)) // same thing, but \amp has no duration
b.p(\a, deg"0 2 4 7" << amp"0.6 0.1@3") // play last notes quieter than first
b.p(\a, deg"0*(5,8) 2 4 7" << amp"0.3 0.7" << pan"[-1 1]*2")
b.hush


// Alternation patterns
// ====================
// You can alternate values in the pattern (choose one each time through, in the order given)
b.p(\a, deg"<0 1> 2 3 <4 5 7>")
b.p(\a, deg"<0 1!3> 2 3 <4 5 6 7>") // you can repeat an element several times
// Note that currently alternation isn't too flexible,
// for example it doesn't properly support sub-sequences.

// Chord patterns
// ==============
// You can also specify chords (parallel notes)
// by using commas instead of spaces between angle brackets
b.p(\a, deg"<0,1> 2 3 <4,5,7>")

// You can specify sub-note repeats using the '*' notation,
// and/or duplication using '!'.
// Repeats occur within a single (sub) event, whereas duplication repeats events.
b.p(\a, deg"0*3 4!2")                 // or deg"[0 0 0] 4 4"
b.p(\a, deg"[0 2]*2 <4 7>!2")         // or deg"[[0 2] [0 2]] <4 7> <4 7>"

// This can lead to quite complex patterns that repeat over a longer period
b.p(\a, deg"[0 <1 2>]*3!2 <4 8 7>!2") // or deg"[[0 <1 2>] [0 <1 2>] [0 <1 2>]] [[0 <1 2>] [0 <1 2>] [0 <1 2>]] <4 8 7> <4 8 7>"

// Euclidean patterns
// ==================
// You can easily define Euclidean sequences (using the "Bjorklund" algorithm),
// for repeats ('*') that use different durations, trying to keep them as
// evenly spaced in time as possible.
// Notation is (numEvents, numSteps, optionalOffset)
// (Bjorklund sequence notation may also be used with duplication ('!'),
// which is...similar to repetition but different ;-)
b.p(\a, deg"0*(3,8) 4*(5,8,-1)" << leg"0.25")
b.p(\a, deg"[0*(3,8) -7 4*(5,8) 7]@2" << leg"0.25")
b.p(\a, deg"[0!(3,7) -7 4!(5,7) 7]@2" << leg"0.25")
b.p(\a, deg"1*(3,8) [2 3]*(2,3,1)" << leg"0.25")


// Variables and pattern lookup
// ============================
// Set variables that can be used when parsing pattern strings
b.varSet(\sd, 61)

b.p(\a, midinote"sd*4")

// Arrays can set as variables, then indexed using colon (e.g. "bd:0 bd:2" or (random) "bd:r")
b.varSet(\bd, [47,48,59,60]);

// The parser variable Dictionary is pushed to the current Environment
// when you're working with Bacalao, so -- as long as you haven't switched
// Environments, you can just use the simpler notation:
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
~dict1 = (lo: 48, hi: [50,51])
~dict2 = (lo: 60, hi: [67,72,79])
b.p(\a, mn~dict1"[lo hi:r]*2")
b.p(\a, mn~dict2"[lo*2 hi:r]*2")

// Changes to these dictionaries are "live"
~dict2.lo = 61 // a fixed value
~dict2[\lo] = rrand(55,65) // a fixed (random) value
~dict2['lo'] = { rrand(55,65) } // a function returning a random value
(
// persistent stream returning a sequence of values
var p = Pbrown(50, 60, 1).trace(prefix: "lo:").asStream;
~dict2.lo = { p };
)

(
var p = Pbrown(70, 80, 3).trace(prefix: "  hi:").asStream;
~dict2.hi = { p };
)

~dict2.lo = 60; ~dict2.hi = [67, 72, { rrand(77,79).postln }];

// Stop the pattern
// We can do it this way, by setting "nothing" as the new pattern
b.p(\a, nil)

// Or we could just clear the whole thing (fades track out over one second, by default)
b.clear(\a)

// Or we can clear all playing tracks
b.clear(b.trks)
// (same thing as)
b.hush

// Parameter abbreviations
// =======================
// There are abbreviations for most common Event keys/parameters:
mn"60" // midinote
deg"0" // degree
oct"4" // octave
leg"1.1" // legato
slow"2" // stretch -- not much of an abbreviation, but similar to TidalCycles
ins"\mySynth" // instrument (can also use inst)

// Rests are specified using tilde (~)
deg"0 1 ~ 3".trace.play
// or you could just use Rest(), but that's longer!
deg"0 1 Rest() 3".trace.play

deg"0 ~@2 3".trace.play
freq"440 <~ 330 ~ 660> 550".play


// Character patterns
// ==================
// There is an alternate kind of pattern: character patterns (using '' instead of "").
// These represent each event with a single character, and rests with spaces.

b.p(\a, deg'012 349 ')
// There are default definitions for a-z, A-Z and 0-9, which are
// different based on what type of parameter you're setting, but
// hopefully are reasonable defaults.
// Normally, a-z are notes from middle C (or degree 0-25),
// A-Z are the same, but two octaves lower than their lower-case counterparts.
// 0-9 are normally the same as a-j.
b.p(\a, deg'abc dej ')
b.p(\a, deg'abcCdejJ')
b.p(\a, note'abcCdejJ')
b.p(\a, mn'Hello There ')
b.p(\a, freq'Hello There ') // default freq is defined as the frequencies of the same (A-Z, a-z and 0-9) midinotes

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

// Pre-defined dictionaries
// ========================
// By default, there are several pre-defined dictionaries.
// You can see the current set of variables by running:
b.varPrint

// The pre-defined dictionary variables include:
// ~amp: 0 (silent) through 9 (loud), plus a-z (low-high) for finer control (A-Z are slightly louder variants)
b.defSet(\a, (instrument: \ping))
b.p(\a, deg'8@abch__ML' << amp'96247621')
// ~freq: 0-9 are middle-C4 through A4, a-z are C4 through Cs6, A-Z are two octaves lower
// ~pan: 0-9 and a-z range from -1 to 1, with letters giving finer control (centre pan would be between 4-5 or m-n)
// You don't really need them -- the above three are used automatically
// when you are setting parameters with those names ('amp', 'freq' and
// 'pan'), but may be useful when setting custom parameters that need
// the same ranges (freq: frequencies, amp: 0 to 1 or pan: -1 to 1).
b.fx(\a->10, b.fxLpf(200, 1))
b.pset(\a->1, p_lpf~freq'A_a_z___' << p_lpq~amp'29191929')

b.clear(\a)

// There are some alternative key mappings (for Native Instruments' Battery 4)
//   that may be useful:
//   ~bat12: Grid mapping for Battery 4, up to six rows (a-f) of 12 columns (1-12)
//           'a1' corresponds to A1 in Battery 4 (MIDI note 36),
//           'b1' is 12 higher, 'c1' is 24 higher...
//   ~bat4: Grid mapping for Battery 4, up to four rows (a-d) of 4 columns (1-4)
//          'a1' corresponds to A1 in Battery 4 (MIDI note 36),
//          'b1' is 4 higher, 'c1' is 8 higher...
//          A lot of expansion kits, like "Halcyon Sky" or "Deep Matter, use this layout.
//   ~batt: Alternative key-based mapping for Battery 4, 4x4 layout
//          For character patterns: key locations physically match the grid.
//           1 (a1)   2 (a2)   3 (a3)   4 (a4)
//           q (b1)   w (b2)   e (b3)   r (b4)
//           a (c1)   s (c2)   d (c3)   f (c4)
//           z (d1)   x (d2)   c (d3)   v (d4)

// If you have Battery 4 installed, you can follow along:
b.vstInit(\drum, "Battery 4")
b.vst(\drum).editor; // now choose a drum kit with 4x12 entries, such as "Session LE Kit"

b.defSet(\drum, (amp: 0.7))
// The following two are equivalent (for 4x4 grids):
b.p(\drum, mn~bat4"a1 a2 a3 a4 | b1 b2 b3 b4 | c1 c2 c3 c4 | d1 d2 d3 d4")
b.p(\drum, mn~batt'1234|qwer|asdf|zxcv')
// Alternatively, you use ~batt to address 4x12 grids using indexing (but not with character patterns)
b.p(\drum, mn~batt"1 [2 2:1] 3 [4 4:2] | 5 [6 6:3] 7 [8:0 8:2] | 9 [10 10:1] 11 12")
b.p(\drum, mn~batt"1 2:1 3:2 4:3 5:2 6:1 7:0 8:1 9:2 10:3 11:2 12:1".mirror1) // nice zig-zag pattern

// You may want to define your own mapping, using keys that make
// sense to you for percussion names. You could do something like
// this:
~dr = ()
~dr.putAll((x: \a1, o: \a3, p: \a5).collect(~bat12.at(_)))

b.p(\drum, mn~bat12"a1 a3 a1 a5")
b.p(\drum, mn~dr'xoxp')
b.p(\drum, mn~batt'1315')
b.p(\drum, mn~batt'8@435drfd84e z  d '.whenmod(8,6, _.faststutter(4) <> _.scramble(552,2)) << amp"Per(0.6,1)")
b.p(\drum->1, mn~bat12"<b1 d1>*(3,8)".every(4->3, _.faststutter(4)))
b.p(\drum->2, mn~bat12"[d12 b4!(7,11)]@5" << amp"[1 0.7@4]@5")

b.clear(\drum)

// ~kb uses a similar scheme to some Native Instruments VST instruments,
//   where the computer keyboard keys map to notes in scales, starting with
//   the white piano keys 'z'=C3 through to 'm'=B3, then 'q'=C4 (middle C)
//   through to 'p'=E5. The black keys are in the corresponding location
//   in the rows above (e.g. 's'=Cs3 and 'd'=Ds3)
//   For the upper rows, shifted values go up an octave, whereas
//   for the lower rows, shifted values go down an octave.
//      2(Cs4)  3(Ds4)       5(Fs4)  6(Gs4)  7(As4)       9(Cs5)  0(Ds5)
//   q(C4)  w(D4)  e(E4)  r(F4)  t(G4)  y(A4)  u(B4)  i(C5)  o(D5)  p(E5)
//      s(Cs3)  d(Ds3)       g(Fs3)  h(Gs3)  j(As3)
//   z(C3)  x(D3)  c(E3)  v(F3)  b(G3)  n(A3)  m(B3)
~starA = mn~kb'zzbbnnb_|vvccxxz_';
~starB = mn~kb'bbvvccx_|bbvvccx_';
b.once(\a, Pseq([~starA, ~starB, ~starA]) << amp"[0.2 0.4]*4") // you're a star!


// Chords and note symbols
// =======================

// If you have the ChordSymbol Quark installed, you can do further things:
// (install it from https://github.com/triss/ChordSymbol)
Quarks.install("https://github.com/triss/ChordSymbol");

// You may specify notes (relative to 0)
b.defSet(\chord, (instrument: \ping, amp: 0.2))
b.p(\chord, note"CM*(5,8)@2 Fmajor*2 GM")
b.p(\chord->0, note"Am@3 FM@5 | CM_F@3 GM@5" << strum"-0.125", quant: 2)

// Using midinote (mn) you can also specify the octave after the note or chord
b.p(\chord->1, mn"A2!3 F2!5 | C2!3 G2!5" << amp"0.4" << toff"[0 0.125]*4", quant: 2)

b.p(\chord->0, note"Am7@3 Fsus2@5 | CM_F7@3 Gsus2@5" << strum"-0.125", quant: 2)

// Something different...
b.p(\chord, mn"D3m D4m D5m | [G5M_A4 G4M_A3 G3M_A2]@2" << strum"4/9 | [-8/9]@2", quant: 3)
b.p(\drum, mn"[36 ~ 38]*3 | [[36*2 38]*3]@2", quant: 3)

b.clear([\chord, \drum])


//////////////////////////////////////////////////////////////////////
// Time chaining
//////////////////////////////////////////////////////////////////////
b.tempo = 0.6;

// A new Event Pattern called PtimeChain allows you to chain Event Patterns
// (similar to Pchain), but using the timing information from each chained
// Pattern.
// This is similar to how TidalCycles patterns compose, using the '#' operator.
// Just as the <> operator may be used in SC to instantiate Pchain, the
// operator << may be used to instantitate Bacalao's PtimeChain pattern.

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
(deg"0 1 2 3".repeat(2) << amp"[Pexprand(0.01,0.2)]").trace.play

// Note that you can also use local or global variables in pattern strings:
(
var a = Pstutter(2, Pbrown(-7, 7, 1, inf)) + Pseq([0,4], inf);
(db"-12*8".repeat(4) << deg"a").trace.play
)

(
// Be careful about naming conflicts with Environment variables, because
// if the variable name is located as a key in the current Environment,
// it will use that, instead of the local variable.
// e.g. If you had defined ~amps = 0.5, it would use that value instead
// of the local variable amps declared here.
var amps = Pexprand(0.1, 0.8);
var notes = Pwrand([-4,-2,0,2,4,7], [1,2,3,2,1,0.5].normalizeSum, inf);
(pan"[-0.5 0 0.5]*4@2".repeat(2) << note"notes" <> amp"amps" <> leg"0.2").trace.play;
)

// There are some shortcut classes, for example Per() instead of Pexprand()
// for exponentially-distributed random numbers, Pr() instead of Pwhite()
// for uniform random numbers, and Pr2() which returns values between +/-value.
// These helpers also take a random seed, so you can get deterministic
// "random" values.
Bake(Per(0.01,1, 1234, 8).round(0.01)) // exponential random from 0.01 to 1 (with seed)
Bake(Pr(0.5,1, 1357, 8).round(0.01)) // uniform random from 0.5 to 1 (with seed)
Bake(Pr2(0.5, nil, 8).round(0.01)) // uniform random bewteen -0.5 and 0.5, different each time)

//////////////////////////////////////////////////////////////////////
// Bacalao patterns (loops)
//////////////////////////////////////////////////////////////////////

// Bacalao patterns can work on any SynthDef instrument. Let's define one:

(
SynthDef(\sinfb, { arg out=0, freq=440, amp=0.1, pan=0.0, gate=1;
	var sig = SinOscFB.ar(freq * Rand(0.995, 1.005!2), ExpRand(0.2, 1.2)).mean;
	var env = EnvGen.kr(Env.asr(0.01, 1, 0.5), gate, doneAction: 2);
	Out.ar(out, Pan2.ar(sig * env, pan, amp));
}).add;
)

b.tempo = 2;

// Bacalao (like TidalCycles) is primarily designed to work with loops of note events.
// You define a looping (or non-looping) pattern like this:
b.p(\melody, deg"5 <2 ~ -2 ~> 4 <3 7 -1> 0 6 4 ~" << leg"Pexprand(0.2,1)" << inst"\sinfb")

// There are a few things you can do to modify patterns (not many so far),
// such as "degrading" it (replacing Events by Rests by a random fraction).
b.p(\melody, deg"5 <2 ~ -2 ~> 4 <3 7 -1> 0 6 4 ~".degrade(0.75) << leg"0.3" << oct"Pwhite(4,5)" << inst"\sinfb")

// We can set a default value for the \melody track,
// so we don't have to keep adding the inst"\sinfb"
b.defSet(\melody, (instrument: \sinfb))

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
// Add a kind of metronome, for contrast
b.p(\drum, deg"0!3 0*2" << ins"\clap_electro" << amp"0.4 0.1@3")
b.clear(\drum)

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
// 'toff' (timingOffset is like 'lag', but tempo-relative rather than in seconds)
b.p(\melody -> 1, deg"[0 1 2 3 4 5 6 7]@2".degrade(0.75) << pan"0.7" << toff"1/4" << leg"0.25", 2);

// If you want to redefine the original without stopping the '1' slot, use index '0' explicity
b.p(\melody -> 0, deg"[3 1 0 6 7 5 4 2]@2" << pan"-0.7", 2);
b.p(\melody -> 1, deg"[3 0 7 4]@2" << oct"3" << pan"0.7", 2);

// Besides the Bjorklund pattern notation "*(k,n,o)" or "!(k,n,o)",
// there is also a way to use Bjorklund patterns to produce masking effects:
b.p(\melody, deg"0 5 3 -1 2 2 9 8" << amp"0.7 0.1@7")
b.p(\melody, deg"0 5 3 -1 2 2 9 8" << amp"0.7 0.1@7" << mask"Pbjork(5,8)")
// mask"Pbjork(...)" can be replaced by PmaskBjork(...) -- longer, but with auto-completion
b.p(\melody, deg"0 5 3 -1 2 2 9 8" << amp"0.7 0.1@7" << PmaskBjork(3,8,1))
b.p(\melody, deg"[0 5 3 -1 2 2 9 8]*2" << amp"0.7 0.1@15" << mask"Pbjork(5,16)")
b.p(\melody, deg"[0 5 3 -1 2 2 9 8]*2" << amp"0.7 0.1@15" << PmaskBjork(7,16))

// You can adjust the volume of a pattern:
b.db(\melody, -6)

// You can clear (and fade out) patterns using clear (or free, which also removes
// VST instruments completely from the server...so normally you should use clear)
b.clear([\melody, \melody2], 8)
b.hush(8) // shorter way, if you want to clear *all* running tracks

//////////////////////////////////////////////////////////////////////
// Bacalao effects
//////////////////////////////////////////////////////////////////////

// You can apply effects to pattern instruments, and then pattern their parameters.
p = Pbrown(-0.7,0.7,0.3);
b.p(\melody, deg"[0 1 2 3 4 5 6 7]@2" << pan"p" << (instrument: \ping, amp: 0.5));
b.fx(\melody -> 1, { arg in; BBandPass.ar((in * \overdrive.kr(1, 0.5)).softclip, \bpf.kr(400, 0.5), \bpq.kr(1, 0.5), 1)})
b.fx(\melody -> 2, { arg in; CombL.ar(in.reverse, 1, 0.375 * b.tempo, 2) }, 0.4);
b.fx(\melody -> 3, { arg in; JPverb.ar(in, 3) }, 0.3)
// There are many built-in effects you can use:
b.fx(\melody -> 2, b.fxDelay(0.375, 2), 0.4);
b.fx(\melody -> 3, b.fxVerb(3), 0.3)

// You can even use function composition for fx:
b.fx(\melody -> 2, b.fxDelay(0.375, 2) <> b.fxCrush(4), 0.4);

b.pset(\melody -> 100, overdrive"1@3 <15 1 4>" << slow"2", 6)
b.pset(\melody -> 101, bpf"80 2000" << bpq"0.5" << slow"4", 4)
// Note there are new patterns (Psine, Psaw) to do time-based sine and saw waves
b.pset(\melody -> 101, bpf"Psine.exprange(16,-0.25,80,2000)" << (dur: 1/8, bpq: 1))
b.pset(\melody -> 101, bpf"Psaw.exprange(4,0,80,2000)" << (dur: 1/8, bpq: 1), 4)
// Stop the "set" pattern
b.pset(\melody -> 101, nil)
// Set values (constant value, not a pattern)
b.set(\melody, \bpf, 800); b.set(\melody, \bpq, 1)

b.clear(\melody, 8)

// Persistent Synth as primary source
// ==================================
// In this more complex example, we use a persistent "analog-like"
// Synth and setting its parameters using patterns.
b = Bacalao().boot.tempo_(1.8)

(
var lag = 0.1;
var synth = {
	var att = \att.kr(0.01, lag);
	var rel = \rel.kr(0.2, lag);
	var freqLag = \freqLag.kr(lag * 2);
	var freq = \freq.kr(440, freqLag);
	var mask = \mask.tr;
	var maskEnv = EnvGen.kr(Env.perc(att, rel), mask) * 1 + 0;
	var amp = \amp.kr(0.2, lag) * maskEnv * (freq > 10);
	var pan = \pan.kr(0, lag);
	var width = \width.kr(0.5, lag);
	var lpf = \lpf.kr(4000, lag);
	var lpq = \lpq.kr(1, lag);
	var n = 4;
	var sig = VarSaw.ar(freq * Rand(0.97, 1.03!n), 0, width);
	sig = Splay.ar(sig, 0.5, center: pan);
	sig = BLowPass.ar(sig, lpf, lpq);
	sig * amp
};
// Start playing the source (persistent)
b.p(\analog, synth);
b.set(\analog, \att, 0.01, 0);
b.set(\analog, \rel, 0.2, 0);
b.set(\analog, \freqLag, lag * 2, 0);
)

// Now, we can pattern-play the arguments:
b.defSet(\analog, (octave: 4, scale: Scale.spanish))
// We wrote our Synth to require a "mask" parameter to trigger amp Envelope
b.pset(\analog->1, deg"0 2*4 4 -2*4") // "I don't hear anything!"
b.pset(\analog->1, deg"0 2*4 4 -2*4" << mask"1") // add "mask" parameter, now we hear it
b.pset(\analog->1, deg"0 2*4 4 -2*4", includeMask: true) // another way to do the same
// You can actually have two patterns setting notes at different rates
b.pset(\analog->2, deg"[~@2  <14 21>]*2" << mask"1")

b.pset(\analog->1, freq"200 300*4 150 180".every(4->3, _.fast) << amp"0.5" << freqLag"0.1" << mask"1")
b.pset(\analog->1, dur"1/16" << note"Psine.range(4,-0.25,-7,7).round" << amp"0.5 0.1@3" << pan'48273815' << mask"1")
b.pset(\analog->1, mask"1*(11,16)" << deg"Psine.range(3.5,-0.25,-7,7).round" << amp"0.7 0.2@3" << freqLag"0.01");

// Now we can set some "non-note" patterns to set parameters
b.fxPrintControls(\analog) // show all controls (per slot) and their current and default values
b.pset(\analog->3, att"0.01 0.1@3" << rel"0.05 0.3@3"); // don't include mask here, no triggering "notes"
b.pset(\analog->4, dur"1/16" << lpf"Psaw.exprange(3,0,110,8000)" << lpq"Psine.exprange(4.5,-0.25,0.2,1.2)")
b.fx(\analog->50, b.fxDelayDub(0.5, 0.6, 0.004) <> b.fxDistort(2), 0.5)
b.pset(\analog->5, delayPre"[4 0.2!3]@2" << distGain"2".every(4->3, _.set(\distGain, 30)))
b.db(\analog, -3)

// Add another audio source (not a filter/effect)
(
b.p(\analog->40, {
	var n=4;
	Splay.ar(Ringz.ar(Decay.ar(Dust2.ar(0.1!n), 3, Impulse.ar(ExpRand(4,10!n))), ExpRand(440,4400!n), 0.2), 0.75)
})
)
b.p(\analog->40, nil) // remove a given slot, whether it's a pattern or an audio effect
b.gui // show the NdefGui for all active tracks (useful for debugging)

// Add some drums:
b.printSynths // show which SynthDefs are currently defined, and pick some
b.p(\drum, inst"[\kick_electro@3 <\snare_electro \snare909>@2]*2".every(4->3, _.degrade <> _.faststutter(4) <> _.scramble) << amp"0.4 0.2@3")

b.fxClear(\analog) // removes all audio slots other than 0
b.removeSlots(\analog, (2..5)) // to remove patterns or audio/fx slots
b.showTree

b.free(b.trks, 8)

//////////////////////////////////////////////////////////////////////
// Baking pattern arrays
//////////////////////////////////////////////////////////////////////

// There is a very useful helper called Bake, which lets you evaluate any
// code, and puts the result into the clipboard, ready for pasting wherever
// you like.

Bake({((-5..5) ++ ('~' ! 4)).choose}!8) // run this line several times to see the output
Bake(Pbrown(-7,7,2,8))
// Now select the contents of the string after deg and paste the result there
b.p(\melody, deg"~ -5 ~ -4 ~ -2 3 ~" << inst"\ping", quant: 2)
b.p(\melody, deg"1 -5 5 ~ -1 -5 ~ 3" << inst"\ping", quant: 2)
b.p(\melody, deg"2 -4 5 -2 -1 ~ ~ 5".whenmod(6,4, _.reverse).every(6->5, _.faststutter) << inst"\ping", quant: 2)
b.p(\melody, deg"-4 -2 0 2 0 2 3 2".degrade(0.75) << inst"\ping", quant: 2)

b.clear(\melody)

//////////////////////////////////////////////////////////////////////
// VST instruments (using VSTPlugin Extension)
//////////////////////////////////////////////////////////////////////

// To install VSTPlugin, copy it from: https://git.iem.at/pd/vstplugin/-/releases
// Extract the appropriate version and place it in your Extensions
// folder:
Platform.userExtensionDir.openOS;

b = Bacalao().boot;
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

// Or you can also read/change preset without changing the VST
// (this will also set the VST plugin's tempo to match b.tempo):
b.vstRead(\piano, "piano")

// Note that some VST instruments (e.g. Native Instruments Reaktor ensembles)
// don't seem to be found when reloading presets saved this way. The
// "trick" I found was to save it as a User Ensemble in Reaktor.

// If you want to know where it's saved that preset, there is a
// separate directory per plugin:
b.vstPresetDir(\piano).openOS

// To stop playing VST instruments, you should clear(), which will
// remove all patterns and effects, but will leave the VST Synth
// still running on a NodeProxy, so you can use it again.
b.clear(\piano, 4)

// Clear also turns off processing for the VST instrument, so it
// won't consume CPU. But it can be reactivated in future without
// caling vstInit()!
(
b.p(\piano, mn"48 <50 51> 55 <60 58 57 56 53>".every(10->9, { |p| Ppar([p, p.add(\midinote, 12) << toff"0.25"]) }) << amp"0.5");
b.p(\piano->1, mn"36".every(5->3, mn"36!(3,7) <43,55,67,79,91,79,67,55,43,31>") << amp"0.6".every(5, _.mul(\amp, 1.5)) << strum"1/8");
)

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
b.fx(\plink -> 10, b.fxDelay(0.75, 2), 0.4)

(
b.fx(\plink -> 10, { arg in;
	var drive = (in * LFDNoise3.kr(1).exprange(0.5,50)).distort;
	CombL.ar(BLowPass.ar(drive, SinOsc.kr(b.tempo / 4).exprange(80,1200), 1), 2, 0.75 / b.tempo, 2)
}, 0.5)
)

b.fx(\plink -> 20, b.fxVerb(4, 0.2), 0.3)

// You can also remove all VST patterns and definitions in one go, by calling:
b.vstFreeAll(5)

//////////////////////////////////////////////////////////////////////
// Playing samples and Buffers
//////////////////////////////////////////////////////////////////////

b = Bacalao().boot.tempo_(2);

// Buffers
// =======
// You can load a Buffer any way you like, using SuperCollider, but
// there is a helper:
b.loadBuffer(Platform.resourceDir +/+ "sounds/a11wlk01.wav")
// This loads a SC Buffer and saves it in a Bacalao variable: ~a11wlk01
~a11wlk01.duration

// NOTE: you can drag any audio file from your File Explorer or Finder
// into the SC IDE, e.g. into the parentheses of "b.loadBuffer()", and
// it will past the complete path as a string, which is very convenient.

// Play the sound in a loop
b.p(\loop, b.chop("a11wlk01")) // accepts string name or a Buffer

// Note that, by default, it loops it over the closest number of bars,
// and adjusts the playback rate so there are no gaps.
// You may change various parameters to get different effects:
b.p(\loop, b.chop(~a11wlk01, 16, 2, 1))
b.db(\loop, 6) // increase volume a bit

// And you can apply functions to the chopped-up loop pattern
b.p(\loop, b.chop(~a11wlk01, 16, 2, 1).reverse)
b.p(\loop, b.chop(~a11wlk01, 16, 1, 0.7).reverse.every(4->3, _.scramble(555)))

b.clear(\loop)

// To see what buffers (and other variables) you have active, you can do this:
b.varPrint

// There's a convenience GUI to allow loading Buffers (and samples) by dropping files:
b.dropGui.alwaysOnTop_(true)

// Samples
// =======
// Samples are similar to Buffers -- they are also based on audio files
// loaded into Buffers, but they are added to a dictionary to be played
// as "events", by reference.

// Normally, you should have a directory of one or more related samples
// with a short name (similar to the layout of the Dirt-Samples files,
// if you have that Quark installed):
Quarks.quarkNameAsLocalPath("Dirt-Samples").openOS

// For example, let's assume you dragged the "808" and "bd" directories from
// Dirt-Samples on top of the "Samples" box in the dropGui.
// It should have been added to the ~samp dictioary with the name '808'.
~samp['808'].size // I have 6 samples here

// To play them, you must use an Event lookup pattern. Event
// lookup is done with '@' instead of a parameter name, because
// the Events may contain several parameters. These must be
// looked up in a dictionary, whose name you must specify.
// In the case of sample playback, the dictionary is named ~samp:

b.defSet(808, (amp: 0.4))
b.p(808, @~samp"808*4")
b.p(808, @~samp"808:5*(5,8) 808:3*(3,8)")
b.p(808, @~samp"808:5*(5,8) [808:3!(4,5) ~ 808:r ~]")
b.p(808, @~samp"808:5*(5,8) [808:3!(4,5) ~ 808:r ~]".every(4, _.perfectShuffle))
b.p(808->1, @~samp"bd:r*4")
b.p(808->1, @~samp"bd:r*4".every(8->7, _.faststutter(4)))

// Let's assume you loaded the "numbers" directory from Dirt-Samples:
b.p(\n, @~samp"numbers:0 <numbers:2 numbers:4> <~!2 numbers:r>" << amp"0.5")
// You can also play the samples as "notes", adjusting the playback speed relative to middle C
b.p(\n, note"0 0 | 2*4 [4!3 -1]" << @~samp"numbers:r" << amp"0.5", quant: [1, 0.95])
b.p(\n, note"0*4 | 2*3 4*2 <-1 -3>*2 | -16 | -28 [8 7 6 5 4 3 2 1]" << @~samp"numbers:8 numbers:r" << amp"0.8 0.4!3", quant: [1, 0.95])
b.hush

//////////////////////////////////////////////////////////////////////
// Modifier functions on Patterns
//////////////////////////////////////////////////////////////////////

