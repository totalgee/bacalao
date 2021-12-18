// Bacalao Pattern helper classes
//   by Glen Fraser

// Pattern that takes a function operating on an Array (like _.scramble,
// _.reverse, _.mirror or _.rotate(-1)), a duration of "clumped" Events
// to consider for the operation
Parrop : FilterPattern {
	var <>arrayOperation;
	var <>dur;
	const tolerance = 0.001;

	// By default, we take a long (but not infinite) duration, so it will
	// take "all" the events for the pattern. If you explicitly want to
	// process a shorter (or longer!) timeframe, you can set it. But the default
	// should be enough to normally take "all" events -- without getting stuck,
	// should you pass in an infinite pattern.

	*new { arg pattern, arrayOperation, dur;
		dur = dur ? 128;
		if (dur <= 0) { Error("Parrop duration must be > 0").throw };
		^super.new(pattern).arrayOperation_(arrayOperation).dur_(dur);
	}

	storeArgs { ^[pattern,arrayOperation,dur] }

	prReturnEvents { arg allEvents, inEvent;
		// Group the incoming events into things at the same time (for Ppar to "work")
		// @note I say "work" -- because it isn't really handled properly. It
		//       reverses/scrambles based on the Event onsets, but in the case
		//       of (e.g.) reversing, one would need to compute new start times
		//       and deltas, because the note end time for two Events starting
		//       at the same time is generally different.
		//       See: https://scsynth.org/t/event-pattern-to-scramble-events/1692/6
		allEvents = allEvents.separate{ arg a, b; (a.key < b.key) };
		// Perform the operation on the Array of Events and yield them all before returning
		allEvents = arrayOperation.value(allEvents);
		// Flatten it back into a simple list
		allEvents = allEvents.flat;
		allEvents.do{ arg timeEvent, index;
			var origTime = timeEvent.key;
			var ev = timeEvent.value;
			// timeEvent.debug("origTime -> event");
			inEvent = inEvent.composeEvents(ev).yield
		};
		^inEvent
	}

	embedInStream { arg inEvent;
		var delta, elapsed = 0.0, nextElapsed, outEvent;
		var localdur = dur.value(inEvent);
		var stream = pattern.asStream;
		var cleanup = EventStreamCleanup.new;
		var allEvents = [];
		var originalInEvent = inEvent;
		inEvent = (); // use a "dummy" Event for the initial skipping-forward part of our operation
		loop {
			outEvent = stream.next(inEvent).asEvent;
			if (outEvent.isNil) {
				inEvent = this.prReturnEvents(allEvents, originalInEvent);
				^cleanup.exit(inEvent)
			};

			cleanup.update(originalInEvent);
			delta = outEvent.delta ?? 1.0; // for Patterns without dur or delta...use default
			nextElapsed = elapsed + delta;
			if (nextElapsed.roundUp(tolerance) >= localdur) {
				// must copy an event before altering it.
				// fix remaining time and yield all the events.
				// We *could* set \delta or \dur here: each has a different meaning
				// Do we want to truncate the length of the last note,
				// or just make sure the pattern's duration (for looping) fits?
				// Pfindur sets \delta, but because we're going to change the
				// order, we modify duration...not sure if what's "best".
				outEvent = outEvent.copy.put(\dur, localdur - elapsed);
				allEvents = allEvents.add(elapsed -> outEvent);
				inEvent = this.prReturnEvents(allEvents, originalInEvent);
				^cleanup.exit(inEvent);
			};

			allEvents = allEvents.add(elapsed -> outEvent);
			elapsed = nextElapsed;
		}
	}
}

// This is a modified version of Pn, which aborts if its embedded Pattern
// never returns any events (e.g. Pn(Pbind(\degree, 0,  \dur, 1)*2) would
// cause an infinite loop, but with PnSafe it throws an Error instead).

// Improved version (with numEmpty) came out of discussion:
//   https://github.com/supercollider/supercollider/issues/4765
PnSafe : FilterPattern {
	var <>repeats, <>key, <>numEmpty;
	*new { arg pattern, repeats = inf, key, numEmpty = 10;
		^super.newCopyArgs(pattern, repeats, key, numEmpty)
	}
	storeArgs { ^[pattern, repeats, key, numEmpty] }

	embedInStream { | event |
		var countEmpty = 0;
		var embedOnce = { arg inval;
			var stream = pattern.asStream;
			var outval;
			var returnedSomething = false;
			while {
				outval = stream.value(inval);
				outval.notNil
			}{
				inval = outval.yield;
				returnedSomething = true;
			};
			if (returnedSomething) {
				countEmpty = 0;
			} {
				countEmpty = countEmpty + 1;
			};
			if (countEmpty >= numEmpty) {
				"PnSafe sub-pattern returned no Events".warn;
				inval = nil.alwaysYield;
			};
			inval
		};

		if(key.isNil) {
			repeats.value(event).do {
				event = embedOnce.value(event)
			}
		} {
			repeats.value(event).do {
				event[key] = true;
				event = embedOnce.value(event)
			};
			event[key] = false;
		};
		^event;
	}
}

// Version of Pnsym that returns Rest() by default (rather than 1)
// It always returns Rest for spaces, but for other failed lookups
// it also posts a warning.
// It also handles indexing, using 'name:2' notation, and random
// choice using 'name:r' notation.
// (See also Bacalao.varLookup)
PnsymRest : Psym {

	*prVarLookup { arg dict, key;
		var elem = key.asString;
		^dict !? {
			var variable, index, substitute;
			// Allow (e.g.) "bd:5"-style indexing, or simply "bd" (equivalent to "bd:0")
			// Also allow "bd:r", which will choose a random value from the collection.
			#variable, index = elem.split($:);
			substitute = dict.at(variable.asSymbol);
			substitute !? {
				if (index.notNil and: { substitute.isSequenceableCollection }) {
					if (index == "r" and: { substitute.size > 1 }) {
						substitute.asArray.choose
					} {
						substitute.asArray.wrapAt(index.asInteger)
					}
				} {
					substitute.first
				}
			}
		}
	}

	lookupClass { ^Pdefn }

	lookUp { arg key;
		key = key.asSymbol;
		if (key == ' ' or: { key == '~' } or: { key == 'Rest()' }) {
			// Return Rest without complaining
			^this.rest
		} {
			var lookupDict = dict ?? { this.lookupClass.all };
			^(PnsymRest.prVarLookup(lookupDict, key) ?? {
				("PnsymRest: '" ++ key ++ "' not found, using Rest").warn;
				this.rest
			})
		}
	}

	// Unlike the library versions Psym, Pnsym, etc.,
	// this version doesn't parallelise, but instead
	// returns arrays of values/Events when it gets
	// a collection for lookup.
	getPattern { arg key;
		^if(key.isSequenceableCollection and: { key.isString.not }) {
			key.collect {|each|
				this.lookUp(each)
			}
		} {
			this.lookUp(key)
		}
	}

	rest {
		^Rest()
	}
}

// Version of PnsymRest that returns an Event with Rest() in one of its
// keys by default (rather than Rest()).
PnsymRestEvent : PnsymRest {

	// This is for Event streams, so it's like Psym (vs Pnsym), so uses Pdef as base lookup
	lookupClass { ^Pdef }

	rest {
		^(mask: Rest())
	}
}

// This and the EventPatternProxy overload (below) should be removed
// (replaced by regular Psync) once the changes from
// https://github.com/supercollider/supercollider/pull/4792 are in our
// supported SuperCollider version.
Psync2 : FilterPattern {
	var <>quant, <>maxdur, <>tolerance, <>mindur;
	*new { arg pattern, quant, maxdur, tolerance = 0.001, mindur = 0;
		^super.new(pattern).quant_(quant).maxdur_(maxdur).tolerance_(tolerance).mindur_(mindur)
	}
	storeArgs { ^[pattern,quant,maxdur,tolerance,mindur] }

	embedInStream { arg event;
		var item, stream, delta, elapsed = 0.0, nextElapsed, clock, inevent;
		var	localquant = quant.value(event), localmaxdur = maxdur.value(event);
		var cleanup = EventStreamCleanup.new;

		stream = pattern.asStream;

		loop {
			inevent = stream.next(event).asEvent;
			if(inevent.isNil) {
				if(localquant.notNil) {
					delta = elapsed.max(mindur.value(event)).roundUp(localquant) - elapsed;
					if(delta > 0) { Event.silent(delta, event).yield };
					^cleanup.exit(event);
				};
			};
			cleanup.update(inevent);

			delta = inevent.delta;
			nextElapsed = elapsed + delta;

			if (localmaxdur.notNil and: { nextElapsed.roundUp(tolerance) >= localmaxdur }) {
				inevent = inevent.copy;
				inevent.put(\delta, localmaxdur - elapsed);
				event = inevent.yield;
				^cleanup.exit(event);
			} {
				elapsed = nextElapsed;
				event = inevent.yield;
			};
		};
	}
}

PmulOrSet : Pset {
	filterEvent { arg event, val;
		var existingVal = event[name];
		^event[name] = if (existingVal.isNil) {
			val
		} {
			existingVal * val
		};
	}
}

Emphasis {
	var <>value;
	var <>emphasis;
	const baseEmphasis = 1.4142;

	*new { arg value, emphasis = baseEmphasis;
		^super.newCopyArgs(value, emphasis.value);
	}

	+ { arg aNumber, adverb;
		if (aNumber.isKindOf(Emphasis)) {
			^Emphasis.new(value + aNumber.value, max(emphasis, aNumber.emphasis));
		} {
			^aNumber.performBinaryOpOnEmphasis('+', this)
		}
	}
	- { arg aNumber, adverb;
		if (aNumber.isKindOf(Emphasis)) {
			^Emphasis.new(value - aNumber.value, max(emphasis, aNumber.emphasis));
		} {
			^aNumber.performBinaryOpOnEmphasis('-', this)
		}
	}
	* { arg aNumber, adverb;
		if (aNumber.isKindOf(Emphasis)) {
			^Emphasis.new(value * aNumber.value, max(emphasis, aNumber.emphasis));
		} {
			^aNumber.performBinaryOpOnEmphasis('*', this)
		}
	}
	/ { arg aNumber, adverb;
		if (aNumber.isKindOf(Emphasis)) {
			^Emphasis.new(value / aNumber.value, max(emphasis, aNumber.emphasis));
		} {
			^aNumber.performBinaryOpOnEmphasis('/', this)
		}
	}
	// mod
	// div
	// pow

	performBinaryOpOnSeqColl { arg aSelector, aSeqColl, adverb;
		^aSeqColl.collect({ arg item;
			this.perform(aSelector, item, adverb)
		})
	}
	performBinaryOpOnSimpleNumber { arg aSelector, aNumber, adverb;
		^Emphasis.new(aNumber, emphasis).perform(aSelector, this, adverb)
	}
	performBinaryOpOnEmphasis { arg aSelector, aNumber, adverb;
		^error("Math operation failed.\n")
	}

}

+SimpleNumber {
	performBinaryOpOnEmphasis { arg aSelector, emph, adverb;
		^emph.perform(aSelector, Emphasis.new(this, emph.emphasis), adverb)
	}
}

+SequenceableCollection {
	performBinaryOpOnEmphasis { arg aSelector, emph, adverb;
		^this.collect({ arg item;
			emph.perform(aSelector, item, adverb)
		})
	}
}

+Number {
	e { arg emphasis = 1.5;
		^Emphasis(this, emphasis)
	}
	f {
		^Emphasis(this, 2.sqrt)
	}
	ff {
		^Emphasis(this, 2.sqrt ** 2)
	}
	p {
		^Emphasis(this, 0.5.sqrt)
	}
	pp {
		^Emphasis(this, 0.5.sqrt ** 2)
	}
}

+Array {
	e { arg emphasis = 1.5;
		^this.collect(Emphasis(_, emphasis))
	}
	f {
		^this.collect(Emphasis(_, 2.sqrt))
	}
	ff {
		^this.collect(Emphasis(_, 2.sqrt ** 2))
	}
	p {
		^this.collect(Emphasis(_, 0.5.sqrt))
	}
	pp {
		^this.collect(Emphasis(_, 0.5.sqrt ** 2))
	}
}

+String {
	chars {
		^this.as(Array)
	}
}

+Array {
	pseq { arg repeats=1, offset=0;
		^Pseq(this, repeats, offset)
	}

	loop {
		^Pseq(this, inf)
	}

	anticipate { arg prob = 0.7, offset = 0.5;
		^this.collect{ |b|
			if (prob.coin) {
				b
			} {
				var o = offset.value; [-1 * o, 0] + b
			}
		}.flatten
	}

	lag { arg prob = 0.7, offset = 0.125;
		^this.collect{ |b|
			if (prob.coin) {
				b
			} {
				b + offset.value
			}
		}
	}

	pb {
		var expandedKeys = this.clump(2).collect{ arg pair;
			BacalaoParser.prResolvePatternSyntax(pair);
		}.flatten;
		^Pbind(*expandedKeys)
	}

	tc {
		var timeChainArgs = this.separate{ arg a, b;
			PtimeChain.isValidArg(a)
		}.collect(_.clump(2)).flatten.collect{ arg x;
			if (x.size == 2) {
				Pbind(*BacalaoParser.prResolvePatternSyntax(x));
			} {
				if (x.size != 1) { Error("Expected single element in tc").throw };
				x[0]
			}
		};
		^PtimeChain(*timeChainArgs)
	}
}

+Pattern {

	scramble { arg randSeed, dur;
		var p = Parrop(this, _.scramble, dur);
		^if (randSeed.notNil) {
			if (randSeed.isNumber) { Pseed(Pn(randSeed, 1), p) } { Pseed(randSeed, p) }
		} {
			p
		}
	}

	rand { arg randSeed, dur;
		var arrayRandFunc = { arg x; Array.fill(x.size, { x.choose} ) };
		var p = Parrop(this, arrayRandFunc, dur);
		^if (randSeed.notNil) {
			if (randSeed.isNumber) { Pseed(Pn(randSeed, 1), p) } { Pseed(randSeed, p) }
		} {
			p
		}
	}

	randSwap { arg numSwaps, adjacent = true, randSeed, dur;
		var arrayRandSwapFunc = { arg arr;
			if (arr.size > 1) {
				numSwaps.do {
					var i = arr.size.rand;
					var j = if (adjacent) { i + 1 } { i };
					if (j == arr.size) { j = arr.size - 2 };
					while { i == j } {
						j = arr.size.rand;
					};
					arr.swap(i, j)
				};
			};
			arr
		};
		var p = Parrop(this, arrayRandSwapFunc, dur);
		^if (randSeed.notNil) {
			if (randSeed.isNumber) { Pseed(Pn(randSeed, 1), p) } { Pseed(randSeed, p) }
		} {
			p
		}
	}

	perfectShuffle { arg dur;
		^Parrop(this, _.perfectShuffle, dur);
	}

	reverse { arg dur;
		^Parrop(this, _.reverse, dur);
	}

	mirror { arg dur;
		^Parrop(this, _.mirror, dur);
	}

	mirror1 { arg dur;
		^Parrop(this, _.mirror1, dur);
	}

	mirror2 { arg dur;
		^Parrop(this, _.mirror2, dur);
	}

	rotate { arg n = 1, dur;
		^Parrop(this, _.rotate(n), dur);
	}

	pyramid { arg patternType = 1, dur;
		if (patternType.isNumber.not) {
			Error("Pattern pyramid only accepts Number arguments for patternType").throw
		};
		^Parrop(this, _.pyramid(patternType), dur);
	}

	// Note: we currently don't support Pattern arguments to permute
	permute { arg nthPermutation = 1, dur;
		if (nthPermutation.isNumber.not) {
			Error("Pattern permute only accepts Number arguments for nthPermutation").throw
		};
		^Parrop(this, _.permute(nthPermutation), dur);
	}

	faststutter { arg rate = 2;
		^this.fast(rate).stutter(rate);
	}

	fast { arg rate = 2;
		// Pstretch might be used; there are pros and cons,
		// but in general this "plays nicer" with PtimeChain.
		^PmulOrSet(\stretch, rate.reciprocal, this);
	}

	slow { arg rate = 2;
		// Pstretch might be used; there are pros and cons,
		// but in general this "plays nicer" with PtimeChain.
		^PmulOrSet(\stretch, rate, this);
	}

	swing { arg stepsInBar = 8, amt = 0.25, reverse = false;
		var func = Pfunc{
			var clk = thisThread.clock;
			var fractInBar = clk.beatInBar / clk.beatsPerBar;
			var offset = (fractInBar * stepsInBar).floor + reverse.if(1,0) % 2;
			var toff = [0, stepsInBar.reciprocal * clk.beatsPerBar * amt][offset];
			toff
		};
		^Padd(\timingOffset, func, this)
	}

	// Modify an Event Pattern to return Rests based on a weighted coin toss
	degrade { arg prob = 0.5, randSeed;
		var p = this.collect{ |ev| prob.coin.if{ ev } { ev.copy.put(\mask, Rest(0)) }};
		^if (randSeed.notNil) { Pseed(Pn(randSeed, 1), p) } { p }
	}

	// Apply a function (or a replacement pattern) once every N bars
	// You can use an Association "every(4->1, ...)" to offset the bar from 0.
	every { arg nBars, funcOrAlternatePattern;
		var alternate = if (funcOrAlternatePattern.isKindOf(Function)) {
			funcOrAlternatePattern.(this)
		} {
			funcOrAlternatePattern
		};
		var offset = 0;
		if (nBars.isKindOf(Association)) {
			#nBars, offset = [nBars.key, nBars.value];
		};
		^Pif(Pfunc{
			thisThread.clock.bar - offset % nBars == 0
		}, alternate, this);
		// equivalent alternative
		// ^Pswitch1([this, alternate], Pfunc{ thisThread.clock.bar - offset % nBars == 0 })
	}

	// Apply a function (or a replacement pattern) every N bars when the
	// modulo is more than "startBar".
	// You can use an Association "whenmod(8->2, 6, ...)" to offset where it applies.
	whenmod { arg nBars, startBar, funcOrAlternatePattern;
		var alternate = if (funcOrAlternatePattern.isKindOf(Function)) {
			funcOrAlternatePattern.(this)
		} {
			funcOrAlternatePattern
		};
		var offset = 0;
		if (nBars.isKindOf(Association)) {
			#nBars, offset = [nBars.key, nBars.value];
		};
		^Pif(Pfunc{
			// Using the clock this way doesn't work well when you start to have scramble or other Parrop operations

			// Fractional bar version (buggy because nextBar is sometimes == this bar):
			// var clk = thisThread.clock;
			// var fractInBar = ((clk.beats.debug("beats") - clk.nextBar.debug("nextBar")) / clk.beatsPerBar) + 1;
			// var bar = clk.bar.debug("bar") + fractInBar;
			// (bar.debug("bar") - offset % nBars).debug("barModulo") >= startBar

			// Integer bar version (better):
			(thisThread.clock.bar - offset % nBars) >= startBar
		}, alternate, this);
		// equivalent alternative
		// ^Pswitch1([this, alternate], Pfunc{ thisThread.clock.bar - offset % nBars >= startBar })
	}

	// This was fixed to work more like TidalCycles' version...
	// The patterns advance together, so they don't get out of
	// sync.
	// These should sound the same:
	// b.p(1, deg"0 1 2 3".rarely(_.add(\degree, 7)))
	// b.p(1, deg"0 1 2 3".add(\degree, Pwrand([0, 7], [0.75, 0.25], inf)))
	// But it can cause weirdness if you use an alternate pattern
	// (rather than function) and the number of notes is different
	// in this vs the alternate.
	sometimesBy { arg prob, funcOrAlternatePattern;
		var alternate = if (funcOrAlternatePattern.isKindOf(Function)) {
			funcOrAlternatePattern.(this)
		} {
			funcOrAlternatePattern
		};
		^Preduce({ |x,y| if (prob.coin, x, y) }, alternate, this);
		// Old version, that allowed patterns to get out of phase
		// ^Pif( Pfunc{ prob.coin }, alternate, this);
	}

	always { arg funcOrAlternatePattern;
		^this.sometimesBy(1, funcOrAlternatePattern);
	}
	almostAlways { arg funcOrAlternatePattern;
		^this.sometimesBy(0.9, funcOrAlternatePattern);
	}
	often { arg funcOrAlternatePattern;
		^this.sometimesBy(0.75, funcOrAlternatePattern);
	}
	sometimes { arg funcOrAlternatePattern;
		^this.sometimesBy(0.5, funcOrAlternatePattern);
	}
	rarely { arg funcOrAlternatePattern;
		^this.sometimesBy(0.25, funcOrAlternatePattern);
	}
	almostNever { arg funcOrAlternatePattern;
		^this.sometimesBy(0.1, funcOrAlternatePattern);
	}
	never { arg funcOrAlternatePattern;
		^this.sometimesBy(0, funcOrAlternatePattern);
	}

	set { arg keyName, value;
		^Pset(keyName, value, this)
	}

	add { arg keyName, value;
		^Padd(keyName, value, this);
	}

	mul { arg keyName, value;
		^Pmul(keyName, value, this);
	}

	// Get (or try to estimate) a pattern's duration
	getDur {
		var dur;
		try {
			// Try to find a Pbind (or Pmono) somewhere up the chain,
			// so we can get its duration key. We can't "solve" for all kinds
			// of patterns -- in which case we just use the default dur of 0,
			// which is fine (will use the "natural" duration, and quantize to
			// the next bar instead of the full pattern duration).
			var eventPat = this;
			var durSeq;
			while { eventPat.respondsTo(\patternpairs).not } {
				case
				{ eventPat.respondsTo(\pattern) } {
					// e.g. FilterPattern
					eventPat = eventPat.pattern
				}
				{ eventPat.respondsTo(\patterns) } {
					// e.g. PtimeChain
					eventPat = eventPat.patterns.first
				}
				{ Error("Don't know how to find a Pbind/Pmono from here").throw }
			};
			durSeq = eventPat.patternpairs.clump(2).detect{|x| x.first == \dur}[1];
			dur = durSeq.list.sum * durSeq.repeats;
			//dur.debug("computed bars");
		} { arg error;
			error.errorString.warn;
			dur = 0; // use "natural" duration
		};
		^dur
	}

}

+Pbind {
	find { arg key;
		patternpairs.pairsDo { |u,x,i|
			if(u == key) { ^i }
		};
		^nil
	}

	set { arg ...args;
		args.pairsDo { |key, val|
			var i = this.find(key);
			if (i.notNil) {
				if (val.isNil) {
					patternpairs.removeAt(i);
					patternpairs.removeAt(i);
				} {
					patternpairs[i+1] = val
				}
			}{
				patternpairs = patternpairs ++ [key, val];
			}
		}
	}
}

// This overload should be removed (replaced by regular prNext) once the
// changes from
// https://github.com/supercollider/supercollider/pull/4801 are in our
// supported SuperCollider version (if they ever are).
//
// It "corrects" the nextTime and nextBeat so they fall precisely on an
// integer beat, if they happen to be *extremely* close to a beat
// (within 1e-12 beats).
+EventStreamPlayer {

	prNext { arg inTime;
		var nextTime;
		var outEvent = stream.next(event.copy);
		if (outEvent.isNil) {
			streamHasEnded = stream.notNil;
			cleanup.clear;
			this.removedFromScheduler;
			^nil
		}{
			var roundedBeat;
			var deltaFromRounded;
			nextTime = outEvent.playAndDelta(cleanup, muteCount > 0);
			if (nextTime.isNil) { this.removedFromScheduler; ^nil };
			nextBeat = inTime + nextTime;	// inval is current logical beat
			// [inTime.asStringPrec(17), nextBeat.asStringPrec(17)].debug("inTime, nextBeat");
			roundedBeat = nextBeat.round;
			deltaFromRounded = roundedBeat - nextBeat;
			if (deltaFromRounded.abs < 1e-12 and: { deltaFromRounded != 0 }) {
				nextBeat = roundedBeat;
				nextTime = nextTime + deltaFromRounded;
				// nextTime.asStringPrec(17).debug("corrected time");
			};
			^nextTime
		};
	}
}

// Fix from SC Pull Request: https://github.com/supercollider/supercollider/pull/4792
// Keep here until your SC version includes these fixes.
// From Pdef.sc, line 176:
+PatternProxy {
	*clear {
		this.all.do { arg pat; pat.clear };
		this.all.clear;
	}
}

// This and Psync2 (above) should be removed once the changes from
// https://github.com/supercollider/supercollider/pull/4792 are in our
// supported SuperCollider version.
+EventPatternProxy {

	// From Pdef.sc, line ~495
	constrainStream { arg stream, newStream, inval, cleanup;
		var delta, tolerance;
		var quantBeat, catchUp, deltaTillCatchUp, forwardTime, quant = this.quant;

		^if(this.quant.isNil) {
			cleanup !? { cleanup.exit(inval) };
			newStream
		} {
			quantBeat = this.quantBeat ? 0;
			catchUp = this.outset;

			delta = thisThread.clock.timeToNextBeat(quant);
			tolerance = quantBeat % delta % 0.125;

			if(catchUp.notNil) {
				deltaTillCatchUp = thisThread.clock.timeToNextBeat(catchUp);
				forwardTime = quantBeat - delta + deltaTillCatchUp;
				delta = newStream.fastForward(forwardTime, tolerance) + deltaTillCatchUp;
			};

			if(fadeTime.isNil) {
				if(delta == 0) {
					cleanup !? { cleanup.exit(inval) };
					newStream
				} {
					Pseq([
						EmbedOnce(
							Psync2(stream, delta, delta, tolerance, delta).asStream,
							cleanup
						),
						newStream
					]).asStream
				}
			}{
				Ppar([
					EmbedOnce(
						PfadeOut(stream, fadeTime, delta, tolerance),
						cleanup
					),
					PfadeIn(newStream, fadeTime, delta, tolerance)
				]).asStream
			}
		}
	}
}
