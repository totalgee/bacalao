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
// (See also Bacalao.lookupVariable)
PnsymRest : Psym {

	*prLookupVariable { arg dict, key;
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
		if (key == ' ' or: { key == '~' }) {
			// Return Rest without complaining
			^this.rest
		} {
			var lookupDict = dict ?? { this.lookupClass.all };
			^(PnsymRest.prLookupVariable(lookupDict, key) ?? {
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
		^if(key.isSequenceableCollection) {
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
