PtimeChain : Pattern {
	var <>patterns;
	const timeEpsilon = 1e-6;

	*new { arg ... patterns;
		patterns.do{ arg p;
			if (p.isKindOf(Pattern).not and: { p.isKindOf(Event).not }) {
				Error("PtimeChain requires Event Patterns or Events").throw
			}
		};
		^super.newCopyArgs(patterns);
	}

	<< { arg aPattern;
		var list;
		if (aPattern.isKindOf(Pattern).not and: { aPattern.isKindOf(Event).not }) {
			Error("PtimeChain requires Event Patterns or Events").throw
		};
		list = patterns.copy.add(aPattern);
		^this.class.new(*list)
	}

	embedInStream { arg inval;
		var structureStream = patterns[0].asStream;
		// Store the value streams, their current time and latest Events
		var valueStreams = patterns[1..].collect{ |p|
			// Extend "right-hand side" patterns to loop forever, so they can
			// repeat as necessary to match up with the left-hand "structure" pattern.
			if (p.class.findRespondingMethodFor(\repeats).notNil) {
				// "Extending % repeats to infinity".format(p).postln;
				p.repeats = inf;
			} {
				// "Wrapping % in infinite Pn".format(p).postln;
				p = Pn(p);
			};
			[p.asStream, 0, ()]
		};
		var inevent, cleanup = EventStreamCleanup.new;
		var structureTime = 0;
		loop {
			var structureEvent;
			var cumulativeEvent = inevent = inval.copy;
			// inevent.debug("inevent at start of loop");
			valueStreams.reverseDo { |strData, i|
				var valueStream, nextValueTime, nextValueEvent;
				#valueStream, nextValueTime, nextValueEvent = strData;
				// [i, nextValueTime, nextValueEvent].debug("next time/Event");
				while {
					nextValueTime <= (structureTime + timeEpsilon);
				} {
					var delta;
					// Suggested "parent" fix from Scott Carver, to handle case of
					// "downstream" events reading from "upstream" events.
					// See https://scsynth.org/t/time-aware-merging-of-two-event-pattern-streams/1482/15
					if (inevent !== cumulativeEvent) {
						inevent.parent_(cumulativeEvent);
					};
					nextValueEvent = valueStream.next(inevent);
					// nextValueEvent.debug("nextValueEvent");
					// Q: Should we exit for value streams that end, or just the structure stream?
					// A: Will have to look at concrete examples, for now: yes, we exit when
					//    any of the streams ends...
					if (nextValueEvent.isNil) { ^cleanup.exit(inval) };
					delta = nextValueEvent.delta.value;
					// Note we only want to move time ahead for Events that *explicitly*
					// have a 'dur' key. (N.B. Events derived from Event.default would have
					// a 'dur' if we wrote ev.includesKey('dur') instead of ev.keys.includes('dur')).
					if (delta.notNil and: { nextValueEvent.keys.includes('dur') }) {
						// Note: we deliberately use 'dur' vs 'delta' here, because otherwise
						// "upstream" value Events won't update properly when we are called
						// from a "downstream" Ppar (when some deltas may be 0).
						nextValueTime = nextValueTime + nextValueEvent.dur.value;
					} {
						// There is no time information, just use our next value
						// for the next structure Event (as regular Pchain would do)
						nextValueTime = structureTime + (timeEpsilon * 2);
					};
					// nextValueTime.debug("nextValueTime updated");
					// Store the values for our next iteration
					strData[1] = nextValueTime;
					// inevent feeds from one into the next, gathering/replacing values
					strData[2] = nextValueEvent;
				};

				// Combine the contributions of all the "current" value events
				// that came before the main structure event.
				cumulativeEvent = cumulativeEvent.composeEvents(nextValueEvent);
				// cumulativeEvent.debug("updated cumulativeEvent");
			};

			structureEvent = structureStream.next(cumulativeEvent);
			if (structureEvent.isNil) { ^cleanup.exit(inval) };
			cleanup.update(structureEvent);
			// structureEvent.debug("yielding structureEvent");
			inval = yield(structureEvent);
			// inval.debug("yield returned inval");
			// Note: we deliberately use 'dur' vs 'delta' here, because otherwise
			// "upstream" value Events won't update properly when we are called
			// from a "downstream" Ppar (when some deltas may be 0).
			// e.g. this example would produce 'db' of 0 for each event for
			//      the first of the two parallel chains:
			//   a = PtimeChain(Pbind(\degree, Pseq((0..3)), \dur, 0.25), Pbind(\db, Pseq((0,3..9))));
			//   Ppar([a, a]).trace.play;
			structureTime = structureTime + structureEvent.dur.value;
			// structureTime.debug("structureTime");
		};
	}

	storeOn { arg stream;
		stream << "(";
		patterns.do { |item,i|  if(i != 0) { stream << " <> " }; stream <<< item; };
		stream << ")"
	}
}

+Pattern {

	<< { arg aPattern;
		// time-based pattern key merging
		^PtimeChain(this, aPattern)
	}

}

+Event {

	<< { arg aPattern;
		// time-based pattern key merging
		^PtimeChain(this, aPattern)
	}

}
