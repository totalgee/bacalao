// From Scott Carver (@scztt)
// https://gist.github.com/scztt/b4a9fc59abc06780ba7b40c4b7dc09e9

PparChain : Pchain {
	var <>patterns;
	*new { arg ... patterns;
		^super.newCopyArgs(patterns);
	}
	<> { arg aPattern;
		var list;
		list = patterns.copy.add(aPattern);
		^this.class.new(*list)
	}
	embedInStream { arg inval;
		var values, inevent, cleanup, players, streams, firstStream, protos;
		var parentThread = thisThread;
		var needsResume = true;

		cleanup = EventStreamCleanup.new;
		firstStream = patterns[0].asStream;
		streams = patterns.collect(_.asStream);
		players = Array.newClear(streams.size);
		protos = streams.size.collect {
			|i|
			Event.default.copy.putAll((
				__index: i,
				play: {
					var index = currentEnvironment['__index'];

					if (currentEnvironment.isRest.not) {
						values[index] = currentEnvironment;
						if (index < (players.size - 1)) {
							players[index + 1].event = values[index].copy.proto_(protos[index + 1]);
						};
					};
				}
			))
		};
		values = protos.collect({ |p| ().proto_(p) });

		// Pull initial values
		streams.reverseDo {
			|stream, i|
			var proto = (values[i-1] ?? ()).copy.proto_(protos[i]);
			var initialDelta, initialValue, initialDelay;

			initialValue = stream.next(proto);
			initialDelta = initialValue.delta;

			players[i] = (initialDelay ++ stream).asEventStreamPlayer(proto);
			values[i] = initialValue;
		};

		cleanup.addFunction({
			players.do({
				|player|
				player.stop.free;
			})
		});

		// Prime Start players
		streams.reverseDo {
			|stream, i|
			var delta = players[i].prNext(0);

			players[i].play(thisThread.clock);
		};

		Event.silent(0).yield;

		loop {
			inevent = values.last.copy.proto_(inval);
			// "yielding for %s".format(inevent.delta).postln;
			inevent = inevent.yield;
			// "returned from yield".postln;
			if(inevent.isNil) { ^cleanup.exit(inval) };
			cleanup.update(inevent);

			values[0] = inevent;
			players[0].event = values[0].copy.proto_(protos[0]);
		}
	}
}
