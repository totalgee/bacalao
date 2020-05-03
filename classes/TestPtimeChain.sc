TestPtimeChain : UnitTest {

	setup {
		// called before each test
	}

	tearDown {
		// called after each test
	}

	compareEvents { arg resultPattern, expectedPattern, numEvents, message;
		var resultEvents = resultPattern.asStream.nextN(numEvents, Event.default);
		var expectedEvents = expectedPattern.asStream.nextN(numEvents, Event.default);
		^this.assertEquals(resultEvents, expectedEvents, message);
	}

	test_embedSimple {
		var a = Pbind(\degree, Pseq((1..4), 2), \dur, 0.5);
		var b = Pbind(\instrument, Pseq([\ia, \ib], inf), \dur, 1);
		var results = PtimeChain(a, b).asStream.nextN(9, ());
		var desired = Pbind(\degree, Pseq((1..4), 2), \dur, 0.5,
			\instrument, Pseq([\ia, \ia, \ib, \ib], inf)).asStream.nextN(9, ());
		this.assertEquals(results, [
			(degree: 1, dur: 0.5, instrument: \ia),
			(degree: 2, dur: 0.5, instrument: \ia),
			(degree: 3, dur: 0.5, instrument: \ib),
			(degree: 4, dur: 0.5, instrument: \ib),
			(degree: 1, dur: 0.5, instrument: \ia),
			(degree: 2, dur: 0.5, instrument: \ia),
			(degree: 3, dur: 0.5, instrument: \ib),
			(degree: 4, dur: 0.5, instrument: \ib),
			nil
		], "literal array of Events");
		this.assertEquals(results, desired, "equivalent Pbind");
	}

	test_embedDifferentDurs {
		var a = Pbind(\degree, Pseq((1..4), 2), \dur, 0.5);
		var b = Pbind(\instrument, Pseq([\ia,\ib,\default], inf), \dur, Pseq([2,0.5,1.5], inf));
		var results = PtimeChain(a, b).asStream.nextN(9, ());
		var expected = [
			(degree: 1, dur: 0.5, instrument: \ia),
			(degree: 2, dur: 0.5, instrument: \ia),
			(degree: 3, dur: 0.5, instrument: \ia),
			(degree: 4, dur: 0.5, instrument: \ia),
			(degree: 1, dur: 0.5, instrument: \ib),
			(degree: 2, dur: 0.5, instrument: \default),
			(degree: 3, dur: 0.5, instrument: \default),
			(degree: 4, dur: 0.5, instrument: \default),
			nil
		];
		this.assertEquals(results, expected, "literal array of Events");
		results = (a << b).asStream.nextN(9, ());
		this.assertEquals(results, expected, "using << operator");
	}

	test_embedThreeWay {
		var degs = [0, 2, 4, 6, 8, 10];
		var noteDur = degs.size.reciprocal;
		// degree:     | 0  2  4  6  8  10 |
		// instrument: | a     b     c     |
		// pan:        |-1        1        |
		var a = Pbind(\degree, Pseq(degs, 1), \dur, noteDur);
		var b = Pbind(\instrument, Pseq([\ia,\ib,\ic], 1), \dur, 3.reciprocal);
		var c = Pbind(\pan, Pseq([-1, 1], 1), \dur, 2.reciprocal);
		var results = PtimeChain(a, b, c).asStream.nextN(degs.size+1, ());
		this.assertEquals(results, [
			(degree: 0, dur: noteDur, instrument: \ia, pan: -1),
			(degree: 2, dur: noteDur, instrument: \ia, pan: -1),
			(degree: 4, dur: noteDur, instrument: \ib, pan: -1),
			(degree: 6, dur: noteDur, instrument: \ib, pan: 1),
			(degree: 8, dur: noteDur, instrument: \ic, pan: 1),
			(degree: 10, dur: noteDur, instrument: \ic, pan: 1),
			nil
		], "a << b << c");

		results = (b << a << c).asStream.all(());
		noteDur = 3.reciprocal;
		this.assertEquals(results, [
			(degree: 0, dur: noteDur, instrument: \ia, pan: -1),
			(degree: 4, dur: noteDur, instrument: \ib, pan: -1),
			(degree: 8, dur: noteDur, instrument: \ic, pan: 1)
		], "b << a << c");
		this.assertEquals(results, (b << c << a).asStream.all(()), "...same as b << c << a");

		results = (c << a << b).asStream.all(());
		noteDur = 2.reciprocal;
		this.assertEquals(results, [
			(degree: 0, dur: noteDur, instrument: \ia, pan: -1),
			(degree: 6, dur: noteDur, instrument: \ib, pan: 1)
		], "c << a << b");
		this.assertEquals(results, (c << b << a).asStream.all(()), "...same as c << b << a");
	}

	test_loopRightHandStreams {
		var degs = (0..5);
		var noteDurs = [1, 0.5, 0.5, 0.5, 0.5, 1];
		// | divisions shown are every 1 unit on the clock (like a bar)
		// degree:     |  0     |  1  2 |  3  4 |  5     |
		// instrument: |  a (b) |  a  b |  a  b |  a (b) |
		// pan:        | -1 (0) |  1 -1 |  0  1 | -1 (0) |
		var a = Pbind(\degree, Pseq(degs, 1), \dur, Pseq(noteDurs, 1));
		var b = Pbind(\instrument, Pseq([\ia,\ib], 1), \dur, 0.5);
		var c = Pbind(\pan, Pseq([-1, 0, 1], 1), \dur, 0.5);
		var results = PtimeChain(a, b, c).asStream.nextN(degs.size+1, ());
		this.assertEquals(results, [
			(degree: 0, instrument: \ia, pan: -1, dur: 1),
			(degree: 1, instrument: \ia, pan:  1, dur: 0.5),
			(degree: 2, instrument: \ib, pan: -1, dur: 0.5),
			(degree: 3, instrument: \ia, pan:  0, dur: 0.5),
			(degree: 4, instrument: \ib, pan:  1, dur: 0.5),
			(degree: 5, instrument: \ia, pan: -1, dur: 1),
			nil
		], "a << b << c");
	}

	test_timelessStream {
		// Event streams without duration/delta
		var degs = [0, 2, 4, 6, 8, 10];
		var noteDur = degs.size.reciprocal;
		var a = Pbind(\degree, Pseq(degs, 1), \dur, noteDur);
		var b = Pbind(\pan, Pseq([-1, 0, 1], inf)); // no \dur keys
		var results = PtimeChain(a, b).asStream.nextN(degs.size+1, ());
		this.assertEquals(results, [
			(degree: 0, dur: noteDur, pan: -1),
			(degree: 2, dur: noteDur, pan: 0),
			(degree: 4, dur: noteDur, pan: 1),
			(degree: 6, dur: noteDur, pan: -1),
			(degree: 8, dur: noteDur, pan: 0),
			(degree: 10, dur: noteDur, pan: 1),
			nil
		], "Pseq");

		b = Pbind(\db, Pseed(5, Pwhite(-24, 0)));
		results = PtimeChain(a, b).asStream.nextN(degs.size+1, ());
		this.assertEquals(results, [
			(degree: 0, dur: noteDur, db: -21),
			(degree: 2, dur: noteDur, db: -20),
			(degree: 4, dur: noteDur, db: -3),
			(degree: 6, dur: noteDur, db: -20),
			(degree: 8, dur: noteDur, db: -10),
			(degree: 10, dur: noteDur, db: -23),
			nil
		], "Pwhite");
	}

	test_Ppar {
		// Testing/fixing an issue -- when using 'delta' vs 'dur' for time updates,
		// "upstream" value Events wouldn't update properly when we were called
		// from a "downstream" Ppar (where some deltas may be 0).
		// e.g. this example would produce 'db' of 0 for each event for
		//      the first of the two parallel chains:
		//   a = PtimeChain(Pbind(\degree, Pseq((0..3)), \dur, 0.25), Pbind(\db, Pseq((0,3..9))));
		//   Ppar([a, a]).trace.play;

		var a = Pbind(\degree, Pseq((0..3)), \dur, 0.5);
		var b = Pbind(\db, Pseq((0,3..9), inf));
		var c = Pbind(\legato, 1);
		var chain = a << b;
		var results = Ppar([chain, Padd(\degree, -7, chain) << c]);
		var desired = Ppar([a <> b, Padd(\degree, -7, a <> b) <> c]);
		var expected = [
			Event.default.putAll( ('degree':  0, 'dur': 0.5, 'delta': 0.0, 'db': 0) ),
			Event.default.putAll( ('degree': -7, 'dur': 0.5, 'delta': 0.5, 'db': 0, 'legato': 1) ),
			Event.default.putAll( ('degree':  1, 'dur': 0.5, 'delta': 0.0, 'db': 3) ),
			Event.default.putAll( ('degree': -6, 'dur': 0.5, 'delta': 0.5, 'db': 3, 'legato': 1) ),
			Event.default.putAll( ('degree':  2, 'dur': 0.5, 'delta': 0.0, 'db': 6) ),
			Event.default.putAll( ('degree': -5, 'dur': 0.5, 'delta': 0.5, 'db': 6, 'legato': 1) ),
			Event.default.putAll( ('degree':  3, 'dur': 0.5, 'delta': 0.0, 'db': 9) ),
			Event.default.putAll( ('degree': -4, 'dur': 0.5, 'delta': 0.5, 'db': 9, 'legato': 1) ),
			Event.default.putAll( ('delta': 0.0, 'dur': Rest(0.0)) ),
			nil
		];

		this.assertEquals(results.asStream.nextN(10, Event.default), expected, "literal array of Events");
		this.compareEvents(results, desired, 10, "equivalent Pbind");
	}

	test_eventChain {
		var a = Pbind(\degree, Pseq((0..3)), \dur, Pseq([0.5, 0.25], inf));
		var b = Pbind(\db, Pseq((0,3..9), inf));
		var e = (pan: -0.5);
		var results = a << b << e;
		var expected = [
			(degree: 0, dur: 0.5, db: 0, pan: -0.5),
			(degree: 1, dur: 0.25, db: 3, pan: -0.5),
			(degree: 2, dur: 0.5, db: 6, pan: -0.5),
			(degree: 3, dur: 0.25, db: 9, pan: -0.5),
			nil
		];
		this.assertEquals(results.asStream.nextN(expected.size, ()), expected, "Event at end");

		e = (pan: 0.25, dur: 0.25);
		~results = e << a << b;
		~expected = [
			(degree: 0, dur: 0.25, db: 0, pan: 0.25),
			(degree: 0, dur: 0.25, db: 3, pan: 0.25),
			(degree: 1, dur: 0.25, db: 6, pan: 0.25),
			(degree: 2, dur: 0.25, db: 9, pan: 0.25),
			(degree: 2, dur: 0.25, db: 0, pan: 0.25),
			(degree: 3, dur: 0.25, db: 3, pan: 0.25),
			nil
		];
		this.assertEquals(results.asStream.nextN(expected.size, ()), expected, "Event at start");
	}

	test_badChainType {
		var a = Pbind(\degree, Pseq((0..3)), \dur, 0.5);
		var b = Pbind(\db, Pseq((0,3..9), inf));
		this.assertException({ a << nil }, Error, "no pattern in constructor");
		this.assertException({ a << "hello" }, Error, "no pattern in constructor (2)");
		this.assertException({ a << b << nil }, Error, "no pattern in << operator");
		this.assertException({ a << b << 5 }, Error, "no pattern in << operator (2)");
	}
}
