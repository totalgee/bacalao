TestBacalao : UnitTest {

	classvar parse;

	*initClass {
		parse = BacalaoParser;
	}

	// setUp and tearDown are run once for each test case
	setUp {
	}

	tearDown {
	}

	roundDurations { arg events;
		^events.collect{ arg ev;
			ev !? { ev[\dur] = ev[\dur].round(1e-9) };
		}
	}

	compareEvents { arg resultPattern, expectedPattern, numEvents, message;
		var resultEvents = resultPattern.asStream.nextN(numEvents, Event.default);
		var expectedEvents = expectedPattern.asStream.nextN(numEvents, Event.default);
		^this.assertEquals(
			this.roundDurations(resultEvents),
			this.roundDurations(expectedEvents),
			message);
	}

	approxCompareDurations { arg result, expected, message;
		this.assertArrayFloatEquals(result.flop[0], expected.flop[0], message + "(values)", 1e-6);
		this.assertArrayFloatEquals(result.flop[1], expected.flop[1], message + "(durations)", 1e-6);
	}

	test_parsing {
		var b = Bacalao();
		var str = "deg\" 1 2    3 4\"";
		var expected = Pbind(\degree, Pseq([1, 2, 3, 4]), \dur, Pseq(0.25!4));
		this.compareEvents(this.prPat(str), expected, 5, "deg");

		b.start;
		this.compareEvents(this.prPat(str), expected, 5, "deg through Interpreter");
		b.stop;

		str = "deg\"0\"";
		expected = Pbind(\degree, 0);
		this.assertEquals(parse.preProcess(str), expected.cs, "deg (single)");

		str = "deg\"[0]\"";
		expected = Pbind(\degree, 0, \dur, 1.0);
		this.assertEquals(parse.preProcess(str), expected.cs, "deg (single array entry)");

		str = "deg\"0@1\"";
		expected = Pbind(\degree, 0, \dur, 1.0);
		this.assertEquals(parse.preProcess(str), expected.cs, "deg (single with duration)");

		str = "freq\"Pgeom(100,1.5,4)\"";
		expected = Pbind(\freq, Pgeom(100, 1.5, 4));
		this.compareEvents(this.prPat(str), expected, 5, "freq Pgeom");

		str = "deg\"1 ~ 3 ~\"";
		expected = Pbind(\degree, Pseq([1, Rest(), 3, Rest()]), \dur, 1/4);
		this.compareEvents(this.prPat(str), expected, 5, "rests");

		this.assertEquals(parse.parseArray("a 1@4 default@2 2"),
			[ ("a" -> 1), ("1" -> 4.0), ("default" -> 2.0), ("2" -> 1) ], "parseArray symbols");

		str = "ins\"\\a \\b \\default \\c\"";
		expected = Pbind(\instrument, Pseq([\a, \b, \default, \c]), \dur, 1/4);
		this.compareEvents(this.prPat(str), expected, 5, "symbols");

		str = "deg\"[0 [1 2] [3 [4 5]] [[6 7 8] 7]]\"";
		expected = Pbind(\degree, Pseq([ 0, 1, 2, 3, 4, 5, 6, 7, 8, 7 ]),
			\dur, Pseq([ 1/4, 1/8, 1/8, 1/8, 1/16, 1/16, 0.041666666666667, 0.041666666666667, 0.041666666666667, 1/8 ]));
		this.compareEvents(this.prPat(str), expected, 11, "nested phrases");

		str = "deg\"0 1 <2 3> 4\"";
		expected = Pbind(\degree, Ppatlace([ 0, 1, Pseq([2, 3], inf), 4 ], 2),
			\dur, 1/4);
		this.compareEvents(this.prPat(str), expected, 9, "alternating elements");

		str = "deg\"1 2 <3 4> 5 <6 7 8>\"";
		expected = Pbind(\degree, Ppatlace([ 1, 2, Pseq([3, 4], inf), 5, Pseq([6, 7, 8], inf) ], 6),
			\dur, 1/5);
		this.compareEvents(this.prPat(str), expected, 31, "alternating elements longer");

		str = "deg\"1@3!2 [2 3]*2!2\"";
		expected = Pbind(\degree, Pseq(1!2 ++ ([2, 3]!2!2).flat),
			\dur, Pseq(0.375!2 ++ (0.03125!8)));
		this.compareEvents(this.prPat(str), expected, 31, "duplicated elements and arrays");

		str = "deg\"1*(3,8,-1) [2 3]!(3,4,1)\"";
		expected = Pbind(\degree, Pseq(1!3 ++ ([2, 3]!3).flat),
			\dur, Pseq([0.05] ++ (0.075!2) ++ (0.1!2) ++ (0.2!2) ++ (0.1!2)));
		this.compareEvents(this.prPat(str), expected, 10, "elements and arrays with Bjorklund sequences 1");

		str = "deg\"[1 [2 3]!(5,7)]\"";
		expected = Pbind(\degree, Pseq([1] ++ ([2, 3]!5).flat),
			\dur, Pseq([0.125] ++ (((0.125!2) ++ (0.0625!2))!2 ++ (0.0625!2)).flat));
		this.compareEvents(this.prPat(str), expected, 12, "elements and arrays with Bjorklund sequences 2");

		str = "deg\"[1 [2 3]]*(2,5)@0.5!(3,4) 5\"";
		expected = Pbind(\degree, Pseq(([1,2,3]!6).flat ++ 5),
			\dur, Pseq([1, 1.5, 1, 1.5, 2, 3] *.x [2,1,1]/60  ++ (1/3)));
		this.compareEvents(this.prPat(str), expected, 20, "elements and arrays with Bjorklund sequences 3");

		str = "deg\"1 2 3 4 5 6 7 8\" << amp\"Pn(Pseries(0.1,0.1,3))\"";
		expected = Pbind(\degree, Pseq((1..8)), \amp, Pn(Pseries(0.1,0.1,3)), \dur, 0.125);
		this.compareEvents(this.prPat(str), expected, 9, "value Patterns in keys 1");

		str = "deg\"[1 2 3 4]@2\" << amp\"Pn(Pseries(0.1,0.4,3))\" << bob\"Pgeom(1,10,inf)@1\"";
		expected = Pbind(\degree, Pseq((1..4)), \amp, Pn(Pseries(0.1,0.4,3)), \bob, Pstutter(2, Pgeom(1,10,inf)), \dur, 0.5);
		this.compareEvents(this.prPat(str), expected, 5, "value Patterns in keys 2");

		str = "deg\"<0 <1,2>> <3 <4,5> 6>\"";
		expected = Pbind('degree', Ppatlace([ Pseq([ 0, [ 1, 2 ] ], inf), Pseq([ 3, [ 4, 5 ], 6 ], inf) ], 6), 'dur', 0.5);
		this.compareEvents(this.prPat(str), expected, 13, "chord elements");

		str = "deg\"<0 1!2> <3!3 4 5>\"";
		expected = Pbind('degree', Ppatlace([ Pseq([ 0, 1, 1 ], inf), Pseq([ 3, 3, 3, 4, 5 ], inf) ], 15), 'dur', 0.5);
		this.compareEvents(this.prPat(str), expected, 31, "alternate values with duplicates");

		str = "deg\"<0 1*2> 2\"";
		this.assertException({ parse.preProcess(str) }, Error, "repeat not supported in alternate values");

		str = "deg\"<0 1@2> 2\"";
		this.assertException({ parse.preProcess(str) }, Error, "hold not supported in alternate values");

		str = "deg'bacalao ' << amp'94041377'";
		expected = Pbind('degree', Pseq([ 1, 0, 2, 0, 11, 0, 14, Rest(1) ], 1.0),
			'dur', 0.125,
			'amp', Pseq([0.9, 0.4, 0, 0.4, 0.1, 0.3, 0.7, 0.7], 1));
		this.compareEvents(this.prPat(str), expected, 9, "char patterns");

		str = "deg\"[1 2 3]@2 |4 5| 6*3\"";
		expected = Pbind('degree', Pseq([ 1, 2, 3, 4, 5, 6, 6, 6 ], 1),
			'dur', Pseq([ 2/3, 2/3, 2/3, 1/2, 1/2, 1/3, 1/3, 1/3 ], 1));
		this.compareEvents(this.prPat(str), expected, 9, "bar dividers");
		str = "deg\"[[1 2 3]@2 [4 5] 6*3]@4\"";
		this.compareEvents(this.prPat(str), expected, 9, "equivalent without bar dividers");
	}

	test_parserVariables {
		var str, expected;
		var b = Bacalao();
		b.varSet("bd", [36, 37, 48, 49]);
		Bacalao.varSet("sd", -38);
		b.varSet("sd", 38);
		b.vars[\bd] = -1 * [36, 37, 48, 49];
		this.assertEquals(b.vars['bd'][1], -37, "Get/set using b.vars");
		~bd = -1 * ~bd;
		this.assertEquals(b.vars['bd'][2], 48, "Get/set using current Environment");

		str = "note\"sd\"";
		expected = Pbind(\note, ~sd);
		this.assertEquals(parse.preProcess(str), expected.cs, "scalar variable");
		this.compareEvents(this.prPat(str), expected, 5, "scalar variable");

		str = "note\"sd:2\"";
		expected = Pbind(\note, ~sd);
		this.compareEvents(this.prPat(str), expected, 5, "scalar variable with index");

		str = "note\"sd:r\"";
		expected = Pbind(\note, ~sd);
		this.compareEvents(this.prPat(str), expected, 5, "scalar variable with random index");

		str = "note\"bd\"";
		expected = Pbind(\note, ~bd[0]);
		this.assertEquals(parse.preProcess(str), expected.cs, "array variable");
		this.compareEvents(this.prPat(str), expected, 5, "array variable");

		str = "note\"bd:3\"";
		expected = Pbind(\note, ~bd[3]);
		this.assertEquals(parse.preProcess(str), expected.cs, "array variable with index");

		str = "note\"bd:-1\"";
		expected = Pbind(\note, ~bd.wrapAt(-1));
		this.assertEquals(parse.preProcess(str), expected.cs, "array variable with negative index");

		str = "Pseed(Pn(33,1), note\"bd:r\" )";
		expected = Pseed(Pn(33,1), Pbind(\note, Prand(~bd, inf)));
		this.compareEvents(this.prPat(str), expected, 10, "array variable with random index");

		str = "Pseed(Pn(39,1), note\"<bd:r sd>\" )";
		expected = Pseed(39, Pbind(\note, Ppatlace([Prand(~bd, inf), ~sd], inf)));
		this.compareEvents(this.prPat(str), expected, 10, "array variable with random index in alternate (no dur)");

		str = "Pseed(Pn(42,2), note\"<bd:r sd> ~\" )";
		expected = Pseed(Pn(42,2), Pbind(\note, Ppatlace([ Ppatlace([ Prand(~bd, inf), ~sd], inf), Rest() ], 2), \dur, 0.5));
		this.compareEvents(this.prPat(str), expected, 10, "array variable with random index in alternate (w/dur)");

		str = "Pseed(Pn(47,1), note\"<sd,bd:r>\" )";
		expected = Pseed(Pn(47,1), Pbind(\note, Ptuple([ ~sd, Prand(~bd, inf) ])));
		this.compareEvents(this.prPat(str), expected, 10, "array variable with random index in chord (no dur)");

		str = "Pseed(Pn(53,1), Pn(note\"bd:1 <bd:r,sd>\",4) )";
		expected = Pseed(Pn(53,1), Pn(Pbind(\note, Ppatlace([ ~bd[1], Ptuple([ Prand(~bd, inf), ~sd ]) ]), \dur, Pseq([ 0.5, 0.5 ])), 4));
		this.compareEvents(this.prPat(str), expected, 10, "array variable with random index in chord (w/dur)");

		~chord = [ [1,3,5,7], [0,1,3,5], [-2,0,1,3], [-4,-2,0,1] ];
		str = "deg\"chord:1 chord:3\"";
		expected = Pbind('degree', Ppatlace([ ~chord[1], ~chord[3] ], 1), 'dur', 0.5);
		this.compareEvents(this.prPat(str), expected, 3, "variables in current Environment");

		~custom = (
			fred: 36,
			bob: [38, 40],
			steve: [ [46, 48, 50] ]
		);
		str = "mn~custom\"fred bob:0 steve bob:1\"";
		expected = Pbind('midinote', Ppatlace([ ~custom.fred, ~custom.bob[0], ~custom.steve[0], ~custom.bob[1]]), 'dur', 0.25);
		this.compareEvents(this.prPat(str), expected, 5, "locally-specified variable");

		str = "mn~unknown\"dave ~\"";
		this.assertException({parse.preProcess(str)}, Error, "unknown event pattern variable lookup");

		str = "mn~custom\"<fred bob:0>@3 <steve:0!2 bob:1!3>\"";
		expected = Pbind('midinote', Ppatlace([ Pseq([ ~custom.fred, ~custom.bob[0] ], inf), Pseq((~custom.steve[0] ! 2) ++ (~custom.bob[1] ! 3), inf) ], 10), 'dur', Pseq([ 0.75, 0.25 ], 10));
		this.compareEvents(this.prPat(str), expected, 21, "alternate values with duplicates (explicit Env)");

		~fr = 36;
		~bo = [38, 40];
		~st = [ [46, 48, 50] ];
		str = "mn\"<fr bo:0>@3 <st:0!2 bo:1!3>\"";
		expected = Pbind('midinote', Ppatlace([ Pseq([ ~fr, ~bo[0] ], inf), Pseq([ ~st[0], ~st[0], ~bo[1], ~bo[1], ~bo[1] ], inf) ], 10), 'dur', Pseq([ 0.75, 0.25 ], 10));
		this.compareEvents(this.prPat(str), expected, 21, "alternate values with duplicates (current Env)");

		str = "mn~custom\"<fred,bob:0>*2 <bob:1,fred>\"";
		expected = Pbind('midinote', Ppatlace([ [ ~custom.fred, ~custom.bob[0] ],
			[ ~custom.fred, ~custom.bob[0] ],
			[ ~custom.bob[1], ~custom.fred ] ]), 'dur', Pseq([ 0.25, 0.25, 0.5 ]));
		this.compareEvents(this.prPat(str), expected, 21, "chords with repeats (explicit Env)");

		~session = (ohh: 50, chh: 48, o: 40, c: 38);
		this.prParseAndCompareEvents("alternate, multi-letter var (no dur)", "mn~session\"<ohh chh>\"", [
			(midinote: ~session.ohh),
			(midinote: ~session.chh),
			(midinote: ~session.ohh),
			(midinote: ~session.chh),
			(midinote: ~session.ohh),
			(midinote: ~session.chh)
		]);
		this.prParseAndCompareEvents("alternate, single-letter var (no dur)", "mn~session\"<o c>\"", [
			(midinote: ~session.o),
			(midinote: ~session.c),
			(midinote: ~session.o),
			(midinote: ~session.c),
			(midinote: ~session.o),
			(midinote: ~session.c)
		]);
		this.prParseAndCompareEvents("alternate, multi-letter var (w/dur)", "mn~session\"<ohh chh> ~\"", [
			(midinote: ~session.ohh, dur: 0.5),
			(midinote: Rest(), dur: 0.5),
			(midinote: ~session.chh, dur: 0.5),
			(midinote: Rest(), dur: 0.5),
			nil
		]);
		this.prParseAndCompareEvents("alternate, single-letter var (w/dur)", "mn~session\"<o c> ~\"", [
			(midinote: ~session.o, dur: 0.5),
			(midinote: Rest(), dur: 0.5),
			(midinote: ~session.c, dur: 0.5),
			(midinote: Rest(), dur: 0.5),
			nil
		]);

		str = "mn\"<fr,bo:0>*2 <bo:1,fr,60>\"";
		expected = Pbind('midinote', Ppatlace([ [ ~fr, ~bo[0] ], [ ~fr, ~bo[0] ], [ ~bo[1], ~fr, 60 ] ]), 'dur', Pseq([ 0.25, 0.25, 0.5 ]));
		this.compareEvents(this.prPat(str), expected, 21, "chords with repeats (current Env)");
	}

	prParseAndCompareEvents { arg testDescription, str, expected;
		var results = this.prPat(str).asStream.nextN(expected.size, ());
		this.assertEquals(results, expected, testDescription);
	}

	test_variablesWithEvents {
		var str;
		var b = Bacalao();
		~varied = (
			a: (note: 2, amp: 0.5),
			b: (midinote: 67, amp: 0.3, detune: 0.1, legato: 2),
			bloud: (midinote: 67, amp: 0.8, detune: 0.1, legato: 2),
			c: (freq: 300, lag: 0.1, dur: 0.75),
			d: (degree: [2,4,6], scale: Scale.minor.degrees, strum: 0.0625)
		);
		this.prParseAndCompareEvents("string pattern", "@~varied\"a bloud [~ c] d\"", [
			~varied.a.copy.dur = 0.25,
			~varied.bloud.copy.dur = 0.25,
			(mask: Rest()).copy.dur = 0.125,
			~varied.c.copy.dur = 0.125,
			~varied.d.copy.dur = 0.25,
			nil
		]);

		this.prParseAndCompareEvents("char pattern", "@~varied'8@ab_cd|a_ b_|d'", [
			~varied.a.copy.dur = 0.125,
			~varied.b.copy.dur = 0.25,
			~varied.c.copy.dur = 0.125,
			~varied.d.copy.dur = 0.125,
			(mask: Rest(), dur: 0.375),
			~varied.a.copy.dur = 0.25,
			(mask: Rest(), dur: 0.125),
			~varied.b.copy.dur = 0.25,
			(mask: Rest(), dur: 0.375),
			~varied.d.copy.dur = 0.125,
			(mask: Rest(), dur: 0.875),
			nil
		]);

		this.prParseAndCompareEvents("string pattern (single event)", "@~varied\"b\"", [
			~varied.b,
			~varied.b,
			~varied.b,
			// forever
		]);

		this.prParseAndCompareEvents("string pattern (single event w/dur)", "@~varied\"b@1\"", [
			~varied.b.copy.dur = 1.0,
			~varied.b.copy.dur = 1.0,
			~varied.b.copy.dur = 1.0,
			// forever
		]);

		this.prParseAndCompareEvents("char pattern (single event)", "@~varied'd'", [
			~varied.d.copy.dur = 1.0,
			~varied.d.copy.dur = 1.0,
			~varied.d.copy.dur = 1.0,
			// forever
		]);

		this.prParseAndCompareEvents("string pattern (single Rest)", "@~varied\"~\"", [
			(mask: Rest()),
			(mask: Rest()),
			(mask: Rest()),
			// forever
		]);

		this.prParseAndCompareEvents("char pattern (single Rest)", "@~varied' '", [
			(mask: Rest()).copy.dur = 1.0,
			(mask: Rest()).copy.dur = 1.0,
			(mask: Rest()).copy.dur = 1.0,
			// forever
		]);

		this.prParseAndCompareEvents("string pattern (one note and Rest)", "@~varied\"a ~\"", [
			~varied.a.copy.dur = 0.5,
			(mask: Rest(), dur: 0.5),
			nil
		]);

		this.prParseAndCompareEvents("char pattern (one note and Rest)", "@~varied'a '", [
			~varied.a.copy.dur = 0.5,
			(mask: Rest(), dur: 0.5),
			nil
		]);

		this.prParseAndCompareEvents("string pattern with alternation",
			"@~varied\"<a bloud> <c ~ d>\"", [
				~varied.a.copy.dur = 0.5,
				~varied.c.copy.dur = 0.5,
				~varied.bloud.copy.dur = 0.5,
				(mask: Rest(), dur: 0.5),
				~varied.a.copy.dur = 0.5,
				~varied.d.copy.dur = 0.5,
				~varied.bloud.copy.dur = 0.5,
				~varied.c.copy.dur = 0.5,
				~varied.a.copy.dur = 0.5,
				(mask: Rest(), dur: 0.5),
				~varied.bloud.copy.dur = 0.5,
				~varied.d.copy.dur = 0.5,
				nil
		]);

		{
			// Event patterns with parallelism/chords: currently not supported
			var testFunc = {
				this.prParseAndCompareEvents("string pattern with chords",
					"@~varied\"<a,bloud> <c,d,b>\"", [
						[ ~varied.a.copy.dur = 0.5, ~varied.bloud.copy.dur = 0.5 ],
						[ ~varied.c.copy.dur = 0.5, ~varied.d.copy.dur = 0.5, ~varied.b.copy.dur = 0.5 ],
						nil
				]);
			};
			this.assertException(testFunc, Error, "string pattern with chords");
		}.value;

		str = "@~unknown\"a b c\"";
		this.assertException({parse.preProcess(str)}, Error, "unknown string pattern variable lookup");

		str = "@~unknown'8@aab_cc__/cd/dad_b_'";
		this.assertException({parse.preProcess(str)}, Error, "unknown char pattern variable lookup");

		str = "@\"a c b d\"";
		this.assertException({parse.preProcess(str)}, Error, "string pattern without variable");

		str = "@'abe'";
		this.assertException({parse.preProcess(str)}, Error, "char pattern without variable");
	}

	test_library {
		// var defaultLibraryPath = nil;
		// var altLibraryPath = "test";
		// var a = Bacalao(altLibraryPath, verbose: false);
		// var d = Bacalao(verbose: false); // use default library path (none)
		// this.assertEquals(a.libraryPath, altLibraryPath, "alternate set correctly");
		// this.assertEquals(d.libraryPath, defaultLibraryPath, "default set correctly");
		//
		// d.initLibrary(Platform.resourceDir);
		// this.bootServer(d.server);
		// this.assertEquals(d.sampleGroups, [ \sounds ], "sampleGroups");
		//
		// d.initLibrary("invalidPath");
		// this.bootServer(d.server);
		// this.assertEquals(d.sampleGroups, [], "sampleGroups");
	}

	test_clock {
		var defaultInitialTempo = TempoClock.default.tempo;
		var a = Bacalao(TempoClock(defaultInitialTempo * 4));
		var d = Bacalao(); // use default clock
		var share;
		this.assertEquals(a.clock.tempo, defaultInitialTempo * 4, "initially set correctly");
		this.assert(TempoClock.default.tempo == defaultInitialTempo, "default didn't change");
		a.tempo = 99;
		this.assertEquals(a.clock.tempo, 99, "set tempo");
		this.assert(TempoClock.default.tempo == defaultInitialTempo, "default didn't change");
		this.assert(d.clock == TempoClock.default, "second instance uses default clock");
		d.tempo = defaultInitialTempo * 2;
		this.assertEquals(TempoClock.default.tempo, defaultInitialTempo * 2, "default set by sharing instance");
		// Set default back to what it was
		TempoClock.default.tempo = defaultInitialTempo;

		share = Bacalao(a); // share clock of alternate instance
		this.assertEquals(share.clock, a.clock, "share with another instance");
	}

	test_server {
		var altServer = Server(\alternate, NetAddr("127.0.0.1", 57123));
		var a = Bacalao(server: altServer);
		var defaultServer = Server.default;
		var d = Bacalao(); // use default Server
		this.assertEquals(a.server, altServer, "alternate set correctly");
		this.assertEquals(d.server, defaultServer, "default set correctly");
	}

	test_stringHandling {
		var result;
		this.assertEquals(parse.splitSimplify("   stuff    like this   ", $ ),
			["stuff", "like", "this"], "splitSimplify");

		this.assertFloatEquals(parse.getFloatArg("-12.75"), -12.75, "getFloatArg");
		this.assertFloatEquals(parse.getFloatArg("13/7"), 13/7, "getFloatArg");
		this.assertFloatEquals(parse.getFloatArg("13/((2*3)+1)"), 13/((2*3)+1), "getFloatArg");
		this.assertEquals(parse.getFloatArg(nil), nil, "getFloatArg");
		this.assertEquals(parse.getFloatArg(""), nil, "getFloatArg");
		this.assertEquals(parse.getFloatArg("0"), 0, "getFloatArg");
		this.assertEquals(parse.getSymbolArg("stuff"), \stuff, "getSymbolArg");
		this.assertEquals(parse.getSymbolArg(42), '42', "getSymbolArg");
		this.assertEquals(parse.getSymbolArg(nil), nil, "getSymbolArg");
		this.assertEquals(parse.evaluateNumberIfPossible("  (5+2)/(2*7) "), 0.5, "evaluateNumberIfPossible - expression");
		this.assertEquals(parse.evaluateNumberIfPossible("  -7 "), -7, "evaluateNumberIfPossible - int");
		this.assertEquals(parse.evaluateNumberIfPossible("  (9.5) "), 9.5, "evaluateNumberIfPossible - float");
		this.assertEquals(parse.evaluateNote([ "5", 2 ]), [5, 2], "evaluateNote");
		result = parse.evaluateNote([ "_", 1.5 ]);
		this.assert(result[0].class == Rest and: { result[1] == 1.5 }, "evaluateNote");
		this.assertEquals(parse.findAngleBracketed("5 stuff"), [ ], "findAngleBracketed - none");
		this.assertEquals(parse.findAngleBracketed("<7 2>"), [ [ 1, "7 2" ] ], "findAngleBracketed - simple");
		this.assertEquals(parse.findAngleBracketed("5 <a b> <1 <2 3>> stuff"), [ [ 3, "a b" ], [ 9, "1 <2 3>" ] ], "findAngleBracketed - multiple");
		this.assertEquals(parse.findChord("5 stuff <8 9>"), [ ], "findChord - none");
		this.assertEquals(parse.findChord("<7,2>"), [ [ 1, "7,2" ] ], "findChord - simple");
		this.assertEquals(parse.findChord("5 <a,b> <1 <2,3>> stuff <1 2>"), [ [ 3, "a,b" ], [ 12, "2,3" ] ], "findChord - multiple");
	}

	test_arrayDuration {
		this.assertEquals(parse.calculateDurations(parse.parseArray("[1 2]")),
			[ [ "1", 0.5 ], [ "2", 0.5 ] ], "simple array");
		this.assertEquals(parse.calculateDurations(parse.parseArray("1 2")),
			[ [ "1", 1 ], [ "2", 1 ] ], "elements only");
		this.assertEquals(parse.calculateDurations(parse.parseArray("1 2 [] 4")),
			[ [ "1", 1 ], [ "2", 1 ], [ "4", 1 ] ], "empty subArray");
		this.assertEquals(parse.calculateDurations(parse.parseArray("[1 2]@3")),
			[ [ "1", 1.5 ], [ "2", 1.5 ] ], "hold top-level array");
		this.assertEquals(parse.calculateDurations(parse.parseArray("[1 2]*2")),
			[ [ "1", 0.25 ], [ "2", 0.25 ], [ "1", 0.25 ], [ "2", 0.25 ]  ], "repeat top-level array");
		this.assertEquals(parse.calculateDurations(parse.parseArray("[1 2@3]*2@3")),
			[ [ "1", 0.375 ], [ "2", 1.125 ], [ "1", 0.375 ], [ "2", 1.125 ]  ], "repeat and hold top-level array with note hold");
		this.assertFloatEquals(parse.calculateDurations(parse.parseArray("[1 2 [3 4@3]]")).flop[1].sum, 1.0, "subArray with hold");
		this.assertEquals(parse.calculateDurations(parse.parseArray("[1 ~@2 [3 4]]")),
			[ [ "1", 0.25 ], [ "~", 0.5 ], [ "3", 0.125 ], [ "4", 0.125 ] ],
			"rest with hold");

		this.assertEquals(parse.parseArray("[1 2]"),
			[ ( [ ( "1" -> 1 ), ( "2" -> 1 ) ] -> 1 ) ], "parseArray simple");
		this.assertEquals(parse.parseArray("[1 2]@1.5"),
			[ ( [ ( "1" -> 1 ), ( "2" -> 1 ) ] -> 1.5 ) ], "parseArray with hold");
		this.assertEquals(parse.parseArray("1 2 3@2 4*2"),
			[ ( "1" -> 1 ), ( "2" -> 1 ), ( "3" -> 2.0 ), ( [ ( "4" -> 1 ), ( "4" -> 1 ) ] -> 1 ) ],
			"parseArray with hold and repeat");
		this.assertEquals(parse.parseArray("1 2 [3 4]*2@1.5"),
			[ ( "1" -> 1 ), ( "2" -> 1 ),
				( [ ( [ ( "3" -> 1 ), ( "4" -> 1 ) ] -> 1 ), ( [ ( "3" -> 1 ), ( "4" -> 1 ) ] -> 1 ) ] ->
					1.5 ) ], "parseArray subArray with repeat and hold");
		this.assertEquals(parse.parseArray("[1 2]*3@4"),
			[ ( [ ( [ ( "1" -> 1 ), ( "2" -> 1 ) ] -> 1 ),
				( [ ( "1" -> 1 ), ( "2" -> 1 ) ] -> 1 ),
				( [ ( "1" -> 1 ), ( "2" -> 1 ) ] -> 1 ) ] -> 4.0 ) ], "parseArray top-level array repeat and hold");
		this.assertEquals(parse.parseArray("[1 2@2 [-4 [5 -6]] 7]*2"),
			[ ( [ ( [ ( "1" -> 1 ), ( "2" -> 2.0 ),
				( [ ( "-4" -> 1 ), ( [ ( "5" -> 1 ), ( "-6" -> 1 ) ] -> 1 ) ] -> 1 ),
				( "7" -> 1 ) ] -> 1 ),
			( [ ( "1" -> 1 ), ( "2" -> 2.0 ),
				( [ ( "-4" -> 1 ), ( [ ( "5" -> 1 ), ( "-6" -> 1 ) ] -> 1 ) ] -> 1 ),
				( "7" -> 1 ) ] -> 1 ) ] -> 1 ) ], "parseArray hierarchy with repeat");
		this.assertEquals(parse.parseArray("[1 2@2 3*2@0.3 [-4 [5 -6]]@2 7]"),
			[ ( [ ( "1" -> 1 ), ( "2" -> 2.0 ),
				( [ ( "3" -> 1 ), ( "3" -> 1 ) ] -> 0.3 ),
				( [ ( "-4" -> 1 ), ( [ ( "5" -> 1 ), ( "-6" -> 1 ) ] -> 1 ) ] -> 2.0 ),
				( "7" -> 1 ) ] -> 1 ) ], "parseArray complex");

		this.assertEquals(parse.findArrayElem("[1 2@2 [-4 5] ]*3@2!4"),
			[ [ "[1 2@2 [-4 5] ]*3@2!4", "", "[1 2@2 [-4 5] ]", "3", "2", "4", "" ] ], "findArrayElem 1");
		this.assertEquals(parse.findArrayElem("1 3d2 bd 2@2 [-4 5]@3"),
			[ [ "1", "1", "", "", "", "", "" ],
				[ "3d2", "3d2", "", "", "", "", "" ],
				[ "bd", "bd", "", "", "", "", "" ],
				[ "2@2", "2@2", "", "", "", "", "" ],
				[ "[-4 5]@3", "", "[-4 5]", "", "3", "", "" ] ], "findArrayElem 2");
		this.assertEquals(parse.findArrayElem("2@2.5 5*4@3.14"),
			[ [ "2@2.5", "2@2.5", "", "", "", "", "" ],
				[ "5*4@3.14", "5*4@3.14", "", "", "", "", "" ] ], "findArrayElem 3");
		this.assertEquals(parse.findBalancedArray("[1 2@2 3 [-4 [5 -6]] 7]*3@2!4 [1 2 3]@2"),
			[ [ "[1 2@2 3 [-4 [5 -6]] 7]*3@2!4", "[1 2@2 3 [-4 [5 -6]] 7]", "3", "2", "4" ],
				[ "[1 2 3]@2", "[1 2 3]", "", "2", "" ] ], "findBalancedArray 1");
		this.assertEquals(parse.findBalancedArray("[1 2@2 3 [-4 [5 -6]] 7]@2.5"),
			[ [ "[1 2@2 3 [-4 [5 -6]] 7]@2.5", "[1 2@2 3 [-4 [5 -6]] 7]", "", "2.5", "" ] ],
			"findBalancedArray 2");
		this.assertEquals(parse.findBalancedArray(""),
			[ ], "findBalancedArray - empty");
		this.assertEquals(parse.findFloat(""),
			[ ], "findFloat - empty");
	}

	test_parseCharPatterns {
		this.assertEquals(parse.calculateDurations(parse.prParseCharArray('degree', "ab D")),
			[ [ 0, 0.25 ], [ 1, 0.25 ], [ Rest(1), 0.25 ], [ -11, 0.25 ] ],
			"degree pattern");
		this.assertEquals(parse.calculateDurations(parse.prParseCharArray('note', "a Bc")),
			[ [ 0, 0.25 ], [ Rest(1), 0.25 ], [ -23, 0.25 ], [ 2, 0.25 ] ],
			"note pattern");
		this.assertEquals(parse.calculateDurations(parse.prParseCharArray('midinote', "cCbAaAzZ")),
			[ [ 62, 0.125 ], [ 38, 0.125 ], [ 61, 0.125 ], [ 36, 0.125 ], [ 60, 0.125 ], [ 36, 0.125 ], [ 85, 0.125 ], [ 61, 0.125 ] ],
			"midinote pattern");
		this.approxCompareDurations(parse.calculateDurations(parse.prParseCharArray('freq', "abmM")),
			[ [ 60.midicps, 0.25 ], [ 61.midicps, 0.25 ], [ 72.midicps, 0.25 ], [ 48.midicps, 0.25 ] ],
			"freq pattern");
		this.approxCompareDurations(parse.calculateDurations(parse.prParseCharArray('amp', "aAbmzZ")),
			[ [ 52.reciprocal, 1/6 ], [ 26.reciprocal, 1/6 ], [ 26.reciprocal*1.5, 1/6 ], [ 0.48076923076923, 1/6 ], [ 0.98076923076923, 1/6 ], [ 1.0, 1/6 ] ],
			"amp pattern with letters", 0);
		this.approxCompareDurations(parse.calculateDurations(parse.prParseCharArray('amp', "0369")),
			[ [ 0, 0.25 ], [ 0.3, 0.25 ], [ 0.6, 0.25 ], [ 0.9, 0.25 ] ],
			"amp pattern with digits");
		this.approxCompareDurations(parse.calculateDurations(parse.prParseCharArray('pan', "aAbmzZ")),
			[ [ -1, 1/6 ], [ -1, 1/6 ], [ -1 + 12.5.reciprocal, 1/6 ], [ -0.04, 1/6 ], [ 1, 1/6 ], [ 1.0, 1/6 ] ],
			"pan pattern with letters", 0);
		this.approxCompareDurations(parse.calculateDurations(parse.prParseCharArray('pan', "0369")),
			[ [ -1, 0.25 ], [ -1/3, 0.25 ], [ 1/3, 0.25 ], [ 1, 0.25 ] ],
			"pan pattern with digits");

		this.approxCompareDurations(parse.calculateDurations(parse.prParseCharArray('degree', "01|234")),
			[ [ 0, 1/2 ], [ 1, 1/2 ], [ 2, 1/3 ], [ 3, 1/3 ], [ 4, 1/3 ] ],
			"pattern with '/' bars");
		this.approxCompareDurations(parse.calculateDurations(parse.prParseCharArray('degree', "abc|defg")),
			[ [ 0, 1/3 ], [ 1, 1/3 ], [ 2, 1/3 ], [ 3, 1/4 ], [ 4, 1/4 ], [ 5, 1/4 ], [ 6, 1/4 ] ],
			"pattern with '|' bars");
		this.assertEquals(parse.calculateDurations(parse.prParseCharArray('degree', "02__7___")),
			[ [ 0, 1/8 ], [ 2, 3/8 ], [ 7, 1/2 ] ],
			"pattern with holds");
		this.assertEquals(parse.calculateDurations(parse.prParseCharArray('degree', "8@012345678")),
			[ [ 0, 1/8 ], [ 1, 1/8 ], [ 2, 1/8 ], [ 3, 1/8 ], [ 4, 1/8 ], [ 5, 1/8 ], [ 6, 1/8 ], [ 7, 1/8 ], [ 8, 1/8 ], [ Rest(), 7/8 ] ],
			"fixed events per bar");
		this.approxCompareDurations(parse.calculateDurations(parse.prParseCharArray('degree', "5@3__278|12")),
			[ [ 3, 3/5 ], [ 2, 1/5 ], [ 7, 1/5 ], [ 8, 1/5 ], [ Rest(), 4/5 ], [ 1, 1/5 ], [ 2, 1/5 ], [ Rest(), 3/5 ] ],
			"fixed events per bar - multiple bars");
		this.approxCompareDurations(parse.calculateDurations(parse.prParseCharArray('degree', "3@1___|27")),
			[ [ 1, 4/3 ], [ Rest(), 2/3 ], [ 2, 1/3 ], [ 7, 1/3 ], [ Rest(), 1/3 ] ],
			"fixed events per bar - hold extending over bar boundary");
		this.approxCompareDurations(parse.calculateDurations(parse.prParseCharArray('degree', "4@1234|8@56|3@3217")),
			[ [ 1, 1/4 ], [ 2, 1/4 ], [ 3, 1/4 ], [ 4, 1/4 ], [ 5, 1/8 ], [ 6, 1/8 ], [ Rest(), 6/8 ], [ 3, 1/3 ] , [ 2, 1/3 ] , [ 1, 1/3 ] , [ 7, 1/3 ] , [ Rest(), 2/3 ] ],
			"fixed events per bar - changing per bar");
		this.approxCompareDurations(parse.calculateDurations(parse.prParseCharArray('degree', "4@123|0@56|43210")),
			[ [ 1, 1/4 ], [ 2, 1/4 ], [ 3, 1/4 ], [ Rest(), 1/4 ], [ 5, 1/2 ], [ 6, 1/2 ], [ 4, 1/5 ] , [ 3, 1/5 ] , [ 2, 1/5 ] , [ 1, 1/5 ] , [ 0, 1/5 ] ],
			"fixed to variable events per bar");
		this.approxCompareDurations(parse.calculateDurations(parse.prParseCharArray('degree', "123|56|8@43210")),
			[ [ 1, 1/3 ], [ 2, 1/3 ], [ 3, 1/3 ], [ 5, 1/2 ], [ 6, 1/2 ], [ 4, 1/8 ] , [ 3, 1/8 ] , [ 2, 1/8 ] , [ 1, 1/8 ] , [ 0, 1/8 ], [ Rest(), 3/8 ] ],
			"variable to fixed events per bar");

		this.approxCompareDurations(parse.calculateDurations(parse.prParseCharArray('degree', "123|56|8@43210")),
			[ [ 1, 1/3 ], [ 2, 1/3 ], [ 3, 1/3 ], [ 5, 1/2 ], [ 6, 1/2 ], [ 4, 1/8 ] , [ 3, 1/8 ] , [ 2, 1/8 ] , [ 1, 1/8 ] , [ 0, 1/8 ], [ Rest(), 3/8 ] ],
			"variable to fixed events per bar");

		~inst = (\o: 36, \x: 38, \h: 42);
		{
			var str = "mn~inst'o x o hh'";
			var expected = Pbind('midinote', Pseq([ 36, Rest(), 38, Rest(), 36, Rest(), 42, 42 ], 1), 'dur', 0.125);
			this.compareEvents(this.prPat(str), expected, 9, "char variable lookup");

			str = "mn~unknown'o x o hh'";
			this.assertException({parse.preProcess(str)}, Error, "unknown char variable lookup");

			~inst = (\o: \fred, \x: 38);
			str = "mn~inst'o_%x _dx'";
			expected = Pbind('midinote', Pseq([ \fred, Rest(), 38, Rest(), Rest(), 38 ], 1), 'dur', Pseq([0.25, 0.125, 0.125, 0.25, 0.125, 0.125]));
			this.compareEvents(this.prPat(str), expected, 7, "missing char variable lookups");

			~samp = (\a: Buffer(), \b: Buffer(), \c: Buffer());
			str = "buf~samp'ab c'";
			expected = Pbind('buf', Pseq([ ~samp.a, ~samp.b, Rest(), ~samp.c ], 1), 'dur', 0.25);
			this.compareEvents(this.prPat(str), expected, 5, "Buffer char variable lookup");

			thisThread.randSeed = 234;
			~weird = (\a: 0, \b: 7, \c: Pwhite(-7,7,1));
			str = "note~weird'ab cacbc'";
			expected = Pbind('note', Pseq([ 0, 7, Rest(), 5, 0, -3, 7, 3 ], 1), 'dur', 0.125);
			this.compareEvents(this.prPat(str), expected, 9, "function in char variable lookup");

			str = "deg'c' << amp'4'";
			expected = Pbind('degree', 2, 'dur', 1, 'amp', 0.4);
			this.compareEvents(this.prPat(str), expected, 2, "single char patterns");

			str = "deg~weird'b' << amp~weird'a'";
			expected = Pbind('degree', Pn(7, 1), 'dur', 1, 'amp', Pn(0, 1));
			this.compareEvents(this.prPat(str), expected, 2, "single char patterns w/variable lookup");
		}.value;
	}

	test_shortKeys {
		this.assertEquals(parse.resolveAbbrev("deg"), \degree, "deg as string");
		this.assertEquals(parse.resolveAbbrev(\deg), \degree, "deg");
	}

	test_helperClasses {
		var str = "deg\"0 1 2 3 4 5 6 7\" << PmaskBjork(3,8)";
		var expected = Pbind(\degree, Pseq((0..7)),
			\dur, 0.125, \mask, Pseq([1, Rest(0), Rest(0), 1, Rest(0), Rest(0), 1, Rest(0)]));
		this.compareEvents(this.prPat(str), expected, 9, "Bjorklund masking 1");

		str = "deg\"0 1 2 3 4 5 6 7\" << amp\"1 0.25@7\" << inst\"\\ping\" << PmaskBjork(3,8)";
		expected = Pbind(\degree, Pseq((0..7)), \amp, Pseq([1] ++ (0.25!7)), \instrument, \ping,
			\dur, 0.125, \mask, Pseq([1, Rest(0), Rest(0), 1, Rest(0), Rest(0), 1, Rest(0)]));
		this.compareEvents(this.prPat(str), expected, 9, "Bjorklund masking 2");
	}

	prPlayAndGetEvents { arg bacalao, pattern, numDesiredEvents = 4, desiredKeys = #[\degree, \dur, \beat], maxTime = 2.0;
		var events = [];
		var finishFunc = Pfunc{ |ev|
			// We set e.parent_(nil) so we lose the extra stuff fromm Event.default and
			// only get a simple Event with the keys we want. (Alternatively could do ().putAll(e))
			events = events.add(ev.select{ |val,key| desiredKeys.includes(key) }.parent_(nil));
			if (events.size < numDesiredEvents) {
				true
			} {
				// When we've got enough events, return nil so the pattern stops,
				// because otherwise we'll get overwhelmed (we've set the clock
				// to run very fast)
				nil
			}
		};
		Ndef.clear;
		Pdef.clear;
		bacalao.p(\test, Pbind(\beat, Ptime(), \finish, finishFunc) <> pattern);
		this.wait({ events.size >= numDesiredEvents }, "waiting for all Events", maxTime);

		// Not sure why the Pfunc adds extra events after the end, so
		// just truncate the list to the desired size...
		^events.keep(numDesiredEvents)
	}

	prPat { arg str;
		^parse.preProcess(str).interpret;
	}

	test_playing {
		var t = TempoClock(50, 0, 0); // make a very fast clock, so we don't wait around
		var b = Bacalao(t);
		this.bootServer;
		1.wait; // wait for ServerTree to be built (Safety quark installed)
		{
			var expected = [
				(degree: 0, dur: 0.25, beat: 0.0),
				(degree: 1, dur: 0.25, beat: 1.0),
				(degree: 2, dur: 0.25, beat: 2.0),
				(degree: 3, dur: 0.25, beat: 3.0),
				(degree: 5, dur: 0.25, beat: 4.0),
				(degree: 1, dur: 0.25, beat: 5.0),
				(degree: 2, dur: 0.25, beat: 6.0),
				(degree: 3, dur: 0.25, beat: 7.0),
				(degree: 0, dur: 0.25, beat: 0.0),
			];
			var events = this.prPlayAndGetEvents(b,
				this.prPat("deg\"<0 5> 1 2 3\""),
				expected.size);
			this.assertEquals(events, expected, "simple alternation");
		}.value;

		{
			var expected = [
				(degree: 0, dur: 0.25, beat: 0.0, amp: 0.9),
				(degree: 1, dur: 0.25, beat: 1.0, amp: 0.7),
				(degree: [2, 9], dur: 0.25, beat: 2.0, amp: 0.5),
				(degree: 3, dur: 0.125, beat: 3.0, amp: 0.3),
				(degree: 4, dur: 0.125, beat: 3.5, amp: 0.2),
				(degree: 5, dur: 0.25, beat: 4.0, amp: 0.9),
				(degree: 1, dur: 0.25, beat: 5.0, amp: 0.7),
				(degree: [2, 9], dur: 0.25, beat: 6.0, amp: 0.5),
				(degree: 3, dur: 0.125, beat: 7.0, amp: 0.3),
				(degree: 4, dur: 0.125, beat: 7.5, amp: 0.2),
				(degree: 0, dur: 0.25, beat: 0.0, amp: 0.9),
			];
			var events = this.prPlayAndGetEvents(b,
				this.prPat("deg\"<0 5> 1 <2,9> [3 4]\"") << this.prPat("amp'98765432'"),
				expected.size, #[\degree, \dur, \beat, \amp]);
			this.assertEquals(events, expected, "time chaining");
		}.value;

		{
			var expected = [
				(midinote: 36, dur: 0.25, beat: 0.0),
				(midinote: 47, dur: 0.25, beat: 1.0),
				(midinote: 49, dur: 0.25, beat: 2.0),
				(midinote: [63, 51], dur: 0.25, beat: 3.0),
				(midinote: 36, dur: 0.25, beat: 0.0),
			];
			var events = this.prPlayAndGetEvents(b,
				this.prPat("mn~batt\"1 12 2:1 <4:2,v>\""),
				expected.size, #[\midinote, \dur, \beat]);
			this.assertEquals(events, expected, "~batt variable lookup");

			events = this.prPlayAndGetEvents(b,
				this.prPat("mn~bat12\"a1 a12 b2 <c4,b4>\""),
				expected.size, #[\midinote, \dur, \beat]);
			this.assertEquals(events, expected, "~bat12 variable lookup");

			expected[3].midinote = [39, 51];
			events = this.prPlayAndGetEvents(b,
				this.prPat("mn~bat4\"a1 c4 d2 <a4,d4>\""),
				expected.size, #[\midinote, \dur, \beat]);
			this.assertEquals(events, expected, "~bat4 variable lookup");
		}.value;
	}

}
