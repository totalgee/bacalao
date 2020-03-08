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

	compareEvents { arg resultPattern, expectedPattern, numEvents, message;
		var resultEvents = resultPattern.asStream.nextN(numEvents, Event.default);
		var expectedEvents = expectedPattern.asStream.nextN(numEvents, Event.default);
		^this.assertEquals(resultEvents, expectedEvents, message);
	}

	approxCompareDurations { arg result, expected, message;
		this.assertArrayFloatEquals(result.flop[0], expected.flop[0], message + "(values)", 1e-6);
		this.assertArrayFloatEquals(result.flop[1], expected.flop[1], message + "(durations)", 1e-6);
	}

	test_parsing {
		var b = Bacalao();
		var str = "deg\" 1 2    3 4\"";
		var expected = Pbind(\degree, Pseq([1, 2, 3, 4]), \dur, Pseq(0.25!4));
		var preProcessed = parse.preProcess(str);
		this.compareEvents(preProcessed.interpret, expected, 5, "deg");

		b.start;
		this.compareEvents(preProcessed.interpret, expected, 5, "deg through Interpreter");
		b.stop;

		str = "deg\"0\"";
		expected = Pbind(\degree, 0);
		preProcessed = parse.preProcess(str);
		this.assertEquals(preProcessed, expected.cs, "deg (single)");

		str = "deg\"[0]\"";
		expected = Pbind(\degree, 0, \dur, 1.0);
		preProcessed = parse.preProcess(str);
		this.assertEquals(preProcessed, expected.cs, "deg (single array entry)");

		str = "deg\"0@1\"";
		expected = Pbind(\degree, 0, \dur, 1.0);
		preProcessed = parse.preProcess(str);
		this.assertEquals(preProcessed, expected.cs, "deg (single with duration)");

		str = "freq\"Pgeom(100,1.5,4)\"";
		expected = Pbind(\freq, Pgeom(100, 1.5, 4));
		preProcessed = parse.preProcess(str);
		this.compareEvents(preProcessed.interpret, expected, 5, "freq Pgeom");

		str = "deg\"1 ~ 3 ~\"";
		expected = Pbind(\degree, Pseq([1, Rest(), 3, Rest()]), \dur, 1/4);
		preProcessed = parse.preProcess(str);
		this.compareEvents(preProcessed.interpret, expected, 5, "rests");

		this.assertEquals(parse.parseArray("a 1@4 default@2 2"),
			[ ("a" -> 1), ("1" -> 4.0), ("default" -> 2.0), ("2" -> 1) ], "parseArray symbols");

		str = "ins\"\\a \\b \\default \\c\"";
		expected = Pbind(\instrument, Pseq([\a, \b, \default, \c]), \dur, 1/4);
		preProcessed = parse.preProcess(str);
		this.compareEvents(preProcessed.interpret, expected, 5, "symbols");

		str = "deg\"[0 [1 2] [3 [4 5]] [[6 7 8] 7]]\"";
		expected = Pbind(\degree, Pseq([ 0, 1, 2, 3, 4, 5, 6, 7, 8, 7 ]),
			\dur, Pseq([ 1/4, 1/8, 1/8, 1/8, 1/16, 1/16, 0.041666666666667, 0.041666666666667, 0.041666666666667, 1/8 ]));
		preProcessed = parse.preProcess(str);
		this.compareEvents(preProcessed.interpret, expected, 11, "nested phrases");

		str = "deg\"0 1 <2 3> 4\"";
		expected = Pbind(\degree, Ppatlace([ 0, 1, Pseq([2, 3], inf), 4 ], 2),
			\dur, 1/4);
		preProcessed = parse.preProcess(str);
		this.compareEvents(preProcessed.interpret, expected, 9, "alternating elements");

		str = "deg\"1 2 <3 4> 5 <6 7 8>\"";
		expected = Pbind(\degree, Ppatlace([ 1, 2, Pseq([3, 4], inf), 5, Pseq([6, 7, 8], inf) ], 6),
			\dur, 1/5);
		preProcessed = parse.preProcess(str);
		this.compareEvents(preProcessed.interpret, expected, 31, "alternating elements longer");

		str = "deg\"1 2 3 4 5 6 7 8\" << amp\"Pn(Pseries(0.1,0.1,3))\"";
		expected = Pbind(\degree, Pseq((1..8)), \amp, Pn(Pseries(0.1,0.1,3)), \dur, 0.125);
		preProcessed = parse.preProcess(str);
		this.compareEvents(preProcessed.interpret, expected, 9, "value Patterns in keys 1");

		str = "deg\"[1 2 3 4]@2\" << amp\"Pn(Pseries(0.1,0.4,3))\" << bob\"Pgeom(1,10,inf)@1\"";
		expected = Pbind(\degree, Pseq((1..4)), \amp, Pn(Pseries(0.1,0.4,3)), \bob, Pstutter(2, Pgeom(1,10,inf)), \dur, 0.5);
		preProcessed = parse.preProcess(str);
		this.compareEvents(preProcessed.interpret, expected, 5, "value Patterns in keys 2");

		str = "deg\"<0 <1,2>> <3 <4,5> 6>\"";
		expected = Pbind('degree', Ppatlace([ Pseq([ 0, [ 1, 2 ] ], inf), Pseq([ 3, [ 4, 5 ], 6 ], inf) ], 6), 'dur', 0.5);
		preProcessed = parse.preProcess(str);
		this.compareEvents(preProcessed.interpret, expected, 13, "chord elements");

		str = "deg'bacalao ' << amp'94041377'";
		expected = Pbind('degree', Pseq([ 1, 0, 2, 0, 11, 0, 14, Rest(1) ], 1.0),
			'dur', 0.125,
			'amp', Pseq([1, 0.5, 0.1, 0.5, 0.2, 0.4, 0.8, 0.8], 1));
		preProcessed = parse.preProcess(str);
		this.compareEvents(preProcessed.interpret, expected, 9, "char patterns");
	}

	test_parserVariables {
		var str, expected;
		var b = Bacalao();
		b.setParserVariable("bd", [36, 37, 48, 49]);
		Bacalao.setParserVariable("sd", 38);

		str = "note\"sd\"";
		expected = Pbind(\note, 38);
		this.assertEquals(parse.preProcess(str), expected.cs, "scalar variable");
		this.compareEvents(parse.preProcess(str).interpret, expected, 5, "scalar variable");

		str = "note\"sd:2\"";
		expected = Pbind(\note, 38);
		this.compareEvents(parse.preProcess(str).interpret, expected, 5, "scalar variable with index");

		str = "note\"sd:r\"";
		expected = Pbind(\note, 38);
		this.compareEvents(parse.preProcess(str).interpret, expected, 5, "scalar variable with random index");

		str = "note\"bd\"";
		expected = Pbind(\note, 36);
		this.assertEquals(parse.preProcess(str), expected.cs, "array variable");
		this.compareEvents(parse.preProcess(str).interpret, expected, 5, "array variable");

		str = "note\"bd:3\"";
		expected = Pbind(\note, 49);
		this.assertEquals(parse.preProcess(str), expected.cs, "array variable with index");

		str = "note\"bd:-1\"";
		expected = Pbind(\note, 49);
		this.assertEquals(parse.preProcess(str), expected.cs, "array variable with negative index");

		str = "Pseed(33, note\"bd:r\" )";
		expected = Pseed(33, Pbind(\note, Prand([36, 37, 48, 49], inf)));
		this.compareEvents(parse.preProcess(str).interpret, expected, 10, "array variable with random index");

		~chord = [ [1,3,5,7], [0,1,3,5], [-2,0,1,3], [-4,-2,0,1] ];
		str = "deg\"chord:1 chord:3\"";
		expected = Pbind('degree', Ppatlace([ [0,1,3,5], [-4,-2,0,1] ], 1), 'dur', 0.5);
		this.compareEvents(parse.preProcess(str).interpret, expected, 3, "variables in current Environment");

		~custom = (
			fred: 36,
			bob: [38, 40],
			steve: [ [46, 48, 50] ]
		);
		str = "mn~custom\"fred bob:0 steve bob:1\"";
		expected = Pbind('midinote', Ppatlace([ 36, 38, [46,48,50], 40]), 'dur', 0.25);
		this.compareEvents(parse.preProcess(str).interpret, expected, 5, "locally-specified variable");

		str = "mn~unknown\"dave ~\"";
		this.assertException({parse.preProcess(str)}, Error, "unknown event pattern variable lookup");
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

		this.assertEquals(parse.findArrayElem("[1 2@2 [-4 5] ]*3@2"),
			[ [ "[1 2@2 [-4 5] ]*3@2", "", "[1 2@2 [-4 5] ]", "3", "2", "" ] ], "findArrayElem 1");
		this.assertEquals(parse.findArrayElem("1 3d2 bd 2@2 [-4 5]@3"),
			[ [ "1", "1", "", "", "", "" ],
				[ "3d2", "3d2", "", "", "", "" ],
				[ "bd", "bd", "", "", "", "" ],
				[ "2@2", "2@2", "", "", "", "" ],
				[ "[-4 5]@3", "", "[-4 5]", "", "3", "" ] ], "findArrayElem 2");
		this.assertEquals(parse.findArrayElem("2@2.5 5*4@3.14"),
			[ [ "2@2.5", "2@2.5", "", "", "", "" ],
				[ "5*4@3.14", "5*4@3.14", "", "", "", "" ] ], "findArrayElem 3");
		this.assertEquals(parse.findBalancedArray("[1 2@2 3 [-4 [5 -6]] 7]*3@2 [1 2 3]@2"),
			[ [ "[1 2@2 3 [-4 [5 -6]] 7]*3@2", "[1 2@2 3 [-4 [5 -6]] 7]", "3", "2" ],
				[ "[1 2 3]@2", "[1 2 3]", "", "2" ] ], "findBalancedArray 1");
		this.assertEquals(parse.findBalancedArray("[1 2@2 3 [-4 [5 -6]] 7]@2.5"),
			[ [ "[1 2@2 3 [-4 [5 -6]] 7]@2.5", "[1 2@2 3 [-4 [5 -6]] 7]", "", "2.5" ] ],
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
			[ [ 0.1, 0.25 ], [ 0.4, 0.25 ], [ 0.7, 0.25 ], [ 1.0, 0.25 ] ],
			"amp pattern with digits");

		this.approxCompareDurations(parse.calculateDurations(parse.prParseCharArray('degree', "01/234")),
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
		this.approxCompareDurations(parse.calculateDurations(parse.prParseCharArray('degree', "5@3__278/12")),
			[ [ 3, 3/5 ], [ 2, 1/5 ], [ 7, 1/5 ], [ 8, 1/5 ], [ Rest(), 4/5 ], [ 1, 1/5 ], [ 2, 1/5 ], [ Rest(), 3/5 ] ],
			"fixed events per bar - multiple bars");
		this.approxCompareDurations(parse.calculateDurations(parse.prParseCharArray('degree', "3@1___/27")),
			[ [ 1, 4/3 ], [ Rest(), 2/3 ], [ 2, 1/3 ], [ 7, 1/3 ], [ Rest(), 1/3 ] ],
			"fixed events per bar - hold extending over bar boundary");
		this.approxCompareDurations(parse.calculateDurations(parse.prParseCharArray('degree', "4@1234/8@56|3@3217")),
			[ [ 1, 1/4 ], [ 2, 1/4 ], [ 3, 1/4 ], [ 4, 1/4 ], [ 5, 1/8 ], [ 6, 1/8 ], [ Rest(), 6/8 ], [ 3, 1/3 ] , [ 2, 1/3 ] , [ 1, 1/3 ] , [ 7, 1/3 ] , [ Rest(), 2/3 ] ],
			"fixed events per bar - changing per bar");
		this.approxCompareDurations(parse.calculateDurations(parse.prParseCharArray('degree', "4@123/0@56/43210")),
			[ [ 1, 1/4 ], [ 2, 1/4 ], [ 3, 1/4 ], [ Rest(), 1/4 ], [ 5, 1/2 ], [ 6, 1/2 ], [ 4, 1/5 ] , [ 3, 1/5 ] , [ 2, 1/5 ] , [ 1, 1/5 ] , [ 0, 1/5 ] ],
			"fixed to variable events per bar");
		this.approxCompareDurations(parse.calculateDurations(parse.prParseCharArray('degree', "123/56/8@43210")),
			[ [ 1, 1/3 ], [ 2, 1/3 ], [ 3, 1/3 ], [ 5, 1/2 ], [ 6, 1/2 ], [ 4, 1/8 ] , [ 3, 1/8 ] , [ 2, 1/8 ] , [ 1, 1/8 ] , [ 0, 1/8 ], [ Rest(), 3/8 ] ],
			"variable to fixed events per bar");

		this.approxCompareDurations(parse.calculateDurations(parse.prParseCharArray('degree', "123/56/8@43210")),
			[ [ 1, 1/3 ], [ 2, 1/3 ], [ 3, 1/3 ], [ 5, 1/2 ], [ 6, 1/2 ], [ 4, 1/8 ] , [ 3, 1/8 ] , [ 2, 1/8 ] , [ 1, 1/8 ] , [ 0, 1/8 ], [ Rest(), 3/8 ] ],
			"variable to fixed events per bar");

		~inst = (\o: 36, \x: 38, \h: 42);
		{
			var str = "mn~inst'o x o hh'";
			var expected = Pbind('midinote', Pseq([ 36, Rest(), 38, Rest(), 36, Rest(), 42, 42 ], 1), 'dur', 0.125);
			this.compareEvents(parse.preProcess(str).interpret, expected, 9, "char variable lookup");

			str = "mn~unknown'o x o hh'";
			this.assertException({parse.preProcess(str)}, Error, "unknown char variable lookup");

			~inst = (\o: \fred, \x: 38);
			str = "mn~inst'o_%x _dx'";
			expected = Pbind('midinote', Pseq([ \fred, Rest(), 38, Rest(), Rest(), 38 ], 1), 'dur', Pseq([0.25, 0.125, 0.125, 0.25, 0.125, 0.125]));
			this.compareEvents(parse.preProcess(str).interpret, expected, 7, "missing char variable lookups");

			~samp = (\a: Buffer(), \b: Buffer(), \c: Buffer());
			str = "buf~samp'ab c'";
			expected = Pbind('buf', Pseq([ ~samp.a, ~samp.b, Rest(), ~samp.c ], 1), 'dur', 0.25);
			this.compareEvents(parse.preProcess(str).interpret, expected, 5, "Buffer char variable lookup");

			thisThread.randSeed = 234;
			~weird = (\a: 0, \b: 7, \c: Pwhite(-7,7,1));
			str = "note~weird'ab cacbc'";
			expected = Pbind('note', Pseq([ 0, 7, Rest(), 5, 0, -3, 7, 3 ], 1), 'dur', 0.125);
			this.compareEvents(parse.preProcess(str).interpret, expected, 9, "function in char variable lookup");
		}.value;

	}

	test_shortKeys {
		this.assertEquals(parse.resolveAbbrev("deg"), \degree, "deg as string");
		this.assertEquals(parse.resolveAbbrev(\deg), \degree, "deg");
	}

}
