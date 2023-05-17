TestBacalaoPatterns : UnitTest {

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

	test_scramblePattern {
		var p, d, a;
		var results, expected;

		// Random Event order
		p = Pbind(\degree, Pseq((0..3)), \dur, 0.25, \amp, Pseq((1..4) / 4)); // degree and amp Pbind
		results = p.scramble(123, 1).asStream.nextN(5, ());
		expected = Pbind(\degree, [1,3,0,2].pseq, \dur, 0.25, \amp, [0.5,1.0,0.25,0.75].pseq).asStream.nextN(5, ());
		this.assertEquals(results, expected, "scramble simple Pbind");

		// With Pchain (scrambling after the chaining)
		d = Pbind(\degree, (0..3).pseq, \dur, 0.25); // degree Pbind
		a = Pbind(\amp, Pseq((1..4) / 4), \dur, 0.25); // amp Pbind
		results = (d <> a).scramble(678).asStream.nextN(5, ());
		expected = [
			(degree: 2, dur: 0.25, amp: 0.75),
			(degree: 3, dur: 0.25, amp: 1.0),
			(degree: 1, dur: 0.25, amp: 0.5),
			(degree: 0, dur: 0.25, amp: 0.25),
			nil
		];
		this.assertEquals(results, expected, "scramble after chaining");

		// Scramble before Pchain (amps remain in order)
		results = (d.scramble(222) <> a).asStream.nextN(5, ());
		expected = [
			(degree: 1, dur: 0.25, amp: 0.25),
			(degree: 3, dur: 0.25, amp: 0.5),
			(degree: 0, dur: 0.25, amp: 0.75),
			(degree: 2, dur: 0.25, amp: 1.0),
			nil
		];
		this.assertEquals(results, expected, "scramble before chaining");


		p = Pbind(\degree, Pseq((0..3)), \dur, 0.25, \amp, Pseq((1..4) / 4)); // degree and amp Pbind
		results = p.scramble([123, 456].pseq).asStream.nextN(9, ());
		expected = [
			(degree: 1, dur: 0.25, amp: 0.5),
			(degree: 3, dur: 0.25, amp: 1.0),
			(degree: 0, dur: 0.25, amp: 0.25),
			(degree: 2, dur: 0.25, amp: 0.75),
			(degree: 3, dur: 0.25, amp: 1.0),
			(degree: 2, dur: 0.25, amp: 0.75),
			(degree: 0, dur: 0.25, amp: 0.25),
			(degree: 1, dur: 0.25, amp: 0.5),
			nil
		];
		this.assertEquals(results, expected, "scramble with seed Pattern");
	}

	test_randPattern {
		var p = Pbind(\degree, Pseq((0..5)));
		var results = p.rand(123, 6).asStream.nextN(7, ());
		var expected = [
			(degree: 2),
			(degree: 4),
			(degree: 4),
			(degree: 2),
			(degree: 3),
			(degree: 1),
			nil
		];
		this.assertEquals(results, expected, "rand");
	}

	test_randSwapPattern {
		var p = Pbind(\degree, Pseq((0..5)));
		var results = p.randSwap(1, true, 159).asStream.nextN(7, ());
		var expected = [
			(degree: 0),
			(degree: 1),
			(degree: 3),
			(degree: 2),
			(degree: 4),
			(degree: 5),
			nil
		];
		this.assertEquals(results, expected, "randSwap adjacent 1");

		results = p.randSwap(3, true, 159).asStream.nextN(7, ());
		expected = [
			(degree: 1),
			(degree: 0),
			(degree: 3),
			(degree: 2),
			(degree: 5),
			(degree: 4),
			nil
		];
		this.assertEquals(results, expected, "randSwap adjacent 3");

		results = p.randSwap(2, false, 159).asStream.nextN(7, ());
		expected = [
			(degree: 2),
			(degree: 1),
			(degree: 4),
			(degree: 3),
			(degree: 0),
			(degree: 5),
			nil
		];
		this.assertEquals(results, expected, "randSwap non-adjacent 2");
	}

	test_perfectShufflePattern {
		var p = Pbind(\degree, Pseq((0..5)));
		var results = p.perfectShuffle(6).asStream.nextN(7, ());
		var expected = [
			(degree: 0),
			(degree: 3),
			(degree: 1),
			(degree: 4),
			(degree: 2),
			(degree: 5, dur: 1.0),
			nil
		];
		this.assertEquals(results, expected, "perfectShuffle");
	}

	test_reversePattern {
		var p = Pbind(\degree, Pseq((1..5)));
		var results = p.reverse(5).asStream.nextN(6, ());
		var expected = [
			(degree: 5, dur: 1.0),
			(degree: 4),
			(degree: 3),
			(degree: 2),
			(degree: 1),
			nil
		];
		this.assertEquals(results, expected, "reverse");
	}

	test_mirrorPatterns {
		var p = Pbind(\degree, Pseq((0..3)), \dur, 0.25);
		var results = p.mirror(1).asStream.nextN(8, ());
		var expected = [
			(degree: 0, dur: 0.25),
			(degree: 1, dur: 0.25),
			(degree: 2, dur: 0.25),
			(degree: 3, dur: 0.25),
			(degree: 2, dur: 0.25),
			(degree: 1, dur: 0.25),
			(degree: 0, dur: 0.25),
			nil
		];
		this.assertEquals(results, expected, "mirror");

		p = Pbind(\degree, Pseq((0..3)), \dur, 0.25);
		results = p.mirror1(1).asStream.nextN(7, ());
		expected = [
			(degree: 0, dur: 0.25),
			(degree: 1, dur: 0.25),
			(degree: 2, dur: 0.25),
			(degree: 3, dur: 0.25),
			(degree: 2, dur: 0.25),
			(degree: 1, dur: 0.25),
			nil
		];
		this.assertEquals(results, expected, "mirror1");

		p = Pbind(\degree, Pseq((0..3)), \dur, 0.25);
		results = p.mirror2(1).asStream.nextN(9, ());
		expected = [
			(degree: 0, dur: 0.25),
			(degree: 1, dur: 0.25),
			(degree: 2, dur: 0.25),
			(degree: 3, dur: 0.25),
			(degree: 3, dur: 0.25),
			(degree: 2, dur: 0.25),
			(degree: 1, dur: 0.25),
			(degree: 0, dur: 0.25),
			nil
		];
		this.assertEquals(results, expected, "mirror2");
	}

	test_rotatePattern {
		var p = Pbind(\degree, Pseq((1..5)));
		var results = p.rotate(-2, 5).asStream.nextN(6, ());
		var expected = [
			(degree: 3),
			(degree: 4),
			(degree: 5, dur: 1.0),
			(degree: 1),
			(degree: 2),
			nil
		];
		this.assertEquals(results, expected, "rotate");
	}

	test_pyramidPattern {
		var p = Pbind(\degree, Pseq((0..3)));
		var results = p.pyramid().asStream.nextN(11, ());
		var expected = [
			(degree: 0),
			(degree: 0),
			(degree: 1),
			(degree: 0),
			(degree: 1),
			(degree: 2),
			(degree: 0),
			(degree: 1),
			(degree: 2),
			(degree: 3),
			nil
		];
		this.assertEquals(results, expected, "pyramid default (1)");

		p = Pbind(\degree, Pseq((0..3)));
		results = p.pyramid(9, 4).asStream.nextN(17, ());
		expected = [
			(degree: 0),
			(degree: 0),
			(degree: 1),
			(degree: 0),
			(degree: 1),
			(degree: 2),
			(degree: 0),
			(degree: 1),
			(degree: 2),
			(degree: 3, dur: 1.0),
			(degree: 1),
			(degree: 2),
			(degree: 3, dur: 1.0),
			(degree: 2),
			(degree: 3, dur: 1.0),
			(degree: 3, dur: 1.0),
			nil
		];
		this.assertEquals(results, expected, "pyramid 9");
	}

	test_permutePattern {
		var p = Pbind(\degree, Pseq((1..5)));
		var results = p.permute(30, 5).asStream.nextN(6, ());
		var expected = [
			(degree: 1),
			(degree: 4),
			(degree: 2),
			(degree: 3),
			(degree: 5, dur: 1.0),
			nil
		];
		this.assertEquals(results, expected, "permute");
	}

	test_conditionalEventKey {
		var d = Pbind(\degree, Pseq((0..3)), \dur, 0.25).collect { |ev|
			if (ev[\degree].odd) { ev[\odd] = true };
			ev;
		};
		var a = Pbind(\amp, Pseq((1..4) / 4), \dur, 0.25);
		var results = (d.scramble(randSeed: 678) <> a).asStream.nextN(5, ());
		var expected = [
			(degree: 2, dur: 0.25, amp: 0.25),
			(degree: 3, dur: 0.25, odd: true, amp: 0.5),
			(degree: 1, dur: 0.25, odd: true, amp: 0.75),
			(degree: 0, dur: 0.25, amp: 1.0),
			nil
		];
		this.assertEquals(results, expected, "set odd keys");
	}

	test_PnSafe {
		// A simple Pbind
		var p = Pbind(\degree, Pseq([ 0, 1 ], 1), \dur, 0.5);
		var results = p.asStream.nextN(3, ());
		var expected = [
			(degree: 0, dur: 0.5),
			(degree: 1, dur: 0.5),
			nil
		];
		this.assertEquals(results, expected, "simple Pbind");

		// (p*2 was a live-coding typo, but the Pbinop produced should return nil...and does)
		results = (p*2).asStream.nextN(3, ());
		expected = nil ! 3;
		this.assertEquals(results, expected, "Pbind*2 returns nil");

		// Calling Pn(p*2) would produce an infinite loop, instead we use
		// PnSafe(p*2), which should throw an Error
		// this.assertException({ results = PnSafe(p*2, inf).asStream.nextN(3, ()) },
		// Error, "PnSafe with Pattern returning no Events should throw");

		// No longer throws an Error, but just prints a message returns nil (ends Pattern)
		results = PnSafe(p*2, inf).asStream.nextN(3, Event.default);
		this.assertEquals(results, expected, "PnSafe avoids infinite loop");
	}

	test_modifiers {
		var p = Pbind(\degree, Pseq((0..3), 2), \dur, 0.5).mul(\degree, 2).add(\dur, Pseq([0, 0.5], inf).stutter(3)).set(\pan, -1);
		var results = p.asStream.nextN(9, ());
		var expected = [
			(degree: 0, dur: 0.5, pan: -1),
			(degree: 2, dur: 0.5, pan: -1),
			(degree: 4, dur: 0.5, pan: -1),
			(degree: 6, dur: 1.0, pan: -1),
			(degree: 0, dur: 1.0, pan: -1),
			(degree: 2, dur: 1.0, pan: -1),
			(degree: 4, dur: 0.5, pan: -1),
			(degree: 6, dur: 0.5, pan: -1),
			nil
		];
		this.assertEquals(results, expected, "mul add set stutter");

		p = Pseed(Pn(175,1), Pbind(\degree, Pseq((0..3), 2), \dur, 0.5).rarely(_.add(\degree, 7)));
		results = p.asStream.nextN(9, ());
		expected = [
			(degree: 0, dur: 0.5),
			(degree: 8, dur: 0.5),
			(degree: 9, dur: 0.5),
			(degree: 3, dur: 0.5),
			(degree: 0, dur: 0.5),
			(degree: 1, dur: 0.5),
			(degree: 2, dur: 0.5),
			(degree: 3, dur: 0.5),
			nil,
		];
		this.assertEquals(results, expected, "sometimesBy - rarely");

		p = Pseed(Pn(19,1), Pbind(\degree, Pseq((0..3), 2), \dur, 0.5).add(\degree, Pwrand([0, 7], [0.75, 0.25], inf)));
		results = p.asStream.nextN(9, ());
		expected = [
			(degree: 0, dur: 0.5),
			(degree: 8, dur: 0.5),
			(degree: 2, dur: 0.5),
			(degree: 10, dur: 0.5),
			(degree: 0, dur: 0.5),
			(degree: 8, dur: 0.5),
			(degree: 2, dur: 0.5),
			(degree: 3, dur: 0.5),
			nil
		];
		this.assertEquals(results, expected, "add random degree");
	}

	test_arp {
		var b = Bacalao();
		var dur = 1/16;
		var p = Arp.up([0,2,4,7], 1, dur);
		var expected = [
			(note: 0, dur: dur),
			(note: 2, dur: dur),
			(note: 4, dur: dur),
			(note: 7, dur: dur),
			(note: 12, dur: dur),
			(note: 14, dur: dur),
			(note: 16, dur: dur),
			(note: 19, dur: dur),
			nil
		];
		var results = p.asStream.nextN(expected.size, ());
		this.assertEquals(results, expected, "Arp.up");

		dur = 1/8;
		p = Arp.down([1, -3, 4], 2, dur);
		expected = [
			(note: 4+24, dur: dur),
			(note: 1+24, dur: dur),
			(note: -3+24, dur: dur),
			(note: 4+12, dur: dur),
			(note: 1+12, dur: dur),
			(note: -3+12, dur: dur),
			(note: 4, dur: dur),
			(note: 1, dur: dur),
			(note: -3, dur: dur),
			nil
		];
		results = p.asStream.nextN(expected.size, ());
		this.assertEquals(results, expected, "Arp.down");

		dur = 1/4;
		p = Arp.order([1, -3, 4], 3, dur);
		expected = [
			(note: 1, dur: dur),
			(note: -3, dur: dur),
			(note: 4, dur: dur),
			(note: 1+12, dur: dur),
			(note: -3+12, dur: dur),
			(note: 4+12, dur: dur),
			(note: 1+24, dur: dur),
			(note: -3+24, dur: dur),
			(note: 4+24, dur: dur),
			(note: 1+36, dur: dur),
			(note: -3+36, dur: dur),
			(note: 4+36, dur: dur),
			nil
		];
		results = p.asStream.nextN(expected.size, ());
		this.assertEquals(results, expected, "Arp.order");

		p = Arp.rev([1, -3, 4], 3, dur);
		expected = expected.drop(-1).reverse ++ nil;
		results = p.asStream.nextN(expected.size, ());
		this.assertEquals(results, expected, "Arp.rev");

		dur = 1/24;
		p = Arp.incl([1, -3, 6], 1, dur, \degree);
		expected = [
			(degree: -3, dur: dur),
			(degree: 1, dur: dur),
			(degree: 6, dur: dur),
			(degree: -3+7, dur: dur),
			(degree: 1+7, dur: dur),
			(degree: 6+7, dur: dur),
		];
		expected = expected.mirror2 ++ nil;
		results = p.asStream.nextN(expected.size, ());
		this.assertEquals(results, expected, "Arp.incl");

		p = Arp.excl([1, -3, 6], 1, dur, \degree);
		expected = expected.keep(6).mirror1 ++ nil;
		results = p.asStream.nextN(expected.size, ());
		this.assertEquals(results, expected, "Arp.excl");

		p = Pseed(1357, Arp.rand([110,130], 2, dur, \freq)).keep(8);
		expected = [
			(freq: 260, dur: dur),
			(freq: 110, dur: dur),
			(freq: 110, dur: dur),
			(freq: 440, dur: dur),
			(freq: 440, dur: dur),
			(freq: 220, dur: dur),
			(freq: 110, dur: dur),
			(freq: 520, dur: dur),
			nil
		];
		results = p.asStream.nextN(expected.size, ());
		this.assertEquals(results, expected, "Arp.rand");

		~notes = [1,4,7,10]; // we will change these on the fly, as we stream
		p = Arp.up('notes', 1, dur);
		expected = [
			(note: 1, dur: dur),
			(note: 4, dur: dur),
			(note: 7, dur: dur),
			(note: 10, dur: dur),
			(note: 4+12, dur: dur),
			(note: 6+12, dur: dur),
			(note: 9+12, dur: dur),
			(note: 11+12, dur: dur),
			nil
		];
		results = p.asStream;
		this.assertEquals(results.nextN(4, ()), expected.keep(4), "Arp.up (note lookup)");
		~notes = [11,4,6,9];
		this.assertEquals(results.nextN(expected.size-4, ()), expected.drop(4), "Arp.up (note lookup post change)");
	}

}