Bacalao {
	classvar parse;
	const numVstChannels = 2;
	const defaultVstPath = "C:/Program Files/Common Files/VST2";
	classvar <preProcessorVariables = 'bacalaoPreProcessorVariables';
	var <clock;
	var <server;
	var <>verbose = true;
	var <quant;
	var <numChannels = 2;
	// Collection of 2-element Arrays [VSTPluginController, NodeProxy], looked up by "name" (a Symbol)
	var <vstDict;
	// Dictionary of default Event properties per track
	// Note that using '*' as the trkName (equivalent to $*) will
	// set global defaults, shared by all trks.
	var <trkDefaults;
	var <spatial; // Settings for spatialization (BacalaoSpatialSettings)
	var <outputGroup; // The Group for the "output fx" Proxy
	var <barClock; // A clock derived from the main clock, one beat per bar (so we can schedule bar-length patterns without stretching)

	*initClass {
		parse = BacalaoParser;
	}

	*new { arg shareClock, server, verbose, quant, numChannels;
		var clock;

		case
		{ shareClock.class == Bacalao } { clock = shareClock.clock }
		{ shareClock.isKindOf(Clock) } { clock = shareClock }
		{ clock = TempoClock.default };

		numChannels = numChannels ? 2;
		clock.permanent = true;
		MIDIClient.init(verbose: false);

		Bacalao.prSetupSynthDefs();
		Bacalao.prSetupEventTypes();

		// Setting a longer buffer for recording is critical, else there are
		// glitches (in recording, not audible) when asynchronously loading
		// VST plugins. The recBufSize must be longer than the time it might
		// take to perform any asynchronous VST operation. Ten seconds is more
		// than enough in my usage.
		server = server ? Server.default;
		server.options.memSize = max(server.options.memSize, 128 * 1024);
		server.recorder.recBufSize = (server.sampleRate ?? { server.options.sampleRate ? 44100 } * 10).nextPowerOfTwo;
		server.options.numOutputBusChannels = max(server.options.numOutputBusChannels, numChannels);

		Bacalao.config();
		^super.newCopyArgs(clock, server, verbose ? true, quant, numChannels, (), (), nil).start.prSetupCmdPeriod.prSetupBarClock.push;
	}

	*config {
		{
			var baseChords = (
				i: [0,2,4].rotate(-2),     // I (major)
				ii: [1,3,5],    // ii (minor)
				iii: [2,4,6],   // iii (minor)
				iv: [3,5,7],    // IV (major)
				v: [4,6,8],   // V (major)
				vi: [5,7,9]-7,  // vi (minor)
				vii: [6,8,10]-7 // viio (diminished)
			);
			var chords = ();
			baseChords.keysValuesDo{ arg k, v;
				chords.put(k, [v, v.rotate(-1)+[0,0,7], v.rotate(-2)+[0,7,7]]);
				chords.put(k.asString.toUpper.asSymbol, [v, v.rotate(-1)+[0,0,7], v.rotate(-2)+[0,7,7]] - 7);
			};
			Bacalao.varSet('chords', chords);
		}.value;

		{
			var freq = ();
			this.setDictChars(freq, $a, $z, (_+60).midicps);
			this.setDictChars(freq, $A, $Z, (_+36).midicps);
			this.setDictChars(freq, $0, $9, (_+60).midicps);
			Bacalao.varSet('freq', freq);
		}.value;
		{
			var amp = ();
			this.setDictChars(amp, $a, $z, _.lincurve(0,25, 0,1, 1));
			this.setDictChars(amp, $A, $Z, _.lincurve(0,25, 0,1, -1));
			this.setDictChars(amp, $0, $9, _.lincurve(0,9, 0,1, 0));
			Bacalao.varSet('amp', amp);
		}.value;
		{
			var pan = ();
			this.setDictChars(pan, $a, $z, _.linlin(0,25, -1,1));
			this.setDictChars(pan, $A, $Z, _.linlin(0,25, -1,1).cubed);
			this.setDictChars(pan, $0, $9, _.linlin(0,9, -1,1));
			Bacalao.varSet('pan', pan);
		}.value;

		{
			// ~kb variable is a QWERTY MIDI keyboard
			var kb = ();
			// Window().front.view.keyDownAction_{ arg view, char, modifiers, unicode, keycode, key;
			// 	char.debug("char");
			// 	modifiers.debug("modifiers");
			// 	unicode.debug("unicode");
			// 	keycode.debug("keycode");
			// 	key.debug("key");
			// }
			// Bottom row
			"zsxdcvgbhnjm".do{ arg ch, i;
				kb.put(ch.toUpper.asSymbol, i + 36);
				kb.put(ch.asSymbol, i + 48);
			};
			"q2w3er5t6y7ui9o0p".do{ arg ch, i;
				kb.put(ch.toUpper.asSymbol, i + 72);
				kb.put(ch.asSymbol, i + 60);
			};
			// The following will be keyboard-layout dependent
			kb.put('@', 73); // modified 2 ('@' us-en)
			kb.put('#', 75); // modified 3 ('#' us-en)
			kb.put('%', 78); // modified 5 ('%' us-en)
			kb.put('&', 80); // modified 6 ('^' us-en)
			kb.put('/', 82); // modified 7 ('&' us-en)
			kb.put(')', 85); // modified 9 ('(' us-en)
			kb.put('=', 87); // modified 0 (')' us-en)
			Bacalao.varSet('kb', kb);
		}.value;

		{
			// ~batt variable is set up for Battery 4
			//  (array of 4x4 notes starting at MIDI 36, with octave arrays)
			var batt = ();
			"1234qwerasdfzxcv".do{ arg ch, i;
				batt.put(ch.asSymbol, i + 36 + ((0..5)*12));
			};
			// Also support columns 5-12 with octave arrays (rows via :index)
			(5..12).do{ arg val;
				batt.put(val.asSymbol, val + 35 + ((0..5)*12));
			};
			Bacalao.varSet('batt', batt);
		}.value;

		{
			// ~bat4 variable is set up for Battery 4
			//  (array of 4 rows (a-d) of 4 notes starting at MIDI 36)
			var bat4 = ();
			"abcd".do{ arg ch, row;
				(1..4).do { arg col;
					bat4.put((ch ++ col).asSymbol, 36 + (4 * row) + col - 1)
				}
			};
			Bacalao.varSet('bat4', bat4);
		}.value;

		{
			// ~bat12 variable is set up for Battery 4
			//  (array of 6 rows (a-f) of 12 notes starting at MIDI 36)
			var bat12 = ();
			"abcdef".do{ arg ch, row;
				(1..12).do { arg col;
					bat12.put((ch ++ col).asSymbol, 36 + (12 * row) + col - 1)
				}
			};
			Bacalao.varSet('bat12', bat12);
		}.value;

		{
			// ~mn variable is set up for named notes
			if (\ChordSymbol.asClass.notNil) {
				var mn = ();
				"cdefgab".do{ arg note;
					(0..8).do{ arg oct;
						var sym = (note ++ oct).asSymbol;
						mn.put(sym, sym.asNote);
						sym = (note ++ "b" ++ oct).asSymbol;
						mn.put(sym, sym.asNote);
						sym = (note ++ "s" ++ oct).asSymbol;
						mn.put(sym, sym.asNote);
					}
				};
				Bacalao.varSet('mn', mn);

				// Some aliases for chord names
				ChordSymbol.shapes.put('maj7', ChordSymbol.shapes.major7);
			}
		}.value;
	}

	*setDictChars { arg dict, start = $a, end = $z, remapFunc = { arg i, size; i.lincurve(0,size-1, 0, 1, 4) };
		var range = (start.ascii..end.ascii);
		range.do{ arg ch, i;
			dict.put(ch.asAscii.asSymbol, remapFunc.value(i, range.size))
		};
		^dict
	}

	*prVSTPluginInstalled {
		var available = \VSTPlugin.asClass.notNil;
		if (available.not) {
			"VST functionality is not available\n==========\n"
			"  To use VSTs, you must install the VSTPlugin extension.\n"
			"  Download the latest from: https://git.iem.at/pd/vstplugin/-/releases\n"
			"  and extract it to your \"Platform.userExtensionDir\"\n==========".warn
		};
		^available
	}

	*prSetupSynthDefs {
		var playFunc;
		if (this.prVSTPluginInstalled) {
			SynthDef(\bacalao_vsti, {arg out = 0, pan = 0, gate=1, fadeTime=1, bypass=0;
				// VST instruments usually don't have inputs
				var vsti = VSTPlugin.ar(nil, numVstChannels, bypass: bypass);
				var env = EnvGen.kr(Env.asr(fadeTime,1,fadeTime), gate, doneAction: Done.freeSelf);
				Out.ar(out, Balance2.ar(vsti.first, vsti.last, pan) * env);
			}).add;
		};

		playFunc = { arg numChannels, withGate;
			var freq = \freq.kr(60.midicps);
			var buf = \buf.kr(0);
			var rate = \rate.kr(1) * (freq / 60.midicps);
			var start = \start.kr(0);
			var length = \length.kr(1);
			var sig = PlayBuf.ar(numChannels, buf, rate * BufRateScale.kr(buf), 1, start * BufSampleRate.kr(buf), 1);
			var fade = 0.01;
			var timeToEnd = (BufDur.kr(buf) - start - fade).max(0);
			var segmentDur = min(length / rate.abs, timeToEnd);
			var env = if (withGate) {
				EnvGen.ar(Env.linen(fade, (segmentDur - fade).max(0), fade), doneAction: Done.none) *
				EnvGen.ar(Env.asr(fade, 1, fade), \gate.kr(1), doneAction: Done.freeSelf);
			} {
				EnvGen.ar(Env.linen(fade, (segmentDur - fade).max(0), fade), doneAction: Done.freeSelf);
			};
			sig * env;
		};

		SynthDef(\sample1, { arg out=0, amp=0.1, pan=0;
			var sig = playFunc.(1, /* withGate */ false);
			Out.ar(out, Pan2.ar(sig, pan, amp));
		}).add;

		SynthDef(\sample1_gate, { arg out=0, amp=0.1, pan=0;
			var sig = playFunc.(1, /* withGate */ true);
			Out.ar(out, Pan2.ar(sig, pan, amp));
		}).add;

		SynthDef(\sample2, { arg out=0, amp=0.1, pan=0;
			var sig = playFunc.(2, /* withGate */ false);
			Out.ar(out, Balance2.ar(sig.first, sig.last, pan, amp));
		}).add;

		SynthDef(\sample2_gate, { arg out=0, amp=0.1, pan=0;
			var sig = playFunc.(2, /* withGate */ true);
			Out.ar(out, Balance2.ar(sig.first, sig.last, pan, amp));
		}).add;

		this.prSetupInstrumentSynths;

		Safety.addSynthDefFunc(\safeLimitNotify, { |numChans, limit=1|
			{
				var limitCtl = \limit.kr(limit).abs;
				var mainOuts = In.ar(0, numChans);
				var safeOuts = ReplaceBadValues.ar(mainOuts);
				// Could also do HPF (20), Compander (0.7,1.05,0.4,0.01,0.01), BHiShelf (2400,1,3)

				var resetTrig = Impulse.ar(0.5);
				var peak = Peak.ar(safeOuts * (1 - resetTrig), resetTrig);
				var maxPeak = peak.reduce(\max); //.poll(1, "max");
				var delayMaxPeak = Delay1.ar(maxPeak);
				var overTrig = delayMaxPeak > limitCtl;
				var limited = Limiter.ar(safeOuts, limitCtl); // safeOuts.clip2(limitCtl);
				(delayMaxPeak / limitCtl).ampdb.round(0.1).poll((delayMaxPeak > maxPeak) * overTrig, "Audio peak exceeded limit by dB");
				ReplaceOut.ar(0, limited);
			}
		});
		// Apply a limiter to prevent clipping if we go above amplitude 1
		Safety.defaultDefName = \safeLimitNotify;
		Safety.all.do(_.defName = \safeLimitNotify);
	}

	// We set up special Event types for playing VST and regular note patterns,
	// which allow combined note and parameter setting in a single Event
	*prSetupEventTypes {
		var handleProxySetParams = {
			var setPrefix = "p_";
			var setParams = currentEnvironment.select{ arg v, k;
				k.asString.beginsWith(setPrefix)
			};
			if (setParams.notEmpty) {
				var setEvent = Event.default.copy;
				var params = [];
				var np = ~proxy;
				setParams.keysValuesDo{ arg k, v;
					var param = k.asString.drop(setPrefix.size).asSymbol;
					params = params.add(param);
					currentEnvironment[k] = nil; // remove param setting from main Event
					setEvent[param] = v.value;
				};
				if (np.isKindOf(NodeProxy)) {
					setEvent.push;
					np.set(*np.controlNames.collect(_.name).envirPairs.asOSCArgArray);
					setEvent.pop;
				} {
					"\\proxy key must be set".warn
				}
			}
		};

		var handleVstSetParams = { arg server;
			var setPrefix = "v_";
			var setEventType = \vst_set;
			var setParams = currentEnvironment.select{ arg v, k;
				k.asString.beginsWith(setPrefix)
			};
			if (setParams.notEmpty) {
				var setEvent = Event.default.copy;
				var params = [];
				setParams.keysValuesDo{ arg k, v;
					var paramNumOrName = k.asString.drop(setPrefix.size);
					var param = if (paramNumOrName[0].isDecDigit) {
						// \vst_set wants numbered parameters to be integer keys
						paramNumOrName.asInteger
					} {
						paramNumOrName.asSymbol
					};
					params = params.add(param);
					currentEnvironment[k] = nil; // remove param setting from main Event
					setEvent[param] = v.value;
				};
				setEvent[\type] = setEventType;
				setEvent[\vst] = ~vst;
				setEvent[\params] = params;
				setEvent.push;
				Event.eventTypes[setEventType].value(server);
				// setEvent.play;
				setEvent.pop;
			}
		};

		Event.addEventType(\vst_midi_and_pset, { |server|
			if (~vst.notNil) {
				handleVstSetParams.(server);
			};
			handleProxySetParams.();

			~type = \vst_midi;
			Event.eventTypes[\vst_midi].value(server);
			// currentEnvironment.play;
		});
		Event.addEventType(\pset_vset_only, { |server|
			if (~vst.notNil) {
				handleVstSetParams.(server);
			};
			// Here we support setting proxy controls using the prefix...
			handleProxySetParams.();

			// ...and without (now that the "p_" params are removed)
			if (~proxy.notNil) {
				// Code from AbstractPlayControl.initClass in JITLib/ProxySpace/wrapForNodeProxy.sc
				var controlsToSet = ~proxy.controlNames.collect(_.name);
				if (~mask.isNil) {
					// No mask parameter -- don't set "normal" note Event keys like freq/amp/pan
					controlsToSet = controlsToSet.reject([\freq, \amp, \pan].includes(_));
				};
				~proxy.set(*controlsToSet.envirPairs.asOSCArgArray)
			};
			// Don't play any note...
		});
		Event.addEventType(\note_and_pset, { |server|
			handleProxySetParams.();

			if (~drum.notNil) {
				~type = \drum;
			} {
				~type = \note;
			};
			Event.eventTypes[~type].value(server);
			// currentEnvironment.play;
		});
	}

	// This sets up automatic substitutions (like variables) that may be used
	// by the preProcessor.
	// These may be single values, but they may also be arrays or Dictionaries,
	// in which case the variable:key syntax may be used to access them. This
	// lets you define multiple values for a variable, like:
	//   b.varSet('bd', [36, 37, 48, 49]);
	//   note"bd:3" --> would produce Pbind(\note, 49)
	*varSet { arg key, values;
		this.vars[key.asSymbol] = values;
	}

	varSet { arg key, values;
		Bacalao.varSet(key, values);
	}

	// This allows the key alone or with a colon to sub-index (e.g. 'bd:3')
	// (See also PnsymRest.prVarLookup).
	// Note: does not return the variable itself, but a separate object.
	//       If you need the "raw" variable, use b.vars[key]
	*varLookup { arg key, dict;
		var parserVariables = dict ?? { this.vars };
		var elem = key.asString;
		^parserVariables !? {
			var variable, index, substitute;
			// Allow (e.g.) "bd:5"-style indexing, or simply "bd" (equivalent to "bd:0")
			// Also allow "bd:r", which will choose a random value from the collection.
			#variable, index = elem.split($:);
			substitute = parserVariables[variable.asSymbol];
			substitute !? {
				if (index.notNil and: { substitute.isSequenceableCollection }) {
					if (index == "r" and: { substitute.size > 1 }) {
						Prand(substitute.asArray,inf)
					} {
						substitute.asArray.wrapAt(index.asInteger)
					}
				} {
					if (substitute.isKindOf(Routine)) {
						substitute.value // take the next value here, not first
					} {
						substitute.first
					}
				}
			}
		}
	}

	boot {
		server.boot;
	}

	varLookup { arg key, dict;
		// dict may be nil (will use Bacalao vars)
		^Bacalao.varLookup(key, dict);
	}

	*varPrint {
		var parserVariables = this.vars;
		var buffers = ();
		var index = 0;
		"----- variables:".post;
		parserVariables.sortedKeysValuesDo{ arg k, v;
			if (v.isKindOf(Buffer) or: { v.first.isKindOf(Buffer) }) {
				buffers[k] = v;
			} {
				var suffix = if (v.size > 0) {
					if (v.isKindOf(SequenceableCollection)) {
						// Show number of elements in Array or similar
						":% ".format(v.size)
					} {
						// Show the number of keys in Dictionary/Event
						"(%) ".format(v.size)
					}
				} {
					" "
				};
				if (index.mod(8) == 0) { "".postln };
				(k.asString ++ suffix).post;
				index = index + 1;
			}
		};
		"\n----- buffers:".post;
		buffers.sortedKeysValuesDo{ arg k, v, index;
			var suffix = if (v.size > 0) { ":% ".format(v.size) } { " " };
			if (index.mod(8) == 0) { "".postln };
			(k.asString ++ suffix).post;
		};
		"\n".postln;
	}

	varPrint {
		Bacalao.varPrint();
	}

	*vars {
		if (topEnvironment[preProcessorVariables].isKindOf(Environment).not) {
			"Defining new parser variable Environment".postln;
			topEnvironment[preProcessorVariables] = Environment();
		};
		^topEnvironment[preProcessorVariables]
	}

	vars { ^Bacalao.vars }

	push {
		var env = this.vars;
		if (currentEnvironment != env) {
			env.push;
		}
	}

	pop {
		var env = this.vars;
		if (currentEnvironment == env) {
			env.pop;
		}
	}

	prMakeVariableName { arg path, name;
		name = (name ?? { path.asPathName.fileNameWithoutExtension });
		name = name.asString.select{ arg ch; (($A.ascii..$Z.ascii) ++ ($a.ascii..$z.ascii) ++ ($0.ascii..$9.ascii) ++ $_).asAscii.includes(ch) };
		name[0] = name[0].toLower;
		^name.asSymbol
	}

	// Load a directory of audio files into a variable in the ~samp Dictionary.
	// These sample instruments can then be played using:
	//   b.loadSamples("/path/to/bd")
	//   b.p(1, @~samp"bd bd:2")
	loadSamples { arg dirPath, name, replace = true;
		var bufs;
		var dict = (this.vars[\samp] ?? {
			this.vars[\samp] = ();
			this.vars[\samp]
		});
		name = this.prMakeVariableName(dirPath, name);
		if (dict[name].isNil) {
			"Creating ~samp key: '%'".format(name).postln;
			dict[name] = [];
		} {
			if (replace) {
				"Replacing ~samp key: '%'".format(name).warn;
				dict[name] = [];
			} {
				"Adding to ~samp key: '%'".format(name).postln
			}
		};
		bufs = (dirPath ++ "/*.wav").pathMatch.collect{ arg path;
			Buffer.read(server, path, action: { arg buf;
				"index %: loaded %".format(dict[name].size, buf).postln;
				dict[name] = dict[name].add(
					(buf: buf, start: 0, length: buf.duration, instrument: ('sample' ++ buf.numChannels).asSymbol)
				)
			})
		};
		if (bufs.isEmpty) {
			// Maybe it was just a standalone file, not a directory
			Buffer.read(server, dirPath, action: { arg buf;
				"index: % loaded %".format(dict[name].size, buf).postln;
				dict[name] = dict[name].add(
					(buf: buf, start: 0, length: buf.duration, instrument: ('sample' ++ buf.numChannels).asSymbol)
				)
			})
		};
	}

	// Load a single Buffer into a variable.
	// These buffers can then be played/looped using b.chop:
	//   b.loadBuffer("/path/to/mySample.wav")
	//   b.p(1, b.chop(~mySample))
	loadBuffer { arg filePath, name, replace = true;
		name = this.prMakeVariableName(filePath, name);
		if (this.vars[name].notNil) {
			if (replace) {
				"Replacing buffer variable ~%".format(name).warn
			} {
				"Variable ~% exists...skipping loadBuffer".format(name).warn
				^this
			}
		} {
			"Creating buffer variable ~%".format(name).postln
		};
		this.vars[name] = Buffer.read(server, filePath, action: { arg buf;
			"loaded %".format(buf).postln;
		});
	}

	prSetupCmdPeriod {
		CmdPeriod.add {
			vstDict.keysValuesDo{ arg k, dictEntry;
				var vst, proxy;
				"Freeing VST instrument '%'".format(k).postln;
				#vst, proxy = dictEntry;
				proxy.clear;
				vst.close;
			};
			vstDict.clear;

			this.spatialFree(playDefault: false);
		}
	}

	prResetBarClockTempo {
		barClock.tempo = clock.tempo / clock.beatsPerBar;
	}

	prSyncBarClockMeterAndBeats {
		barClock.schedAbs(barClock.beats.ceil, {
			// Resetting meter
			barClock.beatsPerBar = 1;
			barClock.beats = barClock.bars2beats(clock.beats2bars(clock.beats));
		});
	}

	prSyncBarClock {
		clock.schedAbs(clock.nextBar, {
			this.prResetBarClockTempo;
			if (barClock.beatsPerBar != 1) {
				this.prSyncBarClockMeterAndBeats;
			} {
				barClock.beats = barClock.bars2beats(clock.beats2bars(clock.beats));
			}
		});
	}

	prSetupBarClock {
		clock.schedAbs(clock.nextBar, {
			var tempoControllers;
			barClock = TempoClock(clock.tempo / clock.beatsPerBar).permanent_(true);
			this.prSyncBarClockMeterAndBeats;
			tempoControllers = [
				SimpleController(clock).put(\tempo,
					{ this.prResetBarClockTempo }),
				SimpleController(clock).put(\meter,
					{ this.prResetBarClockTempo })
			];
		});
	}

	*start {
		thisProcess.interpreter.preProcessor = BacalaoParser.preProcess(_);
		"Bacalao pattern parsing enabled".postln;
	}

	start {
		Bacalao.start;
	}

	*stop {
		thisProcess.interpreter.preProcessor = nil;
		"Bacalao pattern parsing disabled".postln;
	}

	stop {
		Bacalao.stop;
	}

	tempo {
		^clock.tempo;
	}

	tempo_ { arg t, setVstTempo = true;
		clock.tempo = t;
		if (setVstTempo) {
			vstDict.do{ arg entry; entry.first.setTempo(t * 60) }
		}
	}

	prGetNamesAndProxies {
		^vstDict.collect(_.last) ++ (Ndef.all[server.name] ?? ());
	}

	trks {
		var namesAndProxies = this.prGetNamesAndProxies;
		^namesAndProxies.asKeyValuePairs.clump(2).select{ arg kv;
			kv[1].numChannels.notNil
			and: { kv[0] != '*' }
			and: { kv[0] != \multibandEq }
		}.collect(_.first)
	}

	gui {
		var w = Window("Bacalao GUI", scroll: true).bounds_(Rect(1310, 100, 450, 900)).front.alwaysOnTop_(true);
		var namesAndProxies = this.prGetNamesAndProxies;
		w.addFlowLayout;
		w.view.hasHorizontalScroller_(false).background_(Color(0.2,0.2,0.4));
		namesAndProxies.sortedKeysValuesDo{ arg name, nd;
			if (nd.numChannels.notNil) {
				StaticText(w, 50@20).string_(name);
				NdefGui(nd, nd.controlKeys.size.max(4), w, options: NdefGui.big).nameView(name)
			}
		};
		^w;
	}

	dropGui {
		var h = HLayout();
		var win = Window("Sample drop", Rect(400, 400, 600,200)).front.layout_(h);
		var sampleName = TextField();
		var bufferName = TextField();
		var replaceSample = CheckBox(text: "replace").value_(true);
		var replaceBuffer = CheckBox(text: "replace").value_(true);
		var vl = VLayout(StaticText().string_("Samples (in ~samp dictionary)"), HLayout(sampleName, replaceSample));
		var vr = VLayout(StaticText().string_("Buffers"), HLayout(bufferName, replaceBuffer));
		var validateName = { arg name, path;
			if (name.isEmpty)
			{
				this.prMakeVariableName(path)
			} {
				this.prMakeVariableName(name)
			}
		};
		vl.add(DragSink().minSize_(400@100).canReceiveDragHandler_{ arg view;
			View.currentDrag.isString
		}.receiveDragHandler_{ arg view;
			var fileName = View.currentDrag;
			var name = validateName.(sampleName.string, fileName);
			view.object = "'%'\n(as ~samp.%)".format(fileName.asPathName.fileNameWithoutExtension, name);
			this.loadSamples(fileName, name, replaceSample.value);
		});
		h.add(vl);

		vr.add(DragSink().minSize_(400@100).canReceiveDragHandler_{ arg view;
			View.currentDrag.isString
		}.receiveDragHandler_{ arg view;
			var fileName = View.currentDrag;
			var name = validateName.(bufferName.string, fileName);
			view.object = "'%'\n(as ~%)".format(fileName.asPathName.fileNameWithoutExtension, name);
			this.loadBuffer(fileName, name, replaceBuffer.value);
		});
		h.add(vr);
		^win
	}

	defGet { arg trkName;
		var dict = trkDefaults[trkName = trkName.asSymbol];
		dict ?? {
			dict = ();
			trkDefaults[trkName] = dict;
		}
		^dict
	}

	defAdd { arg trkName, patternPairs;
		var dict = this.defGet(trkName);
		if (patternPairs.notNil) {
			dict.putAll(patternPairs);
		};
		^dict
	}

	defSet { arg trkName, patternPairs;
		var dict = this.defGet(trkName);
		dict.clear();
		if (patternPairs.notNil) {
			dict.putAll(patternPairs);
		};
		^dict
	}

	// Play a pattern once on the named NodeProxy.
	once { arg trkName, pattern, quant;
		var slot = nil;
		if (trkName.isKindOf(Association)) {
			#trkName, slot = [trkName.key.asSymbol, trkName.value];
		};
		if (pattern.isKindOf(Pattern)) {
			this.prChangePattern(trkName, slot, pattern, nil, quant);
		} {
			this.prSetSource(trkName, slot, pattern, quant);
		};
	}

	// Could be considered "pattern" or "play"... Set a looping pattern playing
	// on the named NodeProxy.
	// Duration of 0 means use "natural" loop time, whatever the pattern produces
	// Durations > 0 will truncate or extend the pattern as needed to produce
	// exactly the requested duration. By default (if quant is unspecified) the
	// quantization will be a multiple of the duration ([dur, 0]).
	// When duration is unspecified (nil), we attempt to determine the total duration
	// of the pattern passed in.
	// @todo We might want a way to fast-forward when playing a pattern with
	// long duration, see:
	//   https://sc-users.bham.ac.narkive.com/IUvoCSGV/fast-forwarding-a-pattern
	p { arg trkName, pattern, dur, quant, role, includeMask = true;
		var slot;
		if (trkName.isKindOf(Association)) {
			#trkName, slot = [trkName.key.asSymbol, trkName.value];
		};
		if (pattern.isKindOf(Pattern)) {
			if (dur.isNil) {
				pattern.getDur.debug("pattern duration");
				// If we have a very long pattern, we don't want to set the
				// quant to be that long by default, because it will take
				// too long for the new pattern to take effect, so just use
				// the default "natural" loop time.
				// (Tried things like: dur = dur.round.asInt.factors.minItem)
				dur = 0;
			};
			this.prChangePattern(trkName, slot, pattern, dur, quant, role, includeMask);
		} {
			this.prSetSource(trkName, slot, pattern, quant);
		};
	}

	prGetControlValue { arg proxy, controlName;
		var value = proxy.nodeMap[controlName];
		if (value.notNil) {
			^value.value
		} {
			// There is no set value for the control,
			// so see if there is some SynthDef that
			// uses it as a control, and if there's only
			// one (or all have the same value), return
			// that as the "current" value. This will
			// conflict if there are several different
			// Synths with different defaults for the param.
			var control;
			var obj = proxy.objects.detect{ arg obj;
				case
				{ obj.isKindOf(SynthDefControl) } {
					control = obj.synthDef.allControlNames.detect{ arg c; c.name == controlName };
					control.notNil
				}
				{ obj.isKindOf(SynthControl) and: { obj.isKindOf(VSTSynthControl).not } } {
					control = SynthDescLib.global.at(obj.source).controls.detect{ arg c; c.name == controlName };
					control.notNil
				}
				{
					false
				}
			};
			^control !? { control.defaultValue }
		}
	}

	get { arg trkName, controlName, valueOrFunc, fadeTime = 2;
		var np = this.proxy(trkName);
		^np !? { this.prGetControlValue(np, controlName) }
	}

	set { arg trkName, controlName, valueOrFunc, fadeTime = 2;
		var np = this.proxy(trkName);
		var oldControl = np.nodeMap[controlName];
		case
		{ valueOrFunc.isNil } {
			// Clear an existing "set"
			if (oldControl.isKindOf(NodeProxy)) {
				"Unmapping old mapped control".postln;
				np.unmap(controlName);
			} {
				"Unsetting control".postln;
				np.unset(controlName);
				// Not sure why we need to do this twice here, but we do...
				np.unset(controlName);
			}
		}
		{ valueOrFunc.isKindOf(Function) } {
			var value = NodeProxy(server).source_(valueOrFunc);
			// var oldFadeTime = np.fadeTime;
			// np.fadeTime = fadeTime;
			np.set(controlName, value);
			// np.fadeTime = oldFadeTime;
		}
		{ valueOrFunc.isKindOf(Ndef) } {
			np.set(controlName, valueOrFunc);
		}
		{
			// Default case, for simple values
			var targetValue = valueOrFunc;
			var startValue = this.prGetControlValue(np, controlName);
			if (startValue.isNil or: { targetValue.equalWithPrecision(startValue, 1e-4, 0.02) } or: { fadeTime <= 0 }) {
				"Setting from % to %".format(startValue, targetValue).postln;
				np.set(controlName, targetValue)
			} {
				"Setting from % to % over % seconds".format(startValue, targetValue, fadeTime).postln;
				this.prFade(
					{ arg frac; np.set(controlName, frac.linlin(0, 1, startValue, targetValue)) },
					{ np.set(controlName, targetValue) },
					fadeTime);
			}
		};
		// If it was a NodeProxy (but not Ndef), assume we created it
		// and free it here, after a delay. If you want to play with
		// setting your own NodeProxies, you should name them using Ndef,
		// then you're responsible for freeing them yourself.
		if (oldControl.isKindOf(NodeProxy) and: { oldControl.isKindOf(Ndef).not }) {
			"Removing old control".postln;
			clock.sched(server.latency + fadeTime + 0.5,{ oldControl.free; nil });
		}
	}

	vget { arg trkName, vstControlName;
		var vst = this.vst(trkName);
		if (vst.notNil) {
			vst.get(vstControlName, _.postln)
		} {
			Error("'%' is not a VST track".format(trkName)).throw
		}
	}

	vset { arg trkName, controlName, value, fadeTime = 2;
		var vst = this.vst(trkName);
		if (vst.notNil) {
			vst.get(controlName, { arg oldValue;
				// Fade to new value
				var targetValue = value;
				var startValue = oldValue;
				if (targetValue.equalWithPrecision(startValue, 1e-4, 0.02) or: { fadeTime <= 0 }) {
					"Setting from % to %".format(startValue, targetValue).postln;
					vst.set(controlName, targetValue)
				} {
					"Setting from % to % over % seconds".format(startValue, targetValue, fadeTime).postln;
					this.prFade(
						{ arg frac; vst.set(controlName, frac.linlin(0, 1, startValue, targetValue)) },
						{ vst.set(controlName, targetValue) },
						fadeTime);
				}
			})
		} {
			Error("'%' is not a VST track".format(trkName)).throw
		}
	}

	// This is a general "set pattern" method, which can support
	// setting VST params or NodeProxy controls, without trying
	// to produce any kind of note event.
	// It expects Proxy control names to be prefixed with 'p_',
	// and VST param names to be prefixed with 'v_'.
	pset { arg trkName, pattern, dur=0, quant, includeMask = false;
		// \pset_vset_only is not a real Proxy role, but we use it here to
		// flag our special handling for a pattern that only sets Proxy
		// and VST parameters
		this.p(trkName, pattern, dur, quant, \pset_vset_only, includeMask: includeMask);
	}

	// The following two aren't particularly useful, given that you can
	// just call p() with the desired role (\pset or \xset).
	psetRole { arg trkName, pattern, dur=0, quant;
		this.p(trkName, pattern, dur, quant, \pset);
	}

	xsetRole { arg trkName, pattern, dur=0, quant;
		this.p(trkName, pattern, dur, quant, \xset);
	}

	hush { arg fadeTime = 1;
		this.clear(this.trks, fadeTime)
	}

	prFade { arg setFunc, endFunc, fadeTime = 2, timeStep = 0.15;
		var startTime = SystemClock.beats;
		if (setFunc.isKindOf(Function).not) { ^this };
		SystemClock.sched(0.01, { arg time;
			var delta = time - startTime;
			var fraction = delta / fadeTime;
			if (delta > fadeTime) {
				endFunc.value(1.0);
				nil
			} {
				setFunc.value(fraction);
				timeStep
			}
		});
	}

	// Set the playback volume of the playing NodeProxy, with optional fadeTime.
	// e.g. b.db('drum', -12);
	db { arg trkName, db = 0, fadeTime = 2;
		var np = this.proxy(trkName);
		if (fadeTime > 0 and: { db.dbamp.equalWithPrecision(np.vol, 1e-4, 0.02).not } ) {
			var startDb = np.vol.ampdb.max(-120);
			var targetDb = db.max(-120);
			"dB from % to % over % seconds".format(startDb, targetDb, fadeTime).postln;
			this.prFade(
				{ arg frac; np.vol = frac.linlin(0, 1, startDb, targetDb).dbamp },
				{ np.vol = db.dbamp },
				fadeTime);
		} {
			np.vol = db.dbamp;
		}
	}

	sched { arg event, quant = 1;
		barClock.schedAbs(quant.nextTimeOnGrid(barClock),
			{ arg ...args;
				event.value(*args);
				nil
			}
		);
	}

	// Add a filter (needs array index > 0) to modify the NodeProxy
	// e.g. b.fx(\drum -> 3, { arg in; JPverb.ar(in, 3) }, 0.3);
	// You can clear fx in a slot with: b.fx(\drum -> 3);
	fx { arg trkNameAndIndex, filterFunc, wet=1;
		var trkName, index;
		if (trkNameAndIndex.isKindOf(Association).not) {
			"trkName and index should be an Association, e.g: b.fx(\\drum -> 10, ...)".error;
			^this;
		};

		#trkName, index = [trkNameAndIndex.key.asSymbol, trkNameAndIndex.value];
		if (index.isInteger and: {index > 0}) {
			var np = this.proxy(trkName);
			var wetParam = (\wet.asString ++ index).asSymbol;

			// Make it into a audio NodeProxy if it's currently empty
			np.numChannels ?? { np.source = { Silence.ar ! numChannels } };
			if (np[index].isNil) {
				// Start the thing quiet initially, set the wet at the next quant
				np.set(wetParam, 0);
			};
			np[index] = filterFunc !? {\filter -> filterFunc};
			this.sched({
				np.set(wetParam, wet);
			}, 1);
		} {
			"fx index should be an integer greater than 0...skipping".warn;
		}
	}

	fxClear { arg trkName, fadeTime = 1;
		var np = this.proxy(trkName = trkName.asSymbol);
		var oldFadeTime = np.fadeTime;
		np.fadeTime = fadeTime;
		np.objects.indices.copy.do{ arg slot;
			if (slot > 0) {
				np[slot] = nil;
			}
		};
		{
			np.fadeTime = oldFadeTime;
		}.defer(0.1)
	}

	// Set all slot effects to dry (with optional fadeTime), without removing them
	fxDry { arg trkName, fadeTime = 0;
		var np = this.proxy(trkName = trkName.asSymbol);
		np.objects.indices.do{ arg slot;
			if (slot > 0) {
				var key = ("wet" ++ slot).asSymbol;
				this.set(trkName, key, 0, fadeTime);
			}
		}
	}

	// Chop a Buffer into a number of pieces, returning a Pbind with the appropriate
	// start, length and rate parameters (appropriate for use with Bacalao patterns).
	// If you leave desiredBars and/or desiredRate unspecified, values will be
	// calculated to round to the nearest bar/cycle.
	chop { arg bufOrName, pieces = 8, desiredBars, desiredRate, inst;
		var buf = if (bufOrName.isKindOf(Buffer)) {
			bufOrName
		} {
			var lookup = Bacalao.varLookup(bufOrName);
			if (lookup.isKindOf(Buffer).not) { Error("chop variable lookup didn't find a Buffer").throw };
			lookup
		};
		var barDur = clock.beatsPerBar / clock.tempo;
		var bars = (desiredBars ?? { (buf.duration / barDur).max(0.5).round.debug("chop calculated bars") }).max(0.1);
		var origGrainDur = buf.duration / (pieces = pieces.max(1));
		var starts = (0..pieces-1) * origGrainDur;
		var cycleDur = bars * barDur;
		var rate = desiredRate ?? { (buf.duration / cycleDur).debug("chop calculated rate") };
		inst = inst ?? { switch (buf.numChannels,
			1, { \sample1 },
			2, { \sample2 },
			{ \sample2 })
		};
		// Here, the 'dur' should be in cycles, not beats
		^Pbind(\instrument, inst, \buf, buf, \length, origGrainDur, \rate, rate, \start, Pseq(starts), \dur, (1/pieces), \stretch, bars);
	}

	rhythm { arg hits, stepsPerBar = 8, bars = 1, ampBase = 0.5, timeKey = \dur;
		var totalBeats = stepsPerBar * bars;
		var hitsAndAmps, amps, startTimes, deltaTimes;
		hitsAndAmps = [[0, \rest]] ++ hits.collect{ |h|
			if (h.isKindOf(Emphasis)) {
				[ h.value - 1 % totalBeats, h.emphasis * ampBase.value ]
			} {
				[h - 1 % totalBeats, ampBase.value]
			};
		}.sort{ |a, b| a[0] < b[0] };
		#hits, amps = hitsAndAmps.debug("hitsAndAmps").flop;
		startTimes = hits / stepsPerBar ++ bars;
		deltaTimes = startTimes.differentiate.drop(1);
		^Pbind(timeKey, Pseq(deltaTimes.debug("deltaTimes")),
			\amp, Pseq(amps.debug("amps")));
	}

	prVstPrint { arg message, conditionFunc, onlyWithPresets, extraVstPluginSearchPath;
		if (Bacalao.prVSTPluginInstalled.not) { ^this };
		server.waitForBoot{
			if (VSTPlugin.plugins.isEmpty) {
				VSTPlugin.search(server, extraVstPluginSearchPath);
				server.sync;
			};
			"\n% (%)".format(message, if(onlyWithPresets) {"with presets"} {"all"}).postln;
			"==========".postln;
			VSTPlugin.pluginKeys.sort.do{ arg k;
				var p = VSTPlugin.plugins[k];
				var hasPresets = p.presets.isEmpty.not;
				if (conditionFunc.value(p) and: { onlyWithPresets.not || hasPresets}) {
					"%: (% - %)".format(p.name, p.vendor, p.sdkVersion).postln;
					if (hasPresets) {
						("\tpresets:" + p.presets.collect(_.name).cs).postln;
					}
				}
			}
		}
	}

	vstPrintInstruments { arg onlyWithPresets = true, extraVstPluginSearchPath = defaultVstPath;
		this.prVstPrint("VST Instruments", _.synth, onlyWithPresets, extraVstPluginSearchPath);
	}

	vstPrintEffects { arg onlyWithPresets = true, extraVstPluginSearchPath = defaultVstPath;
		this.prVstPrint("VST Effects", _.synth.not, onlyWithPresets, extraVstPluginSearchPath);
	}

	// Add a VST filter effect (needs array index > 0) to modify the NodeProxy
	// e.g. b.vstFx(\drum -> 10, "Reaktor 6", "goldFinalizer"); { arg in; JPverb.ar(in, 3) }, 0.3);
	// You can clear fx in a slot with: b.fx(\drum -> 10);
	vstFx { arg trkNameAndIndex, vstName, programPath, extraVstPluginSearchPath = defaultVstPath, wet=1;
		var trkName, index;
		if (Bacalao.prVSTPluginInstalled.not) { ^this };

		if (trkNameAndIndex.isKindOf(Association).not) {
			"trkName and index should be an Association, e.g: b.vstFx(\\drum -> 10, ...)".error;
			^this;
		};

		#trkName, index = [trkNameAndIndex.key.asSymbol, trkNameAndIndex.value];
		if (index.isInteger and: {index > 0}) {
			server.waitForBoot{
				var vstCtrlVarName;
				var np = this.proxy(trkName);
				var wetParam = ("wet" ++ index).asSymbol;

				// Make it into a audio NodeProxy if it's currently empty
				server.sync;
				np.numChannels ?? { np.source = { Silence.ar ! numChannels } };
				server.sync;
				if (np[index].isNil) {
					// Start the thing quiet initially, set the wet at the next quant
					np.set(wetParam, 0);
				};

				if (VSTPlugin.plugins.isEmpty) {
					VSTPlugin.search(server, extraVstPluginSearchPath);
					server.sync;
				};

				np[index] = \vstFilter -> { arg in; VSTPlugin.ar(in, numChannels) };
				this.sched({
					np.set(wetParam, wet);
				}, 1);
				server.sync;
				vstCtrlVarName = "vstFx_%_%".format(
					if (trkName == '*') { "global" } { trkName },
					index).asSymbol;
				currentEnvironment[vstCtrlVarName] =
				VSTPluginNodeProxyController(np, index).open(
					vstName,
					editor: true,
					verbose: true,
					action: { arg vstController, success;
						if (success) {
							"Opened VSTPluginController for %/% effect with %".format(trkName, index, vstName).postln;
							vstController.setTempo((clock.tempo * 60).debug("set VST tempo post-init"));
							if (programPath.notNil) {
								this.prVstRead(vstController, programPath);
							} {
								"No VST preset specified to load".warn;
							}
						} {
							("Unable to open VST: " ++ vstName).error;
						}
					},
					multiThreading: true
				);
			}
		} {
			"fx index should be an integer greater than 0...skipping".warn;
		}
	}

	vstInit { arg trkName, vstName, programPath, bankAndProgram, extraVstPluginSearchPath = defaultVstPath;
		if (Bacalao.prVSTPluginInstalled.not) { ^this };
		//programPath = programPath !? { VSTPlugin.prResolvePath(programPath, false).postln };
		server.waitForBoot{
			var vstController;
			var vstProxy;
			var dictEntry;
			trkName = trkName.asSymbol;
			if (VSTPlugin.plugins.isEmpty) {
				VSTPlugin.search(server, extraVstPluginSearchPath);
				server.sync;
			};
			dictEntry = vstDict[trkName];
			if (dictEntry.notNil) {
				#vstController, vstProxy = dictEntry;
			};
			vstProxy = vstProxy ?? { this.proxy(trkName).clock_(clock) };
			if (vstProxy.source != (\vstDef -> \bacalao_vsti)) {
				// Don't recreate Synth using VSTPlugin if not necessary
				vstProxy.source = \vstDef -> \bacalao_vsti;
			};
			vstProxy.play;
			server.sync;

			vstController = vstController ?? {
				VSTPluginNodeProxyController(vstProxy).debug("New '%'".format(trkName))
			};
			vstDict[trkName] = [ vstController, vstProxy ];
			vstController.open(vstName, editor: true, action: { arg vstController, success;
				if (success) {
					"Opened VSTPluginController for % with %".format(trkName, vstName).postln;
					vstController.setTempo((clock.tempo * 60).debug("set VST tempo post-init"));
					if (programPath.notNil) {
						if (bankAndProgram.notNil) {
							"Both programPath and bankAndProgram were specified...only using programPath".warn;
						};
						this.prVstRead(vstController, programPath);
					} {
						if (bankAndProgram.isKindOf(Association)) {
							this.vstBankProgram(trkName, bankAndProgram.key, bankAndProgram.value);
						} {
							if (bankAndProgram.notNil) {
								"Expected bankAndProgram to be an Association (bank -> program), e.g. (3 -> 22)".warn;
							}
						}
					}
				} {
					("Unable to open VST: " ++ vstName).error;
				}
			}, multiThreading: true);
		}
	}

	// free does clear plus removes the VST instrument (if there is one)
	free { arg trkNameOrNames, fadeTime = 0;
		if (trkNameOrNames.isSequenceableCollection.not or: {trkNameOrNames.isString }) {
			trkNameOrNames = trkNameOrNames.asArray;
		};
		trkNameOrNames.do{ arg trkName;
			this.prFree(trkName, fadeTime)
		};
	}

	prFree { arg trkName, fadeTime=0;
		var vstCtl;
		var extraTime = 1;
		this.clear(trkName = trkName.asSymbol, fadeTime);

		vstCtl = this.vst(trkName);
		if (vstCtl.notNil) {
			var vstProxy = this.proxy(trkName);
			// Remove the dictionary entry now, so any future vstInit
			// will create a new VST instrument
			vstDict[trkName] = nil;

			// Don't schedule on the Bacalao clock, because fadeTime should be in "wall" clock time
			SystemClock.sched(fadeTime ? 0 + server.latency + extraTime, {
				"Freeing VST %".format(trkName).postln;
				this.despatialize(trkName, playDefault: false);
				vstProxy.clear();
				vstCtl.close;
				nil
			});
		};

		// Don't schedule on the Bacalao clock, because fadeTime should be in "wall" clock time
		SystemClock.sched(fadeTime ? 0 + server.latency + extraTime, {
			"Undefining Ndef %".format(trkName).postln;
			Ndef.dictFor(server).removeAt(trkName);
			// Also remove any defaults for this track
			this.defSet(trkName, ());
			nil
		});
	}

	vstClearAll { arg fadeTime=1;
		vstDict.keys.do{ arg k; this.clear(k, fadeTime) };
	}

	vstFreeAll { arg fadeTime=0;
		vstDict.keys.do{ arg k; this.free(k, fadeTime) };
	}

	prVstRead { arg vstCtl, programPath;
		if (vstCtl.notNil and: { programPath.notNil }) {
			var hasPreset = vstCtl.info.presets.select{|elem| elem[\name] == programPath}.isEmpty.not;
			var action = { arg self, success;
				if (success) { self.setTempo((clock.tempo * 60).debug("set VST tempo post-load")) }
			};
			if (hasPreset) {
				"Loading preset '%'".format(programPath).postln;
				vstCtl.loadPreset(programPath, action, async: true)
			} {
				if (programPath.endsWith(".vstpreset").not) {
					programPath = programPath ++ ".vstpreset";
				};
				"Loading programPath %".format(programPath).postln;
				// async: true is required to avoid the delay-locked loop from
				// getting screwed up in the Server and causing latency issues
				// for a while.
				vstCtl.readProgram(programPath, action, async: true);
			}
		}
	}

	vstRead { arg trkName, programPath;
		if (vstDict[trkName = trkName.asSymbol].notNil) {
			this.prVstRead(vstDict[trkName].first.debug("vstCtl in vstRead"), programPath);
		} {
			"vstRead: unrecognized track".warn;
		}
	}

	vstPresetDir { arg trkName, type = \user;
		var vst = this.vst(trkName.asSymbol);
		^vst !? { vst.info.presetFolder(type) };
	}

	proxy { arg trkName;
		var vst = this.vst(trkName = trkName.asSymbol);
		^if (vst.notNil) {
			vstDict.at(trkName).last
		} {
			if (trkName == '*') {
				server.waitForBoot{
					// Special case for the "global output" Proxy,
					// e.g. to put fx on the final output of all tracks.
					if (outputGroup.isNil or: { outputGroup.isPlaying.not }) {
						// Create a Group for our NodeProxy after everything (but before the Safety)
//						var safety = Safety.all.at(server.name);
//						outputGroup = if (safety.notNil) {
//							Group.before(safety.synth).register
//						} {
//							Group.after(server.defaultGroup).register
//						};

						var previousGroup = if (spatial.notNil and: { spatial.decoderGroup.notNil }) {
							spatial.decoderGroup
						} {
							server.defaultGroup
						};
						outputGroup = Group.after(previousGroup).register;
						server.sync;
					};
					// Define a passthrough
					Ndef(trkName).parentGroup_(outputGroup);
					Ndef(trkName).bus = Bus(\audio, 0, numChannels, server);
				}
			};

			Ndef.ar(trkName -> server.name, numChannels);
			Ndef(trkName -> server.name)
		};
	}

	vst { arg trkName;
		var dictEntry = vstDict.at(trkName.asSymbol);
		^dictEntry !? { dictEntry.first };
	}

	// Can send bank and program as separate arguments, or
	// a single Association (bank -> program) to the bank argument.
	vstBankProgram { arg trkName, bank, program, quant = #[1, 0.925];
		var vst = this.vst(trkName);
		if (bank.isKindOf(Association)) {
			#bank, program = [bank.key, bank.value];
		};
		if (vst.notNil and: { vst.midi.notNil }) {
			var midi = vst.midi;
			var bankMsb = (bank - 1) div: 128;
			var bankLsb = (bank - 1) mod: 128;
			this.sched({
				midi.control(0, 0, 0);
				midi.control(0, 32, bank-1);
				midi.program(0, program-1);
			}, quant);
		} {
			"VST instrument '%' is not defined".format(trkName).postln;
		}
	}

	clear { arg trkNameOrNames, fadeTime = 1;
		if (trkNameOrNames.isSequenceableCollection.not and: {trkNameOrNames.isString.not }) {
			trkNameOrNames = trkNameOrNames.asArray;
		};
		trkNameOrNames.do{ arg trkName;
			this.prClear(trkName, fadeTime)
		};
	}

	prClear { arg trkName, fadeTime=1;
		// Clear an existing Ndef (and ignore everything else)
		var vst = this.vst(trkName = trkName.asSymbol);
		var ndef = this.proxy(trkName);
		if (verbose) {
			"Clearing Ndef(%) (fade %)".format(trkName, fadeTime).postln;
		};

		// Note that if we called ndef.clear(fadeTime) here,
		// the monitor volume would jump to full before fading out.
		// So we call stop() (which doesn't change settings)
		// and then schedule clear after fadeTime has passed.
		ndef.stop(fadeTime);

		// Don't schedule on the Bacalao clock, because fadeTime should be in "wall" clock time
		SystemClock.sched(fadeTime ? 0 + server.latency, {
			var controlProxiesToDelete = ndef.nodeMap.asArray.select{arg val;
				val.isKindOf(NodeProxy) and: { val.isKindOf(Ndef).not }
			};
			this.prClearOtherPatternSlots(trkName);
			this.prRemoveUnusedPatternSlots(trkName);
			if (vst.notNil) {
				// For VST instruments, we don't want to ever clear/free the
				// underlying NodeProxy, otherwise we'd have to reload and
				// reconfigure the VST (expensive)!

				// But we do want to clear effects from the VST proxy
				// and reset the playback volume.
				this.fxClear(trkName);
				this.db(trkName, 0);

				// Perform a soft bypass (it waits for sound to tail out, then stops processing to save CPU)
				ndef.set(\bypass, 2);
			} {
				// For regular (SC Synth-sourced) proxies, we can go ahead
				// and get rid of the whole thing.
				this.despatialize(trkName, playDefault: false);
				ndef.clear();
			};
			controlProxiesToDelete.do{ arg proxy;
				"Freeing old control proxy".postln;
				proxy.free
			};
		});
	}

	removeSlots { arg trkName, indices;
		indices.asArray.do{ arg i;
			this.p(trkName -> i, nil);
		}
	}

	// Return all the Pdefs that we use for "slots" with a given track
	// (we use name mangling for this).
	prGetPdefSlots { arg trkName;
		var trkStr = if (trkName == '*') { "\\*" } { trkName.asString };
		^Pdef.all.select{ arg x;
			"%\\/(nil|\\d+)\\/%".format(trkStr, server.name).matchRegexp(x.key.asString)
		}
	}

	// Clear just sets the source to nil, relying on the Quant to say when it should go away.
	prClearOtherPatternSlots { arg trkName, except;
		// "Clear '%' patterns for other slots".format(trkName).postln;
		this.prGetPdefSlots(trkName).do{ arg pdef;
			if (pdef != except) {
				pdef.debug("stopping").source = nil;
			}
		}
	}

	// Remove actually removes the Pdef if it's not set to any source at the moment
	prRemoveUnusedPatternSlots { arg trkName;
		// "Removing unused '%' slot patterns".format(trkName).postln;
		this.prGetPdefSlots(trkName).do{ arg pdef;
			if (pdef.source.isNil) {
				var slotNum = pdef.key.asString.drop(trkName.asString.size + 1).asInteger;
				pdef.debug("'" ++ trkName ++ "' source (slot " ++ slotNum ++ ") still nil...removing");
				pdef.remove;
				if (this.vst(trkName).isNil) {
					this.proxy(trkName).debug("Removing Ndef slot").removeAt(slotNum, 0);
				}
			}
		}
	}

	prGetPdefName { arg trkName, slot;
		^(trkName.asString ++ $/ ++ (slot ? 0) ++ $/ ++ server.name).asSymbol;
	}

	pdef { arg trkName, slot = 0;
		var pdefName;
		if (trkName.isKindOf(Association)) {
			#trkName, slot = [trkName.key.asSymbol, trkName.value];
		};
		pdefName = this.prGetPdefName(trkName, slot);
		^Pdef(pdefName);
	}

	prChangePattern { arg trkName, slot, pattern, loopDur, patternQuant, role, includeMask = true;
		// Pdef doesn't have a per-Server ProxySpace, so
		// we make up a "unique" name by combining the track name
		// and this instance's Server name.
		var replacePatterns = slot.isNil;
		// Note we don't use +/+ here for cross-platform reasons..we always want joining with '/'
		var pdefName = this.prGetPdefName(trkName, slot);
		var pdef, ndef, vst = this.vst(trkName = trkName.asSymbol);
		var quantStartBeat;

		if (pattern.notNil) {
			var globalDefaultDict = this.defGet('*');
			var defaultDict = this.defGet(trkName);
			// Could potentially use Pfset here, but time-chaining the defaults
			// seems to work better (possibly due to some glitches in PtimeChain
			// with cleanup functions?)

			// NOTE: Running Ptrace (or pat.trace) on these patterns can screw
			//       up the defaults. For example, if your track default has (amp: 0)
			//       it can stop notes from playing, even in patterns that
			//       explicitly set an amp, when they're being "traced"!

			// We set default mask 1 so we can use it for triggering with pset
			// (goes to 0/Rest when using PmaskBjork or degrade).
			if (includeMask) {
				pattern = pattern << Pbind(\mask, 1);
			};
			if (defaultDict.notEmpty) {
				pattern = pattern <> defaultDict;
			};
			if (globalDefaultDict.notEmpty) {
				pattern = pattern <> globalDefaultDict;
			};
		};

		if (loopDur.notNil) {
			if (loopDur > 0) {
				// Shorten or extend the loop duration to a specfic length and loop it
				pattern = PnSafe(Pfindur(loopDur, Pseq([pattern, Pbind(\degree, Rest(), \dur, loopDur)])));
				patternQuant = patternQuant ?? { [loopDur, 0] };
				"Setting up % for looping with duration % and quant %".format(pdefName, loopDur, patternQuant).postln;
			} {
				// loopDur == 0 (or negative)
				// Loop infinitely, with whatever is the "natural" duration
				pattern = PnSafe(pattern);
				"Setting up % for looping with 'natural' duration".format(pdefName).postln;
			};
		};

		// The quantization until this point has been in bars, not beats
		patternQuant = (patternQuant ?? { #[1, 0] });
		pdef = Pdef(pdefName).clock_(barClock).quant_(patternQuant);
		ndef = this.proxy(trkName).clock_(barClock).quant_(patternQuant);
		if (replacePatterns) {
			// Set the source of other slots to nil (uses current Quant)
			this.prClearOtherPatternSlots(trkName, except: pdef);

			// Remove unused ones a bit later (when sources are still nil)
			pdef.sched{
				this.prRemoveUnusedPatternSlots(trkName)
			};
		};

		// We set a 'time' key in our Events, which provides time since playing.
		// Otherwise, the clock restarts on a given sub-pattern, for example amp"Psine.exprange(8,-0.25,0.1,1)"
		quantStartBeat = patternQuant.nextTimeOnGrid(barClock);
		case
		{ vst.notNil and: { role.isNil } } {
			pdef.source = pattern <> (type: \vst_midi_and_pset, vst: vst, proxy: ndef, time: { thisThread.beats - quantStartBeat });
			// Must include clock argument here, even if we set the Pdef's clock
			// See issue: https://github.com/supercollider/supercollider/issues/4803
			pdef.play(barClock);
			// Explicitly resume processing the Synth in case it's bypassed
			ndef.set(\bypass, 0);
		}
		{ role == \pset_vset_only } {
			pdef.source = pattern <> (type: \pset_vset_only, vst: vst, proxy: ndef, time: { thisThread.beats - quantStartBeat });
			// Must include clock argument here, even if we set the Pdef's clock
			// See issue: https://github.com/supercollider/supercollider/issues/4803
			pdef.play(barClock);
		}
		{
			// Don't redefine the Ndef source all the time with
			// the pattern, because that will sometimes double-up the
			// first event (because of the quant). Pdef has special
			// code to handle this "handoff" between Patterns.
			// Just switching the Pdef is enough.
			if (role.notNil) {
				ndef[slot ? 0] = role -> pattern;
			} {
				if (ndef[slot ? 0] != pdef) {
					if (verbose) {
						"Setting Ndef(%) source (%) to Pdef(%)".format(trkName,
							slot ? 0, pdefName).postln
					};
					ndef[slot ? 0] = pdef;
				};

				// Just setting the Pdef source here works properly with
				// quantization, after the fix from:
				// https://github.com/supercollider/supercollider/pull/4779
				pdef.source = pattern <> (type: \note_and_pset, proxy: ndef, time: { thisThread.beats - quantStartBeat });
			};
		};
		if (ndef.isMonitoring.not) {
			if (verbose) {
				"Playing Ndef(%)".format(trkName).postln
			};
			ndef.play;
		}
	}

	prSetSource { arg trkName, slot, source, quant;
		// Use this when setting a NodeProxy, single Event or Function
		// as source of the NodeProxy (not PatternProxy).
		var replacePatterns = slot.isNil;
		// Note we don't use +/+ here for cross-platform reasons..we always want joining with '/'
		var ndef, vst = this.vst(trkName = trkName.asSymbol);

		// The quantization until this point has been in bars, not beats
		quant = quant ?? { #[1, 0] };
		ndef = this.proxy(trkName).clock_(barClock).quant_(quant);
		if (replacePatterns) {
			// Set the source of other slots to nil (uses current Quant)
			this.prClearOtherPatternSlots(trkName, except: nil);
		} {
			// Remove only this slot's Pdef pattern (if there is one)
			var pdefName = this.prGetPdefName(trkName, slot);
			if (Pdef.all.keys.includes(pdefName)) {
				var pdef = Pdef(pdefName);
				pdef.debug("stopping").source = nil;
			};
		};

		if (vst.notNil and: { replacePatterns || (slot == 0) }) {
			"Can't replace slot 0 source of a VST proxy".warn;
			^this;
		};

		slot = slot ? 0;
		if (ndef[slot] != source) {
			"Setting '%' source % to %".format(trkName, slot ? 0, source).postln;
			ndef[slot ? 0] = source;
		};
		if (source.notNil) {
			if (ndef.isMonitoring.not) {
				if (verbose) {
					"Playing Ndef(%)".format(trkName).postln
				};
				ndef.play(out: 0, group: ndef.homeServer.defaultGroup);
			}
		}
	}

}

// Bake a result into a string and save it to the clipboard for later pasting
// Use joinWith = "" for character strings ( e.g. Bake(rrand(0,9!8), "") )
Bake {
	*new { arg x, joinWith = Char.space;
		var bakedText = {
			if (x.isKindOf(Pattern)) {
				x = x.asStream.nextN(64).reject(_.isNil);
			};
			if (x.isKindOf(Collection) and: { x.isKindOf(String).not }) {
				x = x.join(joinWith);
			};
			x.cs.trim("\"")
		}.value;
		var tmpFilePath = Platform.defaultTempDir +/+ "bacalaoPaste.txt";
		var clipboardCmd = Platform.case(
			\osx, { "pbcopy" },
			\linux, { "xclip" },
			\windows, { "clip" }
		);
		File.use(tmpFilePath, "w", { |f| f.write(bakedText) });
		"% < %".format(clipboardCmd, tmpFilePath).unixCmd;

		// Earlier experiment to modify the Document and selection, but
		// had issues where it sometimes worked, then other times got
		// the document screwed up (frozen selection/insertion point).

		// var d = Document.current;
		// var bakedText =  Char.nl ++ x.cs;
		// if (d.notNil) {
		// 	var offset = d.text.find(Char.nl, offset: d.selectionStart).postln ?? {d.text.size};
		// 	d.insertText(bakedText, offset);
		// 	// d.selectRange(offset+1, bakedText.size-1);
		// 	// Document.current.selectedString = x.cs;
		// } {
		// 	Error("Document is nil");
		// };
		^bakedText
	}

	*cs { arg x;
		var bakedText = x.cs;
		var tmpFilePath = Platform.defaultTempDir +/+ "bacalaoPaste.txt";
		var clipboardCmd = Platform.case(
			\osx, { "pbcopy" },
			\linux, { "xclip" },
			\windows, { "clip" }
		);
		File.use(tmpFilePath, "w", { |f| f.write(bakedText) });
		"% < %".format(clipboardCmd, tmpFilePath).unixCmd;
		^bakedText
	}
}

// A Pattern returning ones for "hits" in Bjorklund's Euclidean algorithm and Rests for the "misses".
Pbjork {
	*new { arg k, n, offset=0;
		^Pbjorklund(k, n, inf, offset:offset).collect{|e| if (e==0) { Rest(0) } { e }};
	}
}

PmaskBjork {
	*new { arg k, n, offset=0;
		^Pbind(\mask, Pbjork(k, n, offset));
	}
}

Psine {
	*new { arg periodBars=1, phase=0, mul=1, add=0, repeats=inf;
		// We don't use Ptime (except as fallback when a 'time' key isn't there)
		// so we get elapsed time for the "overall" pattern, not sub-patterns.
		var tPattern = Pif(Pfunc{|ev| ev[\time].notNil }, Pn(Pkey(\time), repeats), Ptime(repeats));
		^((tPattern / Pfunc{thisThread.clock.beatsPerBar} / periodBars + phase) * 2pi).sin * mul + add;
	}

	*range { arg lo = -1.0, hi=1.0, periodBars=1, phase=0, repeats=inf;
		^Psine.new(periodBars, phase, 1, 0, repeats).linlin(-1,1, lo,hi);
	}

	*exprange { arg lo = 0.01, hi=1.0, periodBars=1, phase=0, repeats=inf;
		^Psine.new(periodBars, phase, 1, 0, repeats).linexp(-1,1, lo,hi);
	}

	*curverange { arg lo = 0.01, hi=1.0, curve = -4, periodBars=1, phase=0, repeats=inf;
		^Psine.new(periodBars, phase, 1, 0, repeats).lincurve(-1,1, lo,hi, curve);
	}
}

Psaw {
	*new { arg periodBars=1, phase=0, mul=1, add=0, repeats=inf;
		// We don't use Ptime (except as fallback when a 'time' key isn't there)
		// so we get elapsed time for the "overall" pattern, not sub-patterns.
		var tPattern = Pif(Pfunc{|ev| ev[\time].notNil }, Pn(Pkey(\time), repeats), Ptime(repeats));
		^((tPattern / Pfunc{thisThread.clock.beatsPerBar} + phase).mod(periodBars) / periodBars * 2 - 1) * mul + add;
	}

	*range { arg lo = -1.0, hi=1.0, periodBars=1, phase=0, repeats=inf;
		^Psaw.new(periodBars, phase, 1, 0, repeats).linlin(-1,1, lo,hi);
	}

	*exprange { arg lo = 0.01, hi=1.0, periodBars=1, phase=0, repeats=inf;
		^Psaw.new(periodBars, phase, 1, 0, repeats).linexp(-1,1, lo,hi);
	}

	*curverange { arg lo = 0.01, hi=1.0, curve = -4, periodBars=1, phase=0, repeats=inf;
		^Psaw.new(periodBars, phase, 1, 0, repeats).lincurve(-1,1, lo,hi, curve);
	}
}

// Time-based random periodic function (using Simplex noise)
Prpr {
	*new { arg periodBars=1, offset=#[12.34, 23.45], variationRate=0.005, freqScale=0.2, octaves=3, repeats=inf;
		var tPattern = Pif(Pfunc{|ev| ev[\time].notNil }, Pn(Pkey(\time), repeats), Ptime(repeats)) / Pfunc{thisThread.clock.beatsPerBar};
		^tPattern.collect{ arg t;
			var bar = t.value;
			var x = bar.mod(periodBars) / periodBars;
			Simplex.periodic(x, [variationRate * bar, 0] + offset, freqScale, octaves)
		};
	}

	*range { arg lo=(-1.0), hi=1.0, periodBars=1, offset=#[12.34, 23.45], variationRate=0.005, freqScale=0.2, octaves=3, repeats=inf;
		^Prpr(periodBars, offset, variationRate, freqScale, octaves, repeats).linlin(-1.2,1.2, lo,hi);
	}

	*exprange { arg lo=0.01, hi=1.0, periodBars=1, offset=#[12.34, 23.45], variationRate=0.005, freqScale=0.2, octaves=3, repeats=inf;
		^Prpr(periodBars, offset, variationRate, freqScale, octaves, repeats).linexp(-1.2,1.2, lo,hi);
	}

	*curverange { arg lo=0.0, hi=1.0, curve = -4, periodBars=1, offset=#[12.34, 23.45], variationRate=0.005, freqScale=0.2, octaves=3, repeats=inf;
		^Prpr(periodBars, offset, variationRate, freqScale, octaves, repeats).lincurve(-1.2,1.2, lo,hi, curve);
	}

	*durs{ arg durs=#[0.125,0.125,0.25,0.5], periodBars=1, offset=#[12.34, 23.45], variationRate=0.005, freqScale=0.2, octaves=3, repeats=inf;
		^Pbind(\dur, Pindex(durs, Prpr.range(0, durs.size - 1e-3, periodBars, offset, variationRate, freqScale, octaves, repeats).floor))
	}

}

// Random periodic function with N events per cycle (using Simplex noise)
Pcycrand {
	*new { arg periodSteps=8, offset=#[13, 14], variationRate=0.005, freqScale=0.2, octaves=3;
		^{
			var x = 0;
			periodSteps = max(periodSteps, 1);
			loop{
				Simplex.periodic(x, [variationRate * x, 0] + offset, freqScale, octaves).yield;
				x = x + periodSteps.reciprocal;
			}
		}.p
	}

	*range{ arg lo=(-1.0), hi=1.0, periodSteps=8, offset=#[13, 14], variationRate=0.005, freqScale=0.2, octaves=3;
		^Pcycrand(periodSteps, offset, variationRate, freqScale, octaves).linlin(-1.2,1.2, lo,hi)
	}

	*exprange{ arg lo=0.01, hi=1.0, periodSteps=8, offset=#[13, 14], variationRate=0.005, freqScale=0.2, octaves=3;
		^Pcycrand(periodSteps, offset, variationRate, freqScale, octaves).linexp(-1.2,1.2, lo,hi)
	}

	*curverange{ arg lo=0.0, hi=1.0, curve = -4, periodSteps=8, offset=#[13, 14], variationRate=0.005, freqScale=0.2, octaves=3;
		^Pcycrand(periodSteps, offset, variationRate, freqScale, octaves).lincurve(-1.2,1.2, lo,hi, curve)
	}

	*durs{ arg durs=#[0.125,0.125,0.25,0.5], periodSteps=8, offset=#[13, 14], variationRate=0.005, freqScale=0.2, octaves=3;
		^Pbind(\dur, Pindex(durs, Pcycrand.range(0, durs.size - 1e-3, periodSteps, offset, variationRate, freqScale, octaves).floor, inf))
	}
}

// An Event Pattern (Pbind) helper with random exponential values on the amp
PampRand {
	*new { arg lo = 0.2, hi = 0.8, randSeed;
		^Pbind(\amp, Per(lo, hi, randSeed));
	}

	*periodic { arg lo = 0.2, hi = 0.8, periodBars = 1, offset = #[63.11, -25.9], freqScale=0.2, octaves=3, repeats=inf;
		^Pbind(\amp, Prpr.exprange(periodBars, offset, freqScale, octaves, lo, hi, repeats));
	}
}

// Uniform distribution random stream (alias for Pwhite with extra randSeed argument)
Pr {
	*new { arg lo = 0.0, hi = 1.0, randSeed, length = inf;
		var p = Pwhite(lo, hi, length);
		^if (randSeed.notNil) { Pseed(Pn(randSeed, 1), p) } { p };
	}
}

// Uniform distribution random stream (from -val to +val)
// (like val.rand2)
Pr2 {
	*new { arg val = 0.5, randSeed, length = inf;
		^Pr(-1 * val, val, randSeed, length);
	}
}

// Exponential distribution random stream (alias for Pexprand with extra randSeed argument)
Per {
	*new { arg lo = 0.0001, hi = 1.0, randSeed, length = inf;
		var p = Pexprand(lo, hi, length);
		^if (randSeed.notNil) { Pseed(Pn(randSeed, 1), p) } { p };
	}
}

// Envelope-lookup random generator, similar to Pmeanrand but with adjustable slope (and mean).
// Set negative slope to "stay away" from the mean instead of staying close to it.
// If you set a custom "mean", you can make it asymmetric, so the peak
// (or valley) is shifted (but always within the range lo..hi).
// See: https://scsynth.org/t/what-is-the-opposite-of-pmeanrand/4140
Pmeanrand2 {
	*new { arg lo = 0.0, hi = 1.0, slope = 2, mean = nil;
		var meanParam, env;
		mean = (mean ?? { lo + hi / 2 }).clip(lo, hi);
		meanParam = mean.linlin(lo, hi, 0, 1);
		env = Env([lo, mean, hi], [meanParam, 1.0 - meanParam], [slope.neg, slope]);
		^Pfunc{ env.at(1.0.rand) };
	}
}

+Pbind {
	// Overriding because this "innocent" typo produces a sclang freeze (if you play a Pbind().degrad)!
	degrad {
		"Probably a typo...'degrad' rather than degrade?".postln;
		^this;
	}
}
