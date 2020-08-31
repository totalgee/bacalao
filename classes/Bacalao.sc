Bacalao {
	classvar parse;
	const numChannels = 2;
	classvar <preProcessorVariables = 'bacalaoPreProcessorVariables';
	var <clock;
	var <server;
	var <>verbose = true;
	var <quant;
	// Collection of 2-element Arrays [VSTPluginController, NodeProxy], looked up by "name" (a Symbol)
	var <vstDict;
	// Dictionary of default Event properties per track
	// Note that using '*' as the trkName (equivalent to $*) will
	// set global defaults, shared by all trks.
	var <trkDefaults;
	var <spatial; // Settings for spatialization (BacalaoSpatialSettings)

	*initClass {
		parse = BacalaoParser;
	}

	*new { arg shareClock, server, verbose, quant;
		var clock;

		case
		{ shareClock.class == Bacalao } { clock = shareClock.clock }
		{ shareClock.isKindOf(Clock) } { clock = shareClock }
		{ clock = TempoClock.default };

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

		Bacalao.config();
		^super.newCopyArgs(clock, server, verbose ? true, quant, (), (), nil).start.prSetupCmdPeriod.push;
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
				var vsti = VSTPlugin.ar(nil, numChannels, bypass: bypass);
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

			~type = \note;
			Event.eventTypes[\note].value(server);
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

	start {
		thisProcess.interpreter.preProcessor = BacalaoParser.preProcess(_);
		"Bacalao pattern parsing enabled".postln;
	}

	stop {
		thisProcess.interpreter.preProcessor = nil;
		"Bacalao pattern parsing disabled".postln;
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
			#trkName, slot = [trkName.key, trkName.value];
		};
		if (pattern.isKindOf(Pattern)) {
			this.prChangePattern(trkName.asSymbol, slot, pattern, nil, quant);
		} {
			this.prSetSource(trkName.asSymbol, slot, pattern, quant);
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
			#trkName, slot = [trkName.key, trkName.value];
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
			this.prChangePattern(trkName.asSymbol, slot, pattern, dur, quant, role, includeMask);
		} {
			this.prSetSource(trkName.asSymbol, slot, pattern, quant);
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
				if (obj.isKindOf(SynthDefControl)) {
					control = obj.synthDef.allControlNames.detect{ arg c; c.name == controlName };
					control.notNil
				} {
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
			if (targetValue.equalWithPrecision(startValue, 1e-4, 0.02) or: { fadeTime <= 0 }) {
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
		clock.schedAbs((quant * clock.beatsPerBar).nextTimeOnGrid(clock), { arg ...args; event.value(*args); nil });
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

		#trkName, index = [trkNameAndIndex.key, trkNameAndIndex.value];
		if (index.isInteger and: {index > 0}) {
			var np = this.proxy(trkName);
			var wetParam = (\wet.asString ++ index).asSymbol;

			// Make it into a audio NodeProxy if it's currently empty
			np.numChannels ?? { np.source = { Silence.ar!2 } };
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

	fxClear { arg trkName;
		var np = this.proxy(trkName);
		np.objects.indices[1..].do(np[_] = nil);
	}

	// Set all slot effects to dry (with optional fadeTime), without removing them
	fxDry { arg trkName, fadeTime = 0;
		var np = this.proxy(trkName);
		np.objects.indices[1..].do{ arg slot;
			var key = ("wet" ++ slot).asSymbol;
			this.set(trkName, key, 0, fadeTime);
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
		var cycleDur = bars * clock.beatsPerBar / clock.tempo;
		var rate = desiredRate ?? { (buf.duration / cycleDur).debug("chop calculated rate") };
		inst = inst ?? { switch (buf.numChannels,
			1, { \sample1 },
			2, { \sample2 },
			{ \sample2 })
		};
		// Here, the 'dur' should be in cycles, not beats
		^Pbind(\instrument, inst, \buf, buf, \length, origGrainDur, \rate, rate, \start, Pseq(starts), \dur, (1/pieces), \stretch, bars);
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

	vstPrintInstruments { arg onlyWithPresets = true, extraVstPluginSearchPath = "C:/Program Files/Native Instruments/VSTPlugins 64 bit";
		this.prVstPrint("VST Instruments", _.synth, onlyWithPresets, extraVstPluginSearchPath);
	}

	vstPrintEffects { arg onlyWithPresets = true, extraVstPluginSearchPath = "C:/Program Files/Native Instruments/VSTPlugins 64 bit";
		this.prVstPrint("VST Effects", _.synth.not, onlyWithPresets, extraVstPluginSearchPath);
	}


	vstInit { arg trkName, vstName, programPath, bankAndProgram, extraVstPluginSearchPath = "C:/Program Files/Native Instruments/VSTPlugins 64 bit";
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
			if (vstProxy.source != \bacalao_vsti) {
				// Don't recreate Synth using VSTPlugin if not necessary
				vstProxy.source = \bacalao_vsti;
			};
			vstProxy.play;
			server.sync;

			// Schedule on our TempoClock
			// If we schedule on SystemClock (or AppClock) we get:
			//   "FAILURE IN SERVER /u_cmd Node NNNN not found"
			(vstProxy.clock).play({
				// schedule bundle with Server latency
				var bundle = server.makeBundle(false, {
					vstController = vstController ?? {
						var synth = Synth.basicNew(\bacalao_vsti, server, vstProxy.objects.first.nodeID);
						VSTPluginController(synth).debug("New '%'".format(trkName))
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
				});
				server.listSendBundle(server.latency, bundle);
			});
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
		^Pdef.all.select{ arg x;
			"%\\/(nil|\\d+)\\/%".format(trkName, server.name).matchRegexp(x.key.asString)
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

	prChangePattern { arg trkName, slot, pattern, loopDur, patternQuant, role, includeMask = true;
		// Pdef doesn't have a per-Server ProxySpace, so
		// we make up a "unique" name by combining the track name
		// and this instance's Server name.
		var replacePatterns = slot.isNil;
		// Note we don't use +/+ here for cross-platform reasons..we always want joining with '/'
		var pdefName = this.prGetPdefName(trkName, slot);
		var pdef, ndef, vst = this.vst(trkName = trkName.asSymbol);
		var quantStartBeat;

		// Stretch it so the durations in bars are converted to beats on our clock
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
			if (defaultDict.notNil) {
				pattern = pattern << defaultDict << globalDefaultDict;
			};
			pattern = Pmul(\stretch, Pfunc{clock.beatsPerBar}, pattern);
		};

		if (loopDur.notNil) {
			if (loopDur > 0) {
				// Shorten or extend the loop duration to a specfic length and loop it
				pattern = PnSafe(Pfindur(loopDur * clock.beatsPerBar, Pseq([pattern, Pbind(\degree, Rest(), \dur, loopDur * clock.beatsPerBar)])));
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
		patternQuant = (patternQuant ?? { #[1, 0] }) * clock.beatsPerBar;
		pdef = Pdef(pdefName).clock_(clock).quant_(patternQuant);
		ndef = this.proxy(trkName).clock_(clock).quant_(patternQuant);
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
		quantStartBeat = patternQuant.nextTimeOnGrid(clock);
		case
		{ vst.notNil and: { role.isNil } } {
			pdef.source = pattern <> (type: \vst_midi_and_pset, vst: vst, proxy: ndef, time: { thisThread.beats - quantStartBeat });
			// Must include clock argument here, even if we set the Pdef's clock
			// See issue: https://github.com/supercollider/supercollider/issues/4803
			pdef.play(clock);
			// Explicitly resume processing the Synth in case it's bypassed
			ndef.set(\bypass, 0);
		}
		{ role == \pset_vset_only } {
			pdef.source = pattern <> (type: \pset_vset_only, vst: vst, proxy: ndef, time: { thisThread.beats - quantStartBeat });
			// Must include clock argument here, even if we set the Pdef's clock
			// See issue: https://github.com/supercollider/supercollider/issues/4803
			pdef.play(clock);
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
		quant = (quant ?? { #[1, 0] }) * clock.beatsPerBar;
		ndef = this.proxy(trkName).clock_(clock).quant_(quant);
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


// This helper class implements all the parsing and data extraction helpers
// that are used by the main Bacalao class.
BacalaoParser {
	classvar eventAbbrevs;
	const unsignedInt = "\\d+";
	const eventPattern = "(?:[^a-z@]*|(@|\\b[a-z][_a-zA-Z0-9]*))(?:~([a-z][_a-zA-Z0-9]*))?\"([^\"\\n]*)\"";
	const <charPattern = "(?:[^a-z@]*|(@|\\b[a-z][_a-zA-Z0-9]*))(?:~([a-z][_a-zA-Z0-9]*))?'([^'\\n]*)'";
	classvar numberInt;
	const unsignedFloat = "(?:(?:[0-9]+)?\\.)?[0-9]+";
	const nonArraySpace = "[^[:space:]\\][]+";
	const elemWithoutMods = "[^[:space:]\\][@*!]+";
	const balancedAngleBracket = "(<((?>[^><]|(?1))*)>)";
	const chord = "<([^>< ]+)>";
	classvar <reCharEventsPerBar;
	classvar numberFloat;
	const rest = "~";
	const barSplitChar = $|;
	classvar elemModifiers;
	classvar balancedArray;
	classvar <patternValueArg; // balancedArray or numberWithMods
	classvar arrayElem;
	classvar simpleElemPartial;
	classvar simpleElem;
	const word = "\\b[[:alpha:]_]\\w*\\b";
	const label = "(?:[[:alnum:]]+)";
	// [ overallMatch, trkName ]
	const trkName = "^\\s*\\b(\\w+)\\s*:";
	// [ overallMatch, cmd, argStr ]
	const subCmd = "[:&]\\s*(\\w+)([^:&]*)";
	const <numericExpression = "[0-9./*+()-]+";
	classvar globalCmd;
	classvar cmdArgs;

	*initClass {
		var number;
		var numberWithMods;
		var labelWithMods;
		var elemWithMods;
		var arrayWithMods;
		// Number or Bjorklund sequence args e.g. (3,8) or (7,16,1), but can also
		// extend durations after optional initial Rest: (x3,8) or (x7,16,1)
		var unsignedIntOrBjork;
		numberInt = "-?" ++ unsignedInt;
		unsignedIntOrBjork = unsignedInt ++ "|\\(x?" ++ unsignedInt ++ "," ++ unsignedInt ++ "(?:," ++ numberInt ++ ")?\\)";
		reCharEventsPerBar = "^(" ++ unsignedInt ++ ")@";
		numberFloat = "-?" ++ unsignedFloat;
		// elemModifiers must be in the correct order (optional repeat, then optional hold, then optional duplicate)
		elemModifiers = "(?:\\*(" ++ unsignedIntOrBjork ++ "))?(?:@(" ++ unsignedFloat ++ "))?(?:!(" ++ unsignedIntOrBjork ++ "))?";
		// [ overallMatch, balancedArray, arrayRepeat, arrayHold, arrayDuplicate ]
		balancedArray = "(?:(" ++ "\\[(?>[^][]|(?1))*\\])" ++ elemModifiers ++ ")";
		number = "(?:" ++ numberFloat ++ ")|(?:" ++ rest ++ ")";
		numberWithMods = "(?:(" ++ number ++ ")" ++ elemModifiers ++ ")";
		labelWithMods = "(?:(" ++ label ++ ")" ++ elemModifiers ++ ")";
		elemWithMods = "(?:(" ++ elemWithoutMods ++ ")" ++ elemModifiers ++ ")";
		// [ overallMatch, simpleElem, arrayElem, arrayRepeat, arrayHold, arrayDuplicate, unrecognized ]
		arrayElem = "(" ++ nonArraySpace ++ ")" ++ "|(?:(" ++ "[[](?>[^\][]|(?2))*[\]])" ++ elemModifiers ++ ")|([^[:space:]]+)";
		// [ overallMatch, num, numRepeat, numHold, numDuplicate, labelElem, labelRepeat, labelHold, labelDuplicate ]
		simpleElemPartial = "(?:(?:" ++ numberWithMods ++ ")|(?:" ++ labelWithMods ++ "))";
		// [ overallMatch, elem, repeat, hold, duplicate ]
		simpleElem = "^(?:" ++ elemWithMods ++ ")$";
		patternValueArg = "^\\s*(?:(?:" ++ balancedArray ++ ")|(?:" ++ numberWithMods ++ "))\\s*$";

		cmdArgs = IdentityDictionary();
		cmdArgs[\clear] = numberFloat;
		cmdArgs[\tempo] = numericExpression;
		cmdArgs[\beatsPerBar] = numberFloat;
		cmdArgs[\inst] = word;
		cmdArgs[\quant] = numberFloat;
		// globalCmd must not include a ':'
		// [ overallMatch, command, argStr ]
		globalCmd = "^\\s*(" ++ word ++ ")\\s*([^[:space:]:&]?[^:&]*?)\\s*$";

		//////////
		// Abbreviations for Event keys
		eventAbbrevs = (
			ctr: \ctranspose,
			deg: \degree,
			det: \detune,
			gtr: \gtranspose,
			har: \harmonic,
			ins: \instrument,
			inst: \instrument,
			leg: \legato,
			mid: \midinote,
			mn: \midinote,
			mtr: \mtranspose,
			oct: \octave,
			sca: \scale,
			scl: \scale,
			sus: \sustain,
			vel: \velocity,
			slow: \stretch,
			str: \stretch,
			toff: \timingOffset,
		);
		// Other event members without abbreviations
		// root: 0, stepsPerOctave: 12, octaveRatio: 2.0, note: (not including root or octave),
		// freq, tempo, dur: 1.0, lag: 0.0, strum: 0.0, strumEndsTogether: false
		// amp, db: -20.0, pan: 0.0, trig: 0.5, group, out: 0, addAction: 0, variant: nil,
		// type: (e.g. \midi), latency: 0.2,

		// noteOn:  #{ arg chan=0, midinote=60, amp=0.1;
		// noteOff: #{ arg chan=0, midinote=60, amp=0.1;
		// polyTouch: #{ arg chan=0, midinote=60, polyTouch=125;
		// control: #{ arg chan=0, ctlNum, control=125;
		// program:  #{ arg chan=0, progNum=1; [ chan, progNum ] },
		// touch:  #{ arg chan=0, val=125; [ chan, val ] },
		// bend:  #{ arg chan=0, val=125; [ chan, val ] },
		// allNotesOff: #{ arg chan=0; [chan] },
		// smpte:	#{ arg frames=0, seconds=0, minutes=0, hours=0, frameRate=25;
		// songPtr: #{ arg songPtr; [songPtr] },
		// sysex: #{ arg uid, array; [array] } // Int8Array
		// midicmd: \noteOn
	}

	*prResolveVariables { arg elems, patternType, optVariableName;
		var resolveChord = { arg elem;
			// If we've got the ChordSymbol Quark installed, try to lookup as a chord
			if (\ChordSymbol.asClass.notNil and: { elem.first.isUpper }) {
				switch (patternType)
				{ \degree } {
					var notes = elem.asSymbol.asNoteOrChord;
					if (notes != elem.asSymbol) {
						var degs = notes.asArray.collect { |n|
							// TODO when next version of SC comes out use keyToDegree
							var deg = n.keyToDegree2(Scale.major, 12);
							if (deg == deg.round) { deg.asInteger } { deg }
						}; //.debug("ChordSymbol")
						if (degs.size > 1) { degs } { degs.first }
					} {
						elem
					}
				}
				{
					var notes = elem.asSymbol.asNoteOrChord;
					if (notes.size > 1) { notes } { notes.first }
				} //.debug("ChordSymbol") };
			} {
				// Stick with the original element, there is no lookup
				elem
			}
		};

		var parserVariables = if (optVariableName.isEmpty.not) {
			currentEnvironment[optVariableName.asSymbol]
		} {
			currentEnvironment
		};
		if (parserVariables.isKindOf(Dictionary).not) {
			Error("'~%' lookup Dictionary not found".format(optVariableName)).throw;
		};

		// Replace Bacalao parser variables with their values
		^elems.collect{ arg elem;
			var substitute = Bacalao.varLookup(elem, parserVariables);
			if (substitute.isKindOf(Prand)) {
				// The "elem" must convert to a string with no spaces
				substitute = substitute.cs.reject(_.isSpace);
			};
			substitute ?? { resolveChord.value(elem) };
		}
	}

	*prVariableNameToString { arg elem;
		^if (elem.size == 1) {
			// Return a Char, not Symbol, for one-letter names,
			// otherwise ChordSymbol screws up the lookup by resolving
			// it as a "note" when embedding the Symbol.
			// (see ChordSymbol overload for Symbol.embedInStream)
			elem.first.cs;
		} {
			if (elem.beginsWith("ALTERNATE")) {
				// Don't put quotes around alternates, which are code
				// that will be substituted later.
				elem
			} {
				// We return these as Strings rather than Symbols, because
				// otherwise the ChordSymbol class can mess things up when
				// it embeds Symbols (when using variable lookup in PnSymRest).
				elem.asString.cs
			}
		}
	}

	*prProcessPatterns { arg code;
		var curOffset = 0;
		var pat = code.findRegexp(eventPattern).clump(4).reject{|m| m[1][1].isEmpty};
		// pat.postln;
		pat.do{ arg p;
			var fullMatch = p[0];
			// Convert abbreviation to long name (if one is found)
			var patternType = this.resolveAbbrev(p[1].last);
			var optVariableName = p[2].last;
			var patternString = p[3].last;
			var replaceStr;
			var replacements = [];
			var numAlternates = [];
			// Figure out the replacement string if there are any "alternating"
			// elements, written e.g. as "<1 2 3>" (which means 1 first time around,
			// 2 the second, and 3 the third). The total number of cycles
			// required to see everything in the pattern is the least common multiple
			// of all alternate counts.

			if (patternType == '@' and: { optVariableName.isEmpty }) {
				Error("Can't use default (non-variable) lookup with string pattern").throw;
			};

			// Replace rests
			// @todo Be more selective, allowing env. variables such as "[~foo <1 ~bar>]"
			if (optVariableName.isEmpty) {
				// In the case of variable/namespace lookup, then ~ is allowed;
				// it is interpreted as "Rest" by the PnsymRest class.
				patternString = patternString.replace("~", this.prGetRestString(patternType));
			} {
				if (currentEnvironment[optVariableName.asSymbol].isKindOf(Dictionary).not) {
					Error("'~%' lookup Dictionary not found".format(optVariableName)).throw;
				};
			};

			// First try to split on comma-separated elements in angle brackets (chords)
			this.findChord(patternString).reverseDo{ arg m;
				var alternateElements = BacalaoParser.splitSimplify(m[1], $,);
				if (alternateElements.size > 1) {
					// Replace Bacalao parser variables with their values
					if (optVariableName.isEmpty) {
						alternateElements = this.prResolveVariables(alternateElements, patternType, optVariableName);
					} {
						alternateElements = alternateElements.collect{ arg e;
							this.prVariableNameToString(e);
						};
					};

					patternString = patternString.replaceAt("ALTERNATE" ++ replacements.size.asPaddedString(4), m[0]-1, m[1].size+2);
					replacements = replacements.add("Ptuple([%])".format(alternateElements.join($,)));
				};
			};

			// Now try to split on space-separated elements in angle brackets (alternation)
			this.findAngleBracketed(patternString).reverseDo{ arg m;
				var alternateElements = BacalaoParser.splitSimplify(m[1]);

				alternateElements = alternateElements.collect{ arg elem;
					var elemMatch = this.findSimpleElem(elem);
					if (elemMatch.size == 1) {
						var full, elem, repeat, hold, dupl;
						if (elemMatch[0].size != 5) {
							Error("Unexpected match size: %".format(elemMatch[0])).throw
						};
						#full, elem, repeat, hold, dupl = elemMatch[0];
						if (elem.isEmpty) {
							Error("There is no array element: %".format(full)).throw
						};
						repeat = if (repeat.isEmpty) { 1 } { repeat.asInteger };
						hold = if (hold.isEmpty) { 1 } { hold.asFloat };
						// Note: we don't support Bjorklund e.g. (3,8) inside angle brackets
						dupl = if (dupl.isEmpty) { 1 } { dupl.asInteger };
						if (hold != 1 or: { repeat != 1 }) {
							Error("We don't support hold or repeat inside alternating <> elements, only '!' duplicate").throw
						};
						elem ! dupl;
					} {
						Error("Invalid alternate element entry: %".format(elem.cs)).throw;
					};
				}.flatten;

				// Replace Bacalao parser variables with their values
				if (optVariableName.isEmpty) {
					alternateElements = this.prResolveVariables(alternateElements, patternType, optVariableName);
				} {
					alternateElements = alternateElements.collect{ arg e;
						this.prVariableNameToString(e);
					};
				};
				numAlternates = numAlternates.add(alternateElements.size);
				patternString = patternString.replaceAt("ALTERNATE" ++ replacements.size.asPaddedString(4), m[0]-1, m[1].size+2);
				replacements = replacements.add("Ppatlace([%],inf)".format(alternateElements.join($,)));
			};

			{
				// var patternArray = this.splitSimplify(patternString.replace("~", this.prGetRestString(patternType)));
				var elemsAndDurs = [];
				var elems, durs, resolveChord;
				// Support "|" as bar-split symbol. For example, the following two
				// should be equivalent (but the first is easier to understand):
				//   deg"[1 2 3]@2 | 4 5 | 6*3" (play 1,2,3 over two bars, 4,5 over 1 and 6,6,6 over 1)
				//   deg"[[1 2 3]@2 [4 5] 6*3]@4" (same, but you need to figure out the total bars (4) yourself)
				patternString.split(barSplitChar).do{ arg patternString;
					elemsAndDurs = elemsAndDurs ++ {
						var patternArray = this.parseArray(patternString);
						if (patternArray.size > 1) {
							// "Wrapping top-level pattern array".postln;
							patternArray = [ patternArray -> 1];
						};
						this.calculateDurations(patternArray);
					}.value;
				};
				// Replace alphabetic-only strings by Symbol notation: 'symbol'
				// patternArray = patternArray.collect{ |p| "^[A-Za-z]+$".matchRegexp(p).if(p.asSymbol.cs, p) };
				#elems, durs = elemsAndDurs.flop;

				// Replace Bacalao parser variables with their values, unless we're
				// using an explicit "namespace" variable.
				if (optVariableName.isEmpty) {
					elems = this.prResolveVariables(elems, patternType, optVariableName);
				};

				// *Don't* convert strings to symbols here...this allows you to evaluate
				// variables (e.g. n = Pwhite(0,7,1); deg"n*4") and arbitrary code in patterns!
				// elems = elems.collect{ |elem| "^[A-Za-z]+$".matchRegexp(elem).if(elem.asSymbol.cs, elem) };
				if (elems.size > 1) {
					var numCyclesRequired = numAlternates.reduce(\lcm) ? 1.0;
					var longElemStr = if (optVariableName.isEmpty) {
						var variables = String.streamContents({ arg stream;
							elems.printOn(stream); });
						"Ppatlace(%, %)".format(variables, numCyclesRequired);
					} {
						var pnsym = if (patternType == '@') { "PnsymRestEvent" } { "PnsymRest" };
						"%(Ppatlace([%], %), ~%)".format(pnsym, elems.collect{|e|
							if (e.beginsWith("ALTERNATE"))
							{ e }
							{ this.prVariableNameToString(e) }
						}.join($,), numCyclesRequired, optVariableName);
					};
					var longDursStr = String.streamContents({ arg stream;
						durs.printOn(stream); });
					//var longDursStr = String.streamContents({ arg stream;
					//	durs.collect(_.asStringPrec(17)).printOn(stream); });
					if (patternType == '@') {
						replaceStr = "Pchain(Pbind('dur', Pseq(%, %)), Pn(%, %))".format(longDursStr, numCyclesRequired, longElemStr, numCyclesRequired);
					} {
						replaceStr = "Pbind('%', %, 'dur', Pseq(%, %))".format(patternType, longElemStr, longDursStr, numCyclesRequired);
					}
				} {
					var elemStr = if (optVariableName.isEmpty) {
						String.streamContents({ arg stream; elems[0].printOn(stream); });
					} {
						var pnsym = if (patternType == '@') { "PnsymRestEvent" } { "PnsymRest" };
						var e = this.prVariableNameToString(elems[0].value);
						"%(Pseq([%]), ~%)".format(pnsym, e, optVariableName);
					};
					if (durs[0] === 1) {
						// In the case of a single entry at the top level (and no array braces)
						// just return a single value repeatedly, with no duration.
						// If you want a single event that lasts one cycle, use
						// "[0]" or "0@1"
						if (patternType == '@') {
							replaceStr = "Pn(%)".format(elemStr);
						} {
							replaceStr = "Pbind('%', %)".format(patternType, elemStr);
						}
					} {
						if (patternType == '@') {
							replaceStr = "Pchain(Pbind('dur', Pn(%, 1)), Pn(%))".format(durs[0], elemStr);
						} {
							replaceStr = "Pbind('%', %, 'dur', Pn(%, 1))".format(patternType, elemStr, durs[0]);
						}
					}
				};

				case
				{ patternType == \midimod } {
					replaceStr = replaceStr.replace(patternType.cs, "'control'").drop(-1) ++ ", \\midicmd, \\control, \\ctlNum, 1)";
				}
				{ patternType == \midibreath } {
					replaceStr = replaceStr.replace(patternType.cs, "'control'").drop(-1) ++ ", \\midicmd, \\control, \\ctlNum, 2)";
				}
				{ patternType == \midifoot } {
					replaceStr = replaceStr.replace(patternType.cs, "'control'").drop(-1) ++ ", \\midicmd, \\control, \\ctlNum, 4)";
				}
				{ patternType == \midipedal } { // hold: on >= 64, off < 64
					replaceStr = replaceStr.replace(patternType.cs, "'control'").drop(-1) ++ ", \\midicmd, \\control, \\ctlNum, 64)";
				}
				{ patternType == \midireso } { // aka Timbre
					replaceStr = replaceStr.replace(patternType.cs, "'control'").drop(-1) ++ ", \\midicmd, \\control, \\ctlNum, 71)";
				}
				{ patternType == \midicut } { // aka Brightness
					replaceStr = replaceStr.replace(patternType.cs, "'control'").drop(-1) ++ ", \\midicmd, \\control, \\ctlNum, 74)";
				}
				{ patternType == \midibend } {
					// Expects 0-16383, where 8192 is "0" (no bending)
					replaceStr = replaceStr.replace(patternType.cs, "'val'").drop(-1) ++ ", \\midicmd, \\bend)";
				}
				{ patternType.asString.beginsWith("midicc") } {
					var ccNum = patternType.asString[6..].asInteger.debug("ccNum");
					replaceStr = replaceStr.replace(patternType.cs, "'control'").drop(-1) ++ ", \\midicmd, \\control, \\ctlNum, %)".format(ccNum);
				};

			}.value;
			// [curOffset, fullMatch.first, fullMatch.last, replaceStr].postln;
			(fullMatch.last -> replaceStr).postln;
			// [patternArray, replaceStr].postln;
			if (replaceStr.notNil) {
				replacements.reverseDo{ arg replacement, i;
					var index = replacements.size - 1 - i;
					replaceStr = replaceStr.replace("ALTERNATE" ++ index.asPaddedString(4), replacement);
				};
				code = code.replaceAt(replaceStr, fullMatch.first + curOffset, fullMatch.last.size);
				// code.postln;
				curOffset = curOffset + replaceStr.size - fullMatch.last.size
			}
		};
		^code
	}

	*prGetRest { arg patternType;
		^if (patternType == '@') {
			// These are Event patterns (the variable lookup expects an Event)
			(mask: Rest())
		} {
			// These are all the rest: simple value patterns
			Rest()
		}
	}

	*prGetRestString { arg patternType;
		// Must have no spaces!
		^this.prGetRest(patternType).cs.reject(_.isSpace)
	}

	*prEvalDefaultCharString { arg barString, patternType;
		var patternArray = barString.as(Array).collect{ arg ch;
			var asc = ch.ascii;
			var lowerCaseValue, upperCaseValue, stepFunc, zeroValue, zeroStepFunc;
			var result;
			#lowerCaseValue, upperCaseValue, stepFunc, zeroValue, zeroStepFunc = switch (patternType,
				'degree', [ 0, -14, (_ + 1) ],
				'note', [ 0, -24, (_ + 1) ],
				'midinote', [ 60, 36, (_ + 1) ],
				'freq', [ 60.midicps, 36.midicps, (_ * 1.midiratio) ],
				'amp', [ 26.reciprocal*0.5, 26.reciprocal, (_ + 26.reciprocal), 0, (_ + 10.reciprocal) ],
				'pan', [ -1, -1, (_ + 12.5.reciprocal), -1, (_ + 4.5.reciprocal) ],
				'@', { Error("Can't use default (non-variable) lookup with char string pattern").throw },
				[ 0, 0, (_ + 1) ] // Default values
			);
			result = case
			{ ch == Char.space } {
				this.prGetRest(patternType) }
			{ ch == $_ } { // Extend duration of previous note
				$_ }
			{ asc >= $a.ascii and: { asc <= $z.ascii } } {
				var v = lowerCaseValue;
				(asc - $a.ascii).do{ v = stepFunc.value(v) };
				v }
			{ asc >= $A.ascii and: { asc <= $Z.ascii } } {
				var v = upperCaseValue;
				(asc - $A.ascii).do{ v = stepFunc.value(v) };
				v }
			{ asc >= $0.ascii and: { asc <= $9.ascii } } {
				var v = (zeroValue ? lowerCaseValue);
				(asc - $0.ascii).do{ v = (zeroStepFunc ? stepFunc).value(v) };
				v }
			{ // Default case
				"Char pattern didn't recognize: '%' (ascii %) -- using Rest".format(ch, asc).warn;
				this.prGetRest(patternType)
			};
			result
		};
		^patternArray
	}

	*prEvalVariableCharString { arg barString, variableName;
		//var parserVariables = this.vars;
		var parserVariables = currentEnvironment;
		var lookup = parserVariables[variableName.asSymbol];
		if (lookup.isKindOf(Dictionary).not) {
			Error("'~%' lookup Dictionary not found".format(variableName)).throw;
		};
		^barString.as(Array).collect{ arg ch;
			case
			{ ch == $_ } { // Extend duration of previous note
				$_ }
			{ // Default case (return an Association for later lookup)
				variableName -> ch.asSymbol
			}
		}
	}

	*prParseCharArray { arg patternType, patternString, optVariableName;
		// Support $| as bar-split symbol
		var bars = patternString.split(barSplitChar);
		var eventsPerBar = nil; // if set to a number, use that fixed number of events per bar
		bars = bars.collect{ arg barString;
			var patternArray;
			{
				var match = barString.findRegexp(reCharEventsPerBar)[1];
				if (match.notNil) {
					eventsPerBar = match[1].asInteger;
					// If you use 0@, switch back to variable number of events per bar
					if (eventsPerBar < 1) { eventsPerBar = nil };
					// Get rid of the preix that includes "number@"
					barString = barString.drop(match[1].size + 1);
				};
			}.value;
			patternArray = if (optVariableName.notNil and: {optVariableName.isEmpty.not}) {
				this.prEvalVariableCharString(barString, optVariableName)
			} {
				this.prEvalDefaultCharString(barString, patternType)
			};
			patternArray = patternArray.separate{ arg a, b; (a != $_ && (b != $_)) or: { a == $_ && (b != $_) } }.collect{ arg elems; (elems[0] -> elems.size) };
			if (eventsPerBar.notNil) {
				// 8@0123456789 notation (eventsPerBar@events)
				// Bar markers sync with next bar
				var patternDur = patternArray.collect(_.value).sum;
				var nextBar = patternDur.roundUp(eventsPerBar);
				if (patternDur < nextBar) {
					patternArray = patternArray.add(this.prGetRest(patternType) -> (nextBar - patternDur));
				};
				(patternArray -> (nextBar / eventsPerBar))
			} {
				(patternArray -> 1)
			}
		};
		^bars.flatten
	}

	*prProcessCharPatterns { arg code;
		var curOffset = 0;
		var pat = code.findRegexp(charPattern).clump(4).reject{|m| m[1][1].isEmpty};
		// pat.postln;
		pat.do{ arg p;
			var fullMatch = p[0];
			// Convert abbreviation to long name (if one is found)
			var patternType = this.resolveAbbrev(p[1].last);
			var optVariableName = p[2].last;
			var patternString = p[3].last;
			var replaceStr;
			{
				var patternArray = this.prParseCharArray(patternType, patternString, optVariableName);
				var elemsAndDurs = {
					if (patternArray.size > 1) {
						var totalDur = patternArray.collect(_.value).sum;
						// "Wrapping top-level pattern array".postln;
						patternArray = [ patternArray -> totalDur];
					};
					this.calculateDurations(patternArray);
				}.value;
				var elems, durs;
				#elems, durs = elemsAndDurs.flop;

				if (elems.size > 1) {
					var elemSeq = elems.separate{ arg a, b;
						a.isKindOf(Association) != b.isKindOf(Association)
					}.collect{ arg elems;
						if (elems.first.isKindOf(Association)) {
							var pnsym = if (patternType == '@') { "PnsymRestEvent" } { "PnsymRest" };
							"%(Pseq(%), ~%)".format(pnsym, elems.collect{|e| e.value}.join.cs, elems.first.key);
						} {
							elems.join($,)
						}
					};

					var longElemStr = String.streamContents({ arg stream;
						elemSeq.printOn(stream); });
					var longDursStr = String.streamContents({ arg stream;
						durs.printOn(stream); });
					//var longDursStr = String.streamContents({ arg stream;
					//	durs.collect(_.asStringPrec(17)).printOn(stream); });
					var numCyclesRequired = 1.0;
					if (patternType == '@') {
						replaceStr = "Pchain(Pbind('dur', Pseq(%, %)), Pseq(%, %))".format(longDursStr, numCyclesRequired, longElemStr, numCyclesRequired);
					} {
						replaceStr = "Pbind('%', Pseq(%, %), 'dur', Pseq(%, %))".format(patternType, longElemStr, numCyclesRequired, longDursStr, numCyclesRequired);
					}
				} {
					if (elems[0].isKindOf(Association)) {
						var pnsym = if (patternType == '@') { "PnsymRestEvent" } { "PnsymRest" };
						elems[0] = "%(Pseq(%), ~%)".format(pnsym, elems.collect{|e| e.value}.join.cs, elems.first.key);
					};

					if (durs[0] === 1) {
						// In the case of a single entry at the top level (and no array braces)
						// just return a single value repeatedly, with no duration.
						// If you want a single event that lasts one cycle, use
						// "[0]" or "0@1"
						if (patternType == '@') {
							replaceStr = "Pn(%)".format(elems[0]);
						} {
							replaceStr = "Pbind('%', %)".format(patternType, elems[0]);
						}
					} {
						if (patternType == '@') {
							replaceStr = "Pchain(Pbind('dur', %), Pn(%))".format(durs[0], elems[0]);
						} {
							replaceStr = "Pbind('%', %, 'dur', %)".format(patternType, elems[0], durs[0]);
						}
					}
				};
			}.value;
			// [curOffset, fullMatch.first, fullMatch.last, replaceStr].postln;
			(fullMatch.last -> replaceStr).postln;
			// [patternArray, replaceStr].postln;
			if (replaceStr.notNil) {
				code = code.replaceAt(replaceStr, fullMatch.first + curOffset, fullMatch.last.size);
				// code.postln;
				curOffset = curOffset + replaceStr.size - fullMatch.last.size
			}
		};
		^code
	}

	*preProcess { arg code, interpreter;
		code = this.prProcessPatterns(code);
		code = this.prProcessCharPatterns(code);
		^code
	}

	*resolveAbbrev { arg name;
		var nameSymbol = name.asSymbol;
		var abbrevLookup = eventAbbrevs[nameSymbol];
		^abbrevLookup ? nameSymbol;
	}

	*findInt { arg str, offset=0;
		^str.findRegexp(numberInt, offset).flop[1] ?? #[]
	}

	*findFloat { arg str, offset=0;
		^str.findRegexp(numberFloat, offset).flop[1] ?? #[]
	}

	*findBalancedArray { arg str, offset=0;
		^(str.findRegexp(balancedArray, offset).flop[1] ?? #[]).clump(5)
	}

	*findArrayElem { arg str, offset=0;
		^(str.findRegexp(arrayElem, offset).flop[1] ?? #[]).clump(7)
	}

	*findSimpleElem { arg str, offset=0;
		^(str.findRegexp(simpleElem, offset).flop[1] ?? #[]).clump(5)
	}

	*findWord { arg str, offset=0;
		^str.findRegexp(word, offset).flop[1] ?? #[]
	}

	*findLabel { arg str, offset=0;
		^str.findRegexp(label, offset).flop[1] ?? #[]
	}

	// Return array of entries: [offset, matchStr] inside angled brackets:
	// <7 2> returns [ [ 1, "7 2" ] ]
	*findAngleBracketed { arg str, offset=0;
		^str.findRegexp(balancedAngleBracket, offset).clump(3).collect(_.drop(2)).flatten;
	}

	*findChord { arg str, offset=0;
		^str.findRegexp(chord, offset).clump(2).collect(_.drop(1)).flatten;
	}

	*findTrkName { arg str, offset=0;
		^(str.findRegexp(trkName, offset).flop[1] ?? #[]).clump(2)
	}

	*findSubCmd { arg str, offset=0;
		^(str.findRegexp(subCmd, offset).flop[1] ?? #[]).clump(3)
	}

	*findGlobalCmd { arg str, offset=0;
		^(str.findRegexp(globalCmd, offset).flop[1] ?? #[]).clump(3)
	}

	*getCmdArgs { arg cmd, argStr, failArgType, offset=0;
		var argArray;
		var argRegex = cmdArgs.atFail(cmd, failArgType);
		if (argRegex.notNil) {
			argArray = argStr.findRegexp(argRegex, offset).flop[1];
		}
		^argArray ? #[]
	}
	*splitSimplify { arg str, splitChar = $ ;
		^str.split(splitChar).collect(_.trim).reject(_.isEmpty);
	}

	*evaluateNumberIfPossible { arg str;
		if (this.numericExpression.matchRegexp(str)) {
			// Only call "interpret" if formula-like characters are there;
			// we don't want to run it on just *any* code!
			^str.interpret
		} {
			^nil
		}
	}

	*evaluateNote { arg strAndDur;
		var repeatSplit, durSplit;
		var str = strAndDur[0];
		var relativeDur = strAndDur[1];
		^[this.evaluateNumberIfPossible(str) ?? { Rest.new }, relativeDur]
	}

	*getFloatArg { arg str;
		^str !? { this.evaluateNumberIfPossible(str) }
	}

	*getSymbolArg { arg str;
		^str !? { str.asSymbol }
	}

	*prGetRepeatsArray { arg repeatStr;
		^case
		{ repeatStr.isEmpty } {
			#[1]
		}
		{ repeatStr.first == $( } {
			var k, n, shift;
			var trimmed = repeatStr.trim("()");
			// We provide two options:
			// "(3,8,1)" will give a Euclidean sequence of uniform durations with interspersed Rests
			//   [ Rest(1), Rest(1), 1, Rest(1), Rest(1), 1, Rest(1), 1 ]
			// "(x3,8,1)" will give a Euclidean sequence of extended durations (after optional initial Rest)
			//   [ Rest(2), 3, 2, 1 ]
			if (trimmed.first == $x) {
				trimmed = trimmed.drop(1);
				#k, n, shift = trimmed.split($,).asInteger;
				shift = shift ? 0;
				// This is (almost) equivalent to Bjorklund2(k, n).rotate(-1 * shift),
				// except that we add an initial Rest when required to support rotating
				// through all the possible 'n' positions (not just 'k' positions).
				Bjorklund(k, n).rotate(-1 * shift).integrate.separate{ |a,b| a != b }.collect{ arg x;
					if (x.first == 0) {
						Rest(x.size)
					} {
						x.size
					}
				}
			} {
				#k, n, shift = trimmed.split($,).asInteger;
				shift = shift ? 0;
				Bjorklund(k, n).rotate(-1 * shift).collect{ arg x; if (x == 0) { Rest(1) } { x } }
			};
		}
		{
			1 ! repeatStr.asInteger
		}
	}

	// Input:
	//   A string containing an array, possibly including repeat multipliers
	//   and hold elements, and sub-arrays.
	//   See: https://tidalcycles.org/index.php/Sequence_parser_syntax
	//
	//   [ ] pattern grouping
	//   ~ rest
	//   * repeat pattern
	//   @ hold/elongate a pattern with duration
	//
	// Not yet implemented:
	//   , parallel patterns
	//   / slow down a pattern
	//   < > alternate between patterns (Place)
	//   ! replicate a pattern
	//   _ elongate a pattern
	//   ? randomly remove events from pattern
	//   : select samples
	//   ( ) Bjorklund sequences
	//
	//   For example:
	//     BacalaoParser.parseArray("[1 2@2 3*2 [-4 [5 -6]] 7]")
	//
	// Output:
	//   A hierarchical Array in which each element is an Association with the
	//   "event" (note) followed by its relative duration (at its level). For the
	//   example above:
	//     [ ([ (1 -> 1), (2 -> 2.0), ([ (3 -> 1), (3 -> 1) ] -> 1),
	//       ([ (-4 -> 1), ([ (5 -> 1), (-6 -> 1) ] -> 1) ] -> 1),
	//       (7 -> 1) ] -> 1) ]
	*parseArray { |str|
		var arr = [];
		var arrItems = this.findArrayElem(str);
		arrItems.do{ |item|
			var full, elem, subArr, subArrRepeat, subArrHold, subArrDupl, invalidElem;
			// item.debug("item");
			#full, elem, subArr, subArrRepeat, subArrHold, subArrDupl, invalidElem = item;
			// elem.debug("elem");
			if (elem.notEmpty) {
				var elemMatch = this.findSimpleElem(elem);
				// elemMatch.debug("elemMatch");
				if (elemMatch.size == 1) {
					var full, elem, repeat, hold, dupl;
					if (elemMatch[0].size != 5) {
						Error("Unexpected match size: %".format(elemMatch[0])).throw
					};
					#full, elem, repeat, hold, dupl = elemMatch[0];
					if (elem.isEmpty) {
						Error("There is no array element: %".format(full)).throw
					};
					repeat = this.prGetRepeatsArray(repeat);
					hold = if (hold.isEmpty) { 1 } { hold.asFloat };
					dupl = this.prGetRepeatsArray(dupl);
					// "Adding a result: %".format(elem).postln;
					if (repeat != #[1]) {
						elem = repeat.collect{ arg r; if (r.isRest) { "Rest()" }{ elem } -> r.value };
					};
					dupl.do{ arg d;
						arr = arr.add(if (d.isRest) { "Rest()" }{ elem } -> (hold * d.value));
					};
				} {
					Error("Invalid array entry: %".format(elem.cs)).throw;
				}
			} {
				var repeat = this.prGetRepeatsArray(subArrRepeat);
				var hold = if (subArrHold.isEmpty) { 1 } { subArrHold.asFloat };
				var dupl = this.prGetRepeatsArray(subArrDupl);
				var subArrResult;
				subArr = subArr.drop(1).drop(-1); // get rid of outer brackets
				// if (verbose) { "Will parseArray recursively on %".format(subArr).postln };
				subArrResult = this.parseArray(subArr);
				if (repeat != #[1]) {
					subArrResult = repeat.collect{ arg r; if (r.isRest) { "Rest()" }{ subArrResult } -> r.value };
				};
				dupl.do{ arg d;
					arr = arr.add(if (d.isRest) { "Rest()" }{ subArrResult } -> (hold * d.value));
				};
			}
		};
		// if (verbose) { "Returning %".format(arr).postln };
		^arr
	}


	// Adjust event/durations (produced by calculateDurations) so that when
	// their cumulative duration is very close to an integer (bar), the
	// previous duration is adjusted so the sum is exactly on the bar.
	// e.g. ((1/6)!6).sum - 1 is not exactly 0
	*prAdjustDurations { arg eventAndDurationArray;
		var runningSum = 0;
		^eventAndDurationArray.collect{ arg eventAndDur;
			var distanceToInteger;
			var dur = eventAndDur[1];
			runningSum = runningSum + dur;
			distanceToInteger = runningSum.round - runningSum;
			if (distanceToInteger.abs < 1e-9 and: { distanceToInteger.abs > 0 }) {
				var adjusted = dur + distanceToInteger;
				"Adjusting dur % to %".format(dur.asFloat.asStringPrec(17), adjusted.asStringPrec(17)).postln;
				dur = adjusted;
				runningSum = 0;
			};
			[ eventAndDur[0], dur ]
		}
	}

	// It's about twice as fast to use an Array rather than List with default sizes
	// { 100000.do{ var r = List(); 20.do{ |i| r.add(i) } } }.bench
	// { 100000.do{ var r = []; 20.do{ |i| r = r.add(i) } } }.bench

	// Input:
	//   An Array which is the output of parseArray, that is, a hierarchical
	//   set of Arrays representing the desired sub-phrasing.  Each sub-element
	//   already has a matching "relative" duration.
	//   For example:
	//     BacalaoParser.calculateDurations(BacalaoParser.parseArray("[1 2 [3 [4 5] [6 7 8]]]"))
	//   gives:
	//     [ ([ (1 -> 1), (2 -> 1),
	//       ([ (3 -> 1), ([ (4 -> 1), (5 -> 1) ] -> 1),
	//       ([ (6 -> 1), (7 -> 1), (8 -> 1) ] -> 1) ] -> 1) ] -> 1) ]
	//
	// Output:
	//   An Array in which each element is a two-element Array with the "event" (note)
	//   followed by its relative duration. For the example above:
	//     [ [ 1, 0.33333333333333 ], [ 2, 0.33333333333333 ], [ 3, 0.11111111111111 ],
	//       [ 4, 0.055555555555556 ], [ 5, 0.055555555555556 ],
	//       [ 6, 0.037037037037037 ], [ 7, 0.037037037037037 ], [ 8, 0.037037037037037 ] ]
	*calculateDurations { arg arr, duration = 1;
		var result = [];
		arr.do{ |item|
			var elem = item.key;
			var hold = item.value;
			// "elem % hold %".format(elem.cs, hold.cs).postln;
			if (elem.isArray and: {elem[0].isKindOf(Association)}) {
				// This means it's a sub-group, so recurse
				var cleaned = elem.reject{ |a| a.key.isCollection and: { a.key.isEmpty } };
				if (cleaned.notEmpty) {
					var subDur = cleaned.collect(_.value).sum;
					// "Recurring on % with duration % and size % (subDur %)".format(
					//      cleaned.cs,
					//      duration,
					//      cleaned.size,
					// subDur).postln;
					this.calculateDurations(cleaned, duration * hold / subDur).do{ |x|
						result = result.add(x)
					}
				}
			} {
				// This means it's a normal entry, so just return it with its duration
				if (elem.notNil and: {elem.isCollection.not or: {elem.isEmpty.not}}) {
					result = result.add([elem, duration * hold])
				}
			};
		};
		^result
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

	*range { arg periodBars=1, phase=0, lo = -1.0, hi=1.0, repeats=inf;
		^Psine.new(periodBars, phase, 1, 0, repeats).linlin(-1,1, lo,hi);
	}

	*exprange { arg periodBars=1, phase=0, lo = 0.01, hi=1.0, repeats=inf;
		^Psine.new(periodBars, phase, 1, 0, repeats).linexp(-1,1, lo,hi);
	}
}

Psaw {
	*new { arg periodBars=1, phase=0, mul=1, add=0, repeats=inf;
		// We don't use Ptime (except as fallback when a 'time' key isn't there)
		// so we get elapsed time for the "overall" pattern, not sub-patterns.
		var tPattern = Pif(Pfunc{|ev| ev[\time].notNil }, Pn(Pkey(\time), repeats), Ptime(repeats));
		^((tPattern / Pfunc{thisThread.clock.beatsPerBar} + phase).mod(periodBars) / periodBars * 2 - 1) * mul + add;
	}

	*range { arg periodBars=1, phase=0, lo = -1.0, hi=1.0, repeats=inf;
		^Psaw.new(periodBars, phase, 1, 0, repeats).linlin(-1,1, lo,hi);
	}

	*exprange { arg periodBars=1, phase=0, lo = 0.01, hi=1.0, repeats=inf;
		^Psaw.new(periodBars, phase, 1, 0, repeats).linexp(-1,1, lo,hi);
	}
}

// An Event Pattern (Pbind) helper with random exponential values on the amp
PampRand {
	*new { arg lo = 0.2, hi = 0.8, randSeed;
		^Pbind(\amp, Per(lo, hi, randSeed));
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

+Pbind {
	// Overriding because this "innocent" typo produces a sclang freeze (if you play a Pbind().degrad)!
	degrad {
		"Probably a typo...'degrad' rather than degrade?".postln;
		^this;
	}
}
