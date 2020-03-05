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

		Bacalao.config();
		^super.newCopyArgs(clock, server ? Server.default, verbose ? true, quant, ()).initLibrary.start.prSetupCmdPeriod.push;
	}

	*config {
		var chords = (I: [0,2,4],
			ii: [1,3,5],
			iii: [2,4,6],
			IV: [3,5,7],
			V: [4,6,8],
			vi: [5,7,9],
			viio: [6,8,10]
		);
		chords.keysValuesDo{ arg k, v;
			Bacalao.setParserVariable(k, [v, v.rotate(-1)+[0,0,7], v.rotate(-2)+[-7,0,0]]);
		}
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
		if (this.prVSTPluginInstalled) {
			SynthDef(\bacalao_vsti, {arg out = 0, pan = 0, gate=1, fadeTime=1, bypass=0;
				// VST instruments usually don't have inputs
				var vsti = VSTPlugin.ar(nil, numChannels, bypass: bypass);
				var env = EnvGen.kr(Env.asr(fadeTime,1,fadeTime), gate, doneAction: Done.freeSelf);
				Out.ar(out, Balance2.ar(vsti.first, vsti.last, pan) * env);
			}).add;
		};

		SynthDef(\sample1, { arg out=0, buf, rate=1, amp=0.1, pan=0, start=0.0, length=1.0;
			var sig, env;
			sig = PlayBuf.ar(1, buf, rate * BufRateScale.kr(buf), 1, start * SampleRate.ir, 1);
			env = EnvGen.ar(Env.linen(0.01, length, 0.01), doneAction: 2);
			sig = sig * env;
			Out.ar(out, Pan2.ar(sig, pan, amp));
		}).add;

		SynthDef(\sample2, { arg out=0, buf, rate=1, amp=0.1, pan=0, start=0.0, length=1.0;
			var sig, env;
			sig = PlayBuf.ar(2, buf, rate * BufRateScale.kr(buf), 1, start * SampleRate.ir, 1);
			env = EnvGen.ar(Env.linen(0.01, length, 0.01), doneAction: 2);
			sig = sig * env;
			Out.ar(out, Balance2.ar(sig.first, sig.last, pan, amp));
		}).add;
	}

	// This sets up automatic substitutions (like variables) that may be used
	// by the preProcessor.
	// These may be single values, but they may also be arrays or Dictionaries,
	// in which case the variable:key syntax may be used to access them. This
	// lets you define multiple values for a variable, like:
	//   b.setParserVariable('bd', [36, 37, 48, 49]);
	//   note"bd:3" --> would produce Pbind(\note, 49)
	*setParserVariable { arg key, values;
		if (topEnvironment[preProcessorVariables].isKindOf(Environment).not) {
			"Defining new parser variable Environment".postln;
			topEnvironment[preProcessorVariables] = Environment();
		};
		topEnvironment[preProcessorVariables][key.asSymbol] = values;
	}

	setParserVariable { arg key, values;
		Bacalao.setParserVariable(key, values);
	}

	// This allows the key alone or with a colon to sub-index (e.g. 'bd:3')
	*lookupVariable { arg key;
		var parserVariables = topEnvironment[preProcessorVariables];
		var elem = key.asString;
		^parserVariables !? {
			var variable, index, substitute;
			// Allow (e.g.) "bd:5"-style indexing, or simply "bd" (equivalent to "bd:0")
			// Also allow "bd:r", which will choose a random value from the collection.
			#variable, index = elem.split($:);
			substitute = parserVariables[variable.asSymbol];
			substitute !? {
				if (index.notNil) {
					if (index == "r" and: { substitute.size > 1 }) {
						Prand(substitute.asArray,inf)
					} {
						substitute.asArray.wrapAt(index.asInteger)
					}
				} {
					substitute.first
				}
			}
		}
	}

	lookupVariable { arg key;
		^Bacalao.lookupVariable(key);
	}

	*printVariables {
		var parserVariables = topEnvironment[preProcessorVariables];
		var buffers = ();
		var index = 0;
		"----- variables:".post;
		parserVariables.sortedKeysValuesDo{ arg k, v;
			if (v.isKindOf(Buffer) or: { v.first.isKindOf(Buffer) }) {
				buffers[k] = v;
			} {
				var suffix = if (v.size > 0) { ":% ".format(v.size) } { " " };
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

	printVariables {
		Bacalao.printVariables();
	}

	push {
		if (currentEnvironment != topEnvironment[preProcessorVariables]) {
			topEnvironment[preProcessorVariables].push;
		}
	}

	pop {
		if (currentEnvironment == topEnvironment[preProcessorVariables]) {
			topEnvironment[preProcessorVariables].pop;
		}
	}

	initLibrary {

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

	gui {
		var w = Window().bounds_(Rect(1310, 100, 450, 900)).front.alwaysOnTop_(true);
		var namesAndProxies = vstDict.collect(_.last) ++ (Ndef.all[server.name] ?? ());
		w.addFlowLayout;
		namesAndProxies.keysValuesDo{ arg name, nd;
			if (nd.numChannels.notNil) {
				StaticText(w, 50@20).string_(name);
				NdefGui(nd, 5, w, options: NdefGui.big).nameView(name)
			}
		};
		^w;
	}

	// Play a pattern once on the named NodeProxy.
	once { arg defName, pattern, quant;
		var slot = nil;
		if (defName.isKindOf(Association)) {
			#defName, slot = [defName.key, defName.value];
		};
		this.prChangePattern(defName.asSymbol, slot, pattern, nil, quant);
	}

	// Could be considered "pattern" or "play"... Set a looping pattern playing
	// on the named NodeProxy.
	// Duration of 0 means use "natural" loop time, whatever the pattern produces
	// Durations > 0 will truncate or extend the pattern as needed to produce
	// exactly the requested duration. By default (if quant is unspecified) the
	// quantization will be a multiple of the duration ([dur, 0]).
	p { arg defName, pattern, dur=0, quant, role;
		var slot;
		if (defName.isKindOf(Association)) {
			#defName, slot = [defName.key, defName.value];
		};
		this.prChangePattern(defName.asSymbol, slot, pattern, dur, quant, role);
	}

	set { arg defName, control, valueOrFunc;
		var np = this.proxy(defName);
		var value = valueOrFunc.isKindOf(Function).if{ NodeProxy(server).source_(valueOrFunc) } { valueOrFunc };
		np.set(control, value);
	}

	pset { arg defName, pattern, dur=0, quant;
		this.p(defName, pattern, dur, quant, \pset);
	}

	xset { arg defName, pattern, dur=0, quant;
		this.p(defName, pattern, dur, quant, \xset);
	}

	// Set the playback volume of the playing NodeProxy.
	// e.g. b.db('drum', -12);
	db { arg defName, db = 0;
		var np = this.proxy(defName);
		np.vol = db.dbamp;
	}

	sched { arg event, quant = 1;
		clock.schedAbs((quant * clock.beatsPerBar).nextTimeOnGrid(clock), { arg ...args; event.value(*args); nil });
	}

	// Add a filter (needs array index > 0) to modify the NodeProxy
	// e.g. b.fx(\drum -> 3, { arg in; JPverb.ar(in, 3) }, 0.3);
	// You can clear fx in a slot with: b.fx(\drum -> 3);
	fx { arg defNameAndIndex, filterFunc, wet=1;
		var defName, index;
		if (defNameAndIndex.isKindOf(Association).not) {
			"defName and index should be an Association, e.g: b.fx(\\drum -> 10, ...)".error;
			^this;
		};
		#defName, index = [defNameAndIndex.key, defNameAndIndex.value];
		if (index.isInteger and: {index > 0}) {
			var np = this.proxy(defName);
			np[index] = filterFunc !? {\filter -> filterFunc};
			np.set((\wet.asString ++ index).asSymbol, wet);
		} {
			"fx index should be an integer greater than 0...skipping".warn;
		}
	}

	fxClear { arg defName;
		var np = this.proxy(defName);
		np.objects.indices[1..].do(np[_] = nil);
	}

	// Chop a Buffer into a number of pieces, returning a Pbind with the appropriate
	// start, length and rate parameters (appropriate for use with Bacalao patterns).
	// If you leave desiredBars and/or desiredRate unspecified, values will be
	// calculated to round to the nearest bar/cycle.
	chop { arg bufOrName, pieces = 8, desiredBars, desiredRate, inst;
		var buf = if (bufOrName.isKindOf(Buffer)) {
			bufOrName
		} {
			var lookup = Bacalao.lookupVariable(bufOrName);
			if (lookup.isKindOf(Buffer).not) { Error("chop variable lookup didn't find a Buffer").throw };
			lookup
		};
		var barDur = clock.beatsPerBar / clock.tempo;
		var bars = desiredBars ?? { (buf.duration / barDur).max(0.5).round.debug("chop calculated bars") };
		var origGrainDur = buf.duration / pieces;
		var starts = (0..pieces-1) * origGrainDur;
		var cycleDur = bars * clock.beatsPerBar / clock.tempo;
		var rate = desiredRate ?? { (buf.duration / cycleDur).debug("chop calculated rate") };
		inst = inst ?? { switch (buf.numChannels,
			1, { \sample1 },
			2, { \sample2 },
			{ \sample2 })
		};
		// Here, the 'dur' should be in cycles, not beats
		^Pbind(\instrument, inst, \buf, buf, \length, origGrainDur/rate, \rate, rate, \start, Pseq(starts), \dur, (1/pieces), \stretch, bars);
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
		this.prVstPrint("VST Instruments", _.isSynth, onlyWithPresets, extraVstPluginSearchPath);
	}

	vstPrintEffects { arg onlyWithPresets = true, extraVstPluginSearchPath = "C:/Program Files/Native Instruments/VSTPlugins 64 bit";
		this.prVstPrint("VST Effects", _.isSynth.not, onlyWithPresets, extraVstPluginSearchPath);
	}


	vstInit { arg defName, vstName, programPath, bankAndProgram, extraVstPluginSearchPath = "C:/Program Files/Native Instruments/VSTPlugins 64 bit";
		if (Bacalao.prVSTPluginInstalled.not) { ^this };
		//programPath = programPath !? { VSTPlugin.prResolvePath(programPath, false).postln };
		server.waitForBoot{
			var vstController;
			var vstProxy;
			var dictEntry;
			defName = defName.asSymbol;
			if (VSTPlugin.plugins.isEmpty) {
				VSTPlugin.search(server, extraVstPluginSearchPath);
				server.sync;
			};
			dictEntry = vstDict[defName];
			if (dictEntry.notNil) {
				#vstController, vstProxy = dictEntry;
			};
			vstProxy = vstProxy ?? { NodeProxy(server).clock_(clock) };
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
						VSTPluginController(synth).debug("New '%'".format(defName))
					};
					vstDict[defName] = [ vstController, vstProxy ];
					vstController.open(vstName, editor: true, action: {
						"Opened VSTPluginController for % with %".format(defName, vstName).postln;
						if (programPath.notNil) {
							if (bankAndProgram.notNil) {
								"Both programPath and bankAndProgram were specified...only using programPath".warn;
							};
							this.prVstRead(vstController, programPath);
						} {
							if (bankAndProgram.isKindOf(Association)) {
								this.vstBankProgram(defName, bankAndProgram.key, bankAndProgram.value);
							} {
								if (bankAndProgram.notNil) {
									"Expected bankAndProgram to be an Association (bank -> program), e.g. (3 -> 22)".warn;
								}
							}
						}
					});
				});
				server.listSendBundle(server.latency, bundle);
			});
		}
	}

	// free does clear plus removes the VST instrument (if there is one)
	free { arg defName, fadeTime=0;
		var vstCtl;
		this.clear(defName = defName.asSymbol, fadeTime);

		vstCtl = this.vst(defName);
		if (vstCtl.notNil) {
			var extraTime = 1;
			var vstProxy = this.proxy(defName);
			// Remove the dictionary entry now, so any future vstInit
			// will create a new VST instrument
			vstDict[defName] = nil;

			// Don't schedule on the Bacalao clock, because fadeTime should be in "wall" clock time
			SystemClock.sched(fadeTime ? 0 + server.latency + extraTime, {
				"Freeing VST %".format(defName).postln;
				vstProxy.clear();
				vstCtl.close;
			});
		}
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

	vstRead { arg defName, programPath;
		if (vstDict[defName = defName.asSymbol].notNil) {
			this.prVstRead(vstDict[defName].first.debug("vstCtl in vstRead"), programPath);
		} {
			"vstRead: unrecognized definition".warn;
		}
	}

	vstPresetDir { arg defName, type = \user;
		var vst = this.vst(defName.asSymbol);
		^vst !? { vst.info.presetFolder(type) };
	}

	proxy { arg defName;
		var vst = this.vst(defName = defName.asSymbol);
		^if (vst.notNil) {
			vstDict.at(defName).last
		} {
			Ndef(defName -> server.name)
		};
	}

	vst { arg defName;
		var dictEntry = vstDict.at(defName.asSymbol);
		^dictEntry !? { dictEntry.first };
	}

	// Can send bank and program as separate arguments, or
	// a single Association (bank -> program) to the bank argument.
	vstBankProgram { arg defName, bank, program;
		var vst = this.vst(defName);
		if (bank.isKindOf(Association)) {
			#bank, program = [bank.key, bank.value];
		};
		if (vst.notNil and: { vst.midi.notNil }) {
			var midi = vst.midi;
			var bankMsb = (bank - 1) div: 128;
			var bankLsb = (bank - 1) mod: 128;
			midi.control(0, 0, 0);
			midi.control(0, 32, bank-1);
			midi.program(0, program-1);
		} {
			"VST instrument '%' is not defined".format(defName).postln;
		}
	}

	clear { arg defName, fadeTime=1;
		// Clear an existing Ndef (and ignore everything else)
		var vst = this.vst(defName = defName.asSymbol);
		var ndef = this.proxy(defName);
		if (verbose) {
			"Clearing Ndef(%) (fade %)".format(defName, fadeTime).postln;
		};

		// Note that if we called ndef.clear(fadeTime) here,
		// the monitor volume would jump to full before fading out.
		// So we call stop() (which doesn't change settings)
		// and then schedule clear after fadeTime has passed.
		ndef.stop(fadeTime);

		// Don't schedule on the Bacalao clock, because fadeTime should be in "wall" clock time
		SystemClock.sched(fadeTime ? 0 + server.latency, {
			this.prClearOtherPatternSlots(defName);
			this.prRemoveUnusedPatternSlots(defName);
			if (vst.notNil) {
				// For VST instruments, we don't want to ever clear/free the
				// underlying NodeProxy, otherwise we'd have to reload and
				// reconfigure the VST (expensive)!

				// But we do want to clear effects from the VST proxy
				// and reset the playback volume.
				this.fxClear(defName);
				this.db(defName, 0);

				// Perform a soft bypass (it waits for sound to tail out, then stops processing to save CPU)
				ndef.set(\bypass, 2);
			} {
				// For regular (SC Synth-sourced) proxies, we can go ahead
				// and get rid of the whole thing.
				ndef.clear();
			};
			nil
		});
	}

	removeSlots { arg defName, indices;
		indices.asArray.do{ arg i;
			this.p(defName -> i, nil);
		}
	}

	// Return all the Pdefs that we use for "slots" with a given pattern def
	// (we use name mangling for this).
	prGetPdefSlots { arg defName;
		^Pdef.all.select{ arg x;
			"%\\/(nil|\\d+)\\/%".format(defName, server.name).matchRegexp(x.key.asString)
		}
	}

	// Clear just sets the source to nil, relying on the Quant to say when it should go away.
	prClearOtherPatternSlots { arg defName, except;
		// "Clear '%' patterns for other slots".format(defName).postln;
		this.prGetPdefSlots(defName).do{ arg pdef;
			if (pdef != except) {
				pdef.debug("stopping").source = nil;
			}
		}
	}

	// Remove actually removes the Pdef if it's not set to any source at the moment
	prRemoveUnusedPatternSlots { arg defName;
		// "Removing unused '%' slot patterns".format(defName).postln;
		this.prGetPdefSlots(defName).do{ arg pdef;
			if (pdef.source.isNil) {
				var slotNum = pdef.key.asString.drop(defName.asString.size + 1).asInteger;
				pdef.debug("'" ++ defName ++ "' source (slot " ++ slotNum ++ ") still nil...removing");
				pdef.remove;
				if (this.vst(defName).isNil) {
					Ndef(defName).debug("Removing Ndef slot").removeAt(slotNum, 0);
				}
			}
		}
	}

	prChangePattern { arg defName, slot, pattern, loopDur, patternQuant, role;
		// Pdef doesn't have a per-Server ProxySpace, so
		// we make up a "unique" name by combining the def name
		// and this instance's Server name.
		var replacePatterns = slot.isNil;
		// Note we don't use +/+ here for cross-platform reasons..we always want joining with '/'
		var pdefName = (defName.asString ++ $/ ++ (slot ? 0) ++ $/ ++ server.name).asSymbol.debug("pdefName");
		var pdef, ndef, vst = this.vst(defName = defName.asSymbol);

		// Stretch it so the durations in bars are converted to beats on our clock
		if (pattern.notNil) {
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
		ndef = this.proxy(defName).clock_(clock).quant_(patternQuant);
		if (replacePatterns) {
			// Set the source of other slots to nil (uses current Quant)
			this.prClearOtherPatternSlots(defName, except: pdef);

			// Remove unused ones a bit later (when sources are still nil)
			pdef.sched{
				this.prRemoveUnusedPatternSlots(defName)
			};
		};
		if (vst.notNil and: { role.isNil }) {
			pdef.source = pattern <> (type: \vst_midi, vst: vst);
			pdef.play;
			// Explicitly resume processing the Synth in case it's bypassed
			ndef.set(\bypass, 0);
		} {
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
						"Setting Ndef(%) source (%) to Pdef(%)".format(defName,
							slot ? 0, pdefName).postln
					};
					ndef[slot ? 0] = pdef;
				};

				// Just setting the Pdef source here works properly with
				// quantization, after the fix from:
				// https://github.com/supercollider/supercollider/pull/4779
				pdef.source = pattern;
			};
		};
		if (ndef.isMonitoring.not) {
			if (verbose) {
				"Playing Ndef(%)".format(defName).postln
			};
			ndef.play;
		}
	}
}


// This helper class implements all the parsing and data extraction helpers
// that are used by the main Bacalao class.
BacalaoParser {
	classvar eventAbbrevs;
	const unsignedInt = "\\d+";
	const eventPattern = "(?:[^a-z]*|(\\b[a-z][a-zA-Z0-9]*))\"([^\"\\n]*)\"";
	const <charPattern = "(?:[^a-z]*|(\\b[a-z][a-zA-Z0-9]*))(?:~([a-z][_a-zA-Z0-9]*))?'([^'\\n]*)'";
	classvar numberInt;
	const unsignedFloat = "(?:(?:[0-9]+)?\\.)?[0-9]+";
	const nonArraySpace = "[^[:space:]\\][]+";
	const elemWithoutMods = "[^[:space:]\\][@*]+";
	const balancedAngleBracket = "(<((?>[^><]|(?1))*)>)";
	const chord = "<([^>< ]+)>";
	classvar <reCharEventsPerBar;
	classvar numberFloat;
	const rest = "~";
	classvar elemModifiers;
	classvar balancedArray;
	classvar <patternValueArg; // balancedArray or numberWithMods
	classvar arrayElem;
	classvar simpleElemPartial;
	classvar simpleElem;
	const word = "\\b[[:alpha:]_]\\w*\\b";
	const label = "(?:[[:alnum:]]+)";
	// [ overallMatch, defName ]
	const defName = "^\\s*\\b(\\w+)\\s*:";
	// [ overallMatch, cmd, argStr ]
	const subCmd = "[:&]\\s*(\\w+)([^:&]*)";
	const <numericExpression = "[0-9./*+()-]+";
	classvar globalCmd;
	classvar instDefName;
	classvar midiInstDefCmd;
	classvar cmdArgs;

	*initClass {
		var number;
		var numberWithMods;
		var labelWithMods;
		var elemWithMods;
		var arrayWithMods;
		numberInt = "-?" ++ unsignedInt;
		reCharEventsPerBar = "^(" ++ unsignedInt ++ ")@";
		numberFloat = "-?" ++ unsignedFloat;
		// elemModifiers must be in the correct order (optional repeat, then optional hold)
		elemModifiers = "(?:\\*(" ++ unsignedInt ++ "))?(?:@(" ++ unsignedFloat ++ "))?";
		// [ overallMatch, balancedArray, arrayRepeat, arrayHold ]
		balancedArray = "(?:(" ++ "\\[(?>[^][]|(?1))*\\])" ++ elemModifiers ++ ")";
		number = "(?:" ++ numberFloat ++ ")|(?:" ++ rest ++ ")";
		numberWithMods = "(?:(" ++ number ++ ")" ++ elemModifiers ++ ")";
		labelWithMods = "(?:(" ++ label ++ ")" ++ elemModifiers ++ ")";
		elemWithMods = "(?:(" ++ elemWithoutMods ++ ")" ++ elemModifiers ++ ")";
		// [ overallMatch, simpleElem, arrayElem, arrayRepeat, arrayHold, unrecognized ]
		arrayElem = "(" ++ nonArraySpace ++ ")" ++ "|(?:(" ++ "[[](?>[^\][]|(?2))*[\]])" ++ elemModifiers ++ ")|([^[:space:]]+)";
		// [ overallMatch, num, numRepeat, numHold, labelElem, labelRepeat, labelHold ]
		simpleElemPartial = "(?:(?:" ++ numberWithMods ++ ")|(?:" ++ labelWithMods ++ "))";
		// [ overallMatch, elem, repeat, hold ]
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
		instDefName = "^\\s*inst\\s*\\b(\\w+)\\s*:";
		midiInstDefCmd = instDefName ++ "\\s*midi\\(\\s*([^,]*),\\s*([^,]*),\\s*([^,]*)\\)\\s*$";

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
			sus: \sustain,
			vel: \velocity,
			slow: \stretch,
			str: \stretch,
			toff: \timingOffset,
		);
		// Other event members without abbreviations
		// root: 0, stepsPerOctave: 12, octaveRatio: 2.0, note: (not including root or octave),
		// freq, tempo, dur: 1.0, stretch: 1.0, lag: 0.0, strum: 0.0, strumEndsTogether: false
		// amp, db: -20.0, pan: 0.0, trig: 0.5, group, out: 0, addAction: 0, variant: nil,
		// timingOffset: 0, type: (e.g. \midi), latency: 0.2,

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

	*prProcessPatterns { arg code;
		var curOffset = 0;
		var pat = code.findRegexp(eventPattern).clump(3).reject{|m| m[1][1].isEmpty};
		// pat.postln;
		pat.do{ arg p;
			var fullMatch = p[0];
			// Convert abbreviation to long name (if one is found)
			var patternType = this.resolveAbbrev(p[1].last);
			var patternString = p[2].last;
			var replaceStr;
			var replacements = [];
			var numAlternates = [];
			// Figure out the replacement string if there are any "alternating"
			// elements, written e.g. as "<1 2 3>" (which means 1 first time around,
			// 2 the second, and 3 the third). The total number of cycles
			// required to see everything in the pattern is the least common multiple
			// of all alternate counts.

			// Replace rests
			// @todo Be more selective, allowing env. variables such as "[~foo <1 ~bar>]"
			patternString = patternString.replace("~", "Rest()");

			// First try to split on comma-separated elements in angle brackets (chords)
			this.findChord(patternString).reverseDo{ arg m;
				var alternateElements = BacalaoParser.splitSimplify(m[1], $,);
				if (alternateElements.size > 1) {
					patternString = patternString.replaceAt("ALTERNATE" ++ replacements.size.asPaddedString(4), m[0]-1, m[1].size+2);
					replacements = replacements.add("[%]".format(alternateElements.join($,)));
				};
			};

			// Now try to split on space-separated elements in angle brackets (alternation)
			this.findAngleBracketed(patternString).reverseDo{ arg m;
				var alternateElements = BacalaoParser.splitSimplify(m[1]);
				numAlternates = numAlternates.add(alternateElements.size);
				patternString = patternString.replaceAt("ALTERNATE" ++ replacements.size.asPaddedString(4), m[0]-1, m[1].size+2);
				replacements = replacements.add("Pseq([%],inf)".format(alternateElements.join($,)));
			};

			{
				// var patternArray = this.splitSimplify(patternString.replace("~", "Rest()"));
				var patternArray = this.parseArray(patternString);
				var elemsAndDurs = {
					if (patternArray.size > 1) {
						// "Wrapping top-level pattern array".postln;
						patternArray = [ patternArray -> 1];
					};
					this.calculateDurations(patternArray);
				}.value;
				// Replace alphabetic-only strings by Symbol notation: 'symbol'
				// patternArray = patternArray.collect{ |p| "^[A-Za-z]+$".matchRegexp(p).if(p.asSymbol.cs, p) };
				var parserVariables = topEnvironment[Bacalao.preProcessorVariables];
				var elems, durs;
				var resolveChord = { arg elem;
					// If we've got the ChordSymbol Quark installed, try to lookup as a chord
					if (\ChordSymbol.asClass.notNil and: { elem.first.isUpper }) {
						switch (patternType)
						{ \degree } {
							var notes = ChordSymbol.asNotes(elem);
							if (notes != elem) {
								notes.asArray.collect { |n|
									// TODO when next version of SC comes out use keyToDegree
									n.keyToDegree2(Scale.major, 12);
								} //.debug("ChordSymbol")
							} {
								elem
							}
						}
						{ ChordSymbol.asNotes(elem) } //.debug("ChordSymbol") };
					} {
						// Stick with the original element, there is no lookup
						elem
					}
				};
				#elems, durs = elemsAndDurs.flop;

				// Replace Bacalao parser variables with their values
				elems = elems.collect{ arg elem;
					if (parserVariables.notNil) {
						var variable, index, substitute;
						// Allow (e.g.) "bd:5"-style indexing, or simply "bd" (equivalent to "bd:0")
						// Also allow "bd:r", which will choose a random value from the collection.
						#variable, index = elem.split($:);
						substitute = parserVariables[variable.asSymbol];
						if (substitute.notNil) {
							if (index.notNil) {
								if (index == "r" and: { substitute.size > 1 }) {
									"Prand(%,inf)".format(substitute.asArray)
								} {
									substitute.asArray.wrapAt(index.asInteger)
								}
							} {
								substitute.first
							}
						} {
							resolveChord.value(elem)
						}
					} {
						resolveChord.value(elem)
					}
				};

				// *Don't* convert strings to symbols here...this allows you to evaluate
				// variables (e.g. n = Pwhite(0,7,1); deg"n*4") and arbitrary code in patterns!
				// elems = elems.collect{ |elem| "^[A-Za-z]+$".matchRegexp(elem).if(elem.asSymbol.cs, elem) };
				if (elems.size > 1) {
					var longElemStr = String.streamContents({ arg stream;
						elems.printOn(stream); });
					var longDursStr = String.streamContents({ arg stream;
						durs.printOn(stream); });
					//var longDursStr = String.streamContents({ arg stream;
					//	durs.collect(_.asStringPrec(17)).printOn(stream); });
					var numCyclesRequired = numAlternates.reduce(\lcm) ? 1.0;
					replaceStr = "Pbind('%', Ppatlace(%, %), 'dur', Pseq(%, %))".format(patternType, longElemStr, numCyclesRequired, longDursStr, numCyclesRequired);
				} {
					if (durs[0] === 1) {
						// In the case of a single entry at the top level (and no array braces)
						// just return a single value repeatedly, with no duration.
						// IF you want a single event that lasts one cycle, use
						// "[0]" or "0@1"
						replaceStr = "Pbind('%', %)".format(patternType, elems[0]);
					} {
						replaceStr = "Pbind('%', %, 'dur', %)".format(patternType, elems[0], durs[0]);
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

				// if (patternArray.size > 1) {
				// 	replaceStr = "Pbind('%', Pseq(%), 'dur', %)".format(patternType, patternArray, patternArray.size.reciprocal);
				// } {
				// 	replaceStr = "Pbind('%', %, 'dur', %)".format(patternType, patternArray[0], patternArray.size.reciprocal);
				// };
				// patternString.debug("generic pattern");
			}.value;
			[curOffset, fullMatch.first, fullMatch.last, replaceStr].postln;
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
				'amp', [ 26.reciprocal*0.5, 26.reciprocal, (_ + 26.reciprocal), 10.reciprocal, (_ + 10.reciprocal) ],
				[ 0, 0, (_ + 1) ] // Default values
			);
			result = case
			{ ch == Char.space } {
				Rest() }
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
				Rest()
			};
			result
		};
		^patternArray
	}

	*prEvalVariableCharString { arg barString, variableName;
		//var parserVariables = topEnvironment[Bacalao.preProcessorVariables];
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
		// Support $/ or $| as bar-split symbols
		var bars = patternString.split($/).collect(_.split($|)).flatten;
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
					patternArray = patternArray.add(Rest() -> (nextBar - patternDur));
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
							"PnsymRest(Pseq(%), ~%)".format(elems.collect{|e| e.value}.join.cs, elems.first.key);
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
					replaceStr = "Pbind('%', Pseq(%, %), 'dur', Pseq(%, %))".format(patternType, longElemStr, numCyclesRequired, longDursStr, numCyclesRequired);
				} {
					if (durs[0] === 1) {
						// In the case of a single entry at the top level (and no array braces)
						// just return a single value repeatedly, with no duration.
						// IF you want a single event that lasts one cycle, use
						// "[0]" or "0@1"
						replaceStr = "Pbind('%', %)".format(patternType, elems[0]);
					} {
						replaceStr = "Pbind('%', %, 'dur', %)".format(patternType, elems[0], durs[0]);
					}
				};
			}.value;
			[curOffset, fullMatch.first, fullMatch.last, replaceStr].postln;
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
		^(str.findRegexp(balancedArray, offset).flop[1] ?? #[]).clump(4)
	}

	*findArrayElem { arg str, offset=0;
		^(str.findRegexp(arrayElem, offset).flop[1] ?? #[]).clump(6)
	}

	*findSimpleElem { arg str, offset=0;
		^(str.findRegexp(simpleElem, offset).flop[1] ?? #[]).clump(7)
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

	*findDefName { arg str, offset=0;
		^(str.findRegexp(defName, offset).flop[1] ?? #[]).clump(2)
	}

	*findSubCmd { arg str, offset=0;
		^(str.findRegexp(subCmd, offset).flop[1] ?? #[]).clump(3)
	}

	*findGlobalCmd { arg str, offset=0;
		^(str.findRegexp(globalCmd, offset).flop[1] ?? #[]).clump(3)
	}

	*findInstDefCmd { arg str, offset=0;
		^(str.findRegexp(midiInstDefCmd, offset).flop[1] ?? #[]).clump(5)
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
			var full, elem, subArr, subArrRepeat, subArrHold, invalidElem;
			// item.debug("item");
			#full, elem, subArr, subArrRepeat, subArrHold, invalidElem = item;
			// elem.debug("elem");
			if (elem.notEmpty) {
				var elemMatch = this.findSimpleElem(elem);
				// elemMatch.debug("elemMatch");
				if (elemMatch.size == 1) {
					var full, elem, repeat, hold;
					if (elemMatch[0].size != 4) {
						Error("Unexpected match size: %".format(elemMatch[0])).throw
					};
					#full, elem, repeat, hold = elemMatch[0];
					if (elem.isEmpty) {
						Error("There is no array element: %".format(full)).throw
					};
					repeat = if (repeat.isEmpty) { 1 } { repeat.asInteger };
					hold = if (hold.isEmpty) { 1 } { hold.asFloat };
					// "Adding a result: %".format(elem).postln;
					if (repeat > 1) {
						elem = (elem -> 1) ! repeat;
					};
					arr = arr.add(elem -> hold);
				} {
					Error("Invalid array entry: %".format(elem.cs)).throw;
				}
			} {
				var repeat = if (subArrRepeat.isEmpty) { 1 } { subArrRepeat.asInteger };
				var hold = if (subArrHold.isEmpty) { 1 } { subArrHold.asFloat };
				var subArrResult;
				subArr = subArr.drop(1).drop(-1); // get rid of outer brackets
				// if (verbose) { "Will parseArray recursively on %".format(subArr).postln };
				subArrResult = this.parseArray(subArr);
				if (repeat > 1) {
					subArrResult = (subArrResult -> 1) ! repeat;
				};
				arr = arr.add(subArrResult -> hold);
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
Bake {
	*new { arg x;
		var bakedText = {
			if (x.isKindOf(Pattern)) {
				x = x.asStream.nextN(64).reject(_.isNil);
			};
			if (x.isKindOf(Collection) and: { x.isKindOf(String).not }) {
				x = x.join($ );
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
}

// A Pattern returning ones for "hits" in Bjorklund's Euclidean algorithm and Rests for the "misses".
Pbjork {
	*new { arg k, n, offset=0;
		^Pbjorklund(k, n, inf, offset:offset).collect{|e| if (e==0) { Rest() } { e }};
	}
}

PmaskBjork {
	*new { arg k, n, offset=0;
		^Pbind(\mask, Pbjork(k, n, offset));
	}
}

Psine {
	*new { arg period=1, phase=0, mul=1, add=0, repeats=inf;
		^((Ptime(repeats) / period + phase) * 2pi).sin * mul + add;
	}

	*range { arg period=1, phase=0, lo = -1.0, hi=1.0, repeats=inf;
		^Psine.new(period, phase, 1, 0, repeats).linlin(-1,1, lo,hi);
	}

	*exprange { arg period=1, phase=0, lo = 0.01, hi=1.0, repeats=inf;
		^Psine.new(period, phase, 1, 0, repeats).linexp(-1,1, lo,hi);
	}
}

Psaw {
	*new { arg period=1, phase=0, mul=1, add=0, repeats=inf;
		^((Ptime() + phase).mod(period) / period * 2 - 1) * mul + add;
	}

	*range { arg period=1, phase=0, lo = -1.0, hi=1.0, repeats=inf;
		^Psaw.new(period, phase, 1, 0, repeats).linlin(-1,1, lo,hi);
	}

	*exprange { arg period=1, phase=0, lo = 0.01, hi=1.0, repeats=inf;
		^Psaw.new(period, phase, 1, 0, repeats).linexp(-1,1, lo,hi);
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


// Modify an Event Pattern to return Rests based on a weighted coin toss
+Pattern {
	degrade { arg prob = 0.5, randSeed;
		var p = this.collect{ |ev| prob.coin.if{ ev } { ev.copy.put(\mask, Rest()) }};
		^if (randSeed.notNil) { Pseed(Pn(randSeed, 1), p) } { p }
	}
}

+Pbind {
	// Overriding because this "innocent" typo produces a sclang freeze (if you play a Pbind().degrad)!
	degrad {
		"Probably a typo...'degrad' rather than degrade?".postln;
		^this;
	}
}
