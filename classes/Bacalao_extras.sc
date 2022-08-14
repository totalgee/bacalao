+Bacalao {

	*clockWindow { arg server;
		var publicWinTopRight = 1920@(1080-20);
		var privateTopRight = Window.availableBounds.size.asPoint;
		var startMic = { arg inBus = 0;
			Ndef(\micInput, {
				var sig = HPF.ar(SoundIn.ar(inBus), 60) ! 2;
				sig = NHHall.ar(sig, 1.5) * -21.dbamp + (sig * -1.dbamp);
				sig
			}).play;
		};
		var toggleMic = {
			var ndef = Ndef(\micInput);
			if (ndef.isMonitoring) {
				ndef.clear(1);
				"Switched off mic".postln;
			} {
				"Enabled mic".postln;
				startMic.();
			}
		};

		var publicWin, timeText;
		var clk = ClockFace(0, 1, 1, true);
		var restart = { clk.cursecs = 0; clk.play };

		server = server ?? { Server.default };
		clk.window.bounds_(Rect(privateTopRight.x - 155, 100, 155, 160));
		Button(clk.window, Rect(20, 80, 120, 30)).string_("Restart").action_(restart);
		Button(clk.window, Rect(20, 120, 55, 30)).string_("Record").action_{
			server.waitForBoot{
				restart.();
				server.record
			}
		};
		Button(clk.window, Rect(85, 120, 55, 30)).string_("Stop").action_{ server.stopRecording; clk.stop };
		Button(clk.window, Rect(80 - 20, 5, 40, 20)).string_("Mic").action_{ toggleMic.() };

		// publicWin = Window("Timer").bounds_(Rect(1696, 2020, 220, 100)).front;
		publicWin = Window("Timer").bounds_(Rect(privateTopRight.x - 220, privateTopRight.y - 200, 220, 100)).alwaysOnTop_(true).front;
		timeText = StaticText(publicWin, Rect(10,10, 200,80)).front.font_(Font("Arial", 48)).align_(\left);
		Tdef(\publicClock, { loop { 1.wait; {timeText.string = clk.cursecs.asTimeString.keep(8)}.defer } }).play(quant: 1);
		publicWin.onClose_{ Tdef(\publicClock).clear; "Stopped updating public clock".postln };
		clk.window.onClose_{ "Closed private clock".postln; clk.stop; publicWin.close };
		clk
	}

	clockWindow {
		Bacalao.clockWindow(server)
	}

	keyboard { arg trk;
		var k = KeyboardWindow(61, 0, 0, Rect(80, 140, 1026, 152));
		var kb = ();
		var notes = ();
		k.window.name = "Keyboard: " ++ trk.asSymbol;

		// Bottom row
		"zsxdcvgbhnjm".do{ arg ch, i;
			kb.put(ch.toUpper.ascii, i + [48, 36]);
		};
		// Top row
		"q2w3er5t6y7ui9o0p".do{ arg ch, i;
			kb.put(ch.toUpper.ascii, i + [60, 72]);
		};

		k.userView.keyDownAction = { arg view, char, mods, unicode, keycode, key;
			var note = kb[keycode];
			// [char, mods, unicode, keycode, key].debug("down char, mods, unicode, keycode, key");
			if (note.notNil) {
				note = if (mods.isShift) { note[1] } { note[0] };
				// if (k.activeNotes.includes(note).not) {
				k.pressNote(note, 96);
				// }
			} {
				if (keycode == 27) { // Esc
					k.window.close;
				};
				if (keycode == 32) { // space
					k.unPressAll;
				};
			}
		};
		k.userView.keyUpAction = { arg view, char, mods, unicode, keycode, key;
			var note = kb[keycode];
			// [char, mods, unicode, keycode, key].debug("up char, mods, unicode, keycode, key");
			if (note.notNil) {
				note = if (mods.isShift) { note[1] } { note[0] };
				k.unPressNote(note, 96);
			}
		};
		k.downAction = { arg ch, note, vel;
			[ch,note,vel].debug("down ch,note,vel");
			if (this.vst(trk).notNil) {
				this.vst(trk).midi.noteOn(ch, note, vel);
			} {
				notes[note] ?? {
					var inst = this.defGet(trk).instrument.value ?? \default;
					var proxy = this.proxy(trk);
					notes[note] = Synth(inst, [out: proxy.bus, freq: note.midicps, amp: vel/127], proxy.group);
				};
			};
		};
		k.upAction = { arg ch, note, vel;
			// [ch,note,vel].debug("up ch,note,vel");
			if (this.vst(trk).notNil) {
				this.vst(trk).midi.noteOff(ch, note, vel);
			} {
				notes[note] !? {
					var synth = notes[note];
					synth.release(0.25);
					notes[note] = nil;
				};
			};
		};
		k.userView.onClose = {
			notes.do{ arg n;
				n.release(0.25);
			};
			"closing keyboard '%'".format(trk).postln;
		};

		^k
	}

	*prSetupInstrumentSynths {

		// Glen's "go-to" default sound
		SynthDef(\ping, { arg out = 0, freq = 440, amp = 0.1, pan = 0.0, gate = 1;
			var n = 4;
			var att = \att.ir(0.01);
			var dec = \dec.ir(0.3); // decayTime
			var sus = \sus.ir(0.5); // sustainLevel
			var rel = \rel.ir(0.5);
			var emphasis = amp.lincurve(0.0,1, 0.5,4,-1.5);
			var sig = Splay.ar(SinOscFB.ar(freq * Rand(0.995, 1.005!n), ExpRand(0.2, 0.4!n) * emphasis, 1), 0.5, 1, pan) * -9.dbamp;
			var env = EnvGen.kr(Env.adsr(att, dec, sus, rel), gate, doneAction: Done.freeSelf);
			OffsetOut.ar(out, sig * env * amp);
		}).add;

		SynthDef(\saw, { arg out = 0, freq = 440, amp = 0.1, pan = 0.0, gate = 1;
			var n = 4;
			var att = \att.ir(0.01);
			var dec = \dec.ir(0.3); // decayTime
			var sus = \sus.ir(0.5); // sustainLevel
			var rel = \rel.ir(0.5);
			var pitchShift = \pitchShift.ir(-0.125); // how much to shift pitch in semitones
			var shiftTime = \shiftTime.ir(4); // over how many seconds to shift pitch
			var emphasis = amp.lincurve(0.1,1, 0.7,3,-1.5);

			var sig = Splay.ar(SawDPW.ar(freq * Rand(0.995, 1.005!n) * Line.kr(1, pitchShift.midiratio, shiftTime), Rand(-1.0, 1.0!n)), 0.5, 1, pan) * -3.dbamp;
			var env = EnvGen.kr(Env.adsr(att, dec, sus, rel), gate, doneAction: Done.freeSelf);
			sig = (sig * emphasis).distort;
			OffsetOut.ar(out, sig * env * amp);
		}).add;

		SynthDef(\square, { arg out = 0, freq = 440, amp = 0.1, pan = 0.0, gate = 1;
			var n = 4;
			var att = \att.ir(0.01);
			var dec = \dec.ir(0.3); // decayTime
			var sus = \sus.ir(0.5); // sustainLevel
			var rel = \rel.ir(0.5);
			// See question on sc-users about why width argument doesn't support array arg
			// (unless you call "poll" on it first...)
			var pulseWidth = \width.kr(0.5); // doesn't work if you add: + Rand(-0.02, 0.02!n);
			var pitchShift = \pitchShift.ir(-0.125); // how much to shift pitch in semitones
			var shiftTime = \shiftTime.ir(4); // over how many seconds to shift pitch
			var emphasis = amp.lincurve(0.1,1, 0.7,3,-1.5);

			var sig = Splay.ar(PulseDPW.ar(freq * Rand(0.995, 1.005!n) * Line.kr(1, pitchShift.midiratio, shiftTime), pulseWidth), 0.5, 1, pan) * -3.dbamp;
			var env = EnvGen.kr(Env.adsr(att, dec, sus, rel), gate, doneAction: Done.freeSelf);
			sig = (sig * emphasis).distort;
			OffsetOut.ar(out, sig * env * amp);
		}).add;

		// A persistent Synth, to be played using control patterns and 'mask' trigger for notes
		SynthDef(\analog, { arg out = 0;
			var lag = 0.1;
			var att = \att.kr(0.01, lag);
			var rel = \rel.kr(0.2, lag);
			var freqLag = \freqLag.kr(lag * 2);
			var freq = \freq.kr(440, freqLag);
			var mask = \mask.tr;
			var maskEnv = EnvGen.kr(Env.perc(att, rel), mask) * 1 + 0;
			var amp = \amp.kr(0.2, lag) * maskEnv * (freq > 10);
			var pan = \pan.kr(0, lag);
			var width = \width.kr(0.5, lag);
			var lpf = \lpf.kr(4000, lag);
			var lpq = \lpq.kr(1, lag);
			var n = 4;
			var sig = VarSaw.ar(freq * Rand(0.97, 1.03!n), 0, width);
			sig = Splay.ar(sig, 0.5, center: pan);
			sig = BLowPass.ar(sig, lpf, lpq);
			Out.ar(out, sig * amp);
		}).add;

		SynthDef(\pluck, { arg out=0, freq=440, amp=0.1, pan=0;
			var n = 2;
			var inp = LFClipNoise.ar(2000!n, 0.7) * Env.perc(0.005, 0.05).ar * amp;
			var sig = DWGPlucked.ar(freq * Rand(0.995,1.005!n), amp, 1, Rand(0.1, 0.9!n), amp.expexp(0.01,1, 30,0.3), LFDNoise1.kr(0.27!n).exprange(15,150), inp) * amp;
			DetectSilence.ar(sig, 0.001, doneAction: Done.freeSelf);
			OffsetOut.ar(out, Splay.ar(sig, 0.25, center: pan));
		}).add;

		SynthDef(\bell, { arg out=0, freq=440, amp=0.1, pan=0;
			// http://math.mit.edu/~bush/wordpress/wp-content/uploads/2012/08/TibetanBowls.pdf
			var freqs = (2..6).collect{ arg n; ((n.squared - 1).squared / (1 + n.squared.reciprocal)).sqrt } / 7.2.sqrt;
			var amps = [0.04, 0.13, 0.08, 0.035, 0.075].normalizeSum;
			var rings = freqs.size.collect(_.lincurve(0,freqs.size-1,15,2,2)) * Rand(0.7,1.4!freqs.size);
			var input = LPF.ar(Env.perc(0.001,0.01).ar * PinkNoise.ar(0.5), 3000);
			var n = 3;
			var sig = Klank.ar(`[freqs, amps, rings], input, freq * Rand(0.99,1.01!n), Rand(-0.5,0.5!n)) * 0.7;
			sig = Splay.ar(sig, 0.333, amp, pan);
			DetectSilence.ar(sig, doneAction: Done.freeSelf);
			OffsetOut.ar(out, sig);
		}).add;

		// From Roger Pibernat (aka loopier): based on his \superfm and \fmx7
		// https://github.com/loopier/synthdefs/blob/master/synthdef-fmx7.scd
		//
		// 6-op FM synth (DX7-like) with envelope to be used with Pbind with EG levels and rates.
		//
		// Parameters affecting the overall synth
		// \amp     Float    Overall synth amplitude
		// \freq    Float    Overall synth frequency
		// \att     |
		// \dec     | ADSR envelope
		// \sus     |
		// \rel     |
		//
		// Parameters for each of the operators.  'N' represents the operator index as
		// in \amp1 or \ratio1.
		// \ampN    Float    Output level of each operator (N = operator index)
		// \levelN  Float    Determines the max value of the amplitude or how much it can modulate.
		// Each operator has a 4-stage envelope.  Values can be set for each of the stage's level and rate.
		// Notice that 'rate' is 1/time, so higher values mean faster times.
		// \eglevelNX  Float    Value for each of the envelope levels. N = operator index, X = envelope stage.
		//                      E.g. \eglevel13 is setting the value of the 3rd stage of the first operator's envelope.
		// \egtimeNX   Float    Value for each of the envelope times. N = operator index, X = envelope stage.
		SynthDef(\fmx7, {
			var amp = \amp.kr(1) * (-12.dbamp);
			var out = \out.kr(0);
			var pan = \pan.kr(0);
			var gate = \gate.kr(1);
			var freq = \freq.kr(440);
			var dur = \dur.kr(1);
			var env = EnvGen.ar(Env.adsr(\att.kr(0.01), \dec.kr(0.3), \sus.kr(0.5), \rel.kr(0.5)), gate, doneAction: Done.freeSelf);
			// operator levels are independent of their amplitudes. They can be at full level but
			// muted by the \amp parameter.
			var levels = Array.fill(6, { |i| (\level++(i+1)).asSymbol.kr(1)});
			// on startup first operator's amp is 1, the rest are 0 -- otherwise we'd hear nothing
			var amps = Array.fill(6, { |i| (\amp++(i+1)).asSymbol.kr((i==0).asInteger)});
			var ratios = Array.fill(6, { |i| (\ratio++(i+1)).asSymbol.kr(1)});
			var detunes = Array.fill(6, { |i| (\detune++(i+1)).asSymbol.kr(0)});
			var todo = if (false) {
				// !!! TODO: add modwheel and LFO
				// lfo sensitivity
				var modsenses = Array.fill(6, { |i| (\modsens++(i+1)).asSymbol.kr(0)});
				// selects between FM = 0 and AM = 1
				var lfomode = \lfomode.kr(0);
				var lfofreq = \lfofreq.kr(1);
				var lfodepth = \lfodepth.kr(0.0);
				var lfo = SinOsc.ar(lfofreq, 0, lfodepth);
			};

			var envs = Array.fill(6, { |i|
				EnvGen.kr(
					Env.new(
						[0]++Array.fill(4, { |n| (\eglevel++(i+1)++(n+1)).asSymbol.kr(1) * levels[i] }),
						[
							(\egtime++(i+1)++1).asSymbol.kr(0.01),
							(\egtime++(i+1)++2).asSymbol.kr(0.3),
							(\egtime++(i+1)++3).asSymbol.kr(0.3),
							(\egtime++(i+1)++4).asSymbol.kr(0.5),
						]
					),
					gate: gate
				);
			});

			var ctls = Array.fill(6, { |i|[
				freq * ratios[i] + detunes[i],
				0,
				envs[i],
			]});

			var mods = Array.fill(6, { |i|
				Array.fill(6, { |n| (\mod++(i+1)++(n+1)).asSymbol.kr(0)});
			});

			var sig = FM7.ar(ctls, mods) * amps;
			Out.ar(out, Pan2.ar(sig.sum, pan, amp * env));
		}).add;

		//////////////////////////////////////////////////////////////////////
		// Various others, taken from sccode.org (with links),
		// sometimes cleaned/tweaked
		//////////////////////////////////////////////////////////////////////
		SynthDef(\snare909, { | out=0, mul=1, amp=0.1, pan=0 |
			var excitation, membrane;

			excitation = LPF.ar(WhiteNoise.ar(1), 7040, amp);
			membrane = (
				/* Two simple enveloped oscillators represent the loudest resonances of the drum membranes */
				(LFTri.ar(330,0,1) * EnvGen.ar(Env.perc(0.0005,0.055),doneAction:0) * 0.25)
				+(LFTri.ar(185,0,1) * EnvGen.ar(Env.perc(0.0005,0.075),doneAction:0) * 0.25)

				/* Filtered white noise represents the snare */
				+(excitation * EnvGen.ar(Env.perc(0.0005,0.4),doneAction:2) * 0.2)
				+(HPF.ar(excitation, 523, 1) * EnvGen.ar(Env.perc(0.0005,0.283),doneAction:0) * 0.2)

			) * mul;
			Out.ar(out, Pan2.ar(membrane, pan))
		}).add;

		SynthDef(\neurosnare, {
			var snd;
			// a percussive click to give it some attack
			snd = LPF.ar(HPF.ar(WhiteNoise.ar, 300), 8000) * Env.linen(0.001, 0.01, 0.001).ar;
			// sine sweep body. very important!
			snd = snd + (SinOsc.ar(Env([400, 196, 160], [0.04, 0.2], \exp).ar) * Env.perc(0.04, 0.2).ar * 6.dbamp).tanh;
			// sound of snare coils rattling
			snd = snd + (HPF.ar(BPeakEQ.ar(WhiteNoise.ar, 4000, 0.5, 3), 300) * Env.perc(0.05, 0.2).delay(0.01).ar(2) * -3.dbamp);
			// another sound sweep to improve the attack, optional
			snd = snd + (SinOsc.ar(XLine.kr(3000, 1500, 0.01)) * Env.perc(0.001, 0.02).ar);
			// distortion helps glue everything together and acts as a compressor
			snd = (snd * 1.4).tanh;
			snd = Pan2.ar(snd, \pan.kr(0), \amp.kr(0.1));
			Out.ar(\out.kr(0), snd);

			//By Snapizz
			//http://sccode.org/1-57f

		}).add;

		SynthDef("hihat", { arg out = 0, amp = 0.5, att = 0.01, rel = 0.2, ffreq = 6000, pan = 0;
			var env, snd;
			env = Env.perc(att, rel, amp).kr(doneAction: 2);
			snd = WhiteNoise.ar;
			snd = HPF.ar(in: snd, freq: ffreq, mul: env);
			Out.ar(out, Pan2.ar(snd, pan));

			//By Nathan Ho aka Snappizz
			//http://sccode.org/1-523
		}).add;

		SynthDef("snare", { arg out = 0, amp = 0.1, sinfreq = 180, att = 0.01, rel = 0.2, ffreq = 2000, pan = 0;
			var env, snd1, snd2, sum;
			env = Env.perc(att, rel, amp).kr(doneAction: 2);
			snd1 = HPF.ar(
				in: WhiteNoise.ar,
				freq: ffreq,
				mul: env
			);
			snd2 = SinOsc.ar(freq: sinfreq, mul: env);
			sum = snd1 + snd2;
			Out.ar(out, Pan2.ar(sum, pan));

			//By Nathan Ho aka Snappizz
			//http://sccode.org/1-523
		}).add;

		SynthDef("kick", { arg out = 0, amp = 0.3, sinfreq = 60, glissf = 0.9, att = 0.01, rel = 0.45, pan = 0;
			var env, snd, ramp;
			env = Env.perc(att, rel, amp).kr(doneAction: 2);
			ramp = XLine.kr(
				start: sinfreq,
				end: sinfreq * glissf,
				dur: rel
			);
			snd = SinOsc.ar(freq: ramp, mul: env);
			snd = Pan2.ar(snd, pan);
			Out.ar(out, snd);

			//By Nathan Ho aka Snappizz
			//http://sccode.org/1-523
		}).add;

		SynthDef(\kick1, {
			var snd;
			snd = DC.ar(0);
			snd = snd + (SinOsc.ar(XLine.ar(800, 400, 0.01)) * Env.perc(0.0005, 0.01).ar);
			snd = snd + (BPF.ar(Hasher.ar(Sweep.ar), XLine.ar(800, 100, 0.01), 0.6) * Env.perc(0.001, 0.02).delay(0.001).ar);
			snd = snd + (SinOsc.ar(XLine.ar(172, 50, 0.01)) * Env.perc(0.0001, 0.3, 1, \lin).delay(0.005).ar(2));
			snd = snd.tanh;
			Out.ar(\out.kr(0), Pan2.ar(snd, \pan.kr(0), \amp.kr(0.1)));
			//By Nathan Ho aka Snappizz
			//http://sccode.org/1-57g
		}).add;

		SynthDef(\kick2, {
			var snd;
			snd = DC.ar(0);
			snd = snd + (HPF.ar(Hasher.ar(Sweep.ar), 1320) * Env.perc(0.003, 0.03).ar * 0.5);
			snd = snd + (SinOsc.ar(XLine.ar(750, 161, 0.02)) * Env.perc(0.0005, 0.02).ar);
			snd = snd + (SinOsc.ar(XLine.ar(167, 52, 0.04)) * Env.perc(0.0005, 0.3).ar(2));
			snd = snd.tanh;
			Out.ar(\out.kr(0), Pan2.ar(snd, \pan.kr(0), \amp.kr(0.1)));
			//By Nathan Ho aka Snappizz
			//http://sccode.org/1-57g
		}).add;

		SynthDef(\kick3, {
			var snd;
			snd = DC.ar(0);
			snd = snd + (SinOsc.ar(XLine.ar(1500, 800, 0.01)) * Env.perc(0.0005, 0.01, curve: \lin).ar);
			snd = snd + (BPF.ar(Impulse.ar(0) * SampleRate.ir / 48000, 6100, 1.0) * 3.dbamp);
			snd = snd + (BPF.ar(Hasher.ar(Sweep.ar), 300, 0.9) * Env.perc(0.001, 0.02).ar);
			snd = snd + (SinOsc.ar(XLine.ar(472, 60, 0.045)) * Env.perc(0.0001, 0.3, curve: \lin).delay(0.005).ar(2));
			snd = snd.tanh;
			Out.ar(\out.kr(0), Pan2.ar(snd, \pan.kr(0), \amp.kr(0.1)));
			//By Nathan Ho aka Snappizz
			//http://sccode.org/1-57g
		}).add;

		SynthDef("kick_808", { arg out = 0, freq1 = 240, freq2 = 60, amp = 1, ringTime = 10, rel = 1, dist = 0.5, pan = 0;
			var snd, env;
			snd = Ringz.ar(
				in: Impulse.ar(0), // single impulse
				freq: XLine.ar(freq1, freq2, 0.1),
				decaytime: ringTime);
			env = EnvGen.ar(Env.perc(0.001, rel, amp), doneAction: 2);
			snd = (1.0 - dist) * snd + (dist * (snd.distort));
			snd = snd * env;
			Out.ar(0, Pan2.ar(snd, pan));
			//
			//By Bruno Ruviaro
			//https://github.com/brunoruviaro/SynthDefs-for-Patterns/blob/master/kick808.scd
		}).add;

		SynthDef(\SOSkick, { arg out = 0, freq = 50, mod_freq = 5, mod_index = 5, sustain = 0.4, amp = 0.8, beater_noise_level = 0.025, pan = 0;
			var pitch_contour, drum_osc, drum_lpf, drum_env;
			var beater_source, beater_hpf, beater_lpf, lpf_cutoff_contour, beater_env;
			var kick_mix;
			pitch_contour = Line.kr(freq*2, freq, 0.02);
			drum_osc = PMOsc.ar(	pitch_contour,
				mod_freq,
				mod_index/1.3,
				mul: 1,
				add: 0);
			drum_lpf = LPF.ar(in: drum_osc, freq: 1000, mul: 1, add: 0);
			drum_env = drum_lpf * EnvGen.ar(Env.perc(0.005, sustain), 1.0, doneAction: 2);
			beater_source = WhiteNoise.ar(beater_noise_level);
			beater_hpf = HPF.ar(in: beater_source, freq: 500, mul: 1, add: 0);
			lpf_cutoff_contour = Line.kr(6000, 500, 0.03);
			beater_lpf = LPF.ar(in: beater_hpf, freq: lpf_cutoff_contour, mul: 1, add: 0);
			beater_env = beater_lpf * EnvGen.ar(Env.perc, 1.0, doneAction: 2);
			kick_mix = Mix.new([drum_env, beater_env]) * 2 * amp;
			Out.ar(out, Pan2.ar(kick_mix, pan))
		}).add;
		//DrumSynths SC Example - SOS Drums by Renick Bell, renick_at_gmail.com
		// recipes from Gordon Reid in his Sound on Sound articles
		// SOSkick -------
		// http://www.soundonsound.com/sos/jan02/articles/synthsecrets0102.asp
		// increase mod_freq and mod_index for interesting electronic percussion

		SynthDef(\SOSsnare, { arg out = 0, sustain = 0.1, drum_mode_level = 0.25,
			snare_level = 40, snare_tightness = 1000,
			freq = 405, amp = 0.8, pan = 0;

			var drum_mode_sin_1, drum_mode_sin_2, drum_mode_pmosc, drum_mode_mix, drum_mode_env;
			var snare_noise, snare_brf_1, snare_brf_2, snare_brf_3, snare_brf_4, snare_reson;
			var snare_env;
			var snare_drum_mix;

			drum_mode_env = EnvGen.ar(Env.perc(0.005, sustain), 1.0, doneAction: 2);
			drum_mode_sin_1 = SinOsc.ar(freq*0.53, 0, drum_mode_env * 0.5);
			drum_mode_sin_2 = SinOsc.ar(freq, 0, drum_mode_env * 0.5);
			drum_mode_pmosc = PMOsc.ar(	Saw.ar(freq*0.85),
				184,
				0.5/1.3,
				mul: drum_mode_env*5,
				add: 0);
			drum_mode_mix = Mix.new([drum_mode_sin_1, drum_mode_sin_2, drum_mode_pmosc]) * drum_mode_level;

			// choose either noise source below
			//	snare_noise = Crackle.ar(2.01, 1);
			snare_noise = LFNoise0.ar(20000, 0.1);
			snare_env = EnvGen.ar(Env.perc(0.005, sustain), 1.0, doneAction: 2);
			snare_brf_1 = BRF.ar(in: snare_noise, freq: 8000, mul: 0.5, rq: 0.1);
			snare_brf_2 = BRF.ar(in: snare_brf_1, freq: 5000, mul: 0.5, rq: 0.1);
			snare_brf_3 = BRF.ar(in: snare_brf_2, freq: 3600, mul: 0.5, rq: 0.1);
			snare_brf_4 = BRF.ar(in: snare_brf_3, freq: 2000, mul: snare_env, rq: 0.0001);
			snare_reson = Resonz.ar(snare_brf_4, snare_tightness, mul: snare_level) ;
			snare_drum_mix = Mix.new([drum_mode_mix, snare_reson]) * 5 * amp;
			Out.ar(out, Pan2.ar(snare_drum_mix, pan))
		}).add;
		//DrumSynths SC Example - SOS Drums by Renick Bell, renick_at_gmail.com
		// recipes from Gordon Reid in his Sound on Sound articles
		// SOSsnare -------
		// http://www.soundonsound.com/sos/Mar02/articles/synthsecrets0302.asp

		SynthDef(\SOShats, { arg out = 0, freq = 6000, sustain = 0.1, amp = 0.8, pan = 0;
			var root_cymbal, root_cymbal_square, root_cymbal_pmosc;
			var initial_bpf_contour, initial_bpf, initial_env;
			var body_hpf, body_env;
			var cymbal_mix;

			root_cymbal_square = Pulse.ar(freq, 0.5, mul: 1);
			root_cymbal_pmosc = PMOsc.ar(root_cymbal_square,
				[freq*1.34, freq*2.405, freq*3.09, freq*1.309],
				[310/1.3, 26/0.5, 11/3.4, 0.72772],
				mul: 1,
				add: 0);
			root_cymbal = Mix.new(root_cymbal_pmosc);
			initial_bpf_contour = Line.kr(15000, 9000, 0.1);
			initial_env = EnvGen.ar(Env.perc(0.005, 0.1), 1.0);
			initial_bpf = BPF.ar(root_cymbal, initial_bpf_contour, mul:initial_env);
			body_env = EnvGen.ar(Env.perc(0.005, sustain, 1, -2), 1.0, doneAction: 2);
			body_hpf = HPF.ar(in: root_cymbal, freq: Line.kr(9000, 12000, sustain),mul: body_env, add: 0);
			cymbal_mix = Mix.new([initial_bpf, body_hpf]) * amp;
			Out.ar(out, Pan2.ar(cymbal_mix, pan))
		}).add;
		//DrumSynths SC Example - SOS Drums by Renick Bell, renick_at_gmail.com
		// recipes from Gordon Reid in his Sound on Sound articles
		// SOShats -------
		// http://www.soundonsound.com/sos/Jun02/articles/synthsecrets0602.asp

		SynthDef(\SOStom, { arg out = 0, sustain = 0.4, drum_mode_level = 0.25,
			freq = 90, drum_timbre = 1.0, amp = 0.8, pan = 0;

			var drum_mode_sin_1, drum_mode_sin_2, drum_mode_pmosc, drum_mode_mix, drum_mode_env;
			var stick_noise, stick_env;
			var drum_reson, tom_mix;

			drum_mode_env = EnvGen.ar(Env.perc(0.005, sustain), 1.0, doneAction: 2);
			drum_mode_sin_1 = SinOsc.ar(freq*0.8, 0, drum_mode_env * 0.5);
			drum_mode_sin_2 = SinOsc.ar(freq, 0, drum_mode_env * 0.5);
			drum_mode_pmosc = PMOsc.ar(	Saw.ar(freq*0.9),
				freq*0.85,
				drum_timbre/1.3,
				mul: drum_mode_env*5,
				add: 0);
			drum_mode_mix = Mix.new([drum_mode_sin_1, drum_mode_sin_2, drum_mode_pmosc]) * drum_mode_level;
			stick_noise = Crackle.ar(2.01, 1);
			stick_env = EnvGen.ar(Env.perc(0.005, 0.01), 1.0) * 3;
			tom_mix = Mix.new([drum_mode_mix, stick_env]) * 2 * amp;
			Out.ar(out, Pan2.ar(tom_mix, pan))
		}).add;
		//DrumSynths SC Example - SOS Drums by Renick Bell, renick_at_gmail.com
		// recipes from Gordon Reid in his Sound on Sound articles
		// SOStom -------
		// http://www.soundonsound.com/sos/Mar02/articles/synthsecrets0302.asp

		SynthDef(\kick_electro, { |out = 0, pan = 0, amp = 0.3|
			var body, bodyFreq, bodyAmp;
			var pop, popFreq, popAmp;
			var click, clickAmp;
			var snd;

			// body starts midrange, quickly drops down to low freqs, and trails off
			bodyFreq = EnvGen.ar(Env([261, 120, 51], [0.035, 0.08], curve: \exp));
			bodyAmp = EnvGen.ar(Env.linen(0.005, 0.1, 0.3), doneAction: 2);
			body = SinOsc.ar(bodyFreq) * bodyAmp;
			// pop sweeps over the midrange
			popFreq = XLine.kr(750, 261, 0.02);
			popAmp = EnvGen.ar(Env.linen(0.001, 0.02, 0.001)) * 0.15;
			pop = SinOsc.ar(popFreq) * popAmp;
			// click is spectrally rich, covering the high-freq range
			// you can use Formant, FM, noise, whatever
			clickAmp = EnvGen.ar(Env.perc(0.001, 0.01)) * 0.15;
			click = LPF.ar(Formant.ar(910, 4760, 2110), 3140) * clickAmp;

			snd = body + pop + click;
			snd = snd.tanh;

			Out.ar(out, Pan2.ar(snd, pan, amp));
			//By Nathan Ho aka Snappizz
			//http://sccode.org/1-523
		}).add;

		SynthDef(\snare_electro, { |out = 0, pan = 0, amp = 0.3|
			var pop, popAmp, popFreq;
			var noise, noiseAmp;
			var snd;

			// pop makes a click coming from very high frequencies
			// slowing down a little and stopping in mid-to-low
			popFreq = EnvGen.ar(Env([3261, 410, 160], [0.005, 0.01], curve: \exp));
			popAmp = EnvGen.ar(Env.perc(0.001, 0.11)) * 0.7;
			pop = SinOsc.ar(popFreq) * popAmp;
			// bandpass-filtered white noise
			noiseAmp = EnvGen.ar(Env.perc(0.001, 0.15), doneAction: 2);
			noise = BPF.ar(WhiteNoise.ar, 810, 1.6) * noiseAmp;

			snd = (pop + noise) * 1.3;

			Out.ar(out, Pan2.ar(snd, pan, amp));
			//By Nathan Ho aka Snappizz
			//http://sccode.org/1-523
		}).add;

		SynthDef(\hihat_electro, { |out = 0, pan = 0, amp = 0.3|
			var click, clickAmp;
			var noise, noiseAmp;
			var snd;

			// noise -> resonance -> expodec envelope
			noiseAmp = EnvGen.ar(Env.perc(0.001, 0.3, curve: -8), doneAction: 2);
			noise = Mix(BPF.ar(ClipNoise.ar, [4010, 4151], [0.15, 0.56], [1.0, 0.6])) * 0.7 * noiseAmp;

			snd = noise;

			Out.ar(out, Pan2.ar(snd, pan, amp));
			//By Nathan Ho aka Snappizz
			//http://sccode.org/1-523
		}).add;

		// adapted from a post by Neil Cosgrove (other three are original)
		SynthDef(\clap_electro, { |out = 0, amp = 0.5, pan = 0, dur = 1|
			var env1, env2, snd, noise1, noise2;

			// noise 1 - 4 short repeats
			env1 = EnvGen.ar(
				Env.new(
					[0, 1, 0, 0.9, 0, 0.7, 0, 0.5, 0],
					[0.001, 0.009, 0, 0.008, 0, 0.01, 0, 0.03],
					[0, -3, 0, -3, 0, -3, 0, -4]
				)
			);

			noise1 = WhiteNoise.ar(env1);
			noise1 = HPF.ar(noise1, 600);
			noise1 = LPF.ar(noise1, XLine.kr(7200, 4000, 0.03));
			noise1 = BPF.ar(noise1, 1620, 3);

			// noise 2 - 1 longer single
			env2 = EnvGen.ar(Env.new([0, 1, 0], [0.02, 0.18], [0, -4]), doneAction:2);

			noise2 = WhiteNoise.ar(env2);
			noise2 = HPF.ar(noise2, 1000);
			noise2 = LPF.ar(noise2, 7600);
			noise2 = BPF.ar(noise2, 1230, 0.7, 0.7);

			snd = noise1 + noise2;
			snd = snd * 2;
			snd = snd.softclip;

			Out.ar(out, Pan2.ar(snd,pan,amp));
			//By Nathan Ho aka Snappizz
			//http://sccode.org/1-523
		}).add;

		SynthDef("sawSynth", { arg freq = 440, amp = 0.1, att = 0.1, rel = 2, lofreq = 1000, hifreq = 3000;
			var env, snd;
			env = Env.perc(
				attackTime: att,
				releaseTime: rel,
				level: amp
			).kr(doneAction: 2);
			snd = Saw.ar(freq: freq * [0.99, 1, 1.001, 1.008], mul: env);
			snd = LPF.ar(
				in: snd,
				freq: LFNoise2.kr(1).range(lofreq, hifreq)
			);
			snd = Splay.ar(snd);
			Out.ar(0, snd);
			// Basic saw synth for chords and bass
			//By Bruno Ruviaro
			//http://sccode.org/1-54H
		}).add;

	}

	prPrintInColumns { arg strings, width = 16, maxWidth = 80;
		var curColumn = 0;
		var separator = " | ";
		var needNewLine = true;
		strings.do{ arg k;
			var str = k.asString;
			var newColumn = curColumn + str.size;
			var padStr = str.padRight(newColumn.roundUp(width) - curColumn);
			padStr.post;
			curColumn = curColumn + padStr.size;
			if (curColumn < maxWidth) {
				separator.post;
				needNewLine = true;
				curColumn = curColumn + separator.size;
			} {
				"".postln;
				needNewLine = false;
				curColumn = 0;
			}
		};
		if (needNewLine) { "".postln };
	}

	printSynths {
		var names = SynthDescLib.all[\global].synthDescs.keys.reject{ arg k;
			k.asString.beginsWith("system_")
		}.asSortedList.asArray;
		"----- instruments (SynthDefs):".postln;
		this.prPrintInColumns(names, 16, 80);
		^""
	}

	printSynthControls { arg synthName;
		var defaultSet = Set[\out, \pan, \freq, \amp, \gate];
		var synthDesc = SynthDescLib.all[\global].synthDescs[synthName.asSymbol];
		if (synthDesc.notNil) {
			var getControlStrings = { arg controls;
				controls.collect{ arg c;
					"% def: % [%] %".format(
						c.name.asString.padRight(10),
						c.defaultValue.round(1e-4),
						c.rate,
						if (c.lag > 0) { "(lag %)".format(c.lag) } { "" }
					);
				}
			};
			var defaultControls = synthDesc.controls.select{ arg c; defaultSet.includes(c.name) }.sort{ |a,b| a.name < b.name };
			var otherControls = synthDesc.controls.select{ arg c; defaultSet.includes(c.name).not }.sort{ |a,b| a.name < b.name };
			"----- SynthDef '%' common controls:".format(synthName).postln;
			this.prPrintInColumns(getControlStrings.(defaultControls), 35, 70);
			if (otherControls.size > 0) {
				"----- other controls:".postln;
				this.prPrintInColumns(getControlStrings.(otherControls), 35, 70);
			}
		} {
			"Synth '%' not found".format(synthName).warn;
		};
		^""
	}

}