BacalaoSpatialSource {
	var <synth;
	var <bus;

	*new { arg synth, bus;
		^super.newCopyArgs(synth, bus);
	}
}

BacalaoSpatialSettings {
	var <sourceDict;
	var <decoder; // the VSTController doing decoding
	var <decoderBus;
	var <decoderGroup;
	var <sourceGroup;
	var owningBacalao;
	var <hoaOrder;

	*new { arg decoder, bus, decoderGroup, sourceGroup, owningBacalao, hoaOrder = 3;
		if (owningBacalao.isKindOf(Bacalao).not) {
			Error("BacalaoSpatialSettings must be owned by a Bacalao").throw;
		};
		^super.newCopyArgs((), decoder, bus, decoderGroup, sourceGroup, owningBacalao, hoaOrder);
	}

	*hoaNumChannels { arg order;
		^(order+1).squared.asInteger;
	}

	hoaNumChannels {
		^BacalaoSpatialSettings.hoaNumChannels(hoaOrder);
	}

	free {
		sourceDict.keysValuesDo{ arg k, spatialSource;
			"Freeing spatialization for '%'".format(k).postln;
			spatialSource.synth.free;
			spatialSource.bus.free;
		};
		{
			var decoderSynth = decoder !? { decoder.synth };
			"Freeing binaural decoder (% % % source %)".format(decoderSynth,
				decoderGroup, decoderBus, sourceGroup).postln;
			decoderSynth.free;
			decoderGroup.free;
			decoder !? { decoder.close };
			decoderBus.free;
			sourceGroup.free;
			decoder = decoderBus = decoderGroup = sourceGroup = nil;
		}.value;
		sourceDict.clear;
	}

	set { arg defName ... controlsAndValues;
		var source = sourceDict.at(defName.asSymbol);
		if (source.notNil and: { source.synth.notNil }) {
			source.synth.set(*controlsAndValues);
		} {
			"spatial encoder not found for %".format(defName).error;
		}
	}

	map { arg defName ... controlsAndValues;
		var source = sourceDict.at(defName.asSymbol);
		if (source.notNil and: { source.synth.notNil }) {
			source.synth.map(*controlsAndValues);
		} {
			"spatial encoder not found for %".format(defName).error;
		}
	}

	playBuf { arg buf, azDeg, elevDeg, radius, rate, amp, start, length, echoAmp, echoDelay, echoAzDeg, echoElevDeg, echoRadius;
		if (buf.isKindOf(Buffer).not) {
			"playBuf requires a Buffer".error;
			^this;
		};
		if (decoderBus.isNil or: { sourceGroup.isNil }) {
			"spatialization isn't configured -- skipping".warn;
			^this;
		} {
			var echoSuffix, instrument;
			azDeg = azDeg ?? { 180.rand2 };
			elevDeg = elevDeg ?? { 30.rand2 };
			radius = radius ?? { exprand(1,30) };
			rate = rate ?? { exprand(0.7, 1.1) };
			amp = amp ?? { exprand(0.2, 0.5) };
			start = start ?? { 0 };
			length = length ?? { buf.duration };
			echoAmp = echoAmp ?? 0;
			echoDelay = echoDelay ?? exprand(0.01, 0.1);
			echoAzDeg = echoAzDeg ?? { azDeg + rrand(-150, 150) };
			echoElevDeg = echoElevDeg ?? { elevDeg.neg * rrand(0.5, 1) };
			echoRadius = echoRadius ?? { radius * exprand(1.5, 4.0) };
			echoSuffix = if (echoAmp > 0) { "_echo" } { "" };
			instrument = ("sample" ++ buf.numChannels ++ "_hoa" ++ hoaOrder ++ echoSuffix).asSymbol;
			(
				out: decoderBus,
				group: sourceGroup,
				buf: buf,
				rate: rate,
				amp: amp,
				start: start,
				length: length,
				az: azDeg.degrad,
				elev: elevDeg.degrad,
				radius: radius,
				echoAmp: echoAmp,
				echoDelay: echoDelay,
				echoAz: echoAzDeg.degrad,
				echoElev: echoElevDeg.degrad,
				echoRadius: echoRadius,
				instrument: instrument
			).play;
		}
	}

	prPlayPattern { arg defName, pattern, quant;
		var clock = owningBacalao.clock;
		quant = (quant ?? { #[1, 0] }) * clock.beatsPerBar;
		// Stretch it so the durations in bars are converted to beats on our clock
		pattern = pattern !? {
			// We set default mask 1 so we can use it for triggering with pset
			// (goes to 0/Rest when using PmaskBjork or degrade).
			pattern = Pmul(\stretch, Pfunc{clock.beatsPerBar}, pattern << Pbind(\group, sourceGroup, \out, decoderBus));
		};

		^Pdef(defName.asSymbol, pattern).play(clock, quant: quant);
	}

	playPat { arg defName, pattern, quant = 1;
		^this.prPlayPattern(defName, Pn(pattern, inf), quant);
	}

	oncePat { arg defName, pattern, quant = 1;
		^this.prPlayPattern(defName, pattern, quant);
	}

	stopPat { arg defNameOrNames;
		if (defNameOrNames.isSequenceableCollection.not and: {defNameOrNames.isString.not }) {
			defNameOrNames = defNameOrNames.asArray;
		};
		defNameOrNames.do{ arg defName;
			Pdef(defName.asSymbol).stop;
		};
	}

}

+Bacalao {

	*prAmbisonicInstalled {
		var available = \HOAEncoder.asClass.notNil and: { Bacalao.prVSTPluginInstalled };
		if (available.not) {
			"HOA functionality is not available\n==========\n"
			"  To use HOAs, you must install sc3-plugins, the SC-HOA Quark and the IEM Plug-in Suite.\n"
			"  Download and install the latest VSTs from: https://plugins.iem.at/download/\n"
			"  be sure sc3-plugins is in your \"Platform.userExtensionDir\"\n"
			"  and also run: Quarks.install(\"SC-HOA\")\n==========".warn
		};
		^available
	}

	spatialInit { arg hoaOrder = 3;
		hoaOrder = (hoaOrder ? 1).clip(1,3).debug("Using HOA order");
		if (Bacalao.prAmbisonicInstalled) {
			var hoaNumChannels = BacalaoSpatialSettings.hoaNumChannels(hoaOrder).debug("Ambisonic channels");
			if (spatial.notNil) {
				"Spatialization already initialized -- call b.spatialFree before reinitializing".error;
				^this;
			};
			server.waitForBoot {
				var directionFilter = { arg src, dirVec;
					var forwardDotProd;
					var dist;
					// "forward" here is along the +X axis,Y left,  Z up
					// Spherical(1, 0, 0.1pi).asCartesian.asArray; // consider this the "forward" direction of minimal filtering
					forwardDotProd = (dirVec * [0.95105651629515, 0.0, 0.30901699437495]).sum;
					// (Spherical(1, 150.degrad, 0.degrad).asCartesian.asArray * Cartesian(0.95105651629515, 0.0, 0.30901699437495).asArray).sum.linexp(-1,0.35, 1200, 14000);
					// BLowPass.ar(src, forwardDotProd.linexp(-1, 0.35, 2000, 14000), 3.5);
					BLowPass.ar(src, forwardDotProd.lincurve(-1, 0.35, 2000, 14000, -1), 3.5);
				};

				var hoaEncodeMono = { arg src, az, elev, gain, radius;
					src = HOAEncoder.ar(hoaOrder, src.first, az: az, elev: elev, gain: gain, plane_spherical: 1, radius: radius);
				};

				var tumbleVec = { arg vec, angle; // XZ-plane, in radians (pitch)
					var sinr = angle.sin, cosr = angle.cos;
					[(vec[0] * cosr) - (vec[2] * sinr), vec[1], (vec[0] * sinr) + (vec[2] * cosr)]
				};
				var rotateVec = { arg vec, angle; // XY-plane, in radians (yaw/heading)
					var sinr = angle.sin, cosr = angle.cos;
					[(vec[0] * cosr) - (vec[1] * sinr), (vec[0] * sinr) + (vec[1] * cosr), vec[2]]
				};
				var tiltVec = { arg vec, angle; // YZ-plane, in radians (roll)
					var sinr = angle.sin, cosr = angle.cos;
					[vec[0], (vec[1] * cosr) - (vec[2] * sinr), (vec[1] * sinr) + (vec[2] * cosr)]
				};

				// In ambisonic land: look along +X direction, +Y right, +Z up
				// Rotation of: +yaw/az (Z) to the left
				//              +pitch (Y) downward = -elevation (elevation goes upward)
				//              +roll (X) clockwise (when looking forward)
				var hoaEncode = { arg src, az, elev, roll, gain, radius, width = 0.5pi;
					var hoa;
					var pitch = -1 * elev;
					if (src.size == 2) {
						// Stereo source
						hoa = 2.collect{ arg i;
							var sig = src[i];
							var stereoAz = #[0.5, -0.5][i] * width;
							// var dir = rotateVec.value(tumbleVec.value([stereoAz.cos, stereoAz.sin, 0.0], elev), az);
							// sig = directionFilter.value(sig, dir);
							hoaEncodeMono.value(sig, stereoAz, 0, gain, radius)
						}.sum;
					} {
						// Mono or other source
						var sig = src.first;
						// var dir = rotateVec.value(tumbleVec.value([1.0, 0.0, 0.0], elev), az);
						// sig = directionFilter.value(sig, dir);
						hoa = hoaEncodeMono.value(sig, 0, 0, gain, radius);
					};
					hoa = HOATransRotateXYZ.ar(hoaOrder, hoa, roll: roll);
					hoa = HOATransRotateXYZ.ar(hoaOrder, hoa, pitch: pitch);
					HOATransRotateAz.ar(hoaOrder, hoa, az: az);
				};

				SynthDef(\hoaDecode, { arg inBus, outBus = 0;
					var sig = In.ar(inBus, hoaNumChannels);
					var plugin = VSTPlugin.plugins['BinauralDecoder'];
					Out.ar(outBus, VSTPlugin.ar(sig, numChannels, info: plugin));
				}).add;

				SynthDef(\hoaEncode1, { arg inBus, outBus;
					var src = In.ar(inBus, 2).sum;
					OffsetOut.ar(outBus,
						hoaEncode.value(src, \az.kr(0), \elev.kr(0), \roll.kr(0),
							\gain.kr(0, 0.5), \radius.kr(2, 0.5), /*width*/ 0));
				}).add;

				SynthDef(\hoaEncode2, { arg inBus, outBus;
					var src = In.ar(inBus, 2);
					OffsetOut.ar(outBus,
						hoaEncode.value(src, \az.kr(0), \elev.kr(0), \roll.kr(0),
							\gain.kr(0, 0.5), \radius.kr(2, 0.5), \width.kr(60.degrad, 2)));
				}).add;

				this.prSetupSpatialSynths;

				server.sync;

				{
					// Free existing HOA Synths, Busses and Groups if necessary
					var decoderBus = Bus.audio(server, hoaNumChannels);
					var decoderGroup = Group(server, \addAfter);
					// Free HOA encoder Synths and Busses if necessary
					var sourceGroup = Group(decoderGroup, \addBefore);
					var decoder = VSTPluginController(Synth(\hoaDecode,
						[inBus: decoderBus, outBus: 0],
						target: decoderGroup, addAction: \addToTail)).open("BinauralDecoder", action:
						{ arg self, success;
							if (success) {
								"Setting BinauralDecoder to N3D normalization".postln;
								self.set('Input Normalization', /*N3D*/ 0); // 'Input Normalization' is param 1
								// var presetName = "decode";
								// ("Loading BinauralDecoder preset: " ++ presetName).postln;
								// self.loadPreset(presetName, async: true);
								spatial = BacalaoSpatialSettings(
									decoder,
									decoderBus,
									decoderGroup,
									sourceGroup,
									this,
									hoaOrder);
							} {
								"Failed to load BinauralDecoder VST...spatialization won't work".error;
								this.spatialFree;
							}
						}
					);
				}.value;
			}
		}
	}

	spatialFree { arg playDefault = true;
		if (spatial.notNil) {
			var defs = spatial.sourceDict.keys;
			defs.do(this.despatialize(_, playDefault));
			spatial.free;
			spatial = nil;
		}
	}

	prCreateSpatialSource { arg stereo;
		var sourceBus = Bus.audio(server, numChannels);
		var synthName = if (stereo) { \hoaEncode2 } { \hoaEncode1 };
		var source = Synth(synthName,
			[\inBus, sourceBus, \outBus, spatial.decoderBus],
			target: spatial.sourceGroup, addAction: \addToTail);
		^BacalaoSpatialSource(source, sourceBus)
	}

	despatialize { arg defName, playDefault = true;
		var source = spatial !? { spatial.sourceDict[defName = defName.asSymbol] };
		if (source.notNil) {
			var p = this.proxy(defName);
			var oldQuant = p.quant;
			"stopping spatialization for %".format(defName).warn;
			if (p.isPlaying) { p.stop(0.3) };
			if (playDefault) { p.quant_(0).play(out: 0, fadeTime: 0.3) };
			p.quant = oldQuant;
			{
				source.synth.debug("freeing").free;
				source.bus.debug("freeing").free;
			}.defer(0.3 + 0.01);
			spatial.sourceDict[defName] = nil;
		} {
			"spatialization wasn't on for %".format(defName).warn;
		}
	}

	spatialize { arg defName, stereo = true, azDeg = 0, elevDeg = 0, radius = 2, widthDeg = 60;
		var source = spatial !? { spatial.sourceDict[defName = defName.asSymbol] };
		if (spatial.isNil) {
			"Call spatialInit before trying to spatialize".warn;
			^this
		};

		if (source.isNil) {
			source = this.prCreateSpatialSource(stereo);
			spatial.sourceDict[defName] = source;
		};
		{
			var p = this.proxy(defName);
			var encoder = source.synth;
			var bus = source.bus;
			encoder.set(\az, azDeg.degrad, \elev, elevDeg.degrad, \radius, radius.max(0.1));
			if (stereo) { encoder.set(\width, widthDeg.degrad) };
			if (p.isPlaying.not or: { p.monitor.outs.first != bus.index }) {
				var oldQuant = p.quant;
				if (p.isPlaying) { p.stop(0.3) } { "Proxy wasn't yet playing...".postln; };
				p.quant_(0).play(out: bus, group: spatial.sourceGroup, fadeTime: 0.3, addAction: \addToHead);
				p.quant = oldQuant;
				"Spatialized % on % source % (bus % group %)".format(defName, stereo.if("stereo","mono"), encoder, bus, spatial.sourceGroup).postln;
			}
		}.value;
	}

	showTree {
		var onClose, win = Window("Server Tree", Rect(1300,100,400,900), scroll: true).alwaysOnTop_(true).front;
		win.view.hasHorizontalScroller_(false).background_(Color(0.2,0.2,0.4));
		onClose = server.plotTreeView(0.5, win.view, { defer{win.close} });
		win.onClose = { onClose.value };
		^win;
	}

	prSetupSpatialSynths {
		var applyEnvAndHoaEncode = { arg sig, order, addEcho, rate, length;
			var hoa, env;
			var az = \az.kr(0).wrap(-pi, pi);
			var elev = \elev.kr(0).fold(-0.5pi, 0.5pi);
			var radius = \radius.kr(2).max(0.1);
			var maxEchoDelay = if (addEcho) { 0.5 } { 0 };

			// Free with a bit extra delay, so the echo gets fully played
			order = order.clip(1,3);
			env = EnvGen.kr(Env.linen(0.01, (length / rate.abs - 0.02).max(0), 0.01));
			FreeSelf.kr(TDelay.kr(Done.kr(env), maxEchoDelay));
			sig = sig * env;
			hoa = HOAEncoder.ar(order, sig, az, elev, 0, 1, radius);
			if (addEcho) {
				var echoLpf = \echoLpf.kr(800);
				var echoAmp = \echoAmp.kr(0);
				var echoDelay = \echoDelay.kr(0.03);
				var echoAz = \echoAz.kr(0).wrap(-pi, pi);
				var echoElev = \echoElev.kr(0).fold(-0.5pi, 0.5pi);
				var echoRadius = \echoRadius.kr(2.5).max(0.1);
				hoa = echoAmp * HOAEncoder.ar(
					order,
					DelayL.ar(BLowPass.ar(sig, echoLpf, 1.0), maxEchoDelay, echoDelay),
					echoAz,
					echoElev,
					0,
					1,
					echoRadius) + hoa;
			};
			hoa
		};

		var defineSynths = { arg addEcho;
			var suffix = if (addEcho) { "_echo" } { "" };
			SynthDef(("sample1_hoa3" ++ suffix).asSymbol, { arg out=0, buf, rate=1, amp=0.1, start=0.0, length=1.0;
				var sig = PlayBuf.ar(1, buf, rate * BufRateScale.kr(buf),
					1, start * SampleRate.ir, 1);
				var hoa = applyEnvAndHoaEncode.(sig * amp, 3, addEcho, rate, length);
				Out.ar(out, hoa);
			}).add;

			SynthDef(("sample2_hoa3" ++ suffix).asSymbol, { arg out=0, buf, rate=1, amp=0.1, start=0.0, length=1.0;
				var sig = PlayBuf.ar(2, buf, rate * BufRateScale.kr(buf),
					1, start * SampleRate.ir, 1).mean;
				var hoa = applyEnvAndHoaEncode.(sig * amp, 3, addEcho, rate, length);
				Out.ar(out, hoa);
			}).add;
		};

		defineSynths.(/*addEcho*/ false);
		defineSynths.(/*addEcho*/ true);
	}
}
