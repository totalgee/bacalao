+Bacalao {

	fxPrintControls { arg defNameAndIndex;
		var defName, indices, proxy;
		var showKey = { arg k;
			([\i_out, \out, \gate, \fadeTime].includes(k).not
				and: {k.asString.keep(3) != "wet"})
		};
		if (defNameAndIndex.isKindOf(Association)) {
			#defName, indices = [defNameAndIndex.key, defNameAndIndex.value.asArray];
		} {
			defName = defNameAndIndex;
		};
		proxy = this.proxy(defName);
		indices = indices ?? { proxy.objects.indices };
		indices.do{ arg index;
			var obj = proxy.objects[index];
			if (index == 0) {
				proxy.nodeMap.keys.do{ arg k;
					if (showKey.value(k)) {
						" (set) % = %".format(k, proxy.nodeMap[k]).postln
					}
				}
			};

			"=== Slot % (%)".format(index, obj.class).postln;
			if (obj.isKindOf(SynthDefControl)) {
				proxy.objects[index].synthDef.allControlNames.do({arg c;
					if (showKey.value(c.name)) {
						// value is the curent node map value, or else the default if it's not set
						var value = proxy.nodeMap[c.name];
						var defaultValue = "(default: " ++ c.defaultValue ++ ")";
						value = if (value.isNil) { defaultValue } { value + defaultValue };
						"    % = %".format(c.name, value).postln
					}
				})
			};
		}
	}

	// Some predefined effects can can be used with b.fx
	fxLpf { arg lpf, lpq;
		^{ arg in;
			var freq = \lpf.kr(lpf, 0.1);
			var rq = \lpq.kr(lpq, 0.1);
			BLowPass.ar(in, freq, rq)
		}
	}

	fxBpf { arg bpf, bpq;
		^{ arg in;
			var freq = \bpf.kr(bpf, 0.1);
			var bw = \bpq.kr(bpq, 0.1);
			BBandPass.ar(in, freq, bw)
		}
	}

	fxHpf { arg hpf, hpq;
		^{ arg in;
			var freq = \hpf.kr(hpf, 0.1);
			var rq = \hpq.kr(hpq, 0.1);
			BHiPass.ar(in, freq, rq)
		}
	}

	fxVerb { arg t60 = 1.0, damp = 0.2;
		^{ arg in;
			t60 = \t60.kr(t60, 0.1);
			damp = \damp.kr(damp, 0.1);
			JPverb.ar(in, t60, damp)
		}
	}

	fxVerbLite { arg t60 = 1.0, stereo = 0.5;
		^{ arg in;
			t60 = \t60.kr(t60, 0.1);
			stereo = \stereo.kr(stereo, 0.1);
			NHHall.ar(in, t60, stereo)
		}
	}

	fxCompress { arg compThresh = 0.5, compRatio = 2.0, compClamp = 0.01, compRelax = 0.1;
		^{ arg in;
			var thresh = \compThresh.kr(compThresh, 0.1);
			var ratio = \compRatio.kr(compRatio, 0.1).max(1);
			var clampTime = \compClamp.kr(compClamp, 0.1);
			var relaxTime = \compRelax.kr(compRelax, 0.1);
			CompanderD.ar(in, thresh, 1, ratio.reciprocal, clampTime, relaxTime)
		}
	}

	fxLimit { arg limitLevel = 0.7, limitDur = 0.01;
		^{ arg in;
			var level = \limitLevel.kr(limitLevel, 0.1);
			var dur = \limitDur.kr(limitDur, 0.1);
			Limiter.ar(in, level, dur)
		}
	}


	fxDelay { arg delayBeats = 1.5, decayBeats = 2.0;
		^{ arg in;
			var delayTime = \delTime.kr(delayBeats/this.tempo, 1).max(0);
			var decayTime = \delDecay.kr(decayBeats/this.tempo, 0.1);
			CombL.ar(HPF.ar(in.reverse, 20), delayTime * 2, delayTime, decayTime)
		}
	}

	fxDelayDub { arg delBeats = 1.5, delFb = 0.5, delWobble = 0.002, noiseLevel = 0;
		^{ arg in;
			var sep = \delWobble.kr(delWobble, 0.1);
			var delayTime = (\delTime.kr(delBeats/this.tempo, 1) - ControlDur.ir).max(0);
			var fb = \delFb.kr(delFb, 0.1);

			var numChans = in.asArray.size;
			var local = LocalIn.ar(numChans);
			local = { arg sig;
				var left, right;
				sig = sig*fb + in;
				if (noiseLevel > 0) {
					sig = sig * LPF.ar(WhiteNoise.ar(noiseLevel!numChans, 1), 1200);
				};
				sig = HPF.ar(sig, 400);
				sig = LPF.ar(sig, 5000);
				sig = sig.tanh;
				left = DelayC.ar(sig.last, 0.5, LFNoise2.ar(11).range(0, sep));
				right = DelayC.ar(sig.first, 0.5, LFNoise2.ar(11).range(sep, 0));
				sig = [left, right];
			}.value(local);
			local = DelayN.ar(local, delayTime * 2, delayTime);
			LocalOut.ar(local);
			local;
		}
	}

	fxCrush { arg bits = 4;
		// This version rounds to the square root of values, so
		// more precision is given to quiet sounds, less to loud
		^{ arg in;
			var crush = \crush.kr(bits).max(1);
			in.sqrt.round(0.5 ** (crush-1))**2
		}
	}

	fxCrushHard { arg bits = 4;
		// This is just normal, linear bit crushing
		^{ arg in;
			var crush = \crush.kr(bits).max(1);
			in.round(0.5 ** (crush-1))
		}
	}

	fxDownsample { arg sampleRate = 8000.0;
		^{ arg in;
			var sr = \samplerate.kr(sampleRate).clip(1, SampleRate.ir);
			Latch.ar(in, Impulse.ar(sr)).lag(sr.reciprocal*2)
		}
	}

	fxPitchShift { arg shiftSemi = -12, shiftWin = 0.2, shiftPitchDisp = 0.0, shiftTimeDisp = 0.0;
		^{ arg in;
			var pitchRatio = \shiftSemi.kr(shiftSemi, 0.1).midiratio.clip(0, 4);
			var windowSize = \shiftWin.kr(shiftWin, 0.1);
			var pitchDispersion = \shiftPitchDisp.kr(shiftPitchDisp, 0.1);
			var timeDispersion = \shiftTimeDisp.kr(shiftTimeDisp, 0.1);
			PitchShift.ar(in, windowSize, pitchRatio, pitchDispersion, timeDispersion);
		}
	}

	fxTremolo { arg tremRate = 7.0, tremDepth = 0.5;
		^{ arg in;
			var rate = \tremRate.kr(tremRate, 0.1);
			var depth = \tremDepth.kr(tremDepth, 0.1).clip(0, 1);
			var amp = SinOsc.ar(rate, 0.5pi, depth, 1 - depth);
			in * amp;
		}
	}

	fxVibrato { arg vibRate = 4.0, vibDepth = 0.2;
		^{ arg in;
			var rate = \vibRate.kr(vibRate, 0.1);
			var depth = \vibDepth.kr(vibDepth, 0.1).clip(0, 1);
			var delay = SinOsc.kr(rate, 0, 0.005, 0.005) * depth; // from 0-10ms (at full strength)
			DelayL.ar(in, 0.1, delay);
		}
	}

	fxFlange { arg flanRate = 0.1, flanDepth = 0.5, flanFb = 0.1;
		^{ arg in;
			var rate = \flanRate.kr(flanRate, 0.1);
			var depth = \flanDepth.kr(flanDepth, 0.1).clip(0, 1);
			var fb = \flanFb.kr(flanFb, 0.1);
			var delay = SinOsc.kr(rate, 0, 0.005, 0.005) * depth; // from 0-10ms (at full strength)
			var effect;
			var numChans = in.asArray.size;
			in = in + LocalIn.ar(numChans);
			effect = DelayL.ar(in, 0.1, delay * Rand(0.9,1.1!numChans));
			LocalOut.ar(effect * fb);
			effect;
		}
	}

	fxGate { arg gateBeatFreq = 2.0, gatePhase = 0, gateWidth = 0.5, gateLag = 0.05;
		^{ arg in;
			var freq = \gateFreq.kr(gateBeatFreq * this.tempo, 0.1);
			var phase = \gatePhase.kr(gatePhase, 0.1).clip(0, 1);
			var width = \gateWidth.kr(gateWidth, 0.1).clip(0, 1);
			var lag = \gateLag.kr(gateLag, 0.1);
			in * LFPulse.ar(freq, phase, width).range(0,1).lag(lag)
		}
	}

}
