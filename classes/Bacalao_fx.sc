+Bacalao {

	fxPrintControls { arg trkNameAndIndex;
		var trkName, indices, proxy;
		var showKey = { arg k;
			([\i_out, \out, \gate, \fadeTime].includes(k).not
				and: {k.asString.keep(3) != "wet"})
		};
		if (trkNameAndIndex.isKindOf(Association)) {
			#trkName, indices = [trkNameAndIndex.key, trkNameAndIndex.value.asArray];
		} {
			trkName = trkNameAndIndex;
		};
		proxy = this.proxy(trkName);
		indices = indices ?? { proxy.objects.indices };
		indices.do{ arg index;
			var obj = proxy.objects[index];
			var wetKey = ("wet" ++ index).asSymbol;
			var wet = proxy.nodeMap[wetKey];
			var showValues = { arg c;
				if (showKey.value(c.name)) {
					// value is the curent node map value, or else the default if it's not set
					var value = proxy.nodeMap[c.name];
					var defaultValue = "(default: " ++ c.defaultValue ++ ")";
					value = if (value.isNil) { defaultValue } { value + defaultValue };
					"    % = %".format(c.name, value).postln
				}
			};

			if (index == 0) {
				proxy.nodeMap.keys.do{ arg k;
					if (showKey.value(k)) {
						" (set) % = %".format(k, proxy.nodeMap[k]).postln
					}
				}
			};

			"=== Slot % (%)".format(index, if (wet.notNil) { "% = %".format(wetKey, wet) } { obj.class }).postln;
			case
			{ obj.isKindOf(SynthDefControl) } {
				proxy.objects[index].synthDef.allControlNames.do(showValues);
			}
			{ obj.isKindOf(SynthControl) } {
				proxy.objects[index].synthDesc.controls.do(showValues);
			}
		}
	}

	// Some predefined effects can can be used with b.fx
	fxPanAz { arg azimDeg = 0, width = 45;
		^{ arg in;
			var az = (\panAzim.kr(azimDeg, 0.1) / 180).circleRamp(0.1,-1,1);
			var w = \panWidth.kr(width, 0.1) / 180;
			PanAz.ar(numChannels, in[0..1], [-0.5,0.5] * w + az, orientation: 0).sum
		}
	}

	// Buffers should be an array of L/R pairs of Buffers, matching the number
	// of channels being used by Bacalao (b.numChannels).
	// For example, for an 8-output system with the speakers arranged in
	// a regular octagon, you might use HRTF buffers representing the impulse
	// response at 0, 45, 90, ..., 270, 315 degree azimuths.
	fxBinaural { arg bufPairs;
		^{ arg in;
			if (bufPairs.size != numChannels) {
				Error("bufPairs must be an Array of size %, containing pairs of Buffers".format(numChannels)).throw;
			};
			bufPairs.collect{ arg bufPair, i;
				Convolution2.ar(in[i], bufPair, 0, (bufPair[0].numFrames/2-1).nextPowerOfTwo);
			}.sum * bufPairs.size.sqrt.reciprocal;
		}
	}

	fxLpf { arg lpf = 1200, lpq = 1;
		^{ arg in;
			var freq = \lpf.kr(lpf, 0.1);
			var rq = \lpq.kr(lpq, 0.1);
			BLowPass.ar(in, freq, rq)
		}
	}

	// Nice, noisier analog-simulation LPF (DFM1)
	fxLpfAnalog { arg lpf = 8000, filtRes = 0.95, filtPre = 1, filtNoise = 0.003;
		^{ arg in;
			var freq = \lpf.kr(lpf, 0.1);
			var res = \filtRes.kr(filtRes, 0.1);
			var inputgain = \filtPre.kr(filtPre, 0.1);
			var noiselevel = \filtNoise.kr(filtNoise, 0.1);
			DFM1.ar(in, freq, res, inputgain, 0, noiselevel)
		}
	}

	fxBpf { arg bpf = 1200, bpq = 1;
		^{ arg in;
			var freq = \bpf.kr(bpf, 0.1);
			var bw = \bpq.kr(bpq, 0.1);
			BBandPass.ar(in, freq, bw, /* volume compensation */ bw.max(0.001).reciprocal.sqrt.max(1))
		}
	}

	fxHpf { arg hpf = 1200, hpq = 1;
		^{ arg in;
			var freq = \hpf.kr(hpf, 0.1);
			var rq = \hpq.kr(hpq, 0.1);
			BHiPass.ar(in, freq, rq)
		}
	}

	// Nice, noisier analog-simulation HPF (DFM1)
	fxHpfAnalog { arg hpf = 200, filtRes = 0.95, filtPre = 1, filtNoise = 0.003;
		^{ arg in;
			var freq = \hpf.kr(hpf, 0.1);
			var res = \filtRes.kr(filtRes, 0.1);
			var inputgain = \filtPre.kr(filtPre, 0.1);
			var noiselevel = \filtNoise.kr(filtNoise, 0.1);
			DFM1.ar(in, freq, res, inputgain, 1, noiselevel)
		}
	}

	fxVerb { arg t60 = 1.0, damp = 0.2;
		^{ arg in;
			var verbPre = \verbPre.kr(1, 0.05);
			t60 = \t60.kr(t60, 0.1);
			damp = \damp.kr(damp, 0.1);
			in.clump(2).collect{ arg pair;
				JPverb.ar(pair * verbPre, t60, damp)
			}.flatten
		}
	}

	fxVerbLite { arg t60 = 1.0, stereo = 0.5;
		^{ arg in;
			var verbPre = \verbPre.kr(1, 0.05);
			t60 = \t60.kr(t60, 0.1);
			stereo = \stereo.kr(stereo, 0.1);
			in.clump(2).collect{ arg pair;
				NHHall.ar(pair * verbPre, t60, stereo)
			}.flatten
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
			var delayPre = \delayPre.kr(1, 0.05);
			var delayTime = \delTime.kr(delayBeats/this.tempo, 1).max(0);
			var decayTime = \delDecay.kr(decayBeats/this.tempo, 0.1);
			CombL.ar(HPF.ar(in.reverse * delayPre, 20), delayTime * 2, delayTime, decayTime)
		}
	}

	fxDelayDub { arg delBeats = 1.5, delFb = 0.5, delWobble = 0.002, noiseLevel = 0;
		^{ arg in;
			var delayPre = \delayPre.kr(1, 0.05);
			var sep = \delWobble.kr(delWobble, 0.1);
			var delayTime = (\delTime.kr(delBeats/this.tempo, 1) - ControlDur.ir).max(0);
			var fb = \delFb.kr(delFb, 0.1);
			var noise = \delNoise.kr(noiseLevel, 0.1);
			var delHpf = \delHpf.kr(400, 0.1);
			var delLpf = \delLpf.kr(5000, 0.1);

			var numChans = in.asArray.size;
			var local = LocalIn.ar(numChans);
			local = { arg sig;
				var left, right;
				sig = sig*fb + (in * delayPre);
				sig = sig * LPF.ar(WhiteNoise.ar(noise!numChans, 1), 1200);
				sig = HPF.ar(sig, delHpf);
				sig = LPF.ar(sig, delLpf);
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

	fxDelayPingPong { arg delBeats = 1.5, delFb = 0.5, delWobble = 0.002, noiseLevel = 0;
		^{ arg in;
			var delayPre = \delayPre.kr(1, 0.05);
			var sep = \delWobble.kr(delWobble, 0.1);
			var delayTime = \delTime.kr(delBeats/this.tempo, 1);
			var fb = \delFb.kr(delFb, 0.1);
			var noise = \delNoise.kr(noiseLevel, 0.1);
			var delHpf = \delHpf.kr(250, 0.1);
			var delLpf = \delLpf.kr(5000, 0.1);
			var numChans = in.asArray.size;

			var delaySamps = max(0, delayTime * SampleRate.ir - ControlDur.ir).round;
			var phase, feedbackChannels, delayed;
			var buf = LocalBuf(SampleRate.ir * delayTime * 4, numChans).clear;
			var frames = BufFrames.kr(buf);

			// A customized version of PingPong UGen
			phase = Phasor.ar(0, 1, 0, frames);
			feedbackChannels = LocalIn.ar(numChans) * fb;
			delayed = BufRd.ar(numChans, buf, (phase - delaySamps).wrap(0, frames), 0);

			delayed = delayed * LPF.ar(WhiteNoise.ar(noise!numChans, 1), 1200);

			delayed = HPF.ar(delayed, delHpf);
			delayed = LPF.ar(delayed, delLpf);
			delayed = delayed.tanh;
			delayed[0] = DelayC.ar(delayed[0], 0.5, LFNoise2.ar(11).range(0, sep));
			delayed[1] = DelayC.ar(delayed[1], 0.5, LFNoise2.ar(11).range(sep, 0));
			LocalOut.ar(delayed);

			BufWr.ar((Pan2.ar(in.sum * numChans.sqrt.reciprocal * delayPre, 1) + feedbackChannels).rotate(1) <! delayed.asArray.first, buf, phase, 1);
			delayed
		}
	}

	fxDistort { arg gainIn = 30, gainOut = 0.4;
		^{ arg in;
			var distGain = \distGain.kr(gainIn, 0.1);
			var distOut = \distOut.kr(gainOut, 0.1);
			(in * distGain).softclip * distOut
		}
	}

	fxDistortCross { arg crossAmp = 0.5, crossSmooth = 0.5;
		^{ arg in;
			var amp = \crossAmp.kr(crossAmp, 0.1);
			var smooth = \crossSmooth.kr(crossSmooth, 0.1);
			CrossoverDistortion.ar(in, amp, smooth)
		}
	}

	fxCrush { arg bits = 4;
		// This version rounds to the square root of values, so
		// more precision is given to quiet sounds, less to loud
		^{ arg in;
			var crush = \bits.kr(bits).max(1);
			in.sqrt.round(0.5 ** (crush-1))**2
		}
	}

	fxCrushHard { arg bits = 4;
		// This is just normal, linear bit crushing
		^{ arg in;
			var crush = \bits.kr(bits).max(1);
			in.round(0.5 ** (crush-1))
		}
	}

	fxDownsample { arg sampleRate = 8000.0;
		^{ arg in;
			var sr = \samplerate.kr(sampleRate).clip(1, SampleRate.ir);
			Lag.ar(Latch.ar(in, Impulse.ar(sr)), sr.reciprocal*2)
		}
	}

	fxDecimate { arg sampleRate = 8000.0, bits = 4;
		^{ arg in;
			var crushBits = \bits.kr(bits).max(1);
			var sr = \samplerate.kr(sampleRate).clip(1, SampleRate.ir);
			Decimator.ar(in, sr, crushBits)
		}
	}

	fxWaveLoss { arg lossDrop = 20, lossOutOf = 40;
		^{ arg in;
			var drop = \lossDrop.kr(lossDrop, 0.1);
			var outof = \lossOutOf.kr(lossOutOf, 0.1);
			WaveLoss.ar(in, drop, outof)
		}
	}

	fxSquiz { arg squizRatio = 2, squizZeroCross = 1;
		^{ arg in;
			var pitchratio = \squizRatio.kr(squizRatio, 0.1).max(1);
			var zcperchunk = \squizZeroCross.kr(squizZeroCross, 0.1).max(1);
			Squiz.ar(in, pitchratio, zcperchunk, 0.2)
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
