+Bacalao {

	*clockWindow { arg server;
		var publicWinTopRight = 1920@(1080-20);
		var privateTopRight = Window.availableBounds.size.asPoint;
		var startMic = { arg inBus = 0;
			Ndef(\micInput, {
				var sig = Limiter.ar(BBandPass.ar(HPF.ar(SoundIn.ar(inBus), 20), 1000, 1, 1), 0.9);
				var maxDelay = 0.0005;
				var delay = SinOsc.kr(7,0).range(0, maxDelay);
				sig = DelayL.ar(sig, 0.1, [maxDelay - delay, delay], 1, 0);
				sig = NHHall.ar(sig, 3) * -15.dbamp + sig;
				sig
			}).play;
		};
		var toggleMic = {
			var ndef = Ndef(\micInput);
			if (ndef.isMonitoring) {
				ndef.clear(1);
				"Switched off contact mic".postln;
			} {
				"Enabled contact mic".postln;
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
		publicWin = Window("Timer").bounds_(Rect(publicWinTopRight.x - 220, publicWinTopRight.y - 100, 220, 100)).alwaysOnTop_(true).front;
		timeText = StaticText(publicWin, Rect(10,10, 200,80)).front.font_(Font("Arial", 48)).align_(\left);
		Tdef(\publicClock, { loop { 1.wait; {timeText.string = clk.cursecs.asTimeString.keep(8)}.defer } }).play(quant: 1);
		publicWin.onClose_{ Tdef(\publicClock).clear; "Stopped updating public clock".postln };
		clk.window.onClose_{ "Closed private clock".postln; clk.stop; publicWin.close };
		clk
	}

	clockWindow {
		Bacalao.clockWindow(server)
	}

}