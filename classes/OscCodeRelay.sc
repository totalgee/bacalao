OscCodeRelay {
	classvar relay;
	classvar codeView;

	*connect { arg relayServerAddr = NetAddr("127.0.0.1", 11338);
		if (relay.isNil or: { relay.isConnected.not }) {
			relay = relayServerAddr;
			this.prConnect;
			this.prSetupOscHandler;
		};
		History.clear.end;
		History.start;
		this.createCodeView;
		// Use forwardFunc.addFunc, like History.start?
		History.forwardFunc = { arg str, value, func;
			if (str.beginsWith(this.asString).not) {
				// Don't share messages for calls to OscCodeRelay methods
				OscCodeRelay.prSendMsg("code", str);
			};
			codeView !? { codeView.refresh };
		};
	}

	*disconnect {
		this.prDisconnect;
		History.stop;
		History.forwardFunc = nil;
	}

	*createCodeView {
		var w;
		if (codeView.notNil and: { codeView.parent.notNil } and: { codeView.parent.isClosed.not }) {
			codeView.parent.onClose = nil; // don't want codeView to be reset in this case
			codeView.parent.close;
		};
		w = Window("Code").alwaysOnTop_(true).onClose_({ codeView = nil }).front;
		w.layout = HLayout(codeView = UserView().clearOnRefresh_(false)).margins_(0);
		codeView.drawFunc = { arg v;
			if (History.lines.isEmpty.not) {
				Pen.use {
					Pen.color = Color.grey(0.85, 0.2);
					Pen.fillRect(v.bounds.size.asRect);
					Pen.color = Color.black;
					Pen.stringInRect(History.lines.first[2],
						v.bounds.moveBy(rand(v.bounds.width/3), rand(3*v.bounds.height/4)).sect(v.bounds.size.asRect),
						Font("Arial", 13)
					);
				}
			}
		}
	}

	*prSetupOscHandler {
		"Setup OSC relay handler".postln;
		OSCdef(\relay, { arg msg, time, addr, recvPort;
			"% @ %".format(msg[1..].cs, time).postln;
			switch (msg[1].asSymbol)
			{ 'code' } {
				defer {
					var result;
					result = if (\BacalaoParser.asClass.notNil) {
						BacalaoParser.preProcess(msg[2].asString).interpret
					} {
						msg[2].asString.interpret
					};
					History.enter(msg[2].asString, 'remote');
					codeView !? { codeView.refresh };
					this.prSendMsg("result", result.asString);
				}
			}
		}, "/relay", relay);
	}

	*prCleanupOscHandler {
		"Cleanup OSC relay handler".postln;
		OSCdef(\relay).free;
	}

	*prConnect {
		relay.tryConnectTCP({ arg netAddr;
			if (netAddr.isConnected) {
				"TCP connection established".postln
			}
		}, {
			"TCP connection failed".postln
		}, 1);
	}

	*prDisconnect {
		if (relay.notNil and: { relay.isConnected }) {
			relay.disconnect;
			relay = nil;
			this.prCleanupOscHandler;
		};
	}

	*prValidate {
		if (relay.isConnected.not) {
			this.prConnect;
			Error("Not yet connected").throw;
		}
	}

	*prSendMsg { arg ... args;
		try {
			relay.sendMsg("/relay", *args);
		} { |err|
			if (err.isKindOf(PrimitiveFailedError)
				and: { err.failedPrimitiveName == '_NetAddr_SendMsg' }) {
				"Failed to send (likely server disconnected)".postln;
				this.prDisconnect;
			}
		}
	}

}
