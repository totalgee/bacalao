BacalaoProxy {
	classvar parse;

	var <relay;
	var <bacalao;

	*initClass {
		parse = BacalaoParser;
	}

	*new { arg relayServerAddr, scServer;
		var b = Bacalao(nil, scServer);
		relayServerAddr = relayServerAddr ?? { NetAddr("127.0.0.1", 11338) };
		^super.newCopyArgs(relayServerAddr, b).prConnect.prSetupOscHandler;
	}

	prSetupOscHandler {
		"Setup OSC handler".postln;
		OSCdef(\relay, { arg msg, time, addr, recvPort;
			"% @ %".format(msg[1..].cs, time).postln;
			switch (msg[1].asSymbol)
			{ 'p' } { bacalao.p(msg[2].asString.interpret, msg[3].asString.interpret, msg[4], msg[5], msg[6], msg[7].asBoolean) }
			{ 'tempo' } { bacalao.tempo = msg[2] }
			{ 'boot' } { bacalao.boot }
		}, "/relay", relay);
	}

	prConnect {
		relay.tryConnectTCP({ arg netAddr;
			if (netAddr.isConnected) {
				"TCP connection established".postln
			}
		}, {
			"TCP connection failed".postln
		}, 1);
	}

	prValidate {
		if (relay.isConnected.not) {
			this.prConnect;
			Error("Not yet connected").throw;
		}
	}

	prSendMsg { arg ... args;
		try {
			relay.sendMsg(*args);
		} { |err|
			if (err.isKindOf(PrimitiveFailedError)
				and: { err.failedPrimitiveName == '_NetAddr_SendMsg' }) {
				"Failed to send (likely server disconnected)".postln;
				relay.disconnect;
			}
		}
	}

	p { arg trkName, pattern, dur, quant, role, includeMask = true;
		this.prValidate;
		this.prSendMsg("/relay", "p", trkName.cs, if (pattern.notNil) { pattern.cs }{ $N }, dur ? $N, quant ? $N, role ? $N, includeMask);
	}

	boot {
		this.prValidate;
		this.prSendMsg("/relay", "boot");
	}

	tempo {
		^bacalao.tempo;
	}

	tempo_ { arg tempo;
		this.prValidate;
		this.prSendMsg("/relay", "tempo", tempo);
	}

}
