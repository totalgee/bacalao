DrumSet {
	var <drumDict;
	var <cutGroupDicts;

	*initClass {
		var playFunc = { arg numChannels, withGate;
			var freq = \freq.kr(60.midicps);
			var buf = \buf.kr(0);
			var rate = \rate.kr(1) * (freq / 60.midicps);
			var start = \start.kr(0);
			var length = \length.kr(1);
			var lpf = \lpf.kr(12000);
			var lpq = \lpq.kr(1);
			var drive = \drive.kr(1);
			var sig = PlayBuf.ar(numChannels, buf, rate * BufRateScale.kr(buf), 1, start * BufSampleRate.kr(buf), 1);
			var fade = 0.01;
			var timeToEnd = (BufDur.kr(buf) - start - fade).max(0);
			var segmentDur = min(length, timeToEnd) / rate.abs;
			var env = if (withGate) {
				EnvGen.ar(Env.linen(fade, (segmentDur - fade).max(0), fade), doneAction: Done.none) *
				EnvGen.ar(Env.asr(fade, 1, fade), \gate.kr(1), doneAction: Done.freeSelf);
			} {
				EnvGen.ar(Env.linen(fade, (segmentDur - fade).max(0), fade), doneAction: Done.freeSelf) *
				EnvGen.ar(Env.asr(fade, 1, fade), \cutGate.kr(1), doneAction: Done.freeSelf);
			};
			sig = BLowPass.ar(sig, lpf, lpq);
			sig = (sig * drive).softclip;
			sig * env;
		};

		SynthDef(\drumset, { arg out=0, amp=0.1, pan=0;
			var sig = playFunc.(1, /* withGate */ false);
			Out.ar(out, Pan2.ar(sig, pan, amp));
		}).add;

		Event.addEventType(\drum, { |server|
			if (~drum.notNil and: { ~drumSet.notNil } and: { ~drumSet.isKindOf(DrumSet) }) {
				if (~drum.asString.beginsWith("Rest(").not) {
					var drum = ~drum.asSymbol;
					var drumEntry = ~drumSet.drumDict[drum];
					var buf = drumEntry !? { drumEntry[\buf] };
					if (buf.notNil) {
						var cutGroupName = drumEntry[\cutGroup];
						var parentGroup = ~group.value ?? { server.defaultGroup };
						var cutGroupDict = {
							if (~drumSet.cutGroupDicts[parentGroup].isNil) {
								~drumSet.cutGroupDicts[parentGroup] = ();
							};
							~drumSet.cutGroupDicts[parentGroup]
						}.value;
						var group = cutGroupDict[cutGroupName];
						if (group.isNil) {
							cutGroupDict[cutGroupName] = group = Group(parentGroup ?? { ~proxy.group.value });
							~drumSet.cutGroupDicts[parentGroup] = cutGroupDict;
							"Created % for cut group % inside %\n".postf(group, cutGroupName, group.group);
							// "Forget" the old Group on Cmd-period
							CmdPeriod.doOnce { cutGroupDict[cutGroupName] = nil };
						};
						server.bind{
							group.set(\cutGate, 0);
							~type = \note;
							~instrument = \drumset;
							~buf = buf;
							~group = group;
							Event.eventTypes[\note].value(server);
						};
					} {
						"drum '%' not defined in DrumSet".format(drum).warn;
					}
				}
			} {
				"\\drum Event type must define \\drumSet (DrumSet) and \\drum (Symbol) keys".warn;
			};
		});
	}

	*new { arg drumDict, cutGroups;
		drumDict = drumDict.collect{ arg value, key;
			(buf: value, cutGroup: key)
		};
		cutGroups !? {
			cutGroups.do{ arg set;
				var setName = set.sort.join($_).asSymbol;
				set.do{ arg key;
					drumDict[key.asSymbol][\cutGroup] = setName;
				}
			}
		}
		^super.newCopyArgs(drumDict, ());
	}

	hit { arg drum ... args;
		var buf = drumDict[drum = drum.asSymbol][\buf];
		var cutGroupName = drumDict[drum][\cutGroup];
		var parentGroup = Server.default.asTarget.nodeID;
		var cutGroupDict = {
			if (cutGroupDicts[parentGroup].isNil) {
				cutGroupDicts[parentGroup] = ();
			};
			cutGroupDicts[parentGroup]
		}.value;
		var group = cutGroupDict[cutGroupName];
		if (group.isNil) {
			cutGroupDict[cutGroupName] = group = Group();
			cutGroupDicts[parentGroup] = cutGroupDict;
			"Created % for cut group % inside %\n".postf(group, cutGroupName, group.group);
			// "Forget" the old Group on Cmd-period
			CmdPeriod.doOnce { cutGroupDict[cutGroupName] = nil };
		};
		if (args.size == 1) {
			// So we also support a single Event arg
			args = args.first
		};
		args.postcs;
		if (buf.notNil) {
			Server.default.bind{
				group.set(\cutGate, 0);
				(instrument: \drumset, buf: buf, group: group).merge(args.asEvent).play;
			}.postcs;
		}
	}

}

// Maybe the best solution would be to create actual Groups
// (stored in a Dictionary) for each of the cut groups. These
// would be instantiated where needed (e.g. inside the NodeProxy group
// for Bacalao tracks). Then, simply call release() on these when
// firing new events, or else set() on a new \cutGate arg if these
// Synths don't have a \gate argument.

// Otherwise, use the "redefining" Default Event play() trick from
// https://scsynth.org/t/cut-groups-with-patterns/470/11
// although that seems to produce undesired "Node not found" messages
// from the Server.
