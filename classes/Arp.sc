Arp {

	//////////////////////////////////////////////////////////////////////
	// Arpeggiation methods
	//////////////////////////////////////////////////////////////////////

	*prGetArpNotes { arg notes, reverse = false, sort = true;
		var arr = if (notes.isKindOf(Symbol)) {
			Bacalao.vars.at(notes) ?? {
				Bacalao.vars.put(notes, []);
				Bacalao.vars.at(notes)
			}
		} {
			notes.asArray
		};
		if (sort) { arr = arr.asSortedList };
		^if (reverse) { arr.reverse } { arr };
	}

	*prGetArpOcts { arg oct = 0, reverse = false;
		var o = (0..oct.clip(0,3));
		^if (reverse) { o.reverse } { o }
	}

	*prArpPat { arg notes, oct = 0, reverse = false, sort = true, key = \note;
		var octs = this.prGetArpOcts(oct, reverse);
		^{
			var last = nil;
			var octIndex = 0;
			var reportedEmptyArray = false;
			loop{
				var arr = this.prGetArpNotes(notes, reverse, sort);
				if (arr.isEmpty) {
					if (reportedEmptyArray.not) {
						"Empty array % - returning Rest from arpRand".format(notes.cs).warn;
						reportedEmptyArray = true;
					};
					last = nil;
					octIndex = 0;
					Rest().yield;
				} {
					var curIndex = if (last.isNil) {
						-1
					} {
						arr.indexOf(last.nearestInList(arr.asSortedList));
					};
					var nextIndex = curIndex + 1;
					reportedEmptyArray = false;
					if (nextIndex.inRange(0, arr.size-1)) {
						last = arr[nextIndex];
						this.prOctaveShift(last, octs[octIndex], key).yield;
					} {
						octIndex = octIndex + 1;
						if (octIndex.inRange(0, octs.size-1).not) {
							nil.yield;
						} {
							// Start again, but with the next octave
							last = nil;
						}
					}
				}
			}
		}.p;
	}

	*prOctaveShift { arg value, octaveShift, key;
		^switch(key,
			// Should really use stepsPerOctave, but we don't have the Scale here
			\note, { octaveShift * 12 + value },
			\midinote, { octaveShift * 12 + value },
			\degree, { octaveShift * 7 + value },
			\freq, { 2**octaveShift * value },
			{ Error("Unknown pitch key type %".format(key.cs)).throw }
		)
	}

	*rand { arg notes, oct = 0, dur = (1/16), key = \note;
		var octs = this.prGetArpOcts(oct);
		var pat = {
			var last = nil;
			var octIndex = 0;
			var reportedEmptyArray = false;
			loop{
				var arr = this.prGetArpNotes(notes);
				if (arr.isEmpty) {
					if (reportedEmptyArray.not) {
						"Empty array % - returning Rest from arpRand".format(notes.cs).warn;
						reportedEmptyArray = true;
					};
					last = nil;
					octIndex = 0;
					Rest().yield;
				} {
					reportedEmptyArray = false;
					this.prOctaveShift(arr.choose, octs.choose, key).yield;
				}
			}
		}.p;
		^Pbind(key, pat, \dur, dur)
	}

	*up { arg notes, oct = 0, dur = (1/16), key = \note;
		var pat = this.prArpPat(notes, oct, /* reverse */ false, /* sort */ true, key);
		^Pbind(key, pat, \dur, dur)
	}

	*down { arg notes, oct = 0, dur = (1/16), key = \note;
		var pat = this.prArpPat(notes, oct, /* reverse */ true, /* sort */ true, key);
		^Pbind(key, pat, \dur, dur)
	}

	*order { arg notes, oct = 0, dur = (1/16), key = \note;
		var pat = this.prArpPat(notes, oct, /* reverse */ false, /* sort */ false, key);
		^Pbind(key, pat, \dur, dur)
	}

	*rev { arg notes, oct = 0, dur = (1/16), key = \note;
		var pat = this.prArpPat(notes, oct, /* reverse */ true, /* sort */ false, key);
		^Pbind(key, pat, \dur, dur)
	}

	*prIncExc { arg notes, oct = 0, excl = true, dur = (1/16), key = \note;
		var octs = this.prGetArpOcts(oct);
		var reportedEmptyArray = false;
		var pat = {
			var last = nil;
			var octIndex = 0;
			var offset = 1; // initially moving upward
			loop{
				var arr = this.prGetArpNotes(notes, /*reverse*/ false, /*sort*/ true);
				if (arr.isEmpty) {
					if (reportedEmptyArray.not) {
						"Empty array % - returning Rest from arpIncExc".format(notes.cs).warn;
						reportedEmptyArray = true;
					};
					last = nil;
					octIndex = 0;
					offset = 1;
					Rest().yield;
				} {
					var curIndex = if (last.isNil) {
						-1
					} {
						arr.indexOf(last.nearestInList(arr.asSortedList));
					};
					var nextIndex = curIndex + offset;
					reportedEmptyArray = false;
					if (nextIndex == arr.size) {
						if (octIndex == (octs.size - 1)) {
							// Change direction up->down
							offset = -1;
							nextIndex = nextIndex + offset + if(excl,-1,0);
						} {
							octIndex = octIndex + offset;
							nextIndex = 0;
						}
					};
					if (nextIndex < 0) {
						if (octIndex == 0) {
							// Change direction down->up
							offset = 1;
							nextIndex = nextIndex + offset + if(excl,1,0);
						} {
							octIndex = octIndex + offset;
							nextIndex = arr.size - 1;
						}
					};
					if (nextIndex.inRange(0,arr.size-1)) {
						last = arr[nextIndex];
						this.prOctaveShift(last, octs[octIndex], key).yield;
					} {
						last = arr.first;
					}
				}
			}
		}.p;
		^Pbind(key, pat, \dur, dur)
	}

	*incl { arg notes, oct = 0, dur = (1/16), key = \note;
		^this.prIncExc(notes, oct, false, dur, key)
	}

	*excl { arg notes, oct = 0, dur = (1/16), key = \note;
		^this.prIncExc(notes, oct, true, dur, key)
	}

}
