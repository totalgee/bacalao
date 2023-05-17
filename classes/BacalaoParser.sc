// This helper class implements all the parsing and data extraction helpers
// that are used by the main Bacalao class.
BacalaoParser {
	classvar eventAbbrevs;
	const unsignedInt = "\\d+";
	const eventPattern = "(?:[^a-z\":@]*|(@|\\b[a-z][_a-zA-Z0-9]*))(?:~([a-z][_a-zA-Z0-9]*))?\"([^\"\\n]*)\"";
	// const eventPattern = "(@|[a-z][_a-zA-Z0-9]*)(~([a-z][_a-zA-Z0-9]*))?\"([^\"\\n]*)\"";
	const <charPattern = "(?:[^a-z@]*|(@|\\b[a-z][_a-zA-Z0-9]*))(?:~([a-z][_a-zA-Z0-9]*))?'([^'\\n]*)'";
	classvar numberInt;
	const unsignedFloat = "(?:(?:[0-9]+)?\\.)?[0-9]+";
	const nonArraySpace = "[^[:space:]\\][]+";
	const elemWithoutMods = "[^[:space:]\\][@*!]+";
	const balancedAngleBracket = "(<((?>[^><]|(?1))*)>)";
	const chord = "<([^>< ]+)>";
	classvar <reCharEventsPerBar;
	classvar numberFloat;
	const rest = "~";
	const barSplitChar = $|;
	classvar elemModifiers;
	classvar balancedArray;
	classvar <patternValueArg; // balancedArray or numberWithMods
	classvar arrayElem;
	classvar simpleElemPartial;
	classvar simpleElem;
	const word = "\\b[[:alpha:]_]\\w*\\b";
	const label = "(?:[[:alnum:]]+)";
	// [ overallMatch, trkName ]
	const trkName = "^\\s*\\b(\\w+)\\s*:";
	// [ overallMatch, cmd, argStr ]
	const subCmd = "[:&]\\s*(\\w+)([^:&]*)";
	const <numericExpression = "[0-9./*+()-]+";

	*initClass {
		var number;
		var numberWithMods;
		var labelWithMods;
		var elemWithMods;
		var arrayWithMods;
		// Number or Bjorklund sequence args e.g. (3,8) or (7,16,1), but can also
		// extend durations after optional initial Rest: (x3,8) or (x7,16,1)
		var unsignedIntOrBjork;
		numberInt = "-?" ++ unsignedInt;
		unsignedIntOrBjork = unsignedInt ++ "|\\(x?" ++ unsignedInt ++ "," ++ unsignedInt ++ "(?:," ++ numberInt ++ ")?\\)";
		reCharEventsPerBar = "^(" ++ unsignedInt ++ ")@";
		numberFloat = "-?" ++ unsignedFloat;
		// elemModifiers must be in the correct order (optional repeat, then optional hold, then optional duplicate)
		elemModifiers = "(?:\\*(" ++ unsignedIntOrBjork ++ "))?(?:@(" ++ unsignedFloat ++ "))?(?:!(" ++ unsignedIntOrBjork ++ "))?";
		// [ overallMatch, balancedArray, arrayRepeat, arrayHold, arrayDuplicate ]
		balancedArray = "(?:(" ++ "\\[(?>[^][]|(?1))*\\])" ++ elemModifiers ++ ")";
		number = "(?:" ++ numberFloat ++ ")|(?:" ++ rest ++ ")";
		numberWithMods = "(?:(" ++ number ++ ")" ++ elemModifiers ++ ")";
		labelWithMods = "(?:(" ++ label ++ ")" ++ elemModifiers ++ ")";
		elemWithMods = "(?:(" ++ elemWithoutMods ++ ")" ++ elemModifiers ++ ")";
		// [ overallMatch, simpleElem, arrayElem, arrayRepeat, arrayHold, arrayDuplicate, unrecognized ]
		arrayElem = "(" ++ nonArraySpace ++ ")" ++ "|(?:(" ++ "[[](?>[^\][]|(?2))*[\]])" ++ elemModifiers ++ ")|([^[:space:]]+)";
		// [ overallMatch, num, numRepeat, numHold, numDuplicate, labelElem, labelRepeat, labelHold, labelDuplicate ]
		simpleElemPartial = "(?:(?:" ++ numberWithMods ++ ")|(?:" ++ labelWithMods ++ "))";
		// [ overallMatch, elem, repeat, hold, duplicate ]
		simpleElem = "^(?:" ++ elemWithMods ++ ")$";
		patternValueArg = "^\\s*(?:(?:" ++ balancedArray ++ ")|(?:" ++ numberWithMods ++ "))\\s*$";

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
			scl: \scale,
			sus: \sustain,
			vel: \velocity,
			slow: \stretch,
			str: \stretch,
			toff: \timingOffset,
		);
		// Other event members without abbreviations
		// root: 0, stepsPerOctave: 12, octaveRatio: 2.0, note: (not including root or octave),
		// freq, tempo, dur: 1.0, lag: 0.0, strum: 0.0, strumEndsTogether: false
		// amp, db: -20.0, pan: 0.0, trig: 0.5, group, out: 0, addAction: 0, variant: nil,
		// type: (e.g. \midi), latency: 0.2,

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

	*prResolveVariables { arg elems, patternType, optVariableName;
		var resolveChord = { arg elem;
			// If we've got the ChordSymbol Quark installed, try to lookup as a chord
			if (\ChordSymbol.asClass.notNil
				and: { elem.first.isUpper }
				and: { elem.beginsWith("Rest").not }
				and: { elem.beginsWith("ALTERNATE").not }) {

				var elemSym = elem.asSymbol;
				var scale = Scale.major;
				switch (patternType)
				{ \degree } {
					var degs = ChordSymbol.asDegrees(elemSym, scale);
					if (degs == elemSym) {
						degs = NoteSymbol.asDegree(elemSym, scale);
					};
					if (degs != elemSym) {
						if (degs.size > 1) { degs } { degs.first }
					} {
						elem
					}
				}
				{ \note } {
					var notes = ChordSymbol.asNotes(elemSym);
					if (notes == elemSym) {
						notes = NoteSymbol.asNote(elemSym);
					};
					if (notes != elemSym) {
						if (notes.size > 1) { notes } { notes.first }
					} {
						elem
					}
				}
				{ \midinote } {
					var midinotes = NoteSymbol.asNote(elemSym);
					if (midinotes == elemSym) {
						midinotes = ChordSymbol.asNotes(elemSym);
						if (midinotes != elemSym and: { midinotes.first < scale.pitchesPerOctave }) {
							// If someone entered a chord without specifying an octave, shift near middle C
							midinotes = midinotes + 60;
						}
					};
					if (midinotes != elemSym) {
						if (midinotes.size > 1) { midinotes } { midinotes.first }
					} {
						elem
					}
				}
				{
					var notes = elem.asSymbol.asNoteOrChord.debug("asNoteOrChord");
					if (notes.size > 1) { notes } { notes.first }
				} //.debug("ChordSymbol") };
			} {
				if (patternType == \drum) {
					if (elem[0] == $\\) { elem = elem.drop(1) };
					elem.asSymbol.cs
				} {
					// Stick with the original element, there is no lookup
					elem
				}
			}
		};

		var parserVariables = if (optVariableName.isEmpty.not) {
			currentEnvironment[optVariableName.asSymbol]
		} {
			currentEnvironment
		};
		if (parserVariables.isKindOf(Dictionary).not) {
			Error("'~%' lookup Dictionary not found".format(optVariableName)).throw;
		};

		// Replace Bacalao parser variables with their values
		^elems.collect{ arg elem;
			var substitute = Bacalao.varLookup(elem, parserVariables);
			if (substitute.isKindOf(Prand)) {
				// The "elem" must convert to a string with no spaces
				substitute = substitute.cs.reject(_.isSpace);
			};
			substitute ?? { resolveChord.value(elem) };
		}
	}

	*prVariableNameToString { arg elem;
		^if (elem.size == 1) {
			// Return a Char, not Symbol, for one-letter names,
			// otherwise ChordSymbol screws up the lookup by resolving
			// it as a "note" when embedding the Symbol.
			// (see ChordSymbol overload for Symbol.embedInStream)
			elem.first.cs;
		} {
			if (elem.beginsWith("ALTERNATE")) {
				// Don't put quotes around alternates, which are code
				// that will be substituted later.
				elem
			} {
				// We return these as Strings rather than Symbols, because
				// otherwise the ChordSymbol class can mess things up when
				// it embeds Symbols (when using variable lookup in PnSymRest).
				elem.asString.cs
			}
		}
	}

	*prResolvePatternSyntax { arg pair;
		var value = pair[1];
		var key, optVariableName;
		#key, optVariableName = pair[0].asString.split($~);
		key = this.resolveAbbrev(key);
		optVariableName = optVariableName ? "";
		// Note that we do support key~dict key names here,
		// but not (yet) the '@~dict' form for Event patterns
		// that are supported by the Bacalao interpreter/parser.
		^case
		{ value.isString } {
			this.prReplaceStringPattern(key, optVariableName, value).interpret.patternpairs;
		}
		{ value.isArray and: value.first.class == Char } {
			this.prReplaceCharPattern(key, optVariableName, value.join).interpret.patternpairs;
		} {
			[key, value]
		};
	}

	*prReplaceStringPattern { arg patternType, optVariableName, patternString;
		// Convert abbreviation to long name (if one is found)
		var replaceStr;
		var replacements = [];
		var numAlternates = [];
		patternType = this.resolveAbbrev(patternType);
		// Figure out the replacement string if there are any "alternating"
		// elements, written e.g. as "<1 2 3>" (which means 1 first time around,
		// 2 the second, and 3 the third). The total number of cycles
		// required to see everything in the pattern is the least common multiple
		// of all alternate counts.

		if (patternType == '@' and: { optVariableName.isEmpty }) {
			Error("Can't use default (non-variable) lookup with string pattern").throw;
		};

		// Replace rests
		// @todo Be more selective, allowing env. variables such as "[~foo <1 ~bar>]"
		if (optVariableName.isEmpty) {
			// In the case of variable/namespace lookup, then ~ is allowed;
			// it is interpreted as "Rest" by the PnsymRest class.
			patternString = patternString.replace("~", this.prGetRestString(patternType));
		} {
			if (currentEnvironment[optVariableName.asSymbol].isKindOf(Dictionary).not) {
				Error("'~%' lookup Dictionary not found".format(optVariableName)).throw;
			};
		};

		// First try to split on comma-separated elements in angle brackets (chords)
		this.findChord(patternString).reverseDo{ arg m;
			var alternateElements = BacalaoParser.splitSimplify(m[1], $,);
			if (alternateElements.size > 1) {
				// Replace Bacalao parser variables with their values
				if (optVariableName.isEmpty) {
					alternateElements = this.prResolveVariables(alternateElements, patternType, optVariableName);
				} {
					alternateElements = alternateElements.collect{ arg e;
						this.prVariableNameToString(e);
					};
				};

				patternString = patternString.replaceAt("ALTERNATE" ++ replacements.size.asPaddedString(4), m[0]-1, m[1].size+2);
				replacements = replacements.add("Ptuple([%])".format(alternateElements.join($,)));
			};
		};

		// Now try to split on space-separated elements in angle brackets (alternation)
		this.findAngleBracketed(patternString).reverseDo{ arg m;
			var alternateElements = BacalaoParser.splitSimplify(m[1]);

			alternateElements = alternateElements.collect{ arg elem;
				var elemMatch = this.findSimpleElem(elem);
				if (elemMatch.size == 1) {
					var full, elem, repeat, hold, dupl;
					if (elemMatch[0].size != 5) {
						Error("Unexpected match size: %".format(elemMatch[0])).throw
					};
					#full, elem, repeat, hold, dupl = elemMatch[0];
					if (elem.isEmpty) {
						Error("There is no array element: %".format(full)).throw
					};
					repeat = if (repeat.isEmpty) { 1 } { repeat.asInteger };
					hold = if (hold.isEmpty) { 1 } { hold.asFloat };
					// Note: we don't support Bjorklund e.g. (3,8) inside angle brackets
					dupl = if (dupl.isEmpty) { 1 } { dupl.asInteger };
					if (hold != 1 or: { repeat != 1 }) {
						Error("We don't support hold or repeat inside alternating <> elements, only '!' duplicate").throw
					};
					elem ! dupl;
				} {
					Error("Invalid alternate element entry: %".format(elem.cs)).throw;
				};
			}.flatten;

			// Replace Bacalao parser variables with their values
			if (optVariableName.isEmpty) {
				alternateElements = this.prResolveVariables(alternateElements, patternType, optVariableName);
			} {
				alternateElements = alternateElements.collect{ arg e;
					this.prVariableNameToString(e);
				};
			};
			numAlternates = numAlternates.add(alternateElements.size);
			patternString = patternString.replaceAt("ALTERNATE" ++ replacements.size.asPaddedString(4), m[0]-1, m[1].size+2);
			replacements = replacements.add("Ppatlace([%],inf)".format(alternateElements.join($,)));
		};

		{
			// var patternArray = this.splitSimplify(patternString.replace("~", this.prGetRestString(patternType)));
			var elemsAndDurs = [];
			var elems, durs, resolveChord;
			// Support "|" as bar-split symbol. For example, the following two
			// should be equivalent (but the first is easier to understand):
			//   deg"[1 2 3]@2 | 4 5 | 6*3" (play 1,2,3 over two bars, 4,5 over 1 and 6,6,6 over 1)
			//   deg"[[1 2 3]@2 [4 5] 6*3]@4" (same, but you need to figure out the total bars (4) yourself)
			patternString.split(barSplitChar).do{ arg patternString;
				elemsAndDurs = elemsAndDurs ++ {
					var patternArray = this.parseArray(patternString);
					if (patternArray.size > 1) {
						// "Wrapping top-level pattern array".postln;
						patternArray = [ patternArray -> 1];
					};
					this.calculateDurations(patternArray);
				}.value;
			};
			// Replace alphabetic-only strings by Symbol notation: 'symbol'
			// patternArray = patternArray.collect{ |p| "^[A-Za-z]+$".matchRegexp(p).if(p.asSymbol.cs, p) };
			#elems, durs = elemsAndDurs.flop;

			// Replace Bacalao parser variables with their values, unless we're
			// using an explicit "namespace" variable.
			if (optVariableName.isEmpty) {
				elems = this.prResolveVariables(elems, patternType, optVariableName);
			};

			// *Don't* convert strings to symbols here...this allows you to evaluate
			// variables (e.g. n = Pwhite(0,7,1); deg"n*4") and arbitrary code in patterns!
			// elems = elems.collect{ |elem| "^[A-Za-z]+$".matchRegexp(elem).if(elem.asSymbol.cs, elem) };
			if (elems.size > 1) {
				var numCyclesRequired = numAlternates.reduce(\lcm) ? 1.0;
				var longElemStr = if (optVariableName.isEmpty) {
					var variables = String.streamContents({ arg stream;
						elems.printOn(stream); });
					"Ppatlace(%, %)".format(variables, numCyclesRequired);
				} {
					var pnsym = if (patternType == '@') { "PnsymRestEvent" } { "PnsymRest" };
					"%(Ppatlace([%], %), ~%)".format(pnsym, elems.collect{|e|
						if (e.beginsWith("ALTERNATE"))
						{ e }
						{ this.prVariableNameToString(e) }
					}.join($,), numCyclesRequired, optVariableName);
				};
				var longDursStr = String.streamContents({ arg stream;
					durs.printOn(stream); });
				//var longDursStr = String.streamContents({ arg stream;
				//	durs.collect(_.asStringPrec(17)).printOn(stream); });
				if (patternType == '@') {
					replaceStr = "Pchain(Pbind('dur', Pseq(%, %)), Pn(%, %))".format(longDursStr, numCyclesRequired, longElemStr, numCyclesRequired);
				} {
					replaceStr = "Pbind('%', %, 'dur', Pseq(%, %))".format(patternType, longElemStr, longDursStr, numCyclesRequired);
				}
			} {
				var elemStr = if (optVariableName.isEmpty) {
					String.streamContents({ arg stream; elems[0].printOn(stream); });
				} {
					var pnsym = if (patternType == '@') { "PnsymRestEvent" } { "PnsymRest" };
					var e = this.prVariableNameToString(elems[0].value);
					"%(Pseq([%]), ~%)".format(pnsym, e, optVariableName);
				};
				if (durs[0] === 1) {
					// In the case of a single entry at the top level (and no array braces)
					// just return a single value repeatedly, with no duration.
					// If you want a single event that lasts one cycle, use
					// "[0]" or "0@1"
					if (patternType == '@') {
						replaceStr = "Pn(%)".format(elemStr);
					} {
						replaceStr = "Pbind('%', %)".format(patternType, elemStr);
					}
				} {
					if (patternType == '@') {
						replaceStr = "Pchain(Pbind('dur', Pn(%, 1)), Pn(%))".format(durs[0], elemStr);
					} {
						replaceStr = "Pbind('%', %, 'dur', Pn(%, 1))".format(patternType, elemStr, durs[0]);
					}
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

		}.value;
		if (replaceStr.notNil) {
			replacements.reverseDo{ arg replacement, i;
				var index = replacements.size - 1 - i;
				replaceStr = replaceStr.replace("ALTERNATE" ++ index.asPaddedString(4), replacement);
			};
		};
		^replaceStr
	}

	*prProcessPatterns { arg code;
		var curOffset = 0;
		var pat = code.findRegexp(eventPattern).clump(4).reject{|m| m[1][1].isEmpty};
		// pat.postln;
		pat.do{ arg p;
			var fullMatch = p[0];
			var patternType = p[1].last;
			var optVariableName = p[2].last;
			var patternString = p[3].last;
			var replaceStr = this.prReplaceStringPattern(
				patternType, optVariableName, patternString
			);
			// [curOffset, fullMatch.first, fullMatch.last, replaceStr].postln;
			(fullMatch.last -> replaceStr).postln;
			// [patternArray, replaceStr].postln;
			if (replaceStr.notNil) {
				code = code.replaceAt(replaceStr, fullMatch.first + curOffset, fullMatch.last.size);
				// code.postln;
				curOffset = curOffset + replaceStr.size - fullMatch.last.size
			}
		};
		^code
	}

	*prGetRest { arg patternType;
		^if (patternType == '@') {
			// These are Event patterns (the variable lookup expects an Event)
			(mask: Rest())
		} {
			// These are all the rest: simple value patterns
			Rest()
		}
	}

	*prGetRestString { arg patternType;
		// Must have no spaces!
		^this.prGetRest(patternType).cs.reject(_.isSpace)
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
				'amp', [ 26.reciprocal*0.5, 26.reciprocal, (_ + 26.reciprocal), 0, (_ + 10.reciprocal) ],
				'pan', [ -1, -1, (_ + 12.5.reciprocal), -1, (_ + 4.5.reciprocal) ],
				'@', { Error("Can't use default (non-variable) lookup with char string pattern").throw },
				[ 0, 0, (_ + 1) ] // Default values
			);
			result = case
			{ ch == Char.space } {
				this.prGetRest(patternType) }
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
				this.prGetRest(patternType)
			};
			result
		};
		^patternArray
	}

	*prEvalVariableCharString { arg barString, variableName;
		//var parserVariables = this.vars;
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
		// Support $| as bar-split symbol
		var bars = patternString.split(barSplitChar);
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
					patternArray = patternArray.add(this.prGetRest(patternType) -> (nextBar - patternDur));
				};
				(patternArray -> (nextBar / eventsPerBar))
			} {
				(patternArray -> 1)
			}
		};
		^bars.flatten
	}

	*prReplaceCharPattern { arg patternType, optVariableName, patternString;
		// Convert abbreviation to long name (if one is found)
		var replaceStr;
		patternType = this.resolveAbbrev(patternType);
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
						var pnsym = if (patternType == '@') { "PnsymRestEvent" } { "PnsymRest" };
						"%(Pseq(%), ~%)".format(pnsym, elems.collect{|e| e.value}.join.cs, elems.first.key);
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
				if (patternType == '@') {
					replaceStr = "Pchain(Pbind('dur', Pseq(%, %)), Pseq(%, %))".format(longDursStr, numCyclesRequired, longElemStr, numCyclesRequired);
				} {
					replaceStr = "Pbind('%', Pseq(%, %), 'dur', Pseq(%, %))".format(patternType, longElemStr, numCyclesRequired, longDursStr, numCyclesRequired);
				}
			} {
				if (elems[0].isKindOf(Association)) {
					var pnsym = if (patternType == '@') { "PnsymRestEvent" } { "PnsymRest" };
					elems[0] = "%(Pseq(%), ~%)".format(pnsym, elems.collect{|e| e.value}.join.cs, elems.first.key);
				};

				if (durs[0] === 1) {
					// In the case of a single entry at the top level (and no array braces)
					// just return a single value repeatedly, with no duration.
					// If you want a single event that lasts one cycle, use
					// "[0]" or "0@1"
					if (patternType == '@') {
						replaceStr = "Pn(%)".format(elems[0]);
					} {
						replaceStr = "Pbind('%', %)".format(patternType, elems[0]);
					}
				} {
					if (patternType == '@') {
						replaceStr = "Pchain(Pbind('dur', %), Pn(%))".format(durs[0], elems[0]);
					} {
						replaceStr = "Pbind('%', %, 'dur', %)".format(patternType, elems[0], durs[0]);
					}
				}
			};
		}.value;
		^replaceStr
	}

	*prProcessCharPatterns { arg code;
		var curOffset = 0;
		var pat = code.findRegexp(charPattern).clump(4).reject{|m| m[1][1].isEmpty};
		// pat.postln;
		pat.do{ arg p;
			var fullMatch = p[0];
			var patternType = p[1].last;
			var optVariableName = p[2].last;
			var patternString = p[3].last;
			var replaceStr = this.prReplaceCharPattern(
				patternType, optVariableName, patternString
			);

			// [curOffset, fullMatch.first, fullMatch.last, replaceStr].postln;
			(fullMatch.last -> replaceStr).postln;
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
		^(str.findRegexp(balancedArray, offset).flop[1] ?? #[]).clump(5)
	}

	*findArrayElem { arg str, offset=0;
		^(str.findRegexp(arrayElem, offset).flop[1] ?? #[]).clump(7)
	}

	*findSimpleElem { arg str, offset=0;
		^(str.findRegexp(simpleElem, offset).flop[1] ?? #[]).clump(5)
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

	*findTrkName { arg str, offset=0;
		^(str.findRegexp(trkName, offset).flop[1] ?? #[]).clump(2)
	}

	*findSubCmd { arg str, offset=0;
		^(str.findRegexp(subCmd, offset).flop[1] ?? #[]).clump(3)
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

	*prGetRepeatsArray { arg repeatStr;
		^case
		{ repeatStr.isEmpty } {
			#[1]
		}
		{ repeatStr.first == $( } {
			var k, n, shift;
			var trimmed = repeatStr.trim("()");
			// We provide two options:
			// "(3,8,1)" will give a Euclidean sequence of uniform durations with interspersed Rests
			//   [ Rest(1), Rest(1), 1, Rest(1), Rest(1), 1, Rest(1), 1 ]
			// "(x3,8,1)" will give a Euclidean sequence of extended durations (after optional initial Rest)
			//   [ Rest(2), 3, 2, 1 ]
			if (trimmed.first == $x) {
				trimmed = trimmed.drop(1);
				#k, n, shift = trimmed.split($,).asInteger;
				shift = shift ? 0;
				// This is (almost) equivalent to Bjorklund2(k, n).rotate(-1 * shift),
				// except that we add an initial Rest when required to support rotating
				// through all the possible 'n' positions (not just 'k' positions).
				Bjorklund(k, n).rotate(-1 * shift).integrate.separate{ |a,b| a != b }.collect{ arg x;
					if (x.first == 0) {
						Rest(x.size)
					} {
						x.size
					}
				}
			} {
				#k, n, shift = trimmed.split($,).asInteger;
				shift = shift ? 0;
				Bjorklund(k, n).rotate(-1 * shift).collect{ arg x; if (x == 0) { Rest(1) } { x } }
			};
		}
		{
			1 ! repeatStr.asInteger
		}
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
	//   ! replicate/duplicate a pattern
	//
	// Not yet implemented:
	//   , parallel patterns
	//   / slow down a pattern
	//   < > alternate between patterns (Place)
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
			var full, elem, subArr, subArrRepeat, subArrHold, subArrDupl, invalidElem;
			// item.debug("item");
			#full, elem, subArr, subArrRepeat, subArrHold, subArrDupl, invalidElem = item;
			// elem.debug("elem");
			if (elem.notEmpty) {
				var elemMatch = this.findSimpleElem(elem);
				// elemMatch.debug("elemMatch");
				if (elemMatch.size == 1) {
					var full, elem, repeat, hold, dupl;
					if (elemMatch[0].size != 5) {
						Error("Unexpected match size: %".format(elemMatch[0])).throw
					};
					#full, elem, repeat, hold, dupl = elemMatch[0];
					if (elem.isEmpty) {
						Error("There is no array element: %".format(full)).throw
					};
					repeat = this.prGetRepeatsArray(repeat);
					hold = if (hold.isEmpty) { 1 } { hold.asFloat };
					dupl = this.prGetRepeatsArray(dupl);
					// "Adding a result: %".format(elem).postln;
					if (repeat != #[1]) {
						elem = repeat.collect{ arg r; if (r.isRest) { "Rest()" }{ elem } -> r.value };
					};
					dupl.do{ arg d;
						arr = arr.add(if (d.isRest) { "Rest()" }{ elem } -> (hold * d.value));
					};
				} {
					Error("Invalid array entry: %".format(elem.cs)).throw;
				}
			} {
				var repeat = this.prGetRepeatsArray(subArrRepeat);
				var hold = if (subArrHold.isEmpty) { 1 } { subArrHold.asFloat };
				var dupl = this.prGetRepeatsArray(subArrDupl);
				var subArrResult;
				subArr = subArr.drop(1).drop(-1); // get rid of outer brackets
				// if (verbose) { "Will parseArray recursively on %".format(subArr).postln };
				subArrResult = this.parseArray(subArr);
				if (repeat != #[1]) {
					subArrResult = repeat.collect{ arg r; if (r.isRest) { "Rest()" }{ subArrResult } -> r.value };
				};
				dupl.do{ arg d;
					arr = arr.add(if (d.isRest) { "Rest()" }{ subArrResult } -> (hold * d.value));
				};
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
