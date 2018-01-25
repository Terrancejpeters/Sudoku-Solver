import hw.sudoku._

class Test extends org.scalatest.FunSuite {

	val tstStr = "....8.3...6..7..84.3.5..2.9...1.54.8.........4.27.6...3.1..7.4.72..4..6...4.1...3"
	val Str0Grid = tstStr.toList.map(c => Solution.charGridHelper(c))
	val tstBoard0 = Solution.parse(tstStr)

	val tstStr1 = "2...8.3...6..7..84.3.5..2.9...1.54.8.........4.27.6...3.1..7.4.72..4..6...4.1...3"
	val Str1List = tstStr1.toList
	val Str1Grid = Str1List.map(c => Solution.charGridHelper(c))
	val tstBoard1 = Solution.parse(tstStr1)

	val allPoints = Solution.allPointsList(0).distinct

	val unsolvableString = "..888.3...6..7..84.3.5..2.9...1.54.8.........4.27.6...3.1..7.4.72..4..6...4.1...3"
	val badBoard = Solution.parse(unsolvableString)

	test("First Box Peers"){
		//println(Solution.peers(0,0))
		//println(Solution.peers(3,3))
		//println(Solution.peers(1,1))
		//println(Solution.peers(4,4))
	}

	test("all points test"){
		assert(Solution.allPointsList(0).distinct.size == 81)
		// println(allPoints)
		// println()
		// println()
	}

	test("charGridHelper test"){
		assert(Solution.charGridHelper('3') == List(3))
		assert(Solution.charGridHelper('.') == List(1,2,3,4,5,6,7,8,9))
	}

	test("basic rowParse test"){
		//println(Solution.rowSplit(Str1Grid,0)) //not removing here
		// println(Solution.rowParse(Str0Grid))
		// println()
		// println()
	}
	test("basic colParse test"){
		// println(Solution.colParse(Str0Grid))
		// println()
		// println()
	}

	test("basic boxParse test"){
		//println(Solution.boxParse(Str0Grid))
	}
	test("ROWCOL PARSE BABY"){
		//println(Solution.colParse(Solution.rowParse(Str1Grid)))
		//println(Solution.colParse(Solution.rowParse(Str0Grid)))
		assert(Solution.colParse(Solution.rowParse(Str0Grid)) == Solution.rowParse(Solution.colParse(Str0Grid)))
	}
	test("Board is made test"){
		println(tstBoard0)
		println()
		println()
		// val toZip = Solution.colParse(Solution.rowParse(Str0Grid))
		// println(allPoints.zip(toZip).toMap)
	}

	test("get valueAt"){
		// println(tstBoard0.valueAt(0,0))
		// println(tstBoard0.valueAt(1,0))
		// println(tstBoard0.valueAt(8,8))
	}

	test("isSolved on nonsolved board"){
		assert(tstBoard0.isSolved() == false)
		assert(tstBoard0.place(0,0,1).isSolved == true)
	}
	test("isUnsolvable test"){
		assert(tstBoard0.isUnsolvable() == false)
		assert(badBoard.isUnsolvable() == true)
	}

	test("place test"){
		println("PLACE TEST")
		println(tstBoard0.place(0,0,1))
	}

	test("Solve test"){
		println(tstBoard0.solve().get)
		println(tstBoard1.solve())
		println(badBoard.solve())
	}
	//ok cool gotta get rid of those duplicates
}