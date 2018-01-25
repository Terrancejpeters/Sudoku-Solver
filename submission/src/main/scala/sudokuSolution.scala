import hw.sudoku._
object Solution extends SudokuLike {
	type T = Board
	def parse(str: String): Board = {
		
		//create a list of all 81 tuples
		val allPoints = allPointsList(0).distinct

		//map these two together, then remove values from the lists that represent spots without values
		//Filter based on rows, send into next step
			//take every 9 values of GridFilled, 
			//make list of every value who's size is 1, call this solidVals (?)
			//filter out every one based on what's in solidVals(if there are two of the same type, it'll be empty. USe this as a clue that the board is unsolvable)
		
		//Filter based on columns, send into next step
		val strList = str.toList
		val GridFilledValues = strList.map(c => charGridHelper(c))
		val rowFiltered = rowParse(GridFilledValues)
		val rowcolFiltered = colParse(rowFiltered)
		val TOTALfiltered = boxParse(rowcolFiltered)
		

		//send this into a Map of Keys-> Vals
		//STILL NEED TO FILTER BY THE GRID THEY'RE IN, but this is a partial solution for now
		val boardInput = allPoints.zip(TOTALfiltered).toMap

		new Board(boardInput)
		
		//Filter based on Squares
	} 

	def allPointsList(currRow: Int): List[(Int,Int)] = {
		if (currRow == 8){
			allPointsHelper(8,0)
		}
		else{
			allPointsHelper(currRow,0) ::: allPointsList(currRow + 1)
		}
	}

	def allPointsHelper(row: Int, currCol: Int): List[(Int,Int)] = {
		if(currCol < 9){
			(row, currCol) :: allPointsHelper(row,currCol + 1)
		}
		else{
			Nil
		}
	}

	def charGridHelper(t: Char): List[Int] = {
		val full = List(1,2,3,4,5,6,7,8,9)
		if (t.isDigit){
			List(t.asDigit)
		}
		else{
			full
		}
	}

	def rowParse(theGrid: List[List[Int]]): List[List[Int]] = {
		//take every 9 values of GridFilled, 
		//make list of every value who's size is 1, call this solidVals (?)
		//filter out every one based on what's in solidVals(if there are two of the same type, it'll be empty. USe this as a clue that the board is unsolvable)
		val splitGrid = rowSplit(theGrid,0)
		val filteredGrid = splitGrid.map(x => listFilter(x))
		filteredGrid.flatten
	}

	//recursively split into slices of 9 elements, one per row
	//only used in rowParse
	def rowSplit(theGrid:List[List[Int]], currentPos: Int): List[List[List[Int]]] = {
		if (currentPos < 81){
			theGrid.slice(currentPos, currentPos+9) :: rowSplit(theGrid,currentPos+9)
		}
		else{
			Nil
		}
	}

	def colParse(theGrid: List[List[Int]]): List[List[Int]] ={
		//take every 9th value of GridFilled into a list, 
		//make list of every value who's size is 1, call this solidVals (?)
		//filter out every one based on what's in solidVals(if there are two of the same type, it'll be empty. USe this as a clue that the board is unsolvable)
		val splitGrid = colSplit(theGrid,0)
		val filteredGrid = splitGrid.map(y => listFilter(y))
		val fixGrid = colSplit(filteredGrid.flatten,0)
		fixGrid.flatten

	}

	//only used in colSplit
	def colSplitHelp(theGrid: List[List[Int]], currentPos: Int): List[List[Int]] = {
		if (currentPos < 81){
			theGrid(currentPos) :: colSplitHelp(theGrid,currentPos+9)
		}
		else{
			Nil
		}
	}

	//only used in colParse
	def colSplit(theGrid: List[List[Int]], startPos: Int): List[List[List[Int]]] = {
		if (startPos < 8){
			colSplitHelp(theGrid,startPos) :: colSplit(theGrid,startPos+1)
		}
		else{
			List(colSplitHelp(theGrid,8))
		}
	}

	def boxParse(theGrid:List[List[Int]]): List[List[Int]] = {
		//fuck it I'm just gonna hard code it
		//maybe boxSpliter?
		val splitGrid = boxSplit(theGrid,0)
		val filteredGrid = splitGrid.map(z => listFilter(z))
		val fixGrid = boxSplit(filteredGrid.flatten,0)
		fixGrid.flatten
	}

	def boxSplit(theGrid: List[List[Int]], cBox: Int): List[List[List[Int]]] = {
		if (cBox > 8){
			Nil
		}
		else{
			boxSplitHelper(theGrid,cBox) :: boxSplit(theGrid, cBox + 1)
		}
	}

	def boxSplitHelper(theGrid: List[List[Int]], theBox: Int): List[List[Int]] = {
		theBox match{
			case 0 => theGrid.slice(0,3) ::: theGrid.slice(9,12) ::: theGrid.slice(18,21)
			case 1 => theGrid.slice(3,6) ::: theGrid.slice(12,15) ::: theGrid.slice(21,24)
			case 2 => theGrid.slice(6,9) ::: theGrid.slice(15,18) ::: theGrid.slice(24,27)
			case 3 => theGrid.slice(27,30) ::: theGrid.slice(36,39) ::: theGrid.slice(45,48)
			case 4 => theGrid.slice(30,33) ::: theGrid.slice(39,42) ::: theGrid.slice(48,51)
			case 5 => theGrid.slice(33,36) ::: theGrid.slice(42,45) ::: theGrid.slice(51,54)
			case 6 => theGrid.slice(54,57) ::: theGrid.slice(63,66) ::: theGrid.slice(72, 75)
			case 7 => theGrid.slice(57,60) ::: theGrid.slice(66,69) ::: theGrid.slice(75,78)
			case 8 => theGrid.slice(60,63) ::: theGrid.slice(69,72) ::: theGrid.slice(78,81)
		}
	}
	

	def listFilter(theList: List[List[Int]]): List[List[Int]] = {
		val singList = getSingles(theList)
		theList.map(a => {
			if (a.size > 1){
				a.filterNot(x => singList.contains(x))
			}
			else{
				if(containsSingDup(singList)){
					List()
				}
				else{
					a
				}
			}
		})
	}

	def containsSingDup(theList: List[Int]): Boolean = {
		theList.size != theList.distinct.size
	}

	def getSingles(theList: List[List[Int]]): List[Int] = {
		theList match{
			case a :: b => {
				if (a.size == 1){
					a.head :: getSingles(b)
				}
				else{
					getSingles(b)
				}
			}
			case Nil => Nil
		}
	}

	/////////////


	// You can use a Set instead of a List(or, any Iterable)
	def peers(row: Int, col: Int): List[(Int, Int)] = {
		val rowPeers = peersHelperRow(col,0,row)
		val colPeers = peersHelperCol(row,0,col)
		val RowsCols = rowPeers.union(colPeers)

		//find zeroBox representation through row % 3 & col % 3
		//subtract these values from the row and col respectively, consider this the number to be added for all remaining vals in "boxList"
		//filter out the one value of boxList that is equal to the tuple (zeroBoxRow,zeroBoxCol)
		val boxList = List((0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2))
		val zeroBoxRow = row % 3
		val zeroBoxCol = col % 3
		val rowDiff = row - zeroBoxRow
		val colDiff = col - zeroBoxCol

		val filteredBoxList = boxList.filter(x => x != (zeroBoxRow,zeroBoxCol))
		val transformedBoxList = filteredBoxList.map(entry => entry match{	case (a,b) => (a + rowDiff, b + colDiff)})

		val allwithDups = RowsCols.union(transformedBoxList)
		allwithDups.distinct
	}

	def peersHelperRow(col: Int, currRow: Int, noRow: Int): List[(Int,Int)] ={
		if (currRow == noRow){
			peersHelperRow(col,currRow + 1, noRow)
		}
		else if (currRow < 9){
			(currRow, col) :: peersHelperRow(col,currRow + 1, noRow)
		}
		else{
			//List((8,col))
			Nil
		}
	}

	def peersHelperCol(row: Int, currCol: Int, noCol: Int): List[(Int,Int)] = {
		if (currCol == noCol){
			peersHelperCol(row,currCol+1,noCol)
		}
		else if(currCol < 9){
			(row, currCol) :: peersHelperCol(row,currCol + 1, noCol)
		}
		else{
			//List((row, 8))
			Nil
		}
	}

}
// Top - left corner is(0,0).Bottom - right corner is(8,8).Feel free to
// change the fields of this class .
class Board(val available: Map[(Int, Int), List[Int]]) extends BoardLike[Board] {
	def availableValuesAt(row: Int, col: Int): List[Int] = {
		// Assumes that a missing value means all values are available .Feel
		// free to change this .
		available.getOrElse((row, col), 1.to(9).toList)
	}
	def valueAt(row: Int, col: Int): Option[Int] = {
			val theList = available.get(row,col).get
			if(theList.size == 1){
				Some(theList(0))
			}
			else{
				None
			}
		}
	def isSolved(): Boolean = {
		available.forall(t => t._2.size == 1)
	}
	def isUnsolvable(): Boolean = {
		!available.forall(t => t._2.size != 0)
	}
	
	

	def place(row: Int, col: Int, value: Int): Board = {
		require(availableValuesAt(row, col).contains(value))
		val helpBoard = new Board(this.available + ((row, col) -> List(value)))
		placehelper(helpBoard,Solution.peers(row,col),value)
	}




	def placehelper(cboard: Board, inspected: List[(Int,Int)], value: Int): Board = {
		inspected match {
			case x :: y =>{
				val row = x._1
				val col = x._2
				val BUDS = Solution.peers(x._1,x._2)
				val openSpace = cboard.availableValuesAt(row,col)
				val openFiltered = openSpace.filter(v => v != value)
				if (openSpace.size != openFiltered.size){
					val toBoard = new Board(cboard.available + (x -> openFiltered))
					if (openFiltered.size == 1){
						val nextHelper = placehelper(toBoard,BUDS,openFiltered(0))
						placehelper(nextHelper, y, value)
					}
					else{
						placehelper(toBoard,y,value)
					}
				}
				else {
					placehelper(cboard, y, value)
				}
			}
			case Nil => cboard
		}
	}
	// You can return any Iterable(e.g., Stream)
	def nextStates(): List[Board] = {
		if(isUnsolvable()) {
			List()
		}
		else if (isSolved){
			List()
		}
		else{
			nStatesRowHelp(0)

		}
	}

	def nStatesRowHelp(currRow: Int): List[Board] = {
		if(currRow < 8){
			nStatesColHelp(currRow,0) ::: nStatesRowHelp(currRow + 1)
		}
		else{
			nStatesColHelp(8,0)
		}
	}

	def nStatesColHelp(currRow: Int, currCol: Int): List[Board] = {
		if(currCol < 8){
			nStatesPlaceHelp(currRow,currCol, available.get(currRow,currCol).get) ::: nStatesColHelp(currRow, currCol + 1)
		}
		else{
			nStatesPlaceHelp(currRow,8,available.get(currRow,currCol).get)
		}
	}

	def nStatesPlaceHelp(currRow: Int, currCol: Int, valsLeft: List[Int]): List[Board] = {
		if (valsLeft.size > 1){
			place(currRow,currCol,valsLeft.head) :: nStatesPlaceHelp(currRow,currCol,valsLeft.tail)
		}
		else{
			List(place(currRow,currCol,valsLeft.head))
		}
		
	}



	def solve(): Option[Board] = {
		if(isSolved()){
			Some(this)
		}
		else if (isUnsolvable()){
			None
		}
		else{
			val nextBoards = nextStates()
			val getSolved = nextBoards.filter(x => x.isSolved())
			val potentialBoards = nextBoards.filterNot(x => x.isUnsolvable() == true)
		
			if(getSolved.size > 0){
				Some(getSolved(0))
			}
			
			else if(potentialBoards.size > 0){
				val maybeSolved = potentialBoards.map(x => x.solve())
				if (maybeSolved.exists(x => x.isDefined)){
					maybeSolved.filter(x => x.isDefined)(0)
				}
				else{
					None
				}
			}
			else{
				None
			}
		}
	}
}