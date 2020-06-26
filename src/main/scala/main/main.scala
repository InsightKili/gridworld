package gridworld

object main extends App {
    def createGridWorld(rows: Int, col: Int, wall: List[(Int,Int)], winState: (Int,Int), looseState: (Int, Int) ): gridWorld = {
        val stateSet: Set[gridState] = (for {
            i <- (0 until rows)
            j <- (0 until col)
            if !(wall contains (i,j))
        } yield gridState(i,j)).toSet

        val finalStates: Map[gridState, StateType] = Map((gridState(winState._1,winState._2), Win),(gridState(looseState._1,looseState._2), Loose) )

        new gridWorld(stateSet, finalStates)
    }

    val myGridWorld = createGridWorld(3,4,List((1,1)),(0,3),(1,3))

    val Value: Map[gridState, Double] = (for(s <- myGridWorld.stateSpace) yield (s, 0.5)).toMap

    val PolicyMap: Map[(gridAction, gridState), Double] = Map( 
    ((U, gridState(2,0)) ,1.0),
    ((U, gridState(1,0)) ,1.0),
    ((R, gridState(0,0)) ,1.0), 
    ((R, gridState(0,1)) ,1.0),
    ((R, gridState(0,2)) ,1.0),
    ((L, gridState(2,1)) ,1.0),
    ((U, gridState(2,2)) ,1.0),
    ((U, gridState(1,2)) ,1.0),
    ((L, gridState(2,3)) ,1.0),
    ).withDefaultValue(0.0)

    def Policy(a: gridAction, s: gridState):Double = PolicyMap(a,s)

    val myGridAgent = new gridAgent(Value,Policy)

    var myGridModel = new gridModel(myGridAgent, myGridWorld)

    myGridModel = myGridModel.evaluate(0.0001, 0.9) 

    def printValueF(model: gridModel):String = {

     val newStringBuilder = new StringBuilder("")
     val rows = model.env 
     for(i<- 0 until 3) {
        for(j<- 0 until 4) {
            if (List((1,1),(0,3),(1,3)) contains (i,j)) { newStringBuilder++="     " }
            else { newStringBuilder++="%.2f".format(model.agent.V(gridState(i,j)))
        newStringBuilder++=" " }
        }
        newStringBuilder.append('\n')
    }

    newStringBuilder.toString()
    }

   print(printValueF(myGridModel))


}
