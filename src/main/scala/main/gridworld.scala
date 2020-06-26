package gridworld

import scala.annotation.tailrec

trait gridAction extends Action

object U extends gridAction { override def toString():String ="U"}
object D extends gridAction { override def toString():String ="D"}
object R extends gridAction { override def toString():String ="R"}
object L extends gridAction { override def toString():String ="L"}

case class gridState(row: Int, col: Int) extends State

trait StateType
object Win extends StateType
object Loose extends StateType
object Good extends StateType
object Bad extends StateType


class gridWorld(val stateSpace: Set[gridState], val finalStateTypes: Map[gridState, StateType]) extends Environment[gridAction,gridState]{

    type Reward = Double

    val finalStates = finalStateTypes.keySet

    val actions:Set[gridAction] = Set(U,D,R,L)

    val actionSpace:Map[gridState, Set[gridAction]] = (
        for{s <- stateSpace
             a<-actions
              if (stateSpace contains nextState(a,s)) 
            } yield (s,a)).groupMap(_._1)(_._2)

    def stateType(s: gridState): StateType = {
        if (finalStates contains s) { finalStateTypes(s) }
        else if (stateSpace contains s) { Good }
        else {Bad}
    }

    def rewards(s2: gridState, a: gridAction, s1: gridState): Reward = stateType(s2) match {
        case Win => 1.0
        case Loose => -1.0
        case Good => 0.0
        case Bad => 0.0
    }

    val transitionsMap:Map[(StateType,Reward),Double] = Map((Bad,0.0)->0.0,(Good,0.0)->1.0, (Win,1.0)->1.0, (Loose,-1.0)->1.0).withDefaultValue(0.0)

    def transitions(s2: gridState, r: Reward,  a: gridAction, s1: gridState): Double = {
        if(s2==nextState(a,s1)) {
            transitionsMap(stateType(s2),r)
        }
        else {0}
    }

    def nextState(a: gridAction, s: gridState): gridState = a match {
        case U => gridState(s.row-1,s.col)
        case D => gridState(s.row+1,s.col)
        case L => gridState(s.row,s.col-1)
        case R => gridState(s.row,s.col+1)
    }

}

class gridAgent(Value: (gridState) => Double, Policy: (gridAction, gridState) => Double) extends Agent[gridAction, gridState] {
    def V(s: gridState): Double = Value(s)
    def P(a: gridAction, s: gridState): Double = Policy(a,s)
    def updateV(newV: (gridState) => Double): gridAgent = new gridAgent(newV, P)
    def updateP(newP: (gridAction, gridState) => Double) = new gridAgent(V, newP)
}

class gridModel(val agent: gridAgent,val env: gridWorld) extends RLModel[gridAction, gridState] {

    def evaluate(eps: Double, gamma: Double): gridModel = {
        val newAgent = evaluateValue(updateValue(agent,gamma),agent.V,eps, gamma)
        new gridModel(newAgent, env)
    }

    @tailrec
    private def evaluateValue(a: gridAgent, oldV: (gridState) => Double, eps: Double, gamma: Double): gridAgent = {
        val delta = env.actionSpace.keySet.map(s => scala.math.abs(a.V(s) - oldV(s))).max
        if (delta < eps) {a}
        else {evaluateValue(updateValue(a, gamma), a.V, eps , gamma ) }
    }

    def updateValue(a: gridAgent, gamma: Double): gridAgent = {
        val newV = env.actionSpace.keySet.map(s => (s,V(s,gamma, a))).toMap
        agent.updateV(newV)
    }

    def Q(a: gridAction, s1: gridState, gamma: Double , ag: gridAgent): Double = {
         env.stateSpace.map(s2 => env.transitions(s2,env.rewards(s2,a,s1),a,s1)*(env.rewards(s2,a,s1) + gamma*ag.V(s2))).foldLeft(0.0)(_+_)
    }

    def V(s: gridState, gamma: Double, ag: gridAgent): Double = {
        env.actionSpace(s).map(a => ag.P(a,s)*Q(a,s,gamma,ag)).foldLeft(0.0)(_+_)
    }

    def control(gamma: Double): gridModel = {
        def argMax(s: gridState): gridAction = {
            val inner = (for(a <- env.actionSpace(s).toList) yield (a,Q(a,s,gamma,agent))).maxBy(_._2)
            inner._1
        }

        val policyMap = (for(s <- env.stateSpace) yield (s,argMax(s))).toMap.withDefaultValue(0.0)

        def newP(a: gridAction, s: gridState) = {
            if (policyMap(s)==a) {1}
            else {0}
         }

        val newAgent = agent.updateP(newP)
        new gridModel(newAgent, env)
    }

}



 