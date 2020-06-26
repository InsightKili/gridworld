package gridworld

trait Action

trait State

trait Environment[subAction <: Action, subState <: State] {
    type Reward
    val stateSpace: Set[subState]
    val actionSpace: Map[subState,Set[subAction]]
    def transitions(s2: subState, r: Reward, a: subAction, s1: subState): Double
    def rewards(s2: subState, a: subAction, s1: subState): Reward
}


trait Agent[subAction <: Action, subState <: State] {
    def V(s: subState):Double
    def P(a: subAction, s: subState): Double
}

trait RLModel[subAction <: Action, subState <: State] {
    val agent: Agent[subAction, subState]
    val env: Environment[subAction, subState]
    def evaluate(epsilon: Double, gamma: Double):RLModel[subAction,subState]
    def control(gamma: Double): RLModel[subAction, subState]
}
