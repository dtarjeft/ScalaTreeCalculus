abstract class MathTree
case class Exp(l: MathTree, r: MathTree) extends MathTree
case class Log(quantity: MathTree, base: MathTree) extends MathTree
case class Multiply(l: MathTree, r: MathTree) extends MathTree
case class Div(l: MathTree, r: MathTree) extends MathTree
case class Sum(l: MathTree, r: MathTree) extends MathTree
case class Cos(theta: MathTree) extends MathTree
case class Sin(theta: MathTree) extends MathTree
case class Tan(theta: MathTree) extends MathTree
case class Sec(theta: MathTree) extends MathTree
case class Csc(theta: MathTree) extends MathTree
case class Cot(theta: MathTree) extends MathTree
case class ATan(theta: MathTree) extends MathTree
case class ACos(theta: MathTree) extends MathTree
case class ASin(theta: MathTree) extends MathTree
case class Var(n: String) extends MathTree
case class Const(v: Double) extends MathTree