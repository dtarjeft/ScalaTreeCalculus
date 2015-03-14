object MathTreeOps {
  type Environment = String => Double

  def evalAt(t: MathTree, env: Environment): Double = t match {
    case Exp(l,r) => scala.math.pow(evalAt(l,env),evalAt(r,env))
    case Multiply(l,r) => evalAt(l,env) * evalAt(r, env)
    case Log(q,b) => scala.math.log(evalAt(q,env))/scala.math.log(evalAt(b,env))

    case Div(l,r) => evalAt(l,env)/evalAt(r,env)
    case Sum(l, r) => evalAt(l, env) + evalAt(r, env) 
    case Cos(theta) => math.cos(evalAt(theta, env))
    case Sin(theta)=> math.sin(evalAt(theta, env))
    case Tan(theta) => math.tan(evalAt(theta, env))
    case Sec(theta) => 1/math.cos(evalAt(theta, env))
    case Csc(theta) => 1/math.sin(evalAt(theta, env))
    case Cot(theta) => 2/math.tan(evalAt(theta, env))
    case ATan(theta) => math.atan(evalAt(theta, env))
    case ACos(theta) => math.acos(evalAt(theta, env))
    case ASin(theta) => math.asin(evalAt(theta, env))

    case Var(n) => env(n)
    case Const(v) => v
  }

  def clean(t: MathTree, env: Environment): MathTree = t match {
    case Sum(Sum(Const(l), Const(r)), rest) => Sum(Const(evalAt(Sum(Const(l), Const(r)), env)), clean(rest, env))
    case Sum(Multiply(Const(l), Const(r)),rest) => Sum(Const(evalAt(Multiply(Const(l), Const(r)),env)), clean(rest,env))
    case Sum(Multiply(Const(l), Multiply(Const(l2), r)), rest) => Sum(Multiply(Const(evalAt(Multiply(Const(l), Const(l2)), env)), clean(r, env)), clean(rest,env))
    case Exp(r, Const(0)) => Const(1)
    case Exp(r, Const(1)) => r
    case l => l
  }

  /*TODO:: WARNING! derive() not be complete! If you get an unexpected Const(0.0), you need to add a rule for whatever zeroed out*/
  def derive(t: MathTree, v: String): MathTree = t match {
    // Implicit variable- Derive as normal, but multiply by dn/dv-- env will need to have a value for each one of these later.
      // TODO:: Flesh Implicit Differentiation out- Currently this only supports Product Rule.
    case Exp(Var(n), r) if (v != n) => Multiply(derive(Exp(Var(n),r),n),Var("d"+n+"/d"+v))
    // Power Rule
    case Exp(Var(n), Const(r)) if (v == n)=>  Multiply(Const(r), Exp(Var(n), Const(r-1)))
    // Variable to a non-constant exponent.
    case Exp(Var(n), r) if (r != Const) => Multiply(Exp(Var(n), r),derive(Multiply(Log(Var(n),Const(10)),r),v))
    // e to a power
    case Exp(Const(Math.E),r) => Multiply(derive(r,v),Exp(Const(Math.E),r))
      // General power rule (to catch Trig chain, for instance.)
    case Exp(l, Const(r)) => Multiply(Const(r), Exp(derive(l,v), Const(r-1)))

    // Quality-of-Life cleanup catches...
    case Sum(l, Const(r)) => derive(l,v)
    case Multiply(Const(l), Exp(Var(n), Const(1.0))) if (v == n) => Const(l)
    case Multiply(Const(l), r) => Multiply(Const(l), derive(r,v))
    case Multiply(l, Const(r)) => Multiply(derive(l,v), Const(r))

    // Product Rule
    case Multiply(l, r) => Sum(Multiply(derive(l,v),r),Multiply(l,derive(r,v)))
    // Quotient Rule
    case Div(top,bottom) => Div(Sum(Multiply(derive(top,v),bottom),Multiply(Const(-1),Multiply(top,derive(bottom,v)))),Exp(bottom,Const(2)))

    // Sum Rule
    case Sum(l, r) => Sum(derive(l, v), derive(r, v))
    // Singleton Variable rules
    case Var(n) if (v == n) => Const(1)
    case Var(n) if (v != n) => Var("d"+n+"/d"+v)

      // Trig!
    case Cos(i) => Multiply(Const(-1),Multiply(derive(i,v),Sin(i)))
    case Sin(i) => Multiply(derive(i,v),Cos(i))
    case Tan(i) => Multiply(derive(i,v),Exp(Sec(i),Const(2)))
    case Cot(i) => Multiply(Const(-1),Multiply(derive(i,v),Exp(Csc(i),Const(2))))
    case Sec(i) => Multiply(derive(i,v),Multiply(Sec(i),Tan(i)))
    case Csc(i) => Multiply(Const(-1),Multiply(derive(i,v),Multiply(Csc(i),Cot(i))))

    case _ => Const(0)
  }

  /* ScalaTest isn't working, preliminary research suggests it may not be [just] my formatting...
     so enjoy this cruddy console hard-coding! -- DT
  */
  def main(args: Array[String]){
    val env: Environment = { case "x" => 5 case "y" => 3}
    val result: MathTree = Sum(Multiply(Const(3),Exp(Var("x"), Const(3))), Sum(Multiply(Const(5),Exp(Var("x"), Const(2))), Sum(Multiply(Const(16), Exp(Var("x"),Const(1))), Const(32))))
    println("Statement: " + clean(result, env))
    println("   3*x^3 + 5*x^2 + 16*x^1 + 32")
    println("Derivative WRT x: " + clean(derive(result, "x"), env))
    println("Evaluated at x = 5: " + evalAt(result, env))
    println("Derivative evaluated at x = 5: " + evalAt(clean(derive(result, "x"), env), env))
    println()
    val result2: MathTree = Exp(Csc(Multiply(Const(3),Exp(Var("x"),Const(2)))), Const(3))
    println("Statement: " + result2)
    println("Derivative: " + derive(result2, "x"))
    println("Derivative evaluation (x = 5)should be some bonkers number: " + evalAt(derive(result2,"x"),env))

  }
}