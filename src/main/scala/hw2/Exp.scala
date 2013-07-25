package hw2

import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: chjkw
 * Date: 13. 7. 24.
 * Time: 오후 4:48
 */

abstract class Exp
case object X extends Exp
case class INT(n: Int) extends Exp
case class REAL(r: Float) extends Exp
case class ADD(e1: Exp, e2: Exp) extends Exp
case class SUB(e1: Exp, e2: Exp) extends Exp
case class MUL(e1: Exp, e2: Exp) extends Exp
case class DIV(e1: Exp, e2: Exp)  extends Exp
case class SIGMA(lb: Exp, ub: Exp, expr: Exp) extends Exp
case class INTEGRAL(dom1: Exp, dom2: Exp, expr: Exp) extends Exp

object Exp {
  def mathemadiga(expr: Exp, x: Exp = REAL(0)) : Float = {
    @tailrec
    def sigma(lb: Exp, ub: Exp, expr: Exp, result: Float = 0): Float = {
      if(mathemadiga(SUB(lb, ub)) > 0) result
      else sigma(ADD(lb, INT(1)), ub, expr, result + mathemadiga(expr, lb))
    }

    @tailrec
    def integral(lb: Exp, ub: Exp, expr: Exp, result: Float = 0): Float = {
      val dx = 0.1f
      if(mathemadiga(SUB(lb, ub)) > 0) result
      else integral(ADD(lb, REAL(dx)), ub, expr, result + dx * mathemadiga(expr, lb))
    }

    expr match {
      case X => mathemadiga(x)
      case INT(n) => n
      case REAL(r) => r
      case ADD(e1, e2) => mathemadiga(e1) + mathemadiga(e2)
      case SUB(e1, e2) => mathemadiga(e1) - mathemadiga(e2)
      case MUL(e1, e2) => mathemadiga(e1) * mathemadiga(e2)
      case DIV(e1, e2) => mathemadiga(e1) / mathemadiga(e2)
      case SIGMA(lb, ub, expr) => sigma(lb, ub, expr)
      case INTEGRAL(dom1, dom2, expr) => integral (dom1, dom2, expr)
    }
  }
}