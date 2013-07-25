package hw2

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.Checkers

/**
 * Created with IntelliJ IDEA.
 * User: chjkw
 * Date: 13. 7. 24.
 * Time: 오후 5:20
 */
class ExpTest extends FunSuite with ShouldMatchers with Checkers {
  val lb = INT(1)
  val ub = INT(10)

  test("add test") {
    Exp.mathemadiga(ADD(lb, ub)) should be (1+10)
    Exp.mathemadiga(ADD(REAL(1.5f), REAL(3.5f))) should be (1.5+3.5)
    Exp.mathemadiga(ADD(INT(4), REAL(3.5f))) should be (4+3.5)
  }

  test("sub test") {
    Exp.mathemadiga(SUB(ub, lb)) should be (10-1)
    Exp.mathemadiga(SUB(REAL(1.5f), REAL(3.5f))) should be (1.5-3.5)
    Exp.mathemadiga(SUB(INT(4), REAL(3.5f))) should be (4-3.5)
  }

  test("mul test") {
    Exp.mathemadiga(MUL(INT(3), INT(5))) should be (3*5)
    Exp.mathemadiga(MUL(REAL(1.5f), REAL(3.5f))) should be (1.5*3.5)
    Exp.mathemadiga(MUL(INT(4), REAL(3.5f))) should be (4*3.5)
  }

  test("div test") {
    Exp.mathemadiga(DIV(INT(20), INT(5))) should be (20/5)
    Exp.mathemadiga(DIV(REAL(4.4f), REAL(1.1f))) should be (4.4/1.1)
    Exp.mathemadiga(DIV(INT(4), REAL(3.5f))) should be (4f/3.5f)
    Exp.mathemadiga(X, INT(1)) should be (1)
  }

  test("sigma test") {
    val expr = SIGMA(INT(1), INT(10), X)
    Exp.mathemadiga(expr) should be (55)
  }

  test("integral test") {
    val expr = INTEGRAL(INT(0), INT(10), X)
    Exp.mathemadiga(expr) should be (49.49999f)
  }
}
