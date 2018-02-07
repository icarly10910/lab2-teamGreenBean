package jsy.student

import jsy.lab2.Lab2Like

object Lab2 extends jsy.util.JsyApplication with Lab2Like {
  import jsy.lab2.Parser
  import jsy.lab2.ast._

  /*
   * CSCI 3155: Lab 2
   * <Carly Romig>
   * 
   * Partner: <Arthi Kumar>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the '???' expression with  your code in each function.
   * 
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your lab will not be graded if it does not compile.
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something  that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   *
   */

  /* We represent a variable environment as a map from a string of the
   * variable name to the value to which it is bound.
   * 
   * You may use the following provided helper functions to manipulate
   * environments, which are just thin wrappers around the Map type
   * in the Scala standard library.  You can use the Scala standard
   * library directly, but these are the only interfaces that you
   * need.
   */


  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   *
   * You can catch an exception in Scala using:
   * try ... catch { case ... => ... }
   */

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case B(b) => if (b) 1 else 0
      case S(s) => try {s.toDouble} catch {case _: NumberFormatException => Double.NaN}
      case Undefined => Double.NaN
    }
  }

  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case N(n) => {
        if (n == 0) false
        else if (n.isNaN) false
        else true                 //if n>0, return true
      }
      case S(s) => true           //?? maybe

      case Undefined => false
    }
  }

  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
      case Undefined => "undefined"
      case B(b) => ??? //if (b <= 0) "false" else "true"
      case N(n) => ???
    }
  }

  def eval(env: Env, e: Expr): Expr = {
    e match {
      case Var(x: String) => ???
      case ConstDecl(x: String, e1: Expr, e2: Expr) => ???

      /* Base Cases */

      case Unary(uop: Uop, e1: Expr) => {
        uop match {
          case Neg => N(0 - toNumber(e1))
          case Not => B(!toBoolean(e1))
        }
      }

      case Binary(bop: Bop, e1: Expr, e2: Expr) => {
        bop match {
          case Plus => ???
          case Minus => N(toNumber(e1)-toNumber(e2))
          case Times => N(toNumber(e1)*toNumber(e2))
          case Div => N(toNumber(e1)/toNumber(e2))
          case Eq => B(toNumber(e1) == toNumber(e2))
          case Ne => B(toNumber(e1) != toNumber(e2))
          case Lt => B(toNumber(e1) < toNumber(e2))
          case Le => B(toNumber(e1) <= toNumber(e2))
          case Gt => B(toNumber(e1) > toNumber(e2))
          case Ge => B(toNumber(e1) >= toNumber(e2))

          case And => ???
          case Or => ???

          case Seq => ???
        }
      }

      /* Inductive Cases */
      case Print(e1) => println(pretty(eval(env, e1))); Undefined
      case If(e1: Expr, e2: Expr, e3: Expr) => ???

      case _ => ???
    }
  }



  /* Interface to run your interpreter from the command-line.  You can ignore what's below. */
  def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }

    val expr = Parser.parseFile(file)

    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }

    if (debug) { println("Evaluating ...") }

    val v = eval(expr)

     println(pretty(v))
  }

}
