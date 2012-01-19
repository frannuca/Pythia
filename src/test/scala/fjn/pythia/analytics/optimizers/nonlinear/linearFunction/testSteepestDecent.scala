package fjn.pythia.analytics.optimizers.nonlinear.linearFunction

/**
* Created by IntelliJ IDEA.
* User: fran
* Date: 12/27/11
* Time: 11:34 PM
* To change this template use File | Settings | File Templates.
*/

import org.specs2.mutable.Specification
import scalala.tensor.dense.DenseMatrix
import fjn.pythia.analytics.optimizers.nonlinear.solvers.SteepestDescentSolver

class testSteepestDecent extends  Specification {




  "solving a linear System with steepest descent solver" should {
        "Converge with tolerance = 1e-8" in {
          `solvingSystem` mustEqual true

        }
    }

  def `solvingSystem` = {

    val A2 = DenseMatrix.rand(5,5)
    val A = (A2 * A2.t).toDense

    val b = DenseMatrix.zeros[Double](5,1)
    b(0,0)=  3.4;
    b(1,0) = 2.2;
    b(2,0) = 2.2;
    b(3,0) = 2.2;
    b(4,0) = 2.2;


    val x0 = DenseMatrix.zeros[Double](5,1)
        x0(0,0)=  0.0
        x0(1,0) = 0.0
        x0(2,0) = 0.0
        x0(3,0) = 0.0
        x0(4,0) = 0.0

    val solver = new  SteepestDescentSolver(A,b,tolerance = 0.0)
    val x = solver.iterate(50000,x0)
    val res =  A * x
    println("A="+A.toString)
    println("b="+b.t.toString)
    println("r="+res.t.toString)
    println("x="+x.t.toString)

    val err = (b - res).toDense
    (err*err.t).toDense(0,0)< 1e-8
  }

}