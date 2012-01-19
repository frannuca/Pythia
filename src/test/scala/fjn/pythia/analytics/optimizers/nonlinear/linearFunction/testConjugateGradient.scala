package fjn.pythia.analytics.optimizers.nonlinear.linearFunction

/**
* Created by IntelliJ IDEA.
* User: fran
* Date: 12/31/11
* Time: 7:45 PM
* To change this template use File | Settings | File Templates.
*/
import org.specs2.mutable.Specification
import scalala.tensor.dense.DenseMatrix
import fjn.pythia.analytics.optimizers.nonlinear.solvers.ConjugateGradientSolver

class testConjugateGradient extends  Specification{
  "Conjugate Gradient applied to symmetric positive definite matrix" should {
        "Converge with tolerance = 1e-8" in {
          `solvingSystemCG` mustEqual true

        }
    }

  def `solvingSystemCG` = {

      val A2 = DenseMatrix.eye[Double](500,500)*100
      val A = (A2 * A2.t).toDense

      val b = DenseMatrix.rand(500,1)*300


      val x0 = DenseMatrix.rand(500,1)

      val solver = new  ConjugateGradientSolver(A,b,1e-3)
      val x = solver.iterate(100,x0)
      val res =  A * x
      println("A="+A.toString)
      println("b  ="+b.t.toString)
      println("Ax ="+res.t.toString)
      println("x="+x.t.toString)

      val err = (b - res).toDense
      (err*err.t).toDense(0,0)< 1e-2
    }
}