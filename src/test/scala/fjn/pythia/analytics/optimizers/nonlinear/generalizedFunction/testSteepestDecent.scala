package fjn.pythia.analytics.optimizers.nonlinear.generalizedFunction

/**
* Created by IntelliJ IDEA.
* User: fran
* Date: 12/27/11
* Time: 11:34 PM
* To change this template use File | Settings | File Templates.
*/

import org.specs2.mutable.Specification
import scalala.tensor.dense.DenseMatrix

class testSteepestDecent extends  Specification {



  "solving a linear System with steepest descent optimizer" should {
          "convergence with tolerance = 1e-3 " in {
            `solvingSystemWithoptimization` mustEqual true

          }
      }

  def `solvingSystemWithoptimization`={
    val A = DenseMatrix.rand(5,5)

        val b = DenseMatrix.zeros[Double](5,1)
        b(0,0)=  2.2;
        b(1,0) = 2.2;
        b(2,0) = 2.2;
        b(3,0) = 2.2;
        b(4,0) = 2.2;


        val x0 = DenseMatrix.rand(5,1)

       def func(v:DenseMatrix[Double]):Double={
         val r = A*v-b
         (r.t * r).toDense(0,0)
       }
        val solver = new  SteepestDescent(func,lr=1e-2)
        val x = solver.iterate(10000,x0)
        val res =  A * x
        println("A ="+A.toString)
        println("b ="+b.t.toString)
        println("Ax="+res.t.toString)
        println("x="+x.t.toString)

        val err = (b - res).toDense
        (err*err.t).toDense(0,0)< 1e-3
  }

}