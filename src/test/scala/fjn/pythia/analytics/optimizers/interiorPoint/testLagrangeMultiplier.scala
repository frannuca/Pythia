package fjn.pythia.analytics.optimizers.interiorPoint

import org.specs2.mutable.Specification
import scalala.tensor.dense.DenseMatrix


/**
 * User: fran
 * Date: 1/4/12
 * Time: 9:05 PM
 */



class testLagrangeMultiplier extends Specification{

  "solving lagrange multiplier poblem of intersection x + y and circle" should {
            "converge with tolerance = 1e-5 " in {
              `CGSecantPolakRibiere` mustEqual true
    
            }
        }

  
  
  def `CGSecantPolakRibiere` = {
    
    def f(x:DenseMatrix[Double]):Double = {
      //x(0,0)* x(0,0)+x(1,0)*x(1,0)
      val a = (x(1,0)-3.0) * (x(1,0)-3.0)
      a
    }

    def g(x:DenseMatrix[Double]):Double = {
      x(0,0)-122.33
     }




    val x0 = DenseMatrix.zeros[Double](2,1)
    x0(0,0)=  110;
    x0(1,0) = -2.5;

    val cg = new lagrangeMultiplier(f,x0,DenseMatrix.ones[Double](1,1)*10,DenseMatrix.rand(1,1),List(g))

    val r = cg.solve(100)

    
    println("resulting vector= "+r.toString)
    math.abs(r(0,0)-1.0)<0.01 &&
    math.abs(r(1,0))<0.01

  }

}