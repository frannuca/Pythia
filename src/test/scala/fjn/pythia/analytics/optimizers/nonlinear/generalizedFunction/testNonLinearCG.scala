package fjn.pythia.analytics.optimizers.nonlinear.generalizedFunction

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 1/2/12
 * Time: 12:34 AM
 * To change this template use File | Settings | File Templates.
 */

import org.specs2.mutable.Specification
import scalala.tensor.dense.DenseMatrix

class testNonLinearCG extends Specification{
  
  "Solving system with Secant and Polak Ribière" should {
          "converge with tolerance = 1e-3 " in {
            `CGSecantPolakRibiere` mustEqual true
  
          }
      }

  "Solving system with NewtonRaphson and Fletcher Reeves" should {
            "converge with tolerance = 1e-3 " in {
              `CGNewtonRaphsonFletcherReeves` mustEqual true

            }
        }

  val b = DenseMatrix.zeros[Double](9,1)
                    b(0,0)=  25;
                    b(1,0) = 26;
                    b(2,0) = 27;
                    b(3,0)=  28;
                    b(4,0) = 35;
                    b(5,0) = 36;
                    b(6,0) = 45;
                    b(7,0) = 55;
                    b(8,0) = 65;

    private def f(x:DenseMatrix[Double]):Double={
     val r = ((b-x).t * (b-x)).toDense(0,0)

      r
      
    }

    def `CGSecantPolakRibiere` ={
      val x0 = DenseMatrix.rand(9,1)

      val cg = new CGSecantPolakRibiere(f,tolerance=1e-4,sigma0=1e-5)
      val x = cg.iterate(1000,x0)
     


      f(x) < 1e-3
      
    }

  def `CGNewtonRaphsonFletcherReeves` ={
      val x0 = DenseMatrix.rand(9,1)

      val cg = new CGNewtonRaphsonFletcherReeves(f,tolerance=1e-5)
      val x = cg.iterate(1000,x0)



      f(x) < 1e-3

    }
}