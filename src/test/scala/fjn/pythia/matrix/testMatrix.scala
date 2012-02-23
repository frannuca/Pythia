package fjn.pythia.matrix

import org.specs2.mutable.Specification


/**
 * User: fran
 * Date: 2/19/12
 * Time: 11:39 PM
 */

class testMatrix extends Specification {
  "matrix test" should {
              "run normally" in {
                `matrixtests` mustEqual true
      
              }
          }
  
    
    
    def `matrixtests` = {

      import Complex._

      implicit def DD(v:Double):Complex=Complex(v,0d)
      implicit def II(v:Int):Complex=Complex(v,0d)

      val a1 = Complex(1,1)
      val a2 = Complex(2,3)
      val a3 = a1*a2

      val m = new Matrix[Complex](2,2)
      m.set(0,0,1.0)
      m.set(0,1,2)
      m.set(1,0,3)
      m.set(1,1,5)

      val m2 = new Matrix[Complex](2,2)
      m2.set(0,0,1)
      m2.set(0,1,2)
      m2.set(1,0,3)
      m2.set(1,1,4)

      val m3 = m * m2;

      println(m(0,0))

      true
    }

}