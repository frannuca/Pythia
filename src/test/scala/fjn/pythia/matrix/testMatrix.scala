package fjn.pythia.matrix

import org.specs2.mutable.Specification


/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 2/19/12
 * Time: 11:39 PM
 * To change this template use File | Settings | File Templates.
 */

class testMatrix extends Specification {
  "matrix test" should {
              "run normally" in {
                `matrixtests` mustEqual true
      
              }
          }
  
    
    
    def `matrixtests` = {

      val m = new Matrix[Double](2,2)
      m.set(0,0,1.0)
      m.set(0,1,2)
      m.set(1,0,3)
      m.set(1,1,5)

      val m2 = new Matrix[Double](2,2)
      m2.set(0,0,1)
      m2.set(0,1,2)
      m2.set(1,0,3)
      m2.set(1,1,4)
      
      val m3 = m * m2;

      println(m(0,0))

      true
    }

}