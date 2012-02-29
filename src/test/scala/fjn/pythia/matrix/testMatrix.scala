package fjn.pythia.matrix

import org.specs2.mutable.Specification
import scalala.tensor.dense.DenseMatrix


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

      print(m3.toString)
      println("..........")

      m3.zeros
      print(m3.toString)

      println("..........")
      m3.eye
      print(m3.toString)


      println("..........")
      m3.set(0,0,1.1)
      m3.set(0,1,1.2)
      m3.set(1,0,2.1)
      m3.set(1,1,2.2)

      require(m3(0,0)==1.1)
      require(m3(0,1)==1.2)
      require(m3(1,0)==2.1)
      require(m3(1,1)==2.2)

      print(m3.toString)


      println("creating ms1 eye")
            val ms1 = DenseMatrix.eye[Double](1000)
            println("creating ms2 eye")
            val ms2 = DenseMatrix.eye[Double](1000)

            println("creating ms1*ms2 eye")
            val start2:Long = System.currentTimeMillis();
            val ms3 = ms1 * ms2;
            val stop2:Long = System.currentTimeMillis();


      println("creating m4")

      val m4 = new Matrix[Double](1000,1000,numberOfCores = 1)

      println("creating m5")
      val m5 = new Matrix[Double](1000,1000)

      println("creating m4 eye")
      m4.eye
      m4.set(0,0,3.0)
      m4.set(0,1,5.0)
      println("creating m5 eye")
      m5.eye

      println("creating m4*m5 eye")

      val start:Long = System.currentTimeMillis();
      val m6 = m4*m5
      val stop:Long = System.currentTimeMillis();





      println("Matrix time= "+(stop-start).toString)
      println("Sacalala   = "+(stop2-start2).toString)
      


      true
    }

}