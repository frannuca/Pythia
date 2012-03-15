package fjn.pythia.matrix

import org.specs2.mutable.Specification
import scalala.tensor.dense.DenseMatrix
import util.Random


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

//      import Complex._
//
//
//      val a1 = Complex(1,1)
//      val a2 = Complex(2,3)
//      val a3 = a1*a2
//
//      val m = new Matrix[Complex](2,2)
//      m.set(0,0,1.0)
//      m.set(0,1,2)
//      m.set(1,0,3)
//      m.set(1,1,5)
//
//      val m2 = new Matrix[Complex](2,2)
//      m2.set(0,0,1)
//      m2.set(0,1,2)
//      m2.set(1,0,3)
//      m2.set(1,1,4)
//
//      val m3 = m * m2;
//
//      print(m3.toString)
//      println("..........")
//
//      m3.zeros
//      print(m3.toString)
//
//      println("..........")
//      m3.eye
//      print(m3.toString)
//
//
//      println("..........")
//      m3.set(0,0,1.1)
//      m3.set(0,1,1.2)
//      m3.set(1,0,2.1)
//      m3.set(1,1,2.2)
//
//      require(m3(0,0)==1.1)
//      require(m3(0,1)==1.2)
//      require(m3(1,0)==2.1)
//      require(m3(1,1)==2.2)
//
//      print(m3.toString)
//
//
//

      val mM1 = 200
      val mM2 =300
      val mM3 = 200
      val ms1 = DenseMatrix.zeros[Double](mM1,mM2)
      println("creating ms2 eye")
      val ms2 = DenseMatrix.zeros[Double](mM2,mM3)

      val m1 = new Matrix[Double](mM1,mM2)


      val m2 = new Matrix[Double](mM2,mM3)


      val rndgen  = new Random()

      for(i<- 0 until mM1)
        for(j<- 0 until mM2)
        {
          val r1 = rndgen.nextDouble()
          val r2 = rndgen.nextDouble()
          ms1.update(i,j,r1)
          m1.set(i,j,r1)

        }

        for(i<- 0 until mM2)
              for(j<- 0 until mM3)
              {
                val r1 = rndgen.nextDouble()
                val r2 = rndgen.nextDouble()
                ms2.update(i,j,r1)
                m2.set(i,j,r1)

              }






//      println(m4.toString)
//      println("row 0");
//      m4.getColArray(0).foreach(c => print(c.toString)+",")
//      println(" \n row 1 ")
//      m5.getColArray(0).foreach(c => print(c.toString)+",")
      var ii = 0
      while(ii < 1)
      {

        

              val start2:Long = System.currentTimeMillis();
              val ms3 = ms1 * ms2;
              val stop2:Long = System.currentTimeMillis();
  
        


              val start:Long = System.currentTimeMillis();
              val m6 = m1*m2
              val stop:Long = System.currentTimeMillis();




              println("iter "+ ii.toString)
              println("Matrix time= "+(stop-start).toString)
              println("Sacalala   = "+(stop2-start2).toString)
              println("ratio      =  "+((stop-start).toDouble/(stop2-start2).toDouble).toString)
        
        
//              println(m6.toString)
//              println()
//              println(ms3.toString)
              require(m6.numberCols == ms3.numCols)
              require(m6.numberRows == ms3.numRows)
              for (j <-0 until m6.numberCols)
                for(i <- 0 until m6.numberRows)
                    require(math.abs (m6(i,j)-ms3(i,j))<1e-5)

        
        

             println("-----------------")
              ii = ii + 1

      }


      true
    }

}