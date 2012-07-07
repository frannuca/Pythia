package fjn.pythia.analytics.interpolation.javaInterop

import java.util.ArrayList
import fjn.pythia.matrix.Matrix
import scala.collection.JavaConversions.{_}
import scala.collection.JavaConverters.{_}
import fjn.pythia.analytics.interpolation.{Nurbs1D, Nurbs2D}

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 5/22/12
 * Time: 8:17 PM
 * To change this template use File | Settings | File Templates.
 */


trait NurbsInterpolator{
}


class NurbsInterpolator2D(x:Array[java.lang.Double],
              y:Array[java.lang.Double],
              z:Array[Array[java.lang.Double]],
              orderX:java.lang.Integer,orderY:java.lang.Integer) extends NurbsInterpolator{



      println("Transfering input point into linear array of vectors");
      val qk =
      for(
          iy <- y;
          ix <- x)
        yield
      {
        val m = new Matrix[Double](2,1)
        m.set(0,0,ix)
        m.set(1,0,iy)
        m
      }

      System.out.println("Initializing NURBS2D");
      val bspline = new Nurbs2D(qk.toArray,Array(orderX.toInt,orderY.toInt),Seq(x.length,y.length))
  
      val Z =
      (for ( j<- 0 until y.length;
             i <- 0 until x.length
          )
        yield
      {
         z(j)(i).doubleValue()
      }).toArray

      println("Solving Lineal systems for 2D Nurbs ...")
      bspline.solve(Z);
      println("Finished Solving Lineal systems for 2D Nurbs ...")


   def compute(X:Array[java.lang.Double],Y:Array[java.lang.Double]):Array[java.lang.Double]={

      val xn = X.toList.par.map (s =>  bspline.getNormalizedCoord(s,0)).toArray
      val yn = Y.toList.par.map(s => bspline.getNormalizedCoord(s,1)).toArray
      val d = bspline(xn zip yn)
      d.map(m => m(2,0).asInstanceOf[java.lang.Double]).toArray
    }

  def compute(X:java.lang.Double,Y:java.lang.Double):java.lang.Double={

    val xn = bspline.getNormalizedCoord(X,0)
    val yn = bspline.getNormalizedCoord(Y,1)
    val d = bspline(xn,yn)(2,0).asInstanceOf[java.lang.Double]
    d
  }

  
}


class NurbsInterpolator1D(x:java.util.List[java.lang.Double],
              z:java.util.List[java.lang.Double],
              order:Int) extends NurbsInterpolator{


      val qk =
      for(
          ix <- x)
        yield
      {
        val m = new Matrix[Double](1,1)
        m.set(0,0,ix)
        m
      }

      val bspline = new Nurbs1D(qk.toArray,Array(order.toInt),Seq(x.length))

      val Z =
      (for (i <- 0 until x.length)
        yield
      {
         z(i).doubleValue()
      }).toArray

      bspline.solve(Z);



  def compute(X:java.lang.Double):java.lang.Double={

    val xn = bspline.getNormalizedCoord(X)
    val d = bspline(xn)(1,0).asInstanceOf[java.lang.Double]
    d
  }



}