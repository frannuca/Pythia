package fjn.pythia.analytics.interpolation

import fjn.pythia.matrix.Matrix

/**
 * Created with fjn army of one
 * User: fran
 * Date: 7/9/12
 * Time: 6:52 PM
 */

/**
 * Accepts a list of sclices oriented in v-direction with inhomogeneous u distribution
 * @param samples : list of slices
 */
class ArbitraryNurbs2D(samples:Seq[Seq[Matrix[Double]]],basisOrder:Array[Int]) {

  val uPoints =
  (for (j<- 0 until samples.length;
       i<- 0 until samples(j).length)
    yield
  {
    (samples(j)(i)(0,0))
  }).toSet[Double]

  val vPoints =
    (for (j<- 0 until samples.length;
         i<- 0 until samples(j).length)
      yield
    {
      (samples(j)(i)(1,0))
    }).toSet[Double]



  lazy val homogeneousSlices=
      (for (j<- 0 until samples.length)
            yield
          {
            val zPoints =
                (for (n<- 0 until samples(j).length)
                  yield
                {
                  (samples(j)(n)(2,0))
                }).toArray

            //val qk:Array[Matrix[Double]],val basisOrder:Array[Int],val dim:Seq[Int]
            val nurb = new Nurbs1D(samples(j).toArray,Seq(basisOrder(0)).toArray,Seq(samples(j).length))
            nurb.solve(zPoints)

            (for(u <- uPoints)
              yield
            {
              nurb(nurb.getNormalizedCoord(u))
            }).toArray

          }).toArray.flatMap(x=>x)


  lazy val nurb2D = new Nurbs2D(homogeneousSlices,basisOrder,Seq(uPoints.size,vPoints.size))



}
