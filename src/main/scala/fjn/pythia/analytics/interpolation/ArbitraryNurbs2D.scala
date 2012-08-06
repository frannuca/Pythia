//package fjn.pythia.analytics.interpolation
//
//import fjn.pythia.matrix.Matrix
//
///**
// * Created with fjn army of one
// * User: fran
// * Date: 7/9/12
// * Time: 6:52 PM
// */
//
///**
// * Accepts a list of sclices oriented in v-direction with inhomogeneous u distribution
// * @param samples : list of slices in the following order(x,y): samples(0) contains all x values at y=y0.
// *                thus samples(j)(i) is the yi, xj points of the non-uniform grid.
// *                each Matrix point in this seq of seq must be a 3D point with the following coordiante order:
// *                p(0,0)=x, p(1,0)=y and p(2,0)=z
// * */
//class ArbitraryNurbs2D(samples:Seq[Seq[Matrix[Double]]],val basisOrderv:Seq[Int],implicit val tolerance:Double=1.0e-4)
//  extends Nurbs2DBase with parameterVectorCentripetal
//{
//
//
//  lazy val basisOrder = basisOrderv
//
//
//  //list of u coordinate points
//  lazy val uPoints =
//  (for (j<- 0 until samples.length;
//       i<- 0 until samples(j).length)
//    yield
//  {
//    (samples(j)(i)(0,0))
//  }).toList.sortWith((v1,v2) => v1 < v2).toSet[Double]
//
//  //list of v coordiante points
//  lazy val vPoints =
//   (for (j<- 0 until samples.length)
//     yield
//   {
//     val a = samples(j)(0)(1,0)
//     a
//   }).toList.sortWith((v1,v2) => v1 < v2).toSet[Double]
//
//  val xAxis = uPoints
//  val yAxis = vPoints
//
//  lazy val homogeneousSlices=
//      (
//        for (j<- 0 until samples.length)
//            yield
//          {
//            val zPoints =
//                (for (n<- 0 until samples(j).length)
//                  yield
//                {
//                  (samples(j)(n)(2,0))
//                }).toArray
//
//            val jVal = samples(j)(0)(1,0)
//            //val qk:Array[Matrix[Double]],val basisOrder:Array[Int],val dim:Seq[Int]
//            val nurb = new Nurbs1D(samples(j).toSeq,Seq(basisOrder(0)))
//            nurb.solve(zPoints)
//
//            (for(u <- uPoints)
//              yield
//            {
//              //TODO: add matrix x,y,z
//              val rM = new Matrix[Double](3,1)
//              val auxm = nurb(nurb.getNormalizedCoord(u))
//              rM.set(0,0,auxm(0,0))
//              rM.set(1,0,jVal)
//              rM.set(2,0,auxm(1,0))
//
//              rM
//
//            }).toArray
//
//          }).toArray.flatMap(x=>x)
//
//
//  lazy val qk:Seq[Matrix[Double]]=homogeneousSlices.map(item => {
//    val m = new Matrix[Double](2,2)
//    m.set(0,0,item(0,0))
//    m.set(1,0,item(1,0))
//    m
//  })
//
//  lazy val z = homogeneousSlices.map(item => {
//     val a = item(2,0)
//     a
//   })
//  lazy val dim:Seq[Int]=Seq(uPoints.size,vPoints.size)
//
//
//
//  solve(z);
//
//}
