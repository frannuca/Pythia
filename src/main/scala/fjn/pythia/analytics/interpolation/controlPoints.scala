package fjn.pythia.analytics.interpolation

import fjn.pythia.matrix.Matrix

/**
 * Created by fjn
 * User: fran
 * Date: 5/13/12
 * Time: 3:24 PM
 * To change this template use File | Settings | File Templates.
 */

/**The user must provide an array of samples points which the algorithm
 uses as control points.
 The presentation of the control points is performed as a lineal array of points plus
 a list of dimensions. Typically if our points are part of a rectangular grid
 the we need to provide an array such as [q00,q01,q02,...,q0(n-1),q10,...q1(n-1),...,q(m-1)0,..,q(m-1)(n-1)]
 */
trait controlPoints {
  ///Matrix of points to be interpolated, only in matrix form we
  ///can perform the 2-dimensional interpolation.
  ///the algorithm expects a list of number of samples per dimension
  ///The array of sample in dimension dk is extracted as follows:
  // item(i,j)=qk(j*dim_i+i)
  def qk: Seq[Matrix[Double]] //list of control points as a multi-dimensional grid arrangement

  def dim:Seq[Int] /// list of dimension composing our grid
  lazy val viewer = new MultiArrayView[Matrix[Double]](qk,dim)   //accessor to the point in the given grid

  def apply(w:Seq[Int]):Matrix[Double]=
  {
     viewer(w)
  }
}

