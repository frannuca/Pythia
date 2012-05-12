package fjn.pythia.analytics.interpolation

import fjn.pythia.matrix.Matrix

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 5/9/12
 * Time: 8:09 AM
 * To change this template use File | Settings | File Templates.
 */

trait TSolver
{

    val samples:Int
    var pk: Matrix[Double]
    val weights:Array[Double]

  def solve(z:Array[Double]):Boolean

}
