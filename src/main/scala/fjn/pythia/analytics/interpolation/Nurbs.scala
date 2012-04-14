package fjn.pythia.analytics.interpolation

import fjn.pythia.matrix.Matrix

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 3/23/12
 * Time: 6:38 PM
 * To change this template use File | Settings | File Templates.
 */

class Nurbs(val qk:Array[Matrix[Double]],val basisOrder:Array[Int]) extends controlPoints
    with parameterVector
    with BasisFunctionOrder
    with KnotsVector
    with Basis
    with solver
    {



}
