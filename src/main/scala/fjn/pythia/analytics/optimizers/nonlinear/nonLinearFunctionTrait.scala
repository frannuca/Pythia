package fjn.pythia.analytics.optimizers.nonlinear

import scalala.tensor.dense.DenseMatrix

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 1/3/12
 * Time: 1:15 PM
 * To change this template use File | Settings | File Templates.
 */

trait nonLinearFunctionTrait {

  val pFunc:DenseMatrix[Double] => Double
}