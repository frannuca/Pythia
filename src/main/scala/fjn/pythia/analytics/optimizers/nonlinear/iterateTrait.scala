package fjn.pythia.analytics.optimizers.nonlinear

import scalala.tensor.dense.DenseMatrix

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 1/4/12
 * Time: 1:12 AM
 * To change this template use File | Settings | File Templates.
 */

trait iterateTrait {
  def iterate(numberIterations:Int,x0:DenseMatrix[Double]):DenseMatrix[Double]
}