package fjn.pythia.analytics.optimizers.nonlinear

import fjn.pythia.analytics.commons.NNMatrixExtensions

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 12/31/11
 * Time: 4:34 PM
 * To change this template use File | Settings | File Templates.
 */

trait CGTrait extends NNMatrixExtensions with iterateTrait {

  val jmax:Int
  val tolerance:Double

  
}