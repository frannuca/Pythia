package fjn.pythia.analytics.optimizers.genetics.continuous

import fjn.pythia.analytics.optimizers.genetics.discrete.commons.tAlgorithm
import org.apache.commons.lang.NotImplementedException


/**
 * User: fran
 * Date: 1/18/12
 * Time: 7:14 PM
 */

class SwarmGA(val pFitness:(Array[Double]) => Double)  extends tAlgorithm{

  /**
     * Evolution of the tAlgorithm is performed through calls to next
     */
     def next():Boolean={
      throw new NotImplementedException()
     }







}