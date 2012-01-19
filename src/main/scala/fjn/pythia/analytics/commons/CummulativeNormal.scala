package fjn.pythia.analytics.commons


/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 10/2/11
 * Time: 5:23 PM
 * To change this template use File | Settings | File Templates.
 */

case class Normal(nTerms:Int) {

  val erfCalc = Erf(nTerms)

  val sqrt2 = math.sqrt(2);

  /**
   * cummulative normal distribution computed as a function of the error function 
   */
  def PHI(x:Double):Double={
    0.5 * (erfCalc.erf(x/sqrt2) + 1.0)
  }


  def iPHI(u:Double):Double={
    sqrt2 * erfCalc.invErf(2.0 * u - 1.0)
  }

}
