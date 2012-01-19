package fjn.pythia.analytics.commons

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 10/2/11
 * Time: 1:39 PM
 * To change this template use File | Settings | File Templates.
 */

case class Erf(nTerms: Int) {


  require(nTerms > 0, "number of terms to calculate series must be larger than 0")

  val cn = (for (n <- 1 until nTerms;
                 k <- 0 until n)
  yield 1.0 / ((k + 1) * (2.0 * k + 1.0))).toArray

  cn(0) = 1.0
  for (n <- 1 until cn.length) {
    var numerator = 0.0;
    for (k <- 0 until n) {
      numerator = numerator + cn(k) * cn(n - 1 - k);
    }
    cn(n) = cn(n) * numerator
  }

  val an = (for (n <- 0 until nTerms) yield cn(n) / (2.0 * n + 1.0)).toArray

  def erf(x: Double): Double = {
    org.apache.commons.math.special.Erf.erf(x);
  }

  def invErf(x: Double): Double = {

    var r = 0.0;
    var xp = x;
    val x2 = x * x;
    for (i <- 0 until an.length) {
      r = r + an(i) * xp;
      xp = xp * x2;
    }

    r

  }

}