package fjn.pythia.analytics.randomVariable


import no.uib.cipr.matrix.{Matrix, Matrices, DenseMatrix, Vector}
import fjn.pythia.analytics.commons.matrixExtensions
;

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 9/30/11
 * Time: 8:05 PM
 * To change this template use File | Settings | File Templates.
 */
case class Gaussian(sigma: DenseMatrix, mu: Vector, dim: Int) {

  require(sigma.numColumns() == dim, "Wrong matrix dimension given for gaussion covariance matrix")
  require(mu.size() == dim, "Wrong mean vector dimension for gaussian mean vector specification")

  val detSigma = matrixExtensions.determinant(sigma)
  val I = Matrices.identity(dim);
  val AI = I.copy();
  val invSigma = sigma.copy()
  invSigma.solve(I, AI);

  val scaleFactor = 1.0 / (2.0 * math.Pi)


  val a = Array(
    2.50662823884,
    -18.61500062529,
    41.39119773534,
    -25.44106049637
  )
  val b = Array(
    -8.47351093090,
    23.08336743743,
    -21.06224101826,
    3.13082909833)

  val c = Array(
    0.3374754822726147,
    0.9761690190917186,
    0.1607979714918209,
    0.0276438810333863,
    0.0038405729373609,
    0.0003951896511919,
    0.0000321767881768,
    0.0000002888167364,
    0.0000003960315187)

  def inverseGaussian(u: Double): Double = {
    require(0 <= u && u <= 1, "values accepted by the inverse gaussian calculater are [0,1]")
    var y = u - 0.5
    var x = 0.0
    if (math.abs(y) < 0.42) {
      val r = y * y
      x = y * (((a(3) * r + a(2)) * r + a(1)) * r + a(0)) / ((((b(3) * r + b(2)) * r + b(1)) * r + b(0)) * r + 1)
    } else {
      var r = u
      if (y > 0)
        r = 1 - u

      r = math.log(-math.log(r))
      x = c(0) + r * (c(1) + r * (c(2) + r * (c(3) + r * (c(4) + r * (c(5) + r * (c(6) + r * (c(7) + r * c(8))))))))

      if (y < 0)
        x = -x
    }

    x
  }

  val d = Array(
    0.319381530,
    -0.356563782,
    1.781477937,
    -1.821255978,
    1.330274429
  )

  val p = 0.2316419
  val e = math.log(math.sqrt(2.0 * math.Pi))

  def PHI(x: Double): Double = {

    val v1 = math.abs(x)
    val t = 1.0 / (1 + v1 * p)
    val s = ((((d(4) * t + d(3)) * t + d(2)) * t + d(1)) * t + d(0)) * t
    val y = s * math.exp(-0.5 * x * x - e)
    if (x > 0) 1.0 - y
    else y
  }

  def cummulativeNormalDistributionMarsaglia(x: Double): Double = {
    throw new IllegalArgumentException("not cummulativeNormalDistributionMarsaglia implemented")
  }
}





