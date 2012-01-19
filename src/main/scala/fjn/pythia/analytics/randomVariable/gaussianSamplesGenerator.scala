package fjn.pythia.analytics.randomVariable

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 10/1/11
 * Time: 12:59 AM
 * To change this template use File | Settings | File Templates.
 */

object RndGenerationMethod extends Enumeration {
  val BOXMULLER, MARSAGLIA_BRAY = Value
}

case class gaussianSamplesGenerator(method: RndGenerationMethod.Value) {

  private val rndGen = new scala.util.Random();
  
  def nextNumber(): (Double, Double) = {
    this match {
      case (gaussianSamplesGenerator(RndGenerationMethod.BOXMULLER)) => {
        val U1 = rndGen.nextDouble()
        val U2 = rndGen.nextDouble()
        val R = -2.0 * math.log(U1)
        val V = 2.0 * math.Pi * U2
        val sqrtR = math.sqrt(R)
        (sqrtR * math.cos(V), sqrtR * math.sin(V))
      }
      case (gaussianSamplesGenerator(RndGenerationMethod.MARSAGLIA_BRAY)) => {
        var X = 1e9;
        var U1 = 0.0;
        var U2 = 0.0;
        while (X > 1) {
          U1 = rndGen.nextDouble()
          U2 = rndGen.nextDouble()
          U1 = U1 * 2.0 - 1
          U2 = U2 * 2.0 - 1
          X = U1 * U1 + U2 * U2

        }
        val Y = math.sqrt(-2.0 * math.log(X) / X)
        (U1 * Y, U2 * Y)
      }

      case _ => {
        throw new IllegalArgumentException("Unknown random generation type")
      }
    }
  }

}

object gaussianSamplesGenerator{

}



