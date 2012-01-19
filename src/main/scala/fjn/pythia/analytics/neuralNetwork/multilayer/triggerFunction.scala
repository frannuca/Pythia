package fjn.pythia.analytics.neuralNetwork.multilayer

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 11/25/11
 * Time: 10:58 PM
 * To change this template use File | Settings | File Templates.
 */

trait triggerFunction {


  def f(x: Double): Double;

  def df(x: Double): Double;
}

trait Sigmoidea extends triggerFunction {

  val c: Double

  def f(x: Double): Double = {
    (1.0 - math.exp(-c * x)) / (1.0 + math.exp(-c * x))
  }

  def df(x: Double): Double = {
    (f(x + 1e-7) - f(x - 1e-7)) / 2e-7
  }
}