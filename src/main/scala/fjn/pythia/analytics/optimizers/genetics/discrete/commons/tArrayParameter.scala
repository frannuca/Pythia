package fjn.pythia.analytics.optimizers.genetics.discrete.commons

/**
 * User: fran
 * Date: 7/27/11
 * Time: 12:43 PM
 */

trait tArrayParameter {

   @throws(classOf[IllegalArgumentException])
   def setValue(v:Array[Double]):Unit
   def getValue():Array[Double]
   def getValue(i:Int):Double
   def mutate()
   val minLevelArray: Array[Double]
   val maxLevelArray: Array[Double]

}