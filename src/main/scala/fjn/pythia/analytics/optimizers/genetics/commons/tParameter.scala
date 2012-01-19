package fjn.pythia.analytics.optimizers.genetics.commons

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 7/27/11
 * Time: 12:43 PM
 * To change this template use File | Settings | File Templates.
 */

trait tParameter {

   def setValue(v:Array[Double]):Unit;
   def getValue():Array[Double];
   def getValue(i:Int):Double;
   def mutate():Unit;

}