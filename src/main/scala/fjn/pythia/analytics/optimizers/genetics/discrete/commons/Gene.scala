package fjn.pythia.analytics.optimizers.genetics.discrete.commons

import scala.math.{pow,ceil}
import util.Random


/**
 * Gene class contains a binary codification of a given Double value
 * A gene specifies a double values with the following features:
 *  @param  minLevel  is the lower bound constraint for the internal value
 *  @param maxLevel is the upper bound constraint for the interanl value
 *  @param nBits: pow(2,numberOfBits) is the number of level in which the range (maximum-minimum) is
 *  discretized to codify the internal value into binary.
 */
case class  Gene(minLevel:Double,maxLevel:Double,nBits:Int) extends Ordered[Gene] with tParameter {


  //Constraints on the constructor parameters
  require(minLevel < maxLevel);
  require(nBits>0);


  //Ordered train implementation ...
  def compare(that:Gene):Int={
    this.getValue().compare(that.getValue());
  }
  val rnd = new Random()
  val diff:Double = (maxLevel-minLevel); //range of variation for the internal value


  val dt = diff/ (pow (2.0,nBits)-1); //Resolution to represent a double number in between minimum and maximum
  val binaryCode = new Array[Int](nBits);///binary codification of the internal value in between [minLevel,maxLevel]

  //First initialization of the binary array to 0
  random;   //Generates a random variable, which is necessary in most of the genetic algorithms that my use this class

  /**
   * Assign to the internal value a given double
   * This function produces an representation error less than dt, given the discrete nature of this parameter representation
   * @param d : the value to be codified and transfered to binary code
   */
  def :=(d:Double)={
      this.toBinaryArray(d);
      Unit
    }

  /**
   * copies the given sequence of bits (as integers) into the internal binary code of this class.
   */
  def :=(d:Seq[Int])={d.copyToArray(binaryCode,0)}

  def setValue(d:Double)={
    this.:=(d)
    }

  ///Creates a random internal binary code
  def random={

    this.:=(rnd.nextDouble() * (maxLevel-minLevel)+minLevel)
  }
  /**
   * returns the codified binary value as a double.
    */
  def getValue():Double={
     val listAux = 0.to(nBits-1) zip binaryCode;
     minLevel + (listAux.foldLeft(0.0D)((acc,x)=> acc + pow(2.0,x._1.toDouble)*x._2.toDouble ))*dt;
   }

  //Returns the internal binary code
   def getBits():Array[Int]= binaryCode;


  /**
   * sets the binary code to 0 (or Double value to minLevel)
   */
  private def reset()= binaryCode.foreach(x=>0);

  /**
   * transforms d into a binary code of nBits bits
   * @param d : value to be transformed into a nBits binary code. The result of this operation is stored into
   * this.binaryCode
   */
  private def toBinaryArray(d:Double):Unit = {
    reset();
    var nVal:Long=  ceil(math.ceil((d - minLevel)/dt)).toLong;

    for(i<- (nBits-1).to(0).by(-1)){
      val auxInt = pow(2.0,i).toLong;
      binaryCode(i)= if( (nVal - auxInt) >= 0) 1 else 0
      if(binaryCode(i)>0) nVal -=  auxInt;
    }
  }

}