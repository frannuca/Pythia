package fjn.pythia.analytics.optimizers.genetics.discrete.commons

import scala.util.Random
import com.sun.xml.internal.ws.developer.MemberSubmissionAddressing.Validation


/**
 *  Chromosome class wraps an internal array of Genes (see class Gene). This class represents one individual within a
 *  population.
 *
 *  Chromosome class is setChromosomeI up providing the following signature:
 * @param nBits: number of bits to codify each parameter within the chromosome.
 * @param minLevelArray: lower bound for each parameter
 * @param maxLevelArray: upper bound for each parameter
 */
case class Chromosome(nBits: Array[Int], minLevelArray: Array[Double],maxLevelArray: Array[Double]) extends Ordered[Chromosome] with tArrayParameter {


  /////////////////////////////////////////////////////////////
  // INITIALIZATION
  ////////////////////////////////////////////////////////////////

  //is mandatory that all the following arrays have the same lengths
  require(nBits.size > 0 && nBits.length == minLevelArray.size && minLevelArray.size == maxLevelArray.size);

  //Number of parameters coded into this chromosome
  def size = nBits.size;


  val chromosome: Array[Gene] = (for (i <- 0.until(size)) yield Gene(minLevelArray(i), maxLevelArray(i), nBits(i))).toArray;


  /////////////////////////////////////////////////////////////
  // END OF INITIALIZATION
  ////////////////////////////////////////////////////////////////

  //Total number of bits coding all the parameters in this chromosome
  def totalNumberOfBits = {
    chromosome.foldLeft(0)((acc, x) => acc + x.nBits)
  }

  /**
   * returns true is both chromosome have the same internal binary representation.
   * minLevels and maxLevels are not compared
   */
  override def equals(that: Any): Boolean = that match {
    case other: Chromosome => {
      val barr1 = this.getBits()
      val barr2 = other.getBits()
      if (barr1.size == barr2.size) {
        for (i <- 0 until barr1.size) {
          if (barr1(i) != barr2(i)) {
            return false;
          }
        }
        true
      }
      else
        false


    }
    case _ => false
  }

  /**
   * returns a copy of this object. This function becomes necessary while recreating new populations, since
   * the usage of elitism forces to replace worst fitness chromosome by the best ones, and repetition of best
   * chromosome must not share same references, since operation on one individual will affect several entries
   * in the population (think of mutation for example)
   */
  def copy(): Chromosome = {
    val a = new Chromosome(nBits, minLevelArray, maxLevelArray);
    a.setValue(this.getValue())
    a
  }

  /**
   *   the order is provided by comparing the double parameter values of this chromosome starting from
   *   the highest indexed parameter. E.g: (3.3,5.2,9.1) > (102.4,10003.3,8.0)
   */
  def compare(that: Chromosome): scala.Int = {

    require(this.size == that.size)


    for (i <- (this.size - 1).to(0).by(-1)) {
      if (this.getValue(i) < that.getValue(i)) {
        return -1
      }
      else if (this.getValue(i) > that.getValue(i)) {
        return 1;
      }
    }

    0
  }


  //Sets the given Gene at position index with binary code given in the slice v(offset),v(offset+nBits(i))
  //@param v  must have dimension of  totalNumberOfBits, being the vector resulting from the aggregation
  // of all the Gene.getBits contained in the specified chromosome
  @throws(classOf[IllegalArgumentException])
  def setValueBit(v: Array[Int]) = {

    if (this.totalNumberOfBits != v.size) {
      throw new IllegalArgumentException("total length for array v does not match this chromosome length")
    }

    else {
      var offset: Int = 0;
      for (n <- 0.until(size)) {
        chromosome(n) := v.slice(offset, offset + chromosome(n).nBits - 1)
        offset += chromosome(n).nBits;
      }

    }

  }

  /**Assigns the vector of parameters (Double) to the chromosome.
   * @param v : vector of parameter of size
   */
 @throws(classOf[IllegalArgumentException])
  def setValue(v: Array[Double]):Unit = {

      if(v.size != size)
      {
         throw new IllegalArgumentException("total length for array v does not match this chromosome length")
      }
      else{
       for (n <- 0.until(size)) { chromosome(n) := v(n)  }
    }

  }

  //Returns the array of  parameter (Double) at the position index
  def getValue(): Array[Double] = {

    try {
      chromosome.map(g => g.getValue());
    }
    catch {
      case _ => null
    }
  }

  //Returns the parameter in position index as a double
  //@param index : position of the parameter to be extracted from the chromosome
  def getValue(index: Int): Double = {

    require(index < chromosome.size && index >= 0)

    chromosome(index).getValue();

  }

  //Returns the Gene at the position index
  def getGene(index: Int): Gene = {
    if (index < 0 || index >= chromosome.size) null;
    else {
      return chromosome(index)

    }
  }

  //Returns the binary codification of the Gene parameter at position index
  def getBits(): Array[Int] = {
    try {
      chromosome.map(g => g.getBits()).flatMap(x => x);
    } catch {
      case _ => null
    }
  }


  //Generates a random chromosome sequences
  def random = {
    chromosome.foreach(g => g.random)
  }

  //Mutates 1 bit from the internal chromosome binary representation
  def mutate():Unit = {
    val rnd = new Random()


    val arr = this.getBits();

    val index = rnd.nextInt(arr.length);
    arr(index) match {
      case (0) => arr(index) = 1;
      case _ => arr(index) = 0;
    }
    this.setValueBit(arr);
  }

  //returns a chromosome vector with double values for friendly visualization
  override def toString(): String = {

    val res = new StringBuilder();
    getValue().foreach(x => res.append(x.toString) + " ");
    res.toString()
  }
}


