package fjn.pythia.analytics.optimizers.genetics.discrete.commons

import populationType._


/**
 * A population is 2D Matrix where rows represent vectors of parameters with dimension
 */
class Population (popSize:Int,nBits:Array[Int],minLevel:Array[Double],maxLevel:Array[Double]) extends tPopulation[Chromosome] {


   require(nBits.size > 0 && nBits.size == minLevel.size && minLevel.size == maxLevel.size);


   def numberOfParameters = nBits.size


   var population = new Array[popPair[Chromosome]](popSize);
   for (i<- 0.until(popSize)){
     population(i) = popPair.create(new Chromosome(nBits,minLevel,maxLevel),-1.0);
   }


  def getPopulationType:populationType = populationType.DISCRETE
  def init() {
    //Creates a random population with uniform distribution in between minLevel and MaxLevel and associated fitness 0.0
    population.foreach(c => {c.chr.random});

    for(i<-0.until(population.size))
      setFitness(i,-1.0)
  }
  def setFitness(index:Int,v:Double) {
    population(index).value=v;
  }


  def totalParameterLength:Int={
    population(0).chr.totalNumberOfBits;
  }
  def getFitness(index:Int)={
    population(index).value;
  }

  def getValue(index:Int):Array[Double]={
    if (index<popSize && index>=0){
         population(index).chr.getValue();
    }
    else{
         null;
    }
  }


  def getBits(index:Int):Array[Int]={

    if (index<popSize && index>=0){
         population(index).chr.getBits();
    }
    else{
         null;
    }

  }

  def setChromosomeI(index:Int,b:Array[Int])={
    if (index<popSize && index>=0){
             population(index).chr.setValueBit(b)
              true
        }
        else{
             false;
        }

  }

  def setChromosomeD(index:Int,b:Array[Double])={
    if (index<popSize && index>=0){
             population(index).chr.setValue(b)
              true
        }
        else{
             false;
        }

  }
}

