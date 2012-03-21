package fjn.pythia.analytics.optimizers.genetics.discrete

import commons.{popPair, Population, Chromosome, tAlgorithm}
import math.{log}
import java.lang.Exception
import collection.mutable.ListBuffer
import util.Random
import scala.{Array, Double}
import collection.JavaConversions._
import java.util.HashMap
import actors.Actor


class TreeGA( popSize:Int,
              reinsertationFactor:Double,
              nBits:Array[Int],
              minLevel:Array[Double],
              maxLevel:Array[Double],
              pFunc:(Array[Double])=>Double,
              mutationProb:Double) extends tAlgorithm[Double] {



  val rndGen  = new Random()
  val cInit = 500d;
  val incTree = 2d;
  val alphaTree = 0.75d;
  private var elitism:Int = (math.ceil(reinsertationFactor*popSize.toDouble)).toInt;

  if (elitism < 0) elitism = 0;
  else if (elitism>popSize) elitism=popSize;

  var storage = new Population(popSize,nBits,minLevel,maxLevel)
  var bestVector= new Array[popPair[Chromosome]](elitism);

  def fillBestVector() ={
     bestVector = storage.getBest(elitism);
  }


  val chrLength= storage.totalParameterLength
  var A_Tree:Array[Array[Array[Double]]]= Array.fill[Double](chrLength,chrLength,4)(cInit);
  //Init the Tree
  val initTreeMatrix = {
      //INITIALISATION OF THE TABLE OF PSEUDO PROBABILITIES TO cInit
    A_Tree= Array.fill[Double](chrLength,chrLength,4)(cInit);
  }

  val pFitness:(Array[Double]) => Double =  pFunc

  private def prob(j:Int,val_j:Int,val_ref:Int ):Double =
  {

    val i = val_ref;
    var p_xj:Double = 0.0;
    if (j>i)
    {
      p_xj=(A_Tree(i)(j)(0+val_j)+A_Tree(i)(j)(2+val_j))/(A_Tree(i)(j)(0)+A_Tree(i)(j)(1)+A_Tree(i)(j)(2)+A_Tree(i)(j)(3));
    }
    else if (i>j)
    {
      p_xj=(A_Tree(j)(i)(0+2*val_j)+A_Tree(j)(i)(1+2*val_j))/(A_Tree(j)(i)(0)+A_Tree(j)(i)(1)+A_Tree(j)(i)(2)+A_Tree(j)(i)(3));
    }
    else
    {
      throw new Exception("Probability error in double GeneticAlg::CMTree::prob( int j,int val_j,int val_ref )");
    }

    (p_xj);
  }
  def prob(i:Int, val_i:Int,j:Int,val_j:Int ):Double=
  {
    var p_xj:Double = 0.0;
    var p_xi_xj:Double = 0.0;
    if (i<j)
    {
      p_xj=A_Tree(i)(j)(0+val_j)+A_Tree(i)(j)(2+val_j);  //p(xi=0,Xj=xj)+p(xi=1,Xj=xj)
      //computing randomVariable of p(xi,xj)
      p_xi_xj=A_Tree(i)(j)(2*val_i+val_j);
    }
    else if (i>j)
    {
      p_xj=A_Tree(j)(i)(2*val_j)+A_Tree(j)(i)(1+2*val_j);  //p(xi=0,Xj=xj)+p(xi=1,Xj=xj)
      //computing randomVariable of p(xi,xj)
      p_xi_xj=A_Tree(j)(i)(2*val_j+val_i);
    }
    else
    {
      throw new Exception("Probability error in double GeneticAlg::CMTree::prob( int i,int val_i,int j,int val_j )");
    }
    (p_xi_xj/p_xj);
  }

  def Info(Xi:Int,Xj:Int ):Double={
    (prob(Xi,0,Xj,0)*log((prob(Xi,0,Xj,0)/(prob(Xi,0,Xj)*prob(Xj,0,Xi))))+
		 prob(Xi,0,Xj,1)*log((prob(Xi,0,Xj,1)/(prob(Xi,0,Xj)*prob(Xj,1,Xi))))+
		 prob(Xi,1,Xj,0)*log((prob(Xi,1,Xj,0)/(prob(Xi,1,Xj)*prob(Xj,0,Xi))))+
		 prob(Xi,1,Xj,1)*log((prob(Xi,1,Xj,1)/(prob(Xi,1,Xj)*prob(Xj,1,Xi))))  );
};


  private def rootBit(computeRoot:Boolean):Int={

       val rndGen = new Random();
      rndGen.nextInt(chromosomeLength);

//    computeRoot match{
//      case true =>{
//                    var H_min=1e9;
//                    var root:Int = -1;
//
//                    for (u <- 0.until(chromosomeLength))
//                    {
//                      var vref:Int = 0;
//                      if (u==0)
//                      {
//                        vref=1;
//                      }
//                      else
//                      {
//                        vref=u-1;
//                      }
//
//                      val aux_H:Double = -1.0*(prob(u,0,vref)*log(prob(u,0,vref))+prob(u,1,vref)*log(prob(u,1,vref)));
//
//                      if (aux_H<H_min)
//                      {
//                        H_min=aux_H;
//                        root=u;
//                      }
//                    }
//
//        root;
//      }
//      case(_) => 0
//    }



  }



  def chromosomeLength = storage.population(0).chr.totalNumberOfBits

   class Linkage(parentx:Int,childrenx:ListBuffer[Int]){

    var parent =  parentx;
    var children = childrenx;
  }

  type TreeData = java.util.HashMap[Int,Linkage]
  private def buildDependencyTree(root:Int):TreeData = {

      val bestMatching= new ListBuffer[Int];
      for(jj <- 0 until chromosomeLength)
      {
        bestMatching += root;
      }

      val bits_out_tree = new ListBuffer[Int]();
      for (uu <- 0 until chromosomeLength)
      {
        bits_out_tree += uu;
      }
     //erasing root from bits_in_tree;
      bits_out_tree.remove(root);

    //tree key is the bit position, tree value defines the tuple2 (parent,children)
      val tree = new TreeData()
      tree.put (root , new Linkage(-1,new ListBuffer[Int]()));



      while(bits_out_tree.length > 0)
      {
        var I_max = -1e9;
        var Xadd = -1;
        for (yy <- 0 until bits_out_tree.length)
        {
          val inf=Info(bits_out_tree(yy),bestMatching(bits_out_tree(yy)));
          if (inf>I_max)
          {
            I_max=inf;
            Xadd=yy;//warning, Xadd is the index inside bits_out_tree but the bit is bits_out_tree[Xadd]
          }
        }


        val newChild =  bits_out_tree(Xadd)
        val rootParent =   bestMatching(newChild);
        //adding Xadd into tree with bestmatchin[Xadd] as parent,removing Xadd from bits_out_tree,
        tree .put   (newChild ,new Linkage(rootParent,new ListBuffer[Int]()));

        tree.get(rootParent).children += newChild
        bits_out_tree.remove(Xadd);

        //updating bestmathing vector with the new tree:
        for (yy <- 0 until bits_out_tree.length)
        {
          val inf1=Info(bits_out_tree(yy),newChild);
          val inf2=Info(bits_out_tree(yy),bestMatching(bits_out_tree(yy)));
          if (inf1>inf2) {bestMatching(bits_out_tree(yy))=newChild;}
        }
    }//end while bits_out_tree.size>0

    tree;
  }

  private def reCreatePopulation(tree:TreeData) = {

    var root = -1;

    tree.foreach( node =>
      {if(node._2.parent == -1)
        root = node._1});

    val rndGen = new Random()
       for (n <- 0 until popSize)
         {


           val p = Array.fill(chromosomeLength)(-1);


           //creating binary chromosome numberOfIteration to be inserted in population
           //setting root bit:

           val vref = (root + 1) % chromosomeLength;
           val prob_bit=prob(root,1,vref);//randomVariable of root to be 1;
           val prob_aux= rndGen.nextDouble();//rand value between 0 and 1;
           if (prob_aux<=prob_bit) {p(root)=1;}
           else {p(root)=0;};


           transverseTree(tree,root,p);
           //INSERTING IN POPULATION:

           storage.setChromosomeI(n,p);
           storage.setFitness(n,-1)


         }//end for numberOfIteration<dim_population




  }

  def transverseTree(tree:TreeData,m:Int,p:Array[Int]):Unit={

    if (tree.get(m).children.length > 0){
       val rndGen = new Random()
       tree.get(m).children.foreach(
                k => {
                       val prob_aux= rndGen.nextDouble();//rand value between 0 and 1;
                       val prob_bit=prob(k,1,tree(k).parent,p(tree(k).parent));//conditional randomVariable p(xi/parent(xi))
                       if (prob_aux<=prob_bit) {p(k)=1;}
                       else{p(k)=0;}

                       transverseTree(tree,k,p);
                     })
    }



  }

  /**
   * iterates the pseudo randomVariable matrix by parsing the current population chromosome
   * distribution
   */
  private def ActualizeTree = {


    //decrease all elements of the Pseudo prob. matrix by alphaTree
	val minimum_A=1e-9;
	for (i <- 0 until chromosomeLength -1;  j <- (i+1) until chromosomeLength)
	{

		if (A_Tree(i)(j)(0)>minimum_A)
		{A_Tree(i)(j)(0)=A_Tree(i)(j)(0)*alphaTree;}

		if (A_Tree(i)(j)(1)>minimum_A)
		{A_Tree(i)(j)(1)=A_Tree(i)(j)(1)*alphaTree;}

		if (A_Tree(i)(j)(2)>minimum_A)
		{A_Tree(i)(j)(2)=A_Tree(i)(j)(2)*alphaTree;}

		if (A_Tree(i)(j)(3)>minimum_A)
		{A_Tree(i)(j)(3)=A_Tree(i)(j)(3)*alphaTree;}


	}

	//select the best chromosome from the population and actualise the those values
    // of the matrix representing the chromosome distribution

		storage.getBest(elitism).foreach(item => {
       val  p = item.chr.getBits();
       for (i <- 0 until chromosomeLength-1;  j <- (i+1) until chromosomeLength)
        {
          if (p(i)==0 && p(j)==0) {A_Tree(i)(j)(0) += incTree;}

          if (p(i)==0 && p(j)==1) {A_Tree(i)(j)(1) += incTree;}

          if (p(i)==1 && p(j)==0) {A_Tree(i)(j)(2) += incTree;}

          if (p(i)==1 && p(j)==1) {A_Tree(i)(j)(3) += incTree;}

        }
    })

  }

  /**
   * mutates one bit of the sequence with randomVariable mutationProb
   */
  private def  mutation(doMutation:Boolean):Boolean={
    try{
      if (doMutation){


            var nMutations = mutationProb*popSize;
            if (nMutations<1)
              nMutations=1;
            else if (nMutations>popSize){
              nMutations=popSize;
            }
            for (m <- 0.until(nMutations.toInt)){

              val index = rndGen.nextDouble()* popSize.toDouble
              this.storage.population(index.toInt).chr.mutate();

            }

          }


          true;

    }
    catch {
      case _ => false;
    }


  }

  def func2(x:popPair[Chromosome]):popPair[Chromosome] = {
                     var f = pFunc(x.chr.getValue())
                      var key = x.chr.copy;
                       popPair.create(key,f);
                     }

 class Worker extends Actor {
            def act() { Actor.loop { react {
              case s: popPair[Chromosome] => reply(func2(s))
              case _ => {println("exiting");
                        exit()}
            }}}
          }

   val workers = new ListBuffer[Actor]();
   for (i <- 0 until popSize)
     workers += new Worker().start();
  /**
   * Computes the fitness for all the members of the population and returns the best elitism items
   */
  private def ComputeFitness():Option[Array[popPair[Chromosome]]]={

    try{






          val futures = for ((w,a) <- workers zip storage.population ) yield  w !! a

          storage.population = futures.map(f => f() match {
              case i: popPair[Chromosome] => i
              case _ => throw new Exception("Whoops--didn't expect to get that!")
            }).toArray







//           storage.population = storage.population.seq.map(x => {
//             var f = pFunc(x.chr.getValue());
//             var key = x.chr.copy;
//             popPair.create(key,f);
//
//           }).seq .toArray

    if (bestVector.forall(x => x !=null))
    {
         Some(bestVector)
    }
    else
      None
    }
    catch{
      case _=>  None
    }

  }


  private def replace_worst(b:Array[popPair[Chromosome]] ):Boolean={

        val sz = b.length;

        for (n <- 0.until(sz)){
          val index = storage.getWorst()

          if(b(n).value >  storage.population(index).value)
            storage.population(index) = popPair.create(b(n).chr.copy(),b(n).value);
        }

     true;
  }

  override def next():Boolean={
    if (iterations==0)
    {
      ComputeFitness()
      fillBestVector()
    };



     try{

        val root = rootBit(true)
        val tree = buildDependencyTree(root)
        reCreatePopulation(tree)
        mutation(true)
        val r = ComputeFitness()
        fillBestVector();
        replace_worst(bestVector)

       ActualizeTree


//       val v1 = Array(8.3137d);
//       storage.setChromosomeI(0,v1);
//       val v2 = storage.getValue(0);
//       if (v1(0)!=v2(0)){
//         val a=1.0;
//       }



//       var counter= 0d;
//       for (i <- 0 until popSize if (counter < popSize* 0.8))
//       {
//
//         counter = 0
//         for (j <- 0 until popSize){
//           if(storage.population(i).chr == storage.population(j).chr)
//             counter += 1d
//         }
//
//
//
//       }
////       println(iterations.toString()+"=")
////
////       println (storage.toString())
////
////       println("--------------------------------------------------------")
//
//        if (counter > popSize* 0.9)
//           false;
//        else
          true
     }
     catch{
       case e:Exception => {
         println(e.toString)
         false
       }
     }

  }



}