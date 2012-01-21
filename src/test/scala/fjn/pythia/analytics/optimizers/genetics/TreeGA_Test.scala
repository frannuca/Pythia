
package fjn.pythia.analytics.optimizers.genetics


import org.specs2.mutable.Specification


class TreeGA_Test extends Specification {

  "Creating a optimization test for TREE GA" should {

    "Converge to given solution" in {
    `testAlgorithm` mustEqual true
    }

  }


    def `testAlgorithm` = {
      val popSize = 100;
      val elitismFactor = 0.10;
      val nBits = Array (10, 10, 10, 10);
      val minLevel = Array (- 50d, -50d, -50d, - 50d)
      val maxLevel = Array (50d, 50d, 50d, 50d)
      def pFunc(x: Array[Double]): Double = {


      1.0 / ((x (0) + 9) * (x (0) + 9) + (x (1) - 9) * (x (1) - 9) + (x (2) + 9) * (x (2) + 9) + (x (3) - 9) * (x (3) - 9) );

    }

      //r
      //}


      val mutationPerc = 0.01;

      val talg = new TreeGA (popSize, elitismFactor, nBits, minLevel, maxLevel, pFunc, mutationPerc);

      for (i <- 0 until 100) {
      talg += 3;
      val res = talg.storage.getBest (1);
      print ("iter=" + talg.iterations.toString + " ---> ")
      for (k <- 0.until (res.size) ) {
      res (k).chr.getValue ().foreach (x => {
      print (x + ";")
    });
      println ("");
    }
    }

      val resArray = Array (- 9, 9, - 9, 9);

      (talg.storage.getBest (1) (0).chr.getValue ().toList, resArray.toList).zipped.forall ((x, y) => (scala.math.abs (x - y) <= 0.5) )

    }

  }

