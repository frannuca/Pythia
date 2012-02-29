package fjn.pythia.matrix

import scala.Array

import akka.actor.Actor
import akka.actor.Actor.{actorOf}
import akka.event.EventHandler
import java.util.ArrayList
import akka.dispatch.Future


/**
 * User: fran
 * Date: 2/15/12
 * Time: 2:52 PM
 */


import Complex._

class Matrix[T1](nRows: Int, nCols: Int, isRowMajor: Boolean = true, numberOfCores:Int = 1)(implicit m2: Manifest[T1], implicit val m: Fractional[T1]) {

  outer =>

  private val data: Array[T1] =
    (for (k <- 0 until (nCols * nRows)

    ) yield m.zero).toArray


  class multiplicationWorker(rMatrix: Matrix[T1], b: Matrix[T1]) extends Actor {
    def receive = {
      case ((i: Int, j: Int)) => {
        var k = 0
        var r: T1 = rMatrix(i, j)
        while (k < b.numberRows) {
          val v1: T1 = outer.apply(i, k)
          val v2: T1 = b(k, j)
          val v3: T1 = m.times(v1, v2)
          r = m.plus(r, v3)
          k = k + 1

        }
        rMatrix.set(i, j, r)
      }
      case "end" => self.stop()
    }
  }


  def apply(row: Int, col: Int): T1 = {

    val index = isRowMajor match {
      case true => col * numberRows + row
      case false => row * numberCols + col
    }

    data(index)
  }

  def getColArray(i: Int): Array[T1] = {

    val r =
      for (n <- 0 until this.numberCols;
           val r = this.apply(n, i)
      ) yield r
    r.toArray
  }

  def getRowArray(j: Int): Array[T1] = {

    (for (n <- 0 until this.numberCols
    ) yield this.apply(j, n)).toArray
  }

  val numberRows = nRows
  val numberCols = nCols

  def zeros = {
    var i: Int = 0;
    while (i < numberCols) {
      var j: Int = 0
      while (j < numberRows) {
        this.set(i, j, m.zero)
        j += 1
      }
      i = i + 1
    }
  }

  def eye = {
    zeros
    val limit = math.min(numberRows, numberRows)
    var i = 0
    while (i < limit) {
      this.set(i, i, m.one)
      i = i + 1

    }
  }


  def set[T2 <% T1](row: Int, col: Int, v: T2): Unit = {

    val index = isRowMajor match {
      case true => col * numberRows + row
      case false => row * numberCols + col
    }

    data(index) = v

    Unit
  }

  def *(b: Matrix[T1]): Matrix[T1] = {
    case class RowColMsg(rowIndex: Int, colIndex: Int, a1: Array[T1], a2: Array[T1])

    require(this.numberCols == b.numberRows)
    val rMatrix: Matrix[T1] = new Matrix[T1](this.numberRows, this.numberCols);
    var i = 0
    var j = 0



    if (numberOfCores > 1) {

      implicit val timeout = Actor.Timeout(60000)
      var rr: Int = 0;

      var listOfActors =
              (for (f <- 0 until numberOfCores) yield actorOf(new multiplicationWorker(rMatrix, b)).start()).toArray


      var counter = numberOfCores;

      while (i < rMatrix.numberRows) {

        counter = counter + 1
        if(counter>1000)
                  {
                    counter = 0
                    listOfActors.foreach(a => a ! "end")


                          while (listOfActors.forall(a => a.isRunning))
                            Thread.sleep(200)

                    listOfActors =
                                            (for (f <- 0 until numberOfCores) yield actorOf(new multiplicationWorker(rMatrix, b)).start()).toArray

                  }


        j = 0
        while (j < rMatrix.numberCols) {
          listOfActors(rr % numberOfCores) !(i, j);
         

          j = j + 1


        }
        i = i + 1
      }


      

    }
    else {
      var k = 0
      while (i < rMatrix.numberRows) {

        j = 0
        while (j < rMatrix.numberCols) {
          var r: T1 = rMatrix(i, j)
          k = 0
          while (k < b.numberRows) {
            val v1: T1 = outer.apply(i, k)
            val v2: T1 = b(k, j)
            val v3: T1 = m.times(v1, v2)
            r = m.plus(r, v3)
            k = k + 1
          }
          rMatrix.set(i, j, r)
          j = j + 1
        }
        i = i + 1
      }


    }



    rMatrix
  }

  override def toString = {
    val rStr = new StringBuilder
    var i = 0;
    var j = 0;
    while (i < numberRows) {
      j = 0
      while (j < numberCols) {
        rStr.append(this.apply(i, j))
        rStr.append(",")
        j = j + 1;
      }
      rStr.append('\n')
      i = i + 1
    }

    rStr.toString()

  }
}
