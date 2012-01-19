package fjn.pythia.analytics.commons

import scalala.tensor.dense.DenseMatrix

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 11/25/11
 * Time: 10:50 PM
 * To change this template use File | Settings | File Templates.
 */

trait NNMatrixExtensions {
  def setOnes(m: DenseMatrix[Double]): Unit = {
    for (i <- 0 until m.numCols) {
      m.update(m.numRows - 1, i, 1.0)
    }
  }

  def sub(m: DenseMatrix[Double]): DenseMatrix[Double] =
    m(0 until m.numRows - 1, 0 until m.numCols).toDense

  def copyInExtendedMatrix(dest: DenseMatrix[Double], src: DenseMatrix[Double]) = {
    require(dest.numCols == src.numCols && (dest.numRows - 1) == src.numRows)


    var i: Int = 0
    var j: Int = 0
    while (i < src.numRows) {
      j = 0
      while (j < src.numCols) {
        dest.update(i, j, src(i, j))
        j += 1
      }
      i += 1
    }

  }

  def fillOnes(m: DenseMatrix[Double]): DenseMatrix[Double] = {
    val m2 = DenseMatrix.zeros[Double](m.numRows + 1, m.numCols)
    copyInExtendedMatrix(m2, m)
    setOnes(m2)
    m2
  }

  def applyFunction(pFunc: (Double => Double), m: DenseMatrix[Double]): Unit = {
    var i: Int = 0
    var j: Int = 0
    while (i < m.numRows) {
      j = 0
      while (j < m.numCols) {
        m.update(i, j, pFunc(m(i, j)))
        j += 1
      }
      i += 1
    }


  }

  def toEye(d: DenseMatrix[Double]): DenseMatrix[Double] = {
    val m = DenseMatrix.zeros[Double](d.numRows, d.numRows);
    for (i <- 0 until d.numRows) {
      m(i, i) = d(i, 0)
    }
    m
  }


}