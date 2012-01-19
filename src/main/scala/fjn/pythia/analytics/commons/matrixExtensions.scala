package fjn.pythia.analytics.commons

import no.uib.cipr.matrix.{DenseMatrix, Matrix}


/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 10/1/11
 * Time: 11:32 PM
 * To change this template use File | Settings | File Templates.
 */

object matrixExtensions{
  def determinant(m:Matrix):Double={

    var result = 0.0
    val nCols = m.numColumns()
    val nRows = m.numRows()

    if(nRows==1)
      m.get(0,0)
    else if (nRows == 2)
      m.get(0,0)*m.get(1,1)-m.get(0,1)*m.get(1,0)
    else{
      for (j  <- 0 until nCols)
      {
        val temp = complementaryMatrix(0,j,m)

        result += m.get(0,j) * math.pow(-1, j) * determinant(temp);
      }

      return result;

    }
  }

  def complementaryMatrix(col:Int,row:Int,m:Matrix):Matrix={
    val r = new DenseMatrix(m.numRows()-1,m.numColumns()-1)

    for(i <- 0 until m.numRows() if i!= row;
        j <- 0 until m.numColumns() if j != col )
    {
      var iAux = i
      var jAux = j
      if(i>row)
        iAux = i-1
      if(j>col)
         iAux = j -1

      r.set(iAux,jAux,m.get(i,j))
    }

    r

  }
}