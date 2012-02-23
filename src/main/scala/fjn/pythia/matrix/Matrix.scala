package fjn.pythia.matrix

import scala.Array



/**
 * User: fran
 * Date: 2/15/12
 * Time: 2:52 PM
 */


class Matrix[@specialized(Int,Short,Long,Float,Double,Complex)T1](nRows:Int,nCols:Int, isRowMajor:Boolean=true)(implicit m:Fractional[T1],implicit val m2:Manifest[T1]) {

  def mul(a:T1,b:T1):T1 = m.times(a,b)
  def add(a:T1,b:T1):T1 = m.plus(a,b)
  def div(a:T1,b:T1):T1 = m.times(a, m.div(m.one,b))
  def sub(a:T1,b:T1):T1 = m.minus(a,b)

  private val data:Array[T1]= new Array[T1](nCols*nRows)

  
      
  def apply(row:Int, col:Int):T1={

     val index = isRowMajor match
     {
       case true => col*numberRows+row
       case false => row*numberCols + col
     }

     return data(index)
  }
  
  def getColArray(i:Int):Array[T1]={

    val r =
      for (n <- 0  until this.numberCols;
         val r = this.apply(n,i)
    ) yield r
    r.toArray
  }
  
  def getRowArray(j:Int):Array[T1]={

    (for (n <- 0  until this.numberCols;
             val r = this.apply(j,n)
        ) yield r) toArray
  }
  
  val numberRows = nRows
  val numberCols = nCols
  
  def zeros()={
      var i:Int=0;
      while(i<numberCols)
      {
        var j:Int = 0
        while(j<numberRows)
        {
          this.set(i,j,m.zero)
          j+=1
        }
      }
  }
  
  def eye():Unit ={
    zeros()
    val limit = math.min(numberRows,numberRows)
    var i=0
    while(i<limit)
    {
      this.set(i,i, m.one)     

    }
  }

  
  def set(row:Int, col:Int,v:T1):Unit={

    val index = isRowMajor match
         {
           case true => col*numberRows+row
           case false => row*numberCols + col
         }

         data(index) = v
  }

  def *(b: Matrix[T1]): Matrix[T1] = {
    case class RowColMsg(rowIndex: Int, colIndex: Int, a1: Array[T1], a2: Array[T1])

    require(this.numberCols == b.numberRows)
    val rMatrix:Matrix[T1] =  new Matrix[T1](this.numberRows, this.numberCols);
    var i = 0
    var j = 0
    var k = 0


    while (i < rMatrix.numberRows) {
      j = 0
      while (j < rMatrix.numberCols) {
        k = 0
        var elem:T1 = rMatrix(i,j)
        while (k < b.numberRows) {
          val v1:T1 = this.apply(i, k)
          val v2:T1 = b(k, j)
          val v3:T1 = mul(v1,v2)
          
          val r:T1 = add(elem,v3)
          elem = r
          k = k + 1
        }
        rMatrix.set(i, j, elem)
        j = j + 1       
      }
      
      i = i + 1
    }

    rMatrix
  }

}
