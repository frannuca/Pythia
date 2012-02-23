package fjn.pythia.matrix

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 2/23/12
 * Time: 12:02 AM
 * To change this template use File | Settings | File Templates.
 */
case class Complex(re: Double, im: Double) extends Fractional[Complex]  {

  val ic = Complex(0.0,1.0)
  
  
  def div(x: Complex, y: Complex): Complex = {
    new Complex(x.re/y.abs,x.im/y.abs)         
  }

  def plus(x: Complex, y: Complex): Complex={
    Complex(x.re+y.re, x.im+y.im)
  }

  def minus(x: Complex, y: Complex): Complex  = {
    plus(x,-y)    
  }
  
  def unary_- : Complex = Complex(-re, -im)
  def unary_+ : Complex = Complex(re, im)
  
  lazy val abs:Double =  math.sqrt(re*re+im*im)
  
  def times(x: Complex, y: Complex): Complex ={
    Complex(re*re-im*im,re*im*2.0)
  }
  
  def times(x: Complex, y: Double): Complex ={
      Complex(re*y,re*y)
    }
  
  def times(y: Double,x: Complex): Complex ={
        times(x,y)
      }
  
  def checkConversion[T]()(implicit m3:Manifest[T]):Unit={
    if(im !=0 || !(im.isInstanceOf[T])) throw new IllegalArgumentException("Complex are not"+m3.getClass.getName)
    
  }
  def negate(x: Complex): Complex = Complex(-re, -im)
  def fromInt(x: Int): Complex = Complex(x,0)
  def toInt(x: Complex): Int = checkConversion[Int](); re.toInt
  def toLong(x: Complex): Long = if(im !=0 || !(im.isInstanceOf[Long])) throw new IllegalArgumentException("Complex are not Long") else re.toLong
  def toFloat(x: Complex): Float = if(im!=0) throw new IllegalArgumentException("Complex are not Floats") else re
  def toDouble(x: Complex): Double

}


