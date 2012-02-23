package fjn.pythia.matrix

import scala.math.{ Fractional, Numeric, Ordering };

case class Complex(real : Double, imag : Double) {
  override def toString = real + " + " + imag + "i";

  def +(that : Complex) =
    Complex(this.real + that.real, this.imag + that.imag);

  def +(that : Int) =
    Complex(this.real + that, this.imag);

  def +(that : Long) =
    Complex(this.real + that, this.imag);

  def +(that : Float) =
    Complex(this.real + that, this.imag);

  def +(that : Double) =
    Complex(this.real + that, this.imag);

  def -(that : Complex) =
    Complex(this.real - that.real, this.imag - that.imag);

  def -(that : Int) =
    Complex(this.real - that, this.imag);

  def -(that : Long) =
    Complex(this.real - that, this.imag);

  def -(that : Float) =
    Complex(this.real - that, this.imag);

  def -(that : Double) =
    Complex(this.real - that, this.imag);

  def *(that : Complex) =
    Complex(this.real * that.real - this.imag * that.imag,
            this.real * that.imag + this.imag * that.real);

  def *(that : Int) =
    Complex(this.real * that, this.imag * that);

  def *(that : Long) =
    Complex(this.real * that, this.imag * that);

  def *(that : Float) =
    Complex(this.real * that, this.imag * that);

  def *(that : Double) =
    Complex(this.real * that, this.imag * that);

  def /(that : Complex) = {
    val denom = that.real * that.real + that.imag * that.imag;
    Complex((this.real * that.real + this.imag * that.imag) / denom,
            (this.imag * that.real - this.real * that.imag) / denom);
  }

  def /(that : Int) =
    Complex(this.real / that, this.imag / that);

  def /(that : Long) =
    Complex(this.real / that, this.imag / that);

  def /(that : Float) =
    Complex(this.real / that, this.imag / that);

  def /(that : Double) =
    Complex(this.real / that, this.imag / that);

  def unary_- =
    Complex(-real, -imag);

  def abs =
    math.sqrt(real*real + imag*imag);

  def conjugate =
    Complex(real, -imag);

  override def equals(that : Any) = that match {
    case that : Complex => this.real == that.real && this.imag == that.imag;
    case real : Double => this.real == real && this.imag == 0;
    case real : Int => this.real == real && this.imag == 0;
    case real : Short => this.real == real && this.imag == 0;
    case real : Long => this.real == real && this.imag == 0;
    case real : Float => this.real == real && this.imag == 0;
    case _ => false;
  }
}

object Complex { outer =>

  /** Constant Complex(0,0). */
  val zero = new Complex(0,0);

  /** Constant Complex(1,0). */
  val one = new Complex(1,0);

  /** Constant Complex(NaN, NaN). */
  val nan = new Complex(Double.NaN, Double.NaN);

  /** Constant Complex(0,1). */
  val i = new Complex(0,1);

  //
  // scalar
  //
    //
  // scala.math.Numeric and scala.math.Fractional
  //
  // TODO: Implement scala.math.Integral trait, if this is ever required
  //       for some reason.

  /** `Complex` as `scala.math.Numeric` trait.
    * Conversions to `Int`, `Long`, `Float` and `Double` are only performed
    * if the imaginary component of the complex number is exactly 0. */
  trait ComplexIsConflicted extends Numeric[Complex] {
    def plus(x: Complex, y: Complex): Complex = x + y
    def minus(x: Complex, y: Complex): Complex = x - y
    def times(x: Complex, y: Complex): Complex = x * y
    def negate(x: Complex): Complex = -x
    def fromInt(x: Int): Complex = Complex(x, 0)
    def toInt(x: Complex): Int = strictlyReal(x).toInt
    def toLong(x: Complex): Long = strictlyReal(x).toLong
    def toFloat(x: Complex): Float = strictlyReal(x).toFloat
    def toDouble(x: Complex): Double = strictlyReal(x)

    /** Checks that a `Complex` number is strictly real, and returns the real
      * component. */
    private def strictlyReal(x: Complex): Double = {
      require(x.imag == 0.0)  // only proceed if x.imag is *exactly* zero
      x.real
    }
  }
  /** `Complex` as `scala.math.Fractional` trait. */
  trait ComplexIsFractional extends ComplexIsConflicted
		            with Fractional[Complex]
  {
    def div(x: Complex, y: Complex): Complex = x / y
  }
  /** Ordering for complex numbers: orders lexicographically first
    * on the real, then on the imaginary part of the number. */
  trait ComplexOrdering extends Ordering[Complex] {
    override def compare(a : Complex, b : Complex) = {
      if (a.real < b.real) -1
      else if (a.real > b.real) 1
      else if (a.imag < b.imag) -1
      else if (a.imag > b.imag) 1
      else 0;
    }
  }
  /** Implicit object providing `scala.math.Fractional` capabilities.
    * Although complex numbers have no natural ordering, some kind of
    * `Ordering` is required because `Numeric` extends `Ordering`.  Hence,
    * an ordering based upon the real then imaginary components is used. */
  implicit object ComplexIsFractional extends ComplexIsFractional
                                      with ComplexOrdering
  



}



