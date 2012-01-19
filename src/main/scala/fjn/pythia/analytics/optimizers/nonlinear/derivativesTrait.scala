package fjn.pythia.analytics.optimizers.nonlinear

import scalala.tensor.dense.DenseMatrix

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 1/3/12
 * Time: 12:20 PM
 * To change this template use File | Settings | File Templates.
 */

trait derivativesTrait {
  self:nonLinearFunctionTrait =>

    def _grad(x:DenseMatrix[Double]):DenseMatrix[Double] ={
      var n = 0
      val g = DenseMatrix.zeros[Double](x.numRows,x.numCols)
      val dxx = 1e-6
      while(n < g.numRows)
      {

        val xp = x.copy
        //xp(n,0)=xp(n,0)*(1.0+0.01)
        xp(n,0) += dxx
        if (xp(n,0) == 0)
          xp(n,0) = 1e-6

        val xm = x.copy
        //xm(n,0)=xp(n,0)*(1.0-0.01)
        xm(n,0)-=dxx
        if (xm(n,0) == 0)
          xm(n,0) = -1e-6

        val dx = (xp - xm).toDense(n,0)

        g(n,0)= (self.pFunc(xp)-self.pFunc(xm))/ dx
        n += 1
      }
      g
    }

    def _hessian(x:DenseMatrix[Double]):DenseMatrix[Double]={

      var hasNeg = false;
      var n = 0
      val h = DenseMatrix.zeros[Double](x.numRows,x.numRows)

      val dx = 1e-7
      while(n < h.numRows)
      {

        val xp = x.copy
        val xp2 = x.copy

        xp(n,0) += dx
        xp2(n,0) += 2.0*dx

        val dh = xp2(n,0) - xp(n,0)

        h(n,n)= ( self.pFunc(xp2)-2.0 * self.pFunc(xp) + self.pFunc(x) )/ (dh*dh)
        
        if (h(n,n)<0)
          hasNeg = true;
        n += 1
      }
      if (!hasNeg)
        h
      else
        DenseMatrix.eye[Double](h.numRows,h.numCols)
    }

}