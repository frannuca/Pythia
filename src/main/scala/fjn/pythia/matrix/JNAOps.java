package fjn.pythia.matrix;

import com.sun.jna.Library;

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 4/10/12
 * Time: 6:52 PM
 * To change this template use File | Settings | File Templates.
 */
 public interface JNAOps extends Library {
     void eigenvaluesD(double[] matrix,int dim);
     void invD(double[]  matrix,int dim);
     void svdD(double[]  matrix,int m,int n);
     void mulD(double[]  matrix1,double[]  matrix2,double[] result,int m1,int n1,int n2);
     void CholD(double[]  matrix,int dim,double[] L);

    }


