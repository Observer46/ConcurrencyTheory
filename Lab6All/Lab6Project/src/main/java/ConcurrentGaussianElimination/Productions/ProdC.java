package ConcurrentGaussianElimination.Productions;

import ConcurrentGaussianElimination.Utils.Matrix;

public class ProdC extends AbstractProd{
    private int i, j, k;
    private  double n_ijk;

    public ProdC(Matrix M, int i, int j, int k, double n_ijk){
        this.M = M;
        this.i = i;
        this.j = j;
        this.k = k;
        this.n_ijk = n_ijk;
    }

    @Override
    public void run() {
        this.M.productionC(i, j, k, n_ijk);
        System.out.println("C_(" + i + "," + j + "," + k + ")");
    }
}
