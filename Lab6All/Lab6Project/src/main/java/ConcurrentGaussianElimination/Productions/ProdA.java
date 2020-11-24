package ConcurrentGaussianElimination.Productions;

import ConcurrentGaussianElimination.Utils.Matrix;

public class ProdA extends AbstractProd{
    private int i,k;

    public ProdA(Matrix M, int i, int k){
        this.M = M;
        this.i = i;
        this.k = k;
    }

    @Override
    public void run() {
        this.result = this.M.productionA(i,k);
        System.out.println("A_(" + i + "," + k + ")");
    }

}
