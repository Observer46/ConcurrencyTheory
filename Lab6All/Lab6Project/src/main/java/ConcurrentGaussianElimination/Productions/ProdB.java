package ConcurrentGaussianElimination.Productions;

import ConcurrentGaussianElimination.Utils.Matrix;

public class ProdB extends AbstractProd {
    private int i, j, k;
    private  double m_ki;

    public ProdB(Matrix M, int i, int j, int k, double m_ki){
        this.M = M;
        this.i = i;
        this.j = j;
        this.k = k;
        this.m_ki = m_ki;
    }

    @Override
    public void run() {
        this.result = this.M.productionB(i, j, k, m_ki);
        System.out.println("B_(" + i + "," + j + "," + k + ")");
    }
}
