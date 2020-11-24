package ConcurrentGaussianElimination.Productions;

import ConcurrentGaussianElimination.Utils.Matrix;

public class AbstractProd extends Thread implements IProduction {
    protected Matrix M;
    protected Object result = null;

    @Override
    public Object getResult() {
        return this.result;
    }
}
