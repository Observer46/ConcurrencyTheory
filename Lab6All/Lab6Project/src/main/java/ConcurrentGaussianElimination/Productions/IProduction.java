package ConcurrentGaussianElimination.Productions;

public interface IProduction {
    Object getResult();
    void start();
    void join() throws InterruptedException;
}
