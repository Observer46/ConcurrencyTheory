package ConcurrentGaussianElimination.Utils;

import ConcurrentGaussianElimination.Productions.IProduction;

import java.util.ArrayList;

public class ConcurrentScheduler {
    private ArrayList<IProduction> taskQueue;

    public ConcurrentScheduler(){
        this.taskQueue = new ArrayList<>();
    }

    public void addProd(IProduction IProduction){
        this.taskQueue.add(IProduction);
    }

    public void clearQueue(){
        this.taskQueue = new ArrayList<>();
    }

    public void runAll() throws InterruptedException {
        for (IProduction prod : this.taskQueue)
            prod.start();
        for (IProduction prod : this.taskQueue)
            prod.join();
        this.clearQueue();
    }
}
