package ConcurrentGaussianElimination;

import ConcurrentGaussianElimination.Utils.Executor;
import ConcurrentGaussianElimination.Utils.Matrix;

import java.io.IOException;

public class Main {
    public static void main(String[] args) throws InterruptedException, IOException {
        Matrix mat = new Matrix("in2.txt");
        System.out.println(mat);
        Executor executor = new Executor(mat);
        executor.executeConcurrentGaussElimination();
        mat.toFile("out2mine.txt");
        System.out.println(mat);
    }
}
