package ConcurrentGaussianElimination.Utils;

import ConcurrentGaussianElimination.Productions.ProdA;
import ConcurrentGaussianElimination.Productions.ProdB;
import ConcurrentGaussianElimination.Productions.ProdC;

import java.util.ArrayList;

public class Executor {
    private ConcurrentScheduler scheduler;
    private Matrix M;

    public Executor(Matrix M){
        this.scheduler = new ConcurrentScheduler();
        this.M = M;
    }

    public ArrayList<ProdA> scheduleClassA(int i){      // FA_i
        ArrayList<ProdA> prodsA = new ArrayList<>();
        for(int j = i + 1; j < M.getRows(); j++){
            ProdA prodA = new ProdA(this.M, i,j);
            scheduler.addProd(prodA);
            prodsA.add(prodA);
        }
        return prodsA;
    }

    public ArrayList<ProdB> scheduleClassB(int i, ArrayList<ProdA> prodsA){      // FB_i
        ArrayList<ProdB> prodsB = new ArrayList<>();
        int idx = 0;
        for(int k = i + 1; k < M.getRows(); k++) {
            double m_ki = (double) prodsA.get(idx).getResult();
            for(int j = i ; j < M.getCols(); j++){
                ProdB prodB = new ProdB(this.M, i, j, k, m_ki);
                scheduler.addProd(prodB);
                prodsB.add(prodB);
            }
            idx++;
        }
        return prodsB;
    }

    public void scheduleClassC(int i, ArrayList<ProdB> prodsB){      // FC_i
        int idx = 0;
        for(int k = i + 1; k < M.getRows(); k++){
            for(int j = i ; j < M.getCols(); j++) {
                double n_ijk = (double) prodsB.get(idx).getResult();
                ProdC prodC = new ProdC(this.M, i, j, k, n_ijk);
                scheduler.addProd(prodC);
                idx++;
            }
        }
    }

    public void runTasks() throws InterruptedException {
        this.scheduler.runAll();
    }

    public void executeConcurrentGaussElimination() throws InterruptedException {
        for (int i = 0; i < this.M.getRows() - 1; i++){
            this.M.pivoting(i);
            ArrayList<ProdA> prodsA = this.scheduleClassA(i);
            this.runTasks();
            ArrayList<ProdB> prodsB = this.scheduleClassB(i, prodsA);
            this.runTasks();
            this.scheduleClassC(i, prodsB);
            this.runTasks();
            System.out.println(this.M);
        }
        this.M.backwardSubstitution();
    }
}
