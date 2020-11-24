package ConcurrentGaussianElimination.Utils;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Scanner;

public class Matrix {
    private final int rows, cols;
    private ArrayList<ArrayList<Double>> M;
    private static double eps = 1e-20;

    public Matrix(int rows, int cols, Double defValue){
        if (! (rows > 0 && cols > 0))
            throw new IllegalArgumentException("ConcurrentGaussianElimination.Utils.Matrix size must be a positive integer!");
        this.rows = rows;
        this.cols = cols;
        this.M = new ArrayList<>();
        for (int i=0; i < rows; i++) {
            this.M.add(new ArrayList<>());
            for (int j=0; j < cols; j++)
                this.M.get(i).add(defValue);
        }
    }

    public Matrix(ArrayList<ArrayList<Double>> M){
        int sizeOfRow = -1;
        for (int i=0; i < M.size(); i++){
            if (i == 0)
                sizeOfRow = M.get(i).size();
            else if (M.get(i).size() != sizeOfRow)
                throw new IllegalArgumentException("Input array is not a matrix!");
        }
        this.rows = M.size();
        this.cols = M.get(0).size();
        this.M = M;
    }

    public Matrix(String filename) throws FileNotFoundException {
        Scanner scanner = new Scanner(new File(filename));
        String matrixSizeStr = scanner.next();
        int matrixSize = Integer.parseInt(matrixSizeStr);
        this.rows = matrixSize;
        this.cols = matrixSize + 1;
        ArrayList<ArrayList<Double>> matrix = new ArrayList<>();
        for(int row = 0; row < matrixSize; row++){
            ArrayList<Double> matrixRow = new ArrayList<>();
            for(int col = 0; col < matrixSize; col++){
                String elementStr = scanner.next();
                double element = Double.parseDouble(elementStr);
                matrixRow.add(element);
            }
            matrix.add(matrixRow);
        }
        for(int col = 0; col < matrixSize; col++){
            String elementStr = scanner.next();
            double element = Double.parseDouble(elementStr);
            matrix.get(col).add(element);
        }
        this.M = matrix;
    }

    public int getCols() {
        return this.cols;
    }

    public int getRows() {
        return this.rows;
    }

    public Double get(int row, int col){
        if (row >= this.rows || col >= this.cols)
            throw new ArrayIndexOutOfBoundsException("Trying to access element outside the matrix!");
        return this.M.get(row).get(col);
    }

    public boolean isLowerTriangular(){
        for (int i=0; i < this.rows; i++)
            for (int j=0; j < i; j++)
                if (Math.abs(this.get(i, j)) > Matrix.eps)
                    return false;
        return true;
    }

    public synchronized double productionA(int i, int k){
        return this.get(k,i) / this.get(i, i);
    }

    public synchronized double productionB(int i, int j, int k, double m_ki){
        return this.get(i, j) * m_ki;
    }

    public synchronized void productionC(int i, int j, int k, double n_ijk){
        this.M.get(k).set(j,  this.get(k,j) - n_ijk);
//        this.M.get(k).set(j,  (this.get(k,j) - n_ijk) > Matrix.eps ? (this.get(k,j) - n_ijk) : 0.0 );
//        System.out.println(this.get(k,j) - n_ijk);
//        System.out.println((this.get(k,j) - n_ijk) > Matrix.eps);
    }


    public void pivoting(int col){
        int rowToSwapIdx = col;
        for(int row = col + 1; row < this.rows; row++){
            if (this.get(rowToSwapIdx,col) < this.get(row,col))
                rowToSwapIdx = row;
        }
        if(this.M.get(rowToSwapIdx).get(col) == 0)
            throw new ArithmeticException("Singular matrix!");
        if(rowToSwapIdx != col)
            this.swapRows(col, rowToSwapIdx);
    }

    public void swapRows(int row1, int row2){
        ArrayList<Double> rowList1 = this.M.get(row1);
        ArrayList<Double> rowList2 = this.M.get(row2);
        this.M.set(row1, rowList2);
        this.M.set(row2, rowList1);
    }

    public void backwardSubstitution(){         // REQUIRES FIX
        // Check if matrix is lower triangular
        if (!this.isLowerTriangular())
            //throw new IllegalStateException("Trying to perform backward substitution on a non lower triangular matrix!");
            System.out.println("Trying to perform backward substitution on a non lower triangular matrix");
        for(int row = this.rows - 1; row >= 0; row--){
            double diagEl = this.get(row,row);
            for(int i  = row + 1; i < this.cols; i++)
                this.M.get(row).set(i,  this.get(row,i) / diagEl);
            this.M.get(row).set(row, 1.0);

            for(int i = row + 1; i < this.rows; i++){
                double mul = this.M.get(row).get(i);
                this.M.get(row).set(i, 0.0);
                this.M.get(row).set(this.cols - 1, this.get(row, this.cols-1) - mul * this.get(i,this.cols - 1));
            }


        }
    }

    public void toFile(String filename) throws IOException {
        FileWriter writer = new FileWriter(filename);
        writer.write(this.rows + "\n");
        for(int row = 0; row < this.rows; row++) {
            for (int col = 0; col < this.rows - 1; col++)
                writer.write(this.get(row,col) + " ");
            writer.write(this.get(row, this.rows-1) + "\n");
        }

        for(int i = 0; i < this.rows - 1; i++)
            writer.write(this.get(i, this.cols - 1) + " ");
        writer.write(this.get(this.rows-1,this.cols-1).toString());
        writer.close();
    }

    @Override
    public String toString() {
        StringBuilder res = new StringBuilder();
        for(int i=0; i < this.rows; i++){
            res.append("| ");
            for (int j=0; j < this.cols; j++)
                res.append(this.get(i,j) + " ");
            res.append("|\n");
        }
        return res.toString();
    }
}
