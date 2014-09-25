package at.usmile.timeseries.derotation;

import Jama.EigenvalueDecomposition;
import Jama.Matrix;
import at.usmile.functional.GenericTuple2;

/**
 * Core utilities to optimally derotate timeseries. To actually derotate two 3D
 * time series use {@link DerotationUtil#residuum(Object, Object)}.
 * 
 * See TBA for details.
 * 
 * @author Rainhard Findling
 * @date Sep 22, 2014
 * @version 1
 */
public class DerotationUtil {

	public static Quaternion qmul(Quaternion p, Quaternion q) {
		double x = p.x * q.x - (p.i * q.i + p.j * q.j + p.k * q.k);
		double i = p.x * q.i + q.x * p.i + p.j * q.k - p.k * q.j;
		double j = p.x * q.j + q.x * p.j + p.k * q.i - p.i * q.k;
		double k = p.x * q.k + q.x * p.k + p.i * q.j - p.j * q.i;
		return new Quaternion(x, i, j, k);
	}

	public static Quaternion rotquat(Quaternion p, Quaternion u) {
		Quaternion uc = new Quaternion(u.x, -u.i, -u.j, -u.k);
		Quaternion up = DerotationUtil.qmul(u, p);
		Quaternion upuc = DerotationUtil.qmul(up, uc);
		return upuc;
	}

	public static GenericTuple2<Double, double[][]> reser(double[][] vectorX, double[][] vectorY, Quaternion u) {
		double e = 0;
		double[][] p = new double[vectorX.length][3];
		for (int k = 0; k < vectorX.length; k++) {
			Quaternion y = DerotationUtil.rotquat(new Quaternion(0, vectorX[k]), u);
			p[k] = y.getIJK();
			for (int i = 0; i < 3; i++) {
				e += Math.pow(vectorX[k][i] - p[k][i], 2);
			}
		}
		e /= vectorX.length;
		return new GenericTuple2<Double, double[][]>(e, p);
	}

	public static GenericTuple2<Double, double[][]> residuum(double[][] x, double[][] y, boolean center) {
		if (center) {
			for (double a[][] : new double[][][] { x, y }) {
				center(a);
			}
		}
		// obtain 3x3 correlation matrix
		Matrix matrixX = new Matrix(x).transpose();
		Matrix matrixY = new Matrix(y);
		Matrix r = matrixX.times(matrixY);
		// matrix holding all rotations
		Matrix f = new Matrix(new double[][] {
				{ r.get(0, 0) + r.get(1, 1) + r.get(2, 2), r.get(1, 2) - r.get(2, 1), r.get(2, 0) - r.get(0, 2),
						r.get(0, 1) - r.get(1, 0) },
				{ r.get(1, 2) - r.get(2, 1), r.get(0, 0) - r.get(1, 1) - r.get(2, 2), r.get(0, 1) + r.get(1, 0),
						r.get(0, 2) + r.get(2, 0) },
				{ r.get(2, 0) - r.get(0, 2), r.get(0, 1) + r.get(1, 0), -r.get(0, 0) + r.get(1, 1) - r.get(2, 2),
						r.get(1, 2) + r.get(2, 1) },
				{ r.get(0, 1) - r.get(1, 0), r.get(0, 2) + r.get(2, 0), r.get(1, 2) + r.get(2, 1),
						-r.get(0, 0) - r.get(1, 1) + r.get(2, 2) } });
		// compute eigenvector evvmax of largest eigenvalue ev
		EigenvalueDecomposition eig = f.eig();
		Matrix vectors = eig.getV();
		double[] values = eig.getRealEigenvalues();
		double ev = values[0];
		Matrix evvmax = vectors.getMatrix(new int[] { 0, 1, 2, 3 }, 0, 0);
		for (int i = 1; i < 4; i++) {
			if (values[i] > ev) {
				ev = values[i];
				evvmax = vectors.getMatrix(new int[] { 0, 1, 2, 3 }, i, i);
			}
		}
		// compute error and optimal P=UX
		return reser(x, y, new Quaternion(evvmax.get(0, 0), evvmax.get(1, 0), evvmax.get(2, 0), evvmax.get(3, 0)));
	}

	/**
	 * Centers the array column-wise by subtracting the column's mean from the
	 * column.
	 * 
	 * @param x
	 */
	public static void center(double[][] x) {
		Matrix m = new Matrix(x, 0, 0);
		for (int col = 0; col < x[0].length; col++) {
			double mean = 0;
			for (int row = 0; row < x.length; row++) {
				mean += m.get(row, col);
			}
			mean /= x.length;
			for (int row = 0; row < x.length; row++) {
				m.set(row, col, m.get(row, col) - mean);
			}
		}
	}
}
