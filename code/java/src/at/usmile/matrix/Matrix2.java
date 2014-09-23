package at.usmile.matrix;

import java.io.PrintWriter;
import java.io.StringWriter;

import Jama.Matrix;

/**
 * Extends {@link Matrix} by providing a rudimentary {@link #toString()}
 * implementation.
 * 
 * @author Rainhard Findling
 * @date Sep 23, 2014
 * @version 1
 */
public class Matrix2 extends Matrix {
	private static final long serialVersionUID = 1L;

	public Matrix2(double[] _vals, int _m) {
		super(_vals, _m);
	}

	public Matrix2(double[][] A, int _m, int _n) {
		super(A, _m, _n);
	}

	public Matrix2(double[][] A) {
		super(A);
	}

	public Matrix2(int _m, int _n, double _s) {
		super(_m, _n, _s);
	}

	public Matrix2(int _m, int _n) {
		super(_m, _n);
	}

	@Override
	public String toString() {
		return toString(this);
	}

	public static String toString(Matrix _m) {
		StringWriter sw = new StringWriter();
		_m.print(new PrintWriter(sw), 3, 3);
		return sw.toString();
	}
}
