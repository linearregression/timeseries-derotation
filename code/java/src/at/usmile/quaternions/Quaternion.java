package at.usmile.quaternions;

/**
 * Representation of a quaternion (see e.g.
 * http://en.wikipedia.org/wiki/Quaternion)
 * 
 * @author Rainhard Findling
 * @date Sep 22, 2014
 * @version 1
 */
public class Quaternion {
	/**
	 * precision for determining if quaternion values are treated as being
	 * equal.
	 */
	private static final double EPSILON = 1E-10;

	public double x;
	public double i;
	public double j;
	public double k;

	@Override
	public String toString() {
		return "Quaternion [x=" + x + ", i=" + i + ", j=" + j + ", k=" + k + "]";
	}

	public Quaternion(double x, double i, double j, double k) {
		this.x = x;
		this.i = i;
		this.j = j;
		this.k = k;
	}

	public Quaternion(double x, double[] ijk) {
		this.x = x;
		this.i = ijk[0];
		this.j = ijk[1];
		this.k = ijk[2];
	}

	public double[] getIJK() {
		return new double[] { i, j, k };
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		long temp;
		temp = Double.doubleToLongBits(i);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		temp = Double.doubleToLongBits(j);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		temp = Double.doubleToLongBits(k);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		temp = Double.doubleToLongBits(x);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Quaternion other = (Quaternion) obj;
		if (Math.abs(x - other.x) > EPSILON)
			return false;
		if (Math.abs(i - other.i) > EPSILON)
			return false;
		if (Math.abs(j - other.j) > EPSILON)
			return false;
		if (Math.abs(k - other.k) > EPSILON)
			return false;
		return true;
	}

}
