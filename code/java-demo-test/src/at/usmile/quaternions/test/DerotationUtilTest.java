package at.usmile.quaternions.test;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.FileNotFoundException;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import Jama.Matrix;
import at.usmile.derotation.test.DemoUtil;
import at.usmile.timeseries.derotation.DerotationUtil;
import at.usmile.timeseries.derotation.Quaternion;
import at.usmile.tuple.GenericTuple2;

/**
 * Unit tests of {@link DerotationUtil}.
 * 
 * @author Rainhard Findling
 * @date Sep 22, 2014
 * @version 1
 */
public class DerotationUtilTest extends DerotationUtil {

	private static final double EPSILON = 1E-8;
	// ================================================================================================================
	// MEMBERS

	/** 3D timeseries of device 1 */
	private double[][] x = null;
	/** 3D timeseries of device 2 */
	private double[][] y = null;
	/**
	 * rotated 3D timeseries of device one (rotated with an arbitrary rotation).
	 */
	private double[][] xRotated = null;

	/**
	 * 3D timeseries of device 1 centered by columns.
	 */
	private double[][] xCentered = null;

	// ================================================================================================================
	// METHODS

	@Before
	public void before() throws FileNotFoundException {
		x = DemoUtil.loadTimeseries(new File("../../demo_data/X1.csv"), " ", 200);
		xRotated = DemoUtil.loadTimeseries(new File("../../demo_data/X1_rotated.csv"), " ", 200);
		xCentered = DemoUtil.loadTimeseries(new File("../../demo_data/X1_centered.csv"), " ", 200);
		y = DemoUtil.loadTimeseries(new File("../../demo_data/Y1.csv"), " ", 200);
	}

	@Test
	public void testRotQuat1() {
		assertEquals(new Quaternion(0, 0, 0, 0), rotquat(new Quaternion(0, 0, 0, 0), new Quaternion(0, 0, 0, 0)));
	}

	@Test
	public void testRotQuat2() {
		assertEquals(new Quaternion(0, 0, 0, 1), rotquat(new Quaternion(0, 0, 0, 1), new Quaternion(0, 0, 0, 1)));
	}

	@Test
	public void testRotQuat3() {
		assertEquals(new Quaternion(0, 0, 0, 4), rotquat(new Quaternion(0, 0, 0, 1), new Quaternion(0, 0, 0, 2)));
	}

	@Test
	public void testRotQuat4() {
		assertEquals(new Quaternion(0, 0, 0, 2), rotquat(new Quaternion(0, 0, 0, 2), new Quaternion(0, 0, 0, 1)));
	}

	@Test
	public void testRotQuat5() {
		assertEquals(new Quaternion(0, 0, 0, 0), rotquat(new Quaternion(0, 0, 0, 1), new Quaternion(0, 0, 0, 0)));
	}

	@Test
	public void testRotQuat6() {
		assertEquals(new Quaternion(0, 0, 0, 0), rotquat(new Quaternion(0, 0, 0, 0), new Quaternion(0, 0, 0, 1)));
	}

	@Test
	public void testRotQuat7() {
		assertEquals(new Quaternion(126, 396, 342, 432), rotquat(new Quaternion(1, 2, 3, 4), new Quaternion(4, 5, 6, 7)));
	}

	@Test
	public void testRotQuat8() {
		assertEquals(new Quaternion(-126, -524, -422, -88), rotquat(new Quaternion(-1, 2, -3, 4), new Quaternion(4, 5, 6, -7)));
	}

	@Test
	public void testRotQuat9() {
		assertEquals(new Quaternion(-126, 396, -342, 432), rotquat(new Quaternion(-1, 2, -3, 4), new Quaternion(4, -5, 6, -7)));
	}

	@Test
	public void testCenter() {
		assertEquals(x.length, xCentered.length);
		double[][] centered = new Matrix(x).getArrayCopy();
		center(centered);
		for (int i = 0; i < centered.length; i++) {
			assertArrayEquals(xCentered[i], centered[i], EPSILON);
		}
	}

	@Test
	public void testReserDerotatedValue1() {
		assertEquals(x.length, xRotated.length);
		Quaternion u = new Quaternion(0.00000, 0.26726, 0.53452, 0.80178);
		GenericTuple2<Double, double[][]> rotated = reser(x, y, u);
		for (int i = 0; i < rotated.value2.length; i++) {
			assertArrayEquals(xRotated[i], rotated.value2[i], EPSILON);
		}
	}

	@After
	public void after() {
		x = null;
		y = null;
	}

}
