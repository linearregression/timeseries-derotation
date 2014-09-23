package at.usmile.quaternions.test;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.FileNotFoundException;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import at.usmile.derotation.test.DemoUtil;
import at.usmile.quaternions.DerotationUtil;
import at.usmile.quaternions.Quaternion;
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
	 * derotated 3D timeseries of device one (derotated to maximize similarity
	 * to y).
	 */
	private double[][] xDerotated = null;

	// ================================================================================================================
	// METHODS

	@Before
	public void before() throws FileNotFoundException {
		x = DemoUtil.loadTimeseries(new File("../../demo_data/X1.csv"), "\n");
		xRotated = DemoUtil.loadTimeseries(new File("../../demo_data/X1_rotated.csv"), "\n");
		xDerotated = DemoUtil.loadTimeseries(new File("../../demo_data/X1_derotated.csv"), "\n");
		y = DemoUtil.loadTimeseries(new File("../../demo_data/Y1.csv"), "\n");
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
	public void testReserError1() {
		assertEquals(x.length, xRotated.length);
		Quaternion u = new Quaternion(0.00000, 0.26726, 0.53452, 0.80178);
		GenericTuple2<Double, double[][]> rotated = reser(x, y, u);
		assertEquals(547.9352, rotated.value1.doubleValue(), EPSILON);
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

	@Test
	public void testResiduum() {
		GenericTuple2<Double, double[][]> residuum = residuum(x, y);
		for (int i = 0; i < residuum.value2.length; i++) {
			assertArrayEquals(xDerotated[i], residuum.value2[i], EPSILON);
		}
		// TODO check if that is a problem of having chosen different
		// eigenvectors - most probably that's the case....
	}

	@After
	public void after() {
		x = null;
		y = null;
	}

}
