package at.usmile.derotation.test;

import java.io.File;
import java.io.IOException;

import at.usmile.timeseries.derotation.DerotationUtil;
import at.usmile.tuple.GenericTuple2;

/**
 * Demonstration of derotating two 3D acceleration time series.
 * 
 * @author Rainhard Findling
 * @date Sep 22, 2014
 * @version 1
 */
public class Demo {

	public static void main(String[] args) throws IOException {
		// precondition: load demonstration time series
		double[][] x = DemoUtil.loadTimeseries(new File("../../demo_data/X1.csv"), " ", 200);
		double[][] y = DemoUtil.loadTimeseries(new File("../../demo_data/Y1.csv"), " ", 200);

		// derotate time series
		GenericTuple2<Double, double[][]> residuum = DerotationUtil.residuum(x, y, true);

		// store result in csv file
		DemoUtil.writeTimeseries(new File("../../demo_data/X1_final_derotated.csv"), " ", residuum.value2);
	}
}
