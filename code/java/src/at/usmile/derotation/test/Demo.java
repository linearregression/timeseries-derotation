package at.usmile.derotation.test;

import java.io.File;
import java.io.FileNotFoundException;

/**
 * Demonstration of derotating two time series.
 * 
 * @author Rainhard Findling
 * @date Sep 22, 2014
 * @version 1
 */
public class Demo {

	public static void main(String[] args) throws FileNotFoundException {
		// precondition: load demonstration time series (see TBA for details on
		// time series)

		double[][] x = DemoUtil.loadTimeseries(new File("../../demo_data/X1.csv"), "\n");
		double[][] y = DemoUtil.loadTimeseries(new File("../../demo_data/Y1.csv"), "\n");

		// measure RMSE of arbitrarily rotated time series

		// derotate time series

		// measure RMSE of derotated time series

		System.out.println("Done.");
	}

}
