package at.usmile.derotation.test;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * Collects helper methods for derotation demonstrations.
 * 
 * @author Rainhard Findling
 * @date Sep 22, 2014
 * @version 1
 */
public class DemoUtil {

	/**
	 * Load a 3D (3 axes) time series file to a 2D array.
	 * 
	 * @param file
	 * @param delimeter
	 * @return
	 * @throws FileNotFoundException
	 */
	public static double[][] loadTimeseries(File file, String delimeter) throws FileNotFoundException {
		// load values to list
		Scanner scanner = new Scanner(file);
		scanner.useDelimiter(delimeter);
		List<Double[]> valuesList = new ArrayList<Double[]>();
		while (scanner.hasNext()) {
			String[] tokens = scanner.next().split(" ");
			Double[] curVal = new Double[3]; // we expect 3 orthogonal axes
			for (int i = 0; i < curVal.length; i++) {
				curVal[i] = Double.valueOf(tokens[i]);
			}
			valuesList.add(curVal);
		}
		scanner.close();
		// convert to 2D array
		double[][] valuesArray = new double[valuesList.size()][3];
		for (int sampleNr = 0; sampleNr < valuesArray.length; sampleNr++) {
			Double[] curVal = valuesList.get(sampleNr);
			for (int i = 0; i < curVal.length; i++) {
				valuesArray[sampleNr][i] = curVal[i];
			}
		}
		return valuesArray;
	}

}
