package at.usmile.derotation.test;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
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
	 * Load a 3D (3 axes) time series csv file to a 2D array.
	 * 
	 * @param file
	 * @param delimeter
	 * @param _amountOfLines
	 * @return
	 * @throws FileNotFoundException
	 */
	public static double[][] loadTimeseries(File file, String delimeter, int _amountOfLines) throws FileNotFoundException {
		// load values to list
		Scanner scanner = new Scanner(file);
		scanner.useDelimiter("\n");
		List<Double[]> valuesList = new ArrayList<Double[]>();
		while (scanner.hasNext()) {
			String[] tokens = scanner.next().split(delimeter);
			Double[] curVal = new Double[3]; // we expect 3 orthogonal axes
			for (int i = 0; i < curVal.length; i++) {
				curVal[i] = Double.valueOf(tokens[i]);
			}
			valuesList.add(curVal);
		}
		scanner.close();
		// convert to 2D array
		if (_amountOfLines == -1) {
			_amountOfLines = valuesList.size();
		}
		double[][] valuesArray = new double[_amountOfLines][3];
		for (int sampleNr = 0; sampleNr < _amountOfLines; sampleNr++) {
			Double[] curVal = valuesList.get(sampleNr);
			for (int i = 0; i < curVal.length; i++) {
				valuesArray[sampleNr][i] = curVal[i];
			}
		}
		return valuesArray;
	}

	/**
	 * Write 3D (3 axes) time series from 2D array to csv file.
	 * 
	 * @param file
	 * @param delimeter
	 * @param timeseries
	 * @throws IOException
	 */
	public static void writeTimeseries(File file, String delimeter, double[][] timeseries) throws IOException {
		StringBuilder sb = new StringBuilder();
		for (int row = 0; row < timeseries.length; row++) {
			for (int col = 0; col < timeseries[0].length; col++) {
				sb.append(timeseries[row][col]);
				if (col != timeseries[0].length - 1) {
					sb.append(delimeter);
				}
			}
			sb.append("\n");
		}
		file.delete();
		file.createNewFile();
		FileWriter fileWriter = new FileWriter(file);
		BufferedWriter bufferedWriter = new BufferedWriter(fileWriter);
		bufferedWriter.write(sb.toString());
		bufferedWriter.flush();
		fileWriter.close();
	}
}
