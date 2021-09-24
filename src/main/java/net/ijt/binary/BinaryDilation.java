/**
 * 
 */
package net.ijt.binary;

import ij.IJ;
import ij.ImagePlus;
import ij.gui.GenericDialog;
import ij.plugin.PlugIn;
import ij.process.ByteProcessor;
import ij.process.ImageProcessor;
import inra.ijpb.util.IJUtils;

/**
 * @author dlegland
 *
 */
public class BinaryDilation implements PlugIn
{

    @Override
    public void run(String arg)
    {
    	System.out.println("Start Binary Dilation Plugin");
    	
		ImagePlus imagePlus = IJ.getImage();
		
		if (imagePlus.getStackSize() > 1)
		{
			IJ.error("Binary Dilation Error", "Requires planar image");
		}
		
		// Display dialog options
		GenericDialog gd = new GenericDialog("Binary Dilation");
		gd.addNumericField("Radius", 3, 1);
		
		// wait for user answer
		gd.showDialog();
		if (gd.wasCanceled()) 
			return;

		// parses dialog options
		double radius = gd.getNextNumber();
		
		long t0 = System.currentTimeMillis();

		// process image
		ImageProcessor image = imagePlus.getProcessor();
		ImageProcessor dilated = BinaryMorphology.dilationDisk((ByteProcessor) image, radius);
		
		// create ImagePlus
		String newName = imagePlus.getShortTitle() + "-dil";
		ImagePlus dilatedPlus = new ImagePlus(newName, dilated);
		
		// Display with same settings as original image
		dilatedPlus.copyScale(imagePlus);
		dilatedPlus.show();

		// Display elapsed time
		long t1 = System.currentTimeMillis();
		IJUtils.showElapsedTime("Binary Dilation 3D", t1 - t0, imagePlus);
    }
}
