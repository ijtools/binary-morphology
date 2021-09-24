/**
 * 
 */
package net.ijt.binary;

import ij.IJ;
import ij.ImagePlus;
import ij.ImageStack;
import ij.gui.GenericDialog;
import ij.plugin.PlugIn;
import inra.ijpb.util.IJUtils;

/**
 * @author dlegland
 *
 */
public class BinaryDilation3D implements PlugIn
{

    @Override
    public void run(String arg)
    {
    	System.out.println("Start Binary Dilation 3D Plugin");
    	
		ImagePlus imagePlus = IJ.getImage();
		
		if (imagePlus.getStackSize() == 1)
		{
			IJ.error("Binary Dilation 3D Error", "Requires 3D image");
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
		ImageStack image = imagePlus.getStack();
		ImageStack dilated = BinaryMorphology.dilationBall(image, radius);
		
		// create ImagePlus
		String newName = imagePlus.getShortTitle() + "-dil";
		ImagePlus dilatedPlus = new ImagePlus(newName, dilated);
		
		// Display with same settings as original image
		dilatedPlus.copyScale(imagePlus);
		dilatedPlus.show();

		// For 2D images, select the same visible slice as original image
		dilatedPlus.setZ(imagePlus.getZ());
		dilatedPlus.setSlice(imagePlus.getCurrentSlice());

		// Display elapsed time
		long t1 = System.currentTimeMillis();
		IJUtils.showElapsedTime("Binary Dilation 3D", t1 - t0, imagePlus);
    }
}
