package net.ijt.binary;

import ij.ImageStack;
import ij.process.ByteProcessor;
import ij.process.ImageProcessor;
import inra.ijpb.binary.BinaryImages;
import inra.ijpb.binary.ChamferWeights3D;
import inra.ijpb.data.image.Images3D;

/**
 * Binary morphological filters.
 * 
 * @author dlegland
 *
 */
public class BinaryMorphology
{
	/**
	 * Computes the dilation of the binary image using as structuring element a
	 * disk with the specified radius.
	 * 
	 * Implementation relies on computation of distance transforms. The shape of
	 * the structuring element will depends on the algorithm used for disrtance
	 * transform computation, usually chamfer-based.
	 * 
	 * @param image
	 *            the binary image to dilate
	 * @param radius
	 *            the radius of the disk structuring element
	 * @return the result of dilation.
	 */
	public static final ByteProcessor dilationDisk(ByteProcessor image, double radius)
	{
		// need to invert
		ImageProcessor imageInv = image.duplicate();
		imageInv.invert();
		
		ImageProcessor distMap = BinaryImages.distanceMap(imageInv, new short[]{5, 7, 11}, true);
		
		ByteProcessor res = threshold(distMap, radius);
		res.invert();
		
		return res;
	}
	
	private static final ByteProcessor threshold(ImageProcessor image, double threshold)
	{
		int width = image.getWidth();
		int height = image.getHeight();
		ByteProcessor res = new ByteProcessor(width, height);
		
		for (int y = 0; y < height; y++)
		{
			for (int x = 0; x < width; x++)
			{
				if (image.getf(x, y) > threshold)
				{
					res.set(x, y, 255);
				}
			}
		}
		
		return res;
	}

	/**
	 * Computes the dilation of the binary image using as structuring element a
	 * disk with the specified radius.
	 * 
	 * Implementation relies on computation of distance transforms. The shape of
	 * the structuring element will depends on the algorithm used for disrtance
	 * transform computation, usually chamfer-based.
	 * 
	 * @param image
	 *            the binary image to dilate
	 * @param radius
	 *            the radius of the disk structuring element
	 * @return the result of dilation.
	 */
	public static final ImageStack dilationBall(ImageStack image, double radius)
	{
		// need to invert
		ImageStack imageInv = image.duplicate();
		Images3D.invert(imageInv);
		
		ImageStack distMap = BinaryImages.distanceMap(imageInv, ChamferWeights3D.WEIGHTS_3_4_5_7.getShortWeights(), true);
		
		ImageStack res = threshold(distMap, radius);
		Images3D.invert(res);
		
		return res;
	}
	
	private static final ImageStack threshold(ImageStack image, double threshold)
	{
		int width = image.getWidth();
		int height = image.getHeight();
		int depth = image.getSize();
		ImageStack res = ImageStack.create(width, height, depth, 8);
		
		for (int z = 0; z < depth; z++)
		{
			for (int y = 0; y < height; y++)
			{
				for (int x = 0; x < width; x++)
				{
					if (image.getVoxel(x, y, z) > threshold)
					{
						res.setVoxel(x, y, z, 255);
					}
				}
			}
		}

		return res;
	}

}
