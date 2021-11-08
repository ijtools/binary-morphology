package net.ijt.binary;

import ij.ImageStack;
import ij.process.ByteProcessor;
import ij.process.ImageProcessor;
import inra.ijpb.binary.BinaryImages;
import inra.ijpb.binary.ChamferWeights;
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
	 * the structuring element will depends on the algorithm used for distance
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
		
		short[] weights = ChamferWeights.CHESSKNIGHT.getShortWeights();
		ImageProcessor distMap = BinaryImages.distanceMap(imageInv, weights, false);
		
		return Relational.LT.process(distMap, (radius + 0.5) * weights[0]);
	}
	
	/**
	 * Computes the erosion of the binary image using as structuring element a
	 * disk with the specified radius.
	 * 
	 * Implementation relies on computation of distance transforms. The shape of
	 * the structuring element will depends on the algorithm used for distance
	 * transform computation, usually chamfer-based.
	 * 
	 * @param image
	 *            the binary image to erode
	 * @param radius
	 *            the radius of the disk structuring element
	 * @return the result of erosion.
	 */
	public static final ByteProcessor erosionDisk(ByteProcessor image, double radius)
	{
		short[] weights = ChamferWeights.CHESSKNIGHT.getShortWeights();
		ImageProcessor distMap = BinaryImages.distanceMap(image, weights, false);
		
		return Relational.GE.process(distMap, (radius + 0.5) * weights[0]);
	}
	

	/**
	 * Computes the dilation of the binary stack using as structuring element a
	 * ball with the specified radius.
	 * 
	 * Implementation relies on computation of distance transforms. The shape of
	 * the structuring element will depends on the algorithm used for distance
	 * transform computation, usually chamfer-based.
	 * 
	 * @param image
	 *            the binary 3D image to dilate
	 * @param radius
	 *            the radius of the ball structuring element
	 * @return the result of dilation.
	 */
	public static final ImageStack dilationBall(ImageStack image, double radius)
	{
		// need to invert
		ImageStack imageInv = image.duplicate();
		Images3D.invert(imageInv);
		
		short[] weights = ChamferWeights3D.WEIGHTS_3_4_5_7.getShortWeights();
		ImageStack distMap = BinaryImages.distanceMap(imageInv, weights, false);
		
		return Relational.LT.process(distMap, (radius + 0.5) * weights[0]);
	}
	
	/**
	 * Computes the erosion of the binary stack using as structuring element a
	 * ball with the specified radius.
	 * 
	 * Implementation relies on computation of distance transforms. The shape of
	 * the structuring element will depends on the algorithm used for distance
	 * transform computation, usually chamfer-based.
	 * 
	 * @param image
	 *            the binary 3D image to erode
	 * @param radius
	 *            the radius of the ball structuring element
	 * @return the result of erosion.
	 */
	public static final ImageStack erosionBall(ImageStack image, double radius)
	{
		short[] weights = ChamferWeights3D.WEIGHTS_3_4_5_7.getShortWeights();
		ImageStack distMap = BinaryImages.distanceMap(image, weights, false);
		
		return Relational.GE.process(distMap, (radius + 0.5) * weights[0]);
	}
	
	/**
	 * Private constructor to prevent instantiation.
	 */
	private BinaryMorphology()
	{
	}
}
