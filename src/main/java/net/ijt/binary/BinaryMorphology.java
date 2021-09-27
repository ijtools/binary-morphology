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
		
		ImageProcessor distMap = BinaryImages.distanceMap(imageInv, new short[]{5, 7, 11}, true);
		
		return Relational.LT.process(distMap, radius + 0.5);
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
	 *            the binary image to dilate
	 * @param radius
	 *            the radius of the disk structuring element
	 * @return the result of dilation.
	 */
	public static final ByteProcessor erosionDisk(ByteProcessor image, double radius)
	{
		ImageProcessor distMap = BinaryImages.distanceMap(image, new short[]{5, 7, 11}, true);
		
		return Relational.GE.process(distMap, radius + 0.5);
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
	 *            the binary image to dilate
	 * @param radius
	 *            the radius of the ball structuring element
	 * @return the result of dilation.
	 */
	public static final ImageStack dilationBall(ImageStack image, double radius)
	{
		// need to invert
		ImageStack imageInv = image.duplicate();
		Images3D.invert(imageInv);
		
		ImageStack distMap = BinaryImages.distanceMap(imageInv, ChamferWeights3D.WEIGHTS_3_4_5_7.getShortWeights(), true);
		
		return Relational.LT.process(distMap, radius + 0.5);
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
	 *            the binary image to dilate
	 * @param radius
	 *            the radius of the ball structuring element
	 * @return the result of dilation.
	 */
	public static final ImageStack erosionBall(ImageStack image, double radius)
	{
		ImageStack distMap = BinaryImages.distanceMap(image, ChamferWeights3D.WEIGHTS_3_4_5_7.getShortWeights(), true);
		
		return Relational.GE.process(distMap, radius + 0.5);
	}
}
