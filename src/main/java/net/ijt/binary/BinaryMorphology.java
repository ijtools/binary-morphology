package net.ijt.binary;

import ij.ImageStack;
import ij.process.ByteProcessor;
import ij.process.ImageProcessor;
import inra.ijpb.binary.BinaryImages;
import inra.ijpb.binary.distmap.ChamferMask2D;
import inra.ijpb.binary.distmap.ChamferMask3D;
import inra.ijpb.binary.distmap.ChamferMasks3D;
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
		
		ChamferMask2D mask = ChamferMask2D.CHESSKNIGHT; 
		ImageProcessor distMap = BinaryImages.distanceMap(imageInv, mask, false, false);
		
		double threshold = (radius + 0.5) * mask.getNormalizationWeight();
		return Relational.LT.process(distMap, threshold);
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
		ChamferMask2D mask = ChamferMask2D.CHESSKNIGHT; 
		ImageProcessor distMap = BinaryImages.distanceMap(image, mask, false, false);
		
		double threshold = (radius + 0.5) * mask.getNormalizationWeight();
		return Relational.GE.process(distMap, threshold);
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
		
		ChamferMask3D mask = ChamferMasks3D.WEIGHTS_10_14_17_22_34_30.getMask();
		ImageStack distMap = BinaryImages.distanceMap(imageInv, mask, false, false);
		
		double threshold = (radius + 0.5) * mask.getNormalizationWeight();
		return Relational.LT.process(distMap, threshold);
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
		ChamferMask3D mask = ChamferMasks3D.WEIGHTS_10_14_17_22_34_30.getMask();
		ImageStack distMap = BinaryImages.distanceMap(image, mask, false, false);
		
		double threshold = (radius + 0.5) * mask.getNormalizationWeight();
		return Relational.GE.process(distMap, threshold);
	}
	
	/**
	 * Private constructor to prevent instantiation.
	 */
	private BinaryMorphology()
	{
	}
}
