/**
 * 
 */
package net.ijt.binary.ops;

import ij.process.ByteProcessor;
import ij.process.ImageProcessor;
import inra.ijpb.binary.distmap.ChamferDistanceTransform2D;
import inra.ijpb.binary.distmap.DistanceTransform;
import net.ijt.binary.Relational;

/**
 * Morphological erosion for 2D binary images.
 *
 * @see DistanceMapBinaryDilation
 * @see DistanceMapBinaryClosing
 * @see DistanceMapBinaryOpening
 * @see DistanceMapBinaryErosion3D
 * 
 * @author dlegland
 */
public class DistanceMapBinaryErosion extends DistanceMapBasedOperator
{
	double radius;
	
	public DistanceMapBinaryErosion(DistanceTransform distanceTransform, double radius)
	{
		super(distanceTransform);
		this.radius = radius;
	}

	public DistanceMapBinaryErosion(double radius)
	{
		this.radius = radius;
	}

	@Override
	public ByteProcessor process(ByteProcessor image) 
	{
		// compute distance map
		fireStatusChanged(this, "Compute Distance Map");
		ImageProcessor distMap = this.distanceTransform.distanceMap(image);
		
		// Apply threshold on distance map
		fireStatusChanged(this, "Threshold Distance Map");
		double threshold = (radius + 0.5);
		if (this.distanceTransform instanceof ChamferDistanceTransform2D)
		{
			threshold *= ((ChamferDistanceTransform2D) distanceTransform).mask().getNormalizationWeight();
		}
		return Relational.GE.process(distMap, threshold);
	}
}
