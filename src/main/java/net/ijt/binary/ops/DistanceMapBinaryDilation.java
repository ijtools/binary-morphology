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
 * Morphological dilation for 2D binary images.
 *
 * @see DistanceMapBinaryErosion
 * @see DistanceMapBinaryClosing
 * @see DistanceMapBinaryOpening
 * @see DistanceMapBinaryDilation3D
 * 
 * @author dlegland
 */
public class DistanceMapBinaryDilation extends DistanceMapBasedOperator
{
	double radius;
	
    public DistanceMapBinaryDilation(DistanceTransform distanceTransform, double radius)
    {
        super(distanceTransform);
        this.radius = radius;
    }

	public DistanceMapBinaryDilation(double radius)
	{
		this.radius = radius;
	}

	@Override
	public ByteProcessor process(ByteProcessor image) 
	{
		// need to invert
		fireStatusChanged(this, "Invert image");
		ImageProcessor imageInv = image.duplicate();
		imageInv.invert();
		
		// compute distance map
		fireStatusChanged(this, "Compute Distance Map");
		ImageProcessor distMap = this.distanceTransform.distanceMap(imageInv);
		
		// Apply threshold on distance map
		fireStatusChanged(this, "Threshold Distance Map");
		double threshold = (radius + 0.5);
		if (this.distanceTransform instanceof ChamferDistanceTransform2D)
		{
		    threshold *= ((ChamferDistanceTransform2D) distanceTransform).mask().getNormalizationWeight();
		}
		return Relational.LT.process(distMap, threshold);
	}
}
