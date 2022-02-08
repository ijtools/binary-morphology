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
 * Morphological closing (dilation followed by erosion) for 2D binary images.
 *
 * @see DistanceMapBinaryErosion
 * @see DistanceMapBinaryDilation
 * @see DistanceMapBinaryOpening
 * @see DistanceMapBinaryClosing3D
 * 
 * @author dlegland
 */
public class DistanceMapBinaryClosing extends DistanceMapBasedOperator
{
	double radius;
	
    public DistanceMapBinaryClosing(DistanceTransform distanceTransform, double radius)
    {
        super(distanceTransform);
        this.radius = radius;
    }

    public DistanceMapBinaryClosing(double radius)
    {
        this.radius = radius;
    }

	@Override
	public ByteProcessor processBinary(ByteProcessor image) 
	{
		// compute threshold value for distance maps
        double threshold = (radius + 0.5);
        if (this.distanceTransform instanceof ChamferDistanceTransform2D)
        {
            threshold *= ((ChamferDistanceTransform2D) distanceTransform).mask().getNormalizationWeight();
        }
		
		// need to invert
		fireStatusChanged(this, "Invert image");
		ImageProcessor imageInv = image.duplicate();
		imageInv.invert();
		
		// compute distance map
		fireStatusChanged(this, "Compute Distance Map");
		ImageProcessor distMap = this.distanceTransform.distanceMap(imageInv);
		
		fireStatusChanged(this, "Threshold Distance Map");
		ByteProcessor dilated = Relational.LT.process(distMap, threshold);
		
		// compute distance map on dilated image
		fireStatusChanged(this, "Compute Distance Map on dilated image");
		distMap = this.distanceTransform.distanceMap(dilated);
		
		// Apply threshold on distance map
		fireStatusChanged(this, "Threshold Distance Map");
		return Relational.GE.process(distMap, threshold);
	}
}
