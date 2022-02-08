/**
 * 
 */
package net.ijt.binary.ops;

import ij.ImageStack;
import inra.ijpb.binary.distmap.ChamferDistanceTransform3D;
import inra.ijpb.binary.distmap.DistanceTransform3D;

/**
 * Morphological erosion for 3D binary images.
 *
 * @see DistanceMapBinaryDilation3D
 * @see DistanceMapBinaryClosing3D
 * @see DistanceMapBinaryOpening3D
 * @see DistanceMapBinaryDilation
 * 
 * @author dlegland
 */
public class DistanceMapBinaryErosion3D extends DistanceMapBasedOperator3D
{
	double radius;
	
    public DistanceMapBinaryErosion3D(DistanceTransform3D distanceTransform, double radius)
    {
        super(distanceTransform);
        this.radius = radius;
    }

	public DistanceMapBinaryErosion3D(double radius)
	{
		this.radius = radius;
	}

    @Override
    public ImageStack processBinary(ImageStack image) 
	{
		// compute distance map
		fireStatusChanged(this, "Compute Distance Map");
		ImageStack distMap = this.distanceTransform.distanceMap(image);
		
        // compute the threshold value
        double threshold = (radius + 0.5);
        if (this.distanceTransform instanceof ChamferDistanceTransform3D)
        {
            threshold *= ((ChamferDistanceTransform3D) distanceTransform).mask().getNormalizationWeight();
        }
        
		// compute comparison using previously allocated array
		fireStatusChanged(this, "Threshold Distance Map");
		CompareImageWithValue comp = new CompareImageWithValue();
		comp.addAlgoListener(this);
		return comp.process(distMap, CompareImageWithValue.Operator.GE, threshold);
	}
}
