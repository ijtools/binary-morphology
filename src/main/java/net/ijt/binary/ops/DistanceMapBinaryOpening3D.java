/**
 * 
 */
package net.ijt.binary.ops;

import ij.ImageStack;
import inra.ijpb.binary.distmap.ChamferDistanceTransform3D;
import inra.ijpb.binary.distmap.DistanceTransform3D;

/**
 * Morphological opening (erosion followed by dilation) for 3D binary images.
 *
 * @see DistanceMapBinaryDilation3D
 * @see DistanceMapBinaryErosion3D
 * @see DistanceMapBinaryClosing3D
 * @see DistanceMapBinaryOpening
 * 
 * @author dlegland
 */
public class DistanceMapBinaryOpening3D extends DistanceMapBasedOperator3D
{
	double radius;
	
    public DistanceMapBinaryOpening3D(DistanceTransform3D distanceTransform, double radius)
    {
        super(distanceTransform);
        this.radius = radius;
    }

	public DistanceMapBinaryOpening3D(double radius)
	{
		this.radius = radius;
	}

//	@Override
	public ImageStack process(ImageStack image) 
	{
        // compute the threshold value
        double threshold = (radius + 0.5);
        if (this.distanceTransform instanceof ChamferDistanceTransform3D)
        {
            threshold *= ((ChamferDistanceTransform3D) distanceTransform).mask().getNormalizationWeight();
        }
		
		// compute distance map
		fireStatusChanged(this, "Compute Distance Map");
		ImageStack distMap = distanceTransform.distanceMap(image);
		
		// apply threshold on distance map, and invert (using LT instead of GE)
		fireStatusChanged(this, "Threshold Distance Map");
		CompareImageWithValue comp = new CompareImageWithValue();
		comp.addAlgoListener(this);
		ImageStack res = comp.process(distMap, CompareImageWithValue.Operator.LT, threshold);
		
		// compute distance map on eroded image
		fireStatusChanged(this, "Compute Distance Map on eroded image");
		distMap = distanceTransform.distanceMap(res);
		
		// compute threshold on distance map
		comp.process(distMap, CompareImageWithValue.Operator.LT, threshold, res);
		return res;
	}
}
