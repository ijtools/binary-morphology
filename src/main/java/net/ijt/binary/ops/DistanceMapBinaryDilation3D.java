/**
 * 
 */
package net.ijt.binary.ops;

import ij.ImageStack;
import inra.ijpb.binary.distmap.ChamferDistanceTransform3D;
import inra.ijpb.binary.distmap.DistanceTransform3D;

/**
 * Morphological dilation for 3D binary images.
 *
 * @see DistanceMapBinaryErosion3D
 * @see DistanceMapBinaryClosing3D
 * @see DistanceMapBinaryOpening3D
 * @see DistanceMapBinaryDilation
 * 
 * @author dlegland
 */
public class DistanceMapBinaryDilation3D extends DistanceMapBasedOperator3D
{
	double radius;
	
    public DistanceMapBinaryDilation3D(DistanceTransform3D distanceTransform, double radius)
    {
        super(distanceTransform);
        this.radius = radius;
    }

	public DistanceMapBinaryDilation3D(double radius)
	{
		this.radius = radius;
	}

    @Override
    public ImageStack processBinary(ImageStack image) 
	{
		// need to invert
		fireStatusChanged(this, "Invert image");
		BinaryStackInverter inverter = new BinaryStackInverter();
		inverter.addAlgoListener(this);
		ImageStack imageInv = image.duplicate();
		inverter.processInPlace(imageInv);
		
		// compute distance map
		fireStatusChanged(this, "Compute Distance Map");
		ImageStack distMap = distanceTransform.distanceMap(imageInv);
		
		// threshold the distance map		
		fireStatusChanged(this, "Threshold Distance Map");
		CompareImageWithValue comp = new CompareImageWithValue();
		comp.addAlgoListener(this);
		
        // compute the threshold value
        double threshold = (radius + 0.5);
        if (this.distanceTransform instanceof ChamferDistanceTransform3D)
        {
            threshold *= ((ChamferDistanceTransform3D) distanceTransform).mask().getNormalizationWeight();
        }
        
		// compute comparison using previously allocated array
		comp.process(distMap, CompareImageWithValue.Operator.LT, threshold, imageInv);
		return imageInv;
	}
}
