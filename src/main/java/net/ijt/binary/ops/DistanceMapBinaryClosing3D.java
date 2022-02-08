/**
 * 
 */
package net.ijt.binary.ops;

import ij.ImageStack;
import inra.ijpb.binary.distmap.ChamferDistanceTransform3D;
import inra.ijpb.binary.distmap.DistanceTransform3D;

/**
 * Morphological closing (dilation followed by erosion) for 3D binary images.
 *
 * @see DistanceMapBinaryDilation3D
 * @see DistanceMapBinaryErosion3D
 * @see DistanceMapBinaryOpening3D
 * @see DistanceMapBinaryClosing
 * 
 * @author dlegland
 */
public class DistanceMapBinaryClosing3D extends DistanceMapBasedOperator3D
{
	double radius;
	
    public DistanceMapBinaryClosing3D(DistanceTransform3D distanceTransform, double radius)
    {
        super(distanceTransform);
        this.radius = radius;
    }

	public DistanceMapBinaryClosing3D(double radius)
	{
		this.radius = radius;
	}

	@Override
	public ImageStack processBinary(ImageStack image) 
	{
        // compute the threshold value
        double threshold = (radius + 0.5);
        if (this.distanceTransform instanceof ChamferDistanceTransform3D)
        {
            threshold *= ((ChamferDistanceTransform3D) distanceTransform).mask().getNormalizationWeight();
        }
		
		// need to invert
		fireStatusChanged(this, "Invert image");
		BinaryStackInverter inverter = new BinaryStackInverter();
		inverter.addAlgoListener(this);
		ImageStack imageInv = image.duplicate();
		inverter.processInPlace(imageInv);
		
		// compute distance map
		fireStatusChanged(this, "Compute Distance Map");
		ImageStack distMap = this.distanceTransform.distanceMap(imageInv);
		
		// threshold the distance map		
		fireStatusChanged(this, "Threshold Distance Map");
		CompareImageWithValue comp = new CompareImageWithValue();
		comp.addAlgoListener(this);
		
		// compute comparison using previously allocated array
		comp.process(distMap, CompareImageWithValue.Operator.LT, threshold, imageInv);
		
		// compute distance map on dilated image
		fireStatusChanged(this, "Compute Distance Map on dilated image");
		distMap = this.distanceTransform.distanceMap(imageInv);
		
		comp.process(distMap, CompareImageWithValue.Operator.GE, threshold, imageInv);
		return imageInv;
	}
}
