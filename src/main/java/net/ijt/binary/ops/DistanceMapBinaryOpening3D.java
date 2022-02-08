/**
 * 
 */
package net.ijt.binary.ops;

import ij.ImageStack;
import inra.ijpb.algo.AlgoEvent;
import inra.ijpb.algo.AlgoListener;
import inra.ijpb.algo.AlgoStub;
import inra.ijpb.binary.distmap.ChamferDistanceTransform3DShort;
import inra.ijpb.binary.distmap.ChamferMask3D;
import inra.ijpb.binary.distmap.ChamferMasks3D;
import inra.ijpb.binary.distmap.DistanceTransform3D;

/**
 * @author dlegland
 *
 */
public class DistanceMapBinaryOpening3D extends AlgoStub implements AlgoListener
{
	double radius;
	
	public DistanceMapBinaryOpening3D(double radius)
	{
		this.radius = radius;
	}

//	@Override
	public ImageStack process(ImageStack image) 
	{
		// create distance map operator
		ChamferMask3D mask = ChamferMasks3D.WEIGHTS_10_14_17_22_34_30.getMask();
		DistanceTransform3D algo = new ChamferDistanceTransform3DShort(mask, false);
		algo.addAlgoListener(this);
		double threshold = (radius + 0.5) * mask.getNormalizationWeight(); 
		
		// compute distance map
		this.fireStatusChanged(this, "Compute Distance Map");
		ImageStack distMap = algo.distanceMap(image);
		
		// apply threshold on distance map, and invert (using LT instead of GE)
		this.fireStatusChanged(this, "Threshold Distance Map");
		CompareImageWithValue comp = new CompareImageWithValue();
		comp.addAlgoListener(this);
		ImageStack res = comp.process(distMap, CompareImageWithValue.Operator.LT, threshold);
		
		// compute distance map on eroded image
		this.fireStatusChanged(this, "Compute Distance Map on eroded image");
		distMap = algo.distanceMap(res);
		
		// compute threshold on distance map
		comp.process(distMap, CompareImageWithValue.Operator.LT, threshold, res);
		return res;
	}

	@Override
	public void algoProgressChanged(AlgoEvent evt) 
	{
		this.fireProgressChanged(evt);
	}

	@Override
	public void algoStatusChanged(AlgoEvent evt)
	{
		this.fireStatusChanged(evt);
	}
	
}
