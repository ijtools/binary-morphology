/**
 * 
 */
package net.ijt.binary.ops;

import ij.ImageStack;
import inra.ijpb.algo.AlgoEvent;
import inra.ijpb.algo.AlgoListener;
import inra.ijpb.algo.AlgoStub;
import inra.ijpb.binary.ChamferWeights3D;
import inra.ijpb.binary.distmap.DistanceTransform3D;
import inra.ijpb.binary.distmap.DistanceTransform3D4WeightsShort;

/**
 * @author dlegland
 *
 */
public class DistanceMapBinaryClosing3D extends AlgoStub implements AlgoListener
{
	double radius;
	
	public DistanceMapBinaryClosing3D(double radius)
	{
		this.radius = radius;
	}

//	@Override
	public ImageStack process(ImageStack image) 
	{
		// create distance map operator
		short[] weights = ChamferWeights3D.WEIGHTS_3_4_5_7.getShortWeights();
		DistanceTransform3D algo = new DistanceTransform3D4WeightsShort(weights, false);
		algo.addAlgoListener(this);
		
		// need to invert
		this.fireStatusChanged(this, "Invert image");
		BinaryStackInverter inverter = new BinaryStackInverter();
		inverter.addAlgoListener(this);
		ImageStack imageInv = image.duplicate();
		inverter.processInPlace(imageInv);
		
		// compute distance map
		this.fireStatusChanged(this, "Compute Distance Map");
		ImageStack distMap = algo.distanceMap(imageInv);
		
		// threshold the distance map		
		this.fireStatusChanged(this, "Threshold Distance Map");
		CompareImageWithValue comp = new CompareImageWithValue();
		comp.addAlgoListener(this);
		
		// compute comparison using previously allocated array
		comp.process(distMap, CompareImageWithValue.Operator.LT, (radius + 0.5) * weights[0], imageInv);
		
		// compute distance map on dilated image
		this.fireStatusChanged(this, "Compute Distance Map on dilated image");
		distMap = algo.distanceMap(imageInv);
		
		comp.process(distMap, CompareImageWithValue.Operator.GE, (radius + 0.5) * weights[0], imageInv);
		return imageInv;
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
