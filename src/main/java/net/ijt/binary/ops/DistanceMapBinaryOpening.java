/**
 * 
 */
package net.ijt.binary.ops;

import ij.process.ByteProcessor;
import ij.process.ImageProcessor;
import inra.ijpb.algo.AlgoEvent;
import inra.ijpb.algo.AlgoListener;
import inra.ijpb.algo.AlgoStub;
import inra.ijpb.binary.ChamferWeights;
import inra.ijpb.binary.distmap.DistanceTransform;
import inra.ijpb.binary.distmap.DistanceTransform5x5Short;
import net.ijt.binary.Relational;

/**
 * @author dlegland
 *
 */
public class DistanceMapBinaryOpening extends AlgoStub implements ByteProcessorOperator, AlgoListener
{
	double radius;
	
	public DistanceMapBinaryOpening(double radius)
	{
		this.radius = radius;
	}

	@Override
	public ByteProcessor process(ByteProcessor image) 
	{
		// create distance map operator
		short[] weights = ChamferWeights.CHESSKNIGHT.getShortWeights();
		DistanceTransform algo = new DistanceTransform5x5Short(weights, false);
		algo.addAlgoListener(this);
		
		// compute distance map
		this.fireStatusChanged(this, "Compute Distance Map");
		ImageProcessor distMap = algo.distanceMap(image);
		
		// apply threshold on distance map, and invert (using LT instead of GE)
		this.fireStatusChanged(this, "Threshold Distance Map");
		ByteProcessor erodedInv = Relational.LT.process(distMap, (radius + 0.5) * weights[0]);
		
		// compute distance map on eroded image
		this.fireStatusChanged(this, "Compute Distance Map on erosion");
		distMap = algo.distanceMap(erodedInv);
		
		// compute distance map
		this.fireStatusChanged(this, "Threshold distance map");
		return Relational.LT.process(distMap, (radius + 0.5) * weights[0]);
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
