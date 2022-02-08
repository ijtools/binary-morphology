/**
 * 
 */
package net.ijt.binary.ops;

import ij.process.ByteProcessor;
import ij.process.ImageProcessor;
import inra.ijpb.algo.AlgoEvent;
import inra.ijpb.algo.AlgoListener;
import inra.ijpb.algo.AlgoStub;
import inra.ijpb.binary.distmap.ChamferDistanceTransform2DShort;
import inra.ijpb.binary.distmap.ChamferMask2D;
import inra.ijpb.binary.distmap.DistanceTransform;
import net.ijt.binary.Relational;

/**
 * @author dlegland
 *
 */
public class DistanceMapBinaryErosion extends AlgoStub implements ByteProcessorOperator, AlgoListener
{
	double radius;
	
	public DistanceMapBinaryErosion(double radius)
	{
		this.radius = radius;
	}

	@Override
	public ByteProcessor process(ByteProcessor image) 
	{
		// create distance map operator
//		short[] weights = ChamferWeights.CHESSKNIGHT.getShortWeights();
//		DistanceTransform algo = new DistanceTransform5x5Short(weights, false);
		ChamferMask2D mask = ChamferMask2D.CHESSKNIGHT;
		DistanceTransform algo = new ChamferDistanceTransform2DShort(mask, false);
		algo.addAlgoListener(this);
		
		// compute distance map
		this.fireStatusChanged(this, "Compute Distance Map");
		ImageProcessor distMap = algo.distanceMap(image);
		
		// Apply threshold on distance map
		this.fireStatusChanged(this, "Threshold Distance Map");
		double threshold = (radius + 0.5) * mask.getNormalizationWeight(); 
		return Relational.GE.process(distMap, threshold);
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
