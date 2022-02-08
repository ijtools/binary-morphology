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
public class DistanceMapBinaryClosing extends AlgoStub implements ByteProcessorOperator, AlgoListener
{
	double radius;
	
	public DistanceMapBinaryClosing(double radius)
	{
		this.radius = radius;
	}

	@Override
	public ByteProcessor process(ByteProcessor image) 
	{
		// create distance map operator
		ChamferMask2D mask = ChamferMask2D.CHESSKNIGHT;
		DistanceTransform algo = new ChamferDistanceTransform2DShort(mask, false);
		algo.addAlgoListener(this);
		double threshold = (radius + 0.5) * mask.getNormalizationWeight(); 
		
		// need to invert
		this.fireStatusChanged(this, "Invert image");
		ImageProcessor imageInv = image.duplicate();
		imageInv.invert();
		
		// compute distance map
		this.fireStatusChanged(this, "Compute Distance Map");
		ImageProcessor distMap = algo.distanceMap(imageInv);
		
		this.fireStatusChanged(this, "Threshold Distance Map");
		ByteProcessor dilated = Relational.LT.process(distMap, threshold);
		
		// compute distance map on dilated image
		this.fireStatusChanged(this, "Compute Distance Map on dilated image");
		distMap = algo.distanceMap(dilated);
		
		// Apply threshold on distance map
		this.fireStatusChanged(this, "Threshold Distance Map");
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
