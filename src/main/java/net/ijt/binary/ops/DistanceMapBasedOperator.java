/**
 * 
 */
package net.ijt.binary.ops;

import inra.ijpb.algo.AlgoEvent;
import inra.ijpb.algo.AlgoListener;
import inra.ijpb.algo.AlgoStub;
import inra.ijpb.binary.distmap.ChamferDistanceTransform2DShort;
import inra.ijpb.binary.distmap.ChamferMask2D;
import inra.ijpb.binary.distmap.DistanceTransform;

/**
 * Implementation stub for operators based on a distance transform operator.
 * 
 * This class manages an inner reference to a DistanceTransform operator,
 * provides a default implementation of algorithm listener that propagates
 * events from distance transform to listener of this operator.
 * 
 * @author dlegland
 *
 */
public abstract class DistanceMapBasedOperator extends AlgoStub implements BinaryImageOperator, AlgoListener
{
    protected DistanceTransform distanceTransform;
    
    /**
     * Creates a new operator based on the specified Distance transform
     * operator.
     * 
     * @param distanceTransform
     *            the distanceTransform operator
     */
    protected DistanceMapBasedOperator(DistanceTransform distanceTransform)
    {
        setupDistanceTransform(distanceTransform);
    }
    
    /**
     * Creates a new operator based on a default Distance transform operator.
     */
    protected DistanceMapBasedOperator()
    {
        ChamferMask2D mask = ChamferMask2D.CHESSKNIGHT;
        DistanceTransform algo = new ChamferDistanceTransform2DShort(mask, false);
        setupDistanceTransform(algo);
    }
    
    private void setupDistanceTransform(DistanceTransform algo)
    {
        this.distanceTransform = algo;
        this.distanceTransform.addAlgoListener(this);
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
