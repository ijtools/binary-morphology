/**
 * 
 */
package net.ijt.binary.ops;

import ij.ImageStack;

/**
 * Simple definition of an operator that transforms a 3D binary image into
 * another 3D binary image.
 * 
 * @see BinaryImageOperator
 * 
 * @author dlegland
 */
public interface BinaryImageOperator3D
{
	public ImageStack processBinary(ImageStack image);
}
