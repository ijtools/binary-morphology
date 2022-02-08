/**
 * 
 */
package net.ijt.binary.ops;

import ij.process.ByteProcessor;

/**
 * Simple definition of an operator that transforms a binary image into another
 * binary image.
 * 
 * @author dlegland
 */
public interface BinaryImageOperator
{
	public ByteProcessor process(ByteProcessor image);
}
