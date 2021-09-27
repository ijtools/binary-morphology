/**
 * 
 */
package net.ijt.binary.ops;

import ij.ImageStack;
import ij.process.ImageProcessor;
import inra.ijpb.algo.AlgoStub;

/**
 * @author dlegland
 *
 */
public class BinaryStackInverter extends AlgoStub
{
	public void processInPlace(ImageStack image)
	{
		int sizeX = image.getWidth();
		int sizeY = image.getHeight();
		int nSlices = image.getSize();
		
		for (int z = 0; z < nSlices; z++)
		{
			this.fireProgressChanged(this, z, nSlices);
			
			ImageProcessor ip = image.getProcessor(z + 1);
			for (int y = 0; y < sizeY; y++)
			{
				for (int x = 0; x < sizeX; x++)
				{
					ip.set(x, y, 255 - ip.get( x, y ));
				}
			}
		}

		this.fireProgressChanged(this, 1, 1);
	}

}
