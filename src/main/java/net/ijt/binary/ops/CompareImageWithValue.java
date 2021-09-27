/**
 * 
 */
package net.ijt.binary.ops;

import ij.ImageStack;
import ij.process.ByteProcessor;
import ij.process.ImageProcessor;
import inra.ijpb.algo.AlgoStub;

/**
 * @author dlegland
 *
 */
public class CompareImageWithValue extends AlgoStub
{
	public enum Operator
	{
		GE,
		GT,
		LT,
		LE,
		EQ, 
		NE;
	}
	
	public CompareImageWithValue()
	{
	}
	
	public ImageProcessor process(ImageProcessor image, Operator op, double value)
	{
		int sizeX = image.getWidth();
		int sizeY = image.getHeight();
		ByteProcessor res = new ByteProcessor(sizeX, sizeY);

		switch (op)
		{
		case GE:
			for (int y = 0; y < sizeY; y++)
			{
				for (int x = 0; x < sizeX; x++)
				{
					if (image.getf(x, y) >= value)
					{
						res.set(x, y, 255);
					};
				}
			}
			break;

		case GT:
			for (int y = 0; y < sizeY; y++)
			{
				for (int x = 0; x < sizeX; x++)
				{
					if (image.getf(x, y) > value)
					{
						res.set(x, y, 255);
					};
				}
			}
			break;

		case LE:
			for (int y = 0; y < sizeY; y++)
			{
				for (int x = 0; x < sizeX; x++)
				{
					if (image.getf(x, y) <= value)
					{
						res.set(x, y, 255);
					};
				}
			}
			break;

		case LT:
			for (int y = 0; y < sizeY; y++)
			{
				for (int x = 0; x < sizeX; x++)
				{
					if (image.getf(x, y) < value)
					{
						res.set(x, y, 255);
					};
				}
			}
			break;

		case EQ:
			for (int y = 0; y < sizeY; y++)
			{
				for (int x = 0; x < sizeX; x++)
				{
					if (image.getf(x, y) == value)
					{
						res.set(x, y, 255);
					};
				}
			}
			break;

		case NE:
			for (int y = 0; y < sizeY; y++)
			{
				for (int x = 0; x < sizeX; x++)
				{
					if (image.getf(x, y) != value)
					{
						res.set(x, y, 255);
					};
				}
			}
			break;
		}

		return res;
	}

	public void process(ImageProcessor image, Operator op, double value, ByteProcessor target)
	{
		int sizeX = image.getWidth();
		int sizeY = image.getHeight();

		switch (op)
		{
		case GE:
			for (int y = 0; y < sizeY; y++)
			{
				for (int x = 0; x < sizeX; x++)
				{
					target.set(x, y, image.getf(x, y) >= value ? 255 : 0);
				}
			}
			break;

		case GT:
			for (int y = 0; y < sizeY; y++)
			{
				for (int x = 0; x < sizeX; x++)
				{
					target.set(x, y, image.getf(x, y) > value ? 255 : 0);
				}
			}
			break;

		case LE:
			for (int y = 0; y < sizeY; y++)
			{
				for (int x = 0; x < sizeX; x++)
				{
					target.set(x, y, image.getf(x, y) <= value ? 255 : 0);
				}
			}
			break;

		case LT:
			for (int y = 0; y < sizeY; y++)
			{
				for (int x = 0; x < sizeX; x++)
				{
					target.set(x, y, image.getf(x, y) < value ? 255 : 0);
				}
			}
			break;

		case EQ:
			for (int y = 0; y < sizeY; y++)
			{
				for (int x = 0; x < sizeX; x++)
				{
					target.set(x, y, image.getf(x, y) == value ? 255 : 0);
				}
			}
			break;

		case NE:
			for (int y = 0; y < sizeY; y++)
			{
				for (int x = 0; x < sizeX; x++)
				{
					target.set(x, y, image.getf(x, y) != value ? 255 : 0);
				}
			}
			break;
		}
	}

	public ImageStack process(ImageStack image, Operator op, double value)
	{
		int sizeX = image.getWidth();
		int sizeY = image.getHeight();
		int sizeZ = image.getSize();
		ImageStack res = ImageStack.create(sizeX, sizeY, sizeZ, 8);
		
		process(image, op, value, res);
		return res;
	}
	
	public void process(ImageStack image, Operator op, double value, ImageStack target)
	{
		int sizeZ = image.getSize();
		// TODO: check output is byte
		// TODO: check size compatibility
		
		for (int z = 0; z < sizeZ; z++)
		{
			this.fireProgressChanged(this, z, sizeZ);
			ImageProcessor inputSlice = image.getProcessor(z + 1);
			ByteProcessor outputSlice = (ByteProcessor) target.getProcessor(z + 1);
			
			this.process(inputSlice, op, value, outputSlice);
		}

		this.fireProgressChanged(this, 1, 1);
	}
}
