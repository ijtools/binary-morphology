/**
 * 
 */
package net.ijt.binary;

import ij.ImageStack;
import ij.process.ByteProcessor;
import ij.process.ImageProcessor;

/**
 * A static collection of relational operators that can be used to apply
 * thresholds on 2D/3D images.
 * 
 * @author dlegland
 *
 */
public interface Relational
{
	public final static Relational EQ = new Relational()
	{
		@Override
		public ImageStack process(ImageStack image,
				double value)
		{
			int width = image.getWidth();
			int height = image.getHeight();
			int depth = image.getSize();
			ImageStack res = ImageStack.create(width, height, depth, 8);
			
			for (int z = 0; z < depth; z++)
			{
				for (int y = 0; y < height; y++)
				{
					for (int x = 0; x < width; x++)
					{
						res.setVoxel(x, y, z, image.getVoxel(x, y, z) == value ? 255 : 0);
					}
				}
			}

			return res;
		}
		
		@Override
		public ByteProcessor process(ImageProcessor image,
				double value)
		{
			int width = image.getWidth();
			int height = image.getHeight();
			ByteProcessor res = new ByteProcessor(width, height);

			for (int y = 0; y < height; y++)
			{
				for (int x = 0; x < width; x++)
				{
					res.set(x, y, image.getf(x, y) == value ? 255 : 0);
				}
			}

			return res;
		}
	};
	
	public final static Relational GE = new Relational()
	{
		@Override
		public ImageStack process(ImageStack image,
				double value)
		{
			int width = image.getWidth();
			int height = image.getHeight();
			int depth = image.getSize();
			ImageStack res = ImageStack.create(width, height, depth, 8);
			
			for (int z = 0; z < depth; z++)
			{
				for (int y = 0; y < height; y++)
				{
					for (int x = 0; x < width; x++)
					{
						res.setVoxel(x, y, z, image.getVoxel(x, y, z) >= value ? 255 : 0);
					}
				}
			}

			return res;
		}
		
		@Override
		public ByteProcessor process(ImageProcessor image,
				double value)
		{
			int width = image.getWidth();
			int height = image.getHeight();
			ByteProcessor res = new ByteProcessor(width, height);

			for (int y = 0; y < height; y++)
			{
				for (int x = 0; x < width; x++)
				{
					res.set(x, y, image.getf(x, y) >= value ? 255 : 0);
				}
			}

			return res;
		}
	};
	
	public final static Relational GT = new Relational()
	{
		@Override
		public ImageStack process(ImageStack image,
				double value)
		{
			int width = image.getWidth();
			int height = image.getHeight();
			int depth = image.getSize();
			ImageStack res = ImageStack.create(width, height, depth, 8);
			
			for (int z = 0; z < depth; z++)
			{
				for (int y = 0; y < height; y++)
				{
					for (int x = 0; x < width; x++)
					{
						res.setVoxel(x, y, z, image.getVoxel(x, y, z) > value ? 255 : 0);
					}
				}
			}

			return res;
		}
		
		@Override
		public ByteProcessor process(ImageProcessor image,
				double value)
		{
			int width = image.getWidth();
			int height = image.getHeight();
			ByteProcessor res = new ByteProcessor(width, height);

			for (int y = 0; y < height; y++)
			{
				for (int x = 0; x < width; x++)
				{
					res.set(x, y, image.getf(x, y) > value ? 255 : 0);
				}
			}

			return res;
		}
	};
	
	public final static Relational LE = new Relational()
	{
		@Override
		public ImageStack process(ImageStack image,
				double value)
		{
			int width = image.getWidth();
			int height = image.getHeight();
			int depth = image.getSize();
			ImageStack res = ImageStack.create(width, height, depth, 8);
			
			for (int z = 0; z < depth; z++)
			{
				for (int y = 0; y < height; y++)
				{
					for (int x = 0; x < width; x++)
					{
						res.setVoxel(x, y, z, image.getVoxel(x, y, z) <= value ? 255 : 0);
					}
				}
			}

			return res;
		}
		
		@Override
		public ByteProcessor process(ImageProcessor image,
				double value)
		{
			int width = image.getWidth();
			int height = image.getHeight();
			ByteProcessor res = new ByteProcessor(width, height);

			for (int y = 0; y < height; y++)
			{
				for (int x = 0; x < width; x++)
				{
					res.set(x, y, image.getf(x, y) <= value ? 255 : 0);
				}
			}

			return res;
		}
	};
	

	public final static Relational LT = new Relational()
	{
		@Override
		public ImageStack process(ImageStack image,
				double value)
		{
			int width = image.getWidth();
			int height = image.getHeight();
			int depth = image.getSize();
			ImageStack res = ImageStack.create(width, height, depth, 8);
			
			for (int z = 0; z < depth; z++)
			{
				for (int y = 0; y < height; y++)
				{
					for (int x = 0; x < width; x++)
					{
						res.setVoxel(x, y, z, image.getVoxel(x, y, z) < value ? 255 : 0);
					}
				}
			}

			return res;
		}
		
		@Override
		public ByteProcessor process(ImageProcessor image,
				double value)
		{
			int width = image.getWidth();
			int height = image.getHeight();
			ByteProcessor res = new ByteProcessor(width, height);

			for (int y = 0; y < height; y++)
			{
				for (int x = 0; x < width; x++)
				{
					res.set(x, y, image.getf(x, y) < value ? 255 : 0);
				}
			}

			return res;
		}
	};
	
	public final static Relational NE = new Relational()
	{
		@Override
		public ImageStack process(ImageStack image,
				double value)
		{
			int width = image.getWidth();
			int height = image.getHeight();
			int depth = image.getSize();
			ImageStack res = ImageStack.create(width, height, depth, 8);
			
			for (int z = 0; z < depth; z++)
			{
				for (int y = 0; y < height; y++)
				{
					for (int x = 0; x < width; x++)
					{
						res.setVoxel(x, y, z, image.getVoxel(x, y, z) != value ? 255 : 0);
					}
				}
			}

			return res;
		}
		
		@Override
		public ByteProcessor process(ImageProcessor image,
				double value)
		{
			int width = image.getWidth();
			int height = image.getHeight();
			ByteProcessor res = new ByteProcessor(width, height);

			for (int y = 0; y < height; y++)
			{
				for (int x = 0; x < width; x++)
				{
					res.set(x, y, image.getf(x, y) != value ? 255 : 0);
				}
			}

			return res;
		}
	};
	
	
	
	/**
     * Applies this relational operator to the 2D input image, using the
     * specified value as threshold.
     * 
     * @param image
     *            the image to process
     * @param value
     *            the value to use as threshold
     * @return a new (binary) image corresponding to the comparison of each
     *         element within the input image
     */
	public ByteProcessor process(ImageProcessor image, double value);

    /**
     * Applies this relational operator to the 3D input image, using the
     * specified value as threshold.
     * 
     * @param image
     *            the 3D image to process
     * @param value
     *            the value to use as threshold
     * @return a new (binary) 3D image corresponding to the comparison of each
     *         element within the input image
     */
	public ImageStack process(ImageStack image, double value);

}
