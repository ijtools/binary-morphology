/**
 * 
 */
package net.ijt.binary;

import ij.ImageStack;
import ij.process.ByteProcessor;
import ij.process.ImageProcessor;

/**
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
	
	
	
	
	public ByteProcessor process(ImageProcessor image, double value);

	public ImageStack process(ImageStack image, double value);

}
