/**
 * 
 */
package net.ijt.binary;

import static org.junit.Assert.*;

import org.junit.Test;

import ij.ImageStack;
import ij.process.ByteProcessor;
import inra.ijpb.data.image.Images3D;

/**
 * @author dlegland
 *
 */
public class BinaryMorphologyTest {

	/**
	 * Test method for {@link net.ijt.binary.BinaryMorphology#dilationDisk(ij.process.ByteProcessor, double)}.
	 */
	@Test
	public final void testDilationDisk() {
		ByteProcessor image = new ByteProcessor(11, 11);
		image.set(5, 5, 255);
		
		ByteProcessor res = BinaryMorphology.dilationDisk(image, 3);
		
		assertEquals(255, res.get(5, 5));
		assertEquals(255, res.get(8, 5));
		assertEquals(255, res.get(2, 5));
		assertEquals(255, res.get(5, 8));
		assertEquals(255, res.get(5, 2));
		assertEquals(  0, res.get(9, 5));
		assertEquals(  0, res.get(1, 5));
		assertEquals(  0, res.get(5, 9));
		assertEquals(  0, res.get(5, 1));
	}

	/**
	 * Test method for {@link net.ijt.binary.BinaryMorphology#erosionDisk(ij.process.ByteProcessor, double)}.
	 */
	@Test
	public final void testErosionDisk() {
		ByteProcessor image = new ByteProcessor(11, 11);
		image.set(5, 5, 255);
		image.invert();
		
		ByteProcessor res = BinaryMorphology.erosionDisk(image, 3);
		
		assertEquals(  0, res.get(5, 5));
		assertEquals(  0, res.get(8, 5));
		assertEquals(  0, res.get(2, 5));
		assertEquals(  0, res.get(5, 8));
		assertEquals(  0, res.get(5, 2));
		assertEquals(255, res.get(9, 5));
		assertEquals(255, res.get(1, 5));
		assertEquals(255, res.get(5, 9));
		assertEquals(255, res.get(5, 1));
	}

	/**
	 * Test method for {@link net.ijt.binary.BinaryMorphology#dilationBall(ij.ImageStack, double)}.
	 */
	@Test
	public final void testDilationBall() {
		ImageStack image = ImageStack.create(11, 11, 11, 8);
		image.setVoxel(5, 5, 5, 255);
		
		ImageStack res = BinaryMorphology.dilationBall(image, 3);
		
		assertEquals(255, res.getVoxel(5, 5, 5), .01);
		assertEquals(255, res.getVoxel(8, 5, 5), .01);
		assertEquals(255, res.getVoxel(2, 5, 5), .01);
		assertEquals(255, res.getVoxel(5, 8, 5), .01);
		assertEquals(255, res.getVoxel(5, 2, 5), .01);
		assertEquals(255, res.getVoxel(5, 5, 8), .01);
		assertEquals(255, res.getVoxel(5, 5, 2), .01);
		assertEquals(  0, res.getVoxel(9, 5, 5), .01);
		assertEquals(  0, res.getVoxel(1, 5, 5), .01);
		assertEquals(  0, res.getVoxel(5, 9, 5), .01);
		assertEquals(  0, res.getVoxel(5, 1, 5), .01);
		assertEquals(  0, res.getVoxel(5, 5, 9), .01);
		assertEquals(  0, res.getVoxel(5, 5, 1), .01);
	}

	/**
	 * Test method for {@link net.ijt.binary.BinaryMorphology#erosionBall(ij.ImageStack, double)}.
	 */
	@Test
	public final void testErosionBall() {
		ImageStack image = ImageStack.create(11, 11, 11, 8);
		image.setVoxel(5, 5, 5, 255);
		Images3D.invert(image);
		
		ImageStack res = BinaryMorphology.erosionBall(image, 3);
		
		assertEquals(  0, res.getVoxel(5, 5, 5), .01);
		assertEquals(  0, res.getVoxel(8, 5, 5), .01);
		assertEquals(  0, res.getVoxel(2, 5, 5), .01);
		assertEquals(  0, res.getVoxel(5, 8, 5), .01);
		assertEquals(  0, res.getVoxel(5, 2, 5), .01);
		assertEquals(  0, res.getVoxel(5, 5, 8), .01);
		assertEquals(  0, res.getVoxel(5, 5, 2), .01);
		assertEquals(255, res.getVoxel(9, 5, 5), .01);
		assertEquals(255, res.getVoxel(1, 5, 5), .01);
		assertEquals(255, res.getVoxel(5, 9, 5), .01);
		assertEquals(255, res.getVoxel(5, 1, 5), .01);
		assertEquals(255, res.getVoxel(5, 5, 9), .01);
		assertEquals(255, res.getVoxel(5, 5, 1), .01);
	}

}
