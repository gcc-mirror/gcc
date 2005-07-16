/* Copyright (C) 2004  Free Software Foundation

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

package java.awt.image;

import java.awt.Point;

/**
 * The BufferedImageFilter class wraps BufferedImageOp objects in a Filter.
 * 
 * When pixels are pushed through the filter, we create a BufferedImage,
 * apply the BufferedImageOp, and pass the filtered pixels to the base class.
 * 
 * @author jlquinn@optonline.net
 */
public class BufferedImageFilter extends ImageFilter implements Cloneable
{
  private BufferedImageOp op;

  /**
   * 
   */
  public BufferedImageFilter(BufferedImageOp op)
  {
    super();
    if (op == null)
      throw new NullPointerException("BufferedImageFilter null"
				     + " op in constructor");
    this.op = op;
  }
  
  /**
   * @return Returns the contained BufferedImageOp.
   */
  public BufferedImageOp getBufferedImageOp()
  {
    return op;
  }

  // FIXME: Definitely not sure this is the right thing.  I'm not sure how to
  // create a compatible sample model that incorporates scansize != w.  I
  // asume off is handled by the db itself.
  public void setPixels(int x, int y, int w, int h, ColorModel model,
			byte[] pixels, int off, int scansize)
  {
    // Create an input BufferedImage
    DataBufferByte db = new DataBufferByte(pixels, scansize * h + off, off);
    SampleModel sm = model.createCompatibleSampleModel(scansize, h);
    WritableRaster wr = new WritableRaster(sm, db, new Point(0, 0));
    BufferedImage in =
      new BufferedImage(model, wr, model.isAlphaPremultiplied(), null);
    BufferedImage out = op.createCompatibleDestImage(in, model);
    op.filter(in, out);
    DataBuffer dbout = out.getRaster().getDataBuffer(); 
    super.setPixels(0, 0, w, h, model, ((DataBufferByte)dbout).getData(), 0,
		    scansize);
  }

  // FIXME: Definitely not sure this is the right thing.  I'm not sure how
  // to create a compatible sample model that incorporates
  // scansize != w.  I asume off is handled by the db itself.
  public void setPixels(int x, int y, int w, int h, ColorModel model,
			int[] pixels, int off, int scansize)
  {
    // Create an input BufferedImage
    DataBufferInt db = new DataBufferInt(pixels, scansize * h + off, off);
    SampleModel sm = model.createCompatibleSampleModel(scansize, h);
    WritableRaster wr = new WritableRaster(sm, db, new Point(0, 0));
    BufferedImage in =
      new BufferedImage(model, wr, model.isAlphaPremultiplied(), null);
    BufferedImage out = op.createCompatibleDestImage(in, model);
    op.filter(in, out);
    DataBuffer dbout = out.getRaster().getDataBuffer(); 
    super.setPixels(0, 0, w, h, model, ((DataBufferInt)dbout).getData(), 0,
		    scansize);
  }
}
