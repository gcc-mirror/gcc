/* AreaAveragingScaleFilter.java -- Java class for filtering images
   Copyright (C) 1999 Free Software Foundation, Inc.

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

/**
 * This filter should produce images which do not have image artifacts
 * like broken lines which were originally unbroken.  The cost is of
 * course speed.  Using bi-linear interpolation here against 4 pixel
 * points should give the desired results although Sun does not 
 * specify what the exact algorithm should be.
 * <br>
 * Currently this filter does nothing and needs to be implemented.
 *
 * @author C. Brian Jones (cbj@gnu.org) 
 */
public class AreaAveragingScaleFilter extends ReplicateScaleFilter
{
    /**
     * Construct an instance of <code>AreaAveragingScaleFilter</code> which
     * should be used in conjunction with a <code>FilteredImageSource</code>
     * object.
     * 
     * @param width the width of the destination image
     * @param height the height of the destination image
     */
    public AreaAveragingScaleFilter(int width, int height) {
	super(width, height);
    }

    /**
     * The <code>ImageProducer</code> should call this method with a
     * bit mask of hints from any of <code>RANDOMPIXELORDER</code>,
     * <code>TOPDOWNLEFTRIGHT</code>, <code>COMPLETESCANLINES</code>,
     * <code>SINGLEPASS</code>, <code>SINGLEFRAME</code> from the 
     * <code>ImageConsumer</code> interface.
     * <br>
     * FIXME - more than likely Sun's implementation desires 
     * <code>TOPDOWNLEFTRIGHT</code> order and this method is overloaded here
     * in order to assure that mask is part of the hints added to
     * the consumer.
     * 
     * @param flags a bit mask of hints
     * @see ImageConsumer
     */
    public void setHints(int flags)
    {
      if (consumer != null)
	consumer.setHints(flags);
    }

    /**
     * This function delivers a rectangle of pixels where any
     * pixel(m,n) is stored in the array as a <code>byte</code> at
     * index (n * scansize + m + offset).  
     *
     * @param x the x coordinate of the rectangle
     * @param y the y coordinate of the rectangle
     * @param w the width of the rectangle
     * @param h the height of the rectangle
     * @param model the <code>ColorModel</code> used to translate the pixels
     * @param pixels the array of pixel values
     * @param offset the index of the first pixels in the <code>pixels</code> array
     * @param scansize the width to use in extracting pixels from the <code>pixels</code> array
     */
    public void setPixels(int x, int y, int w, int h, 
	   ColorModel model, byte[] pixels, int offset, int scansize)
    {
      if (consumer != null)
	consumer.setPixels(x, y, w, h, model, pixels, offset, scansize);
    }

    /**
     * This function delivers a rectangle of pixels where any
     * pixel(m,n) is stored in the array as an <code>int</code> at
     * index (n * scansize + m + offset).  
     *
     * @param x the x coordinate of the rectangle
     * @param y the y coordinate of the rectangle
     * @param w the width of the rectangle
     * @param h the height of the rectangle
     * @param model the <code>ColorModel</code> used to translate the pixels
     * @param pixels the array of pixel values
     * @param offset the index of the first pixels in the <code>pixels</code> array
     * @param scansize the width to use in extracting pixels from the <code>pixels</code> array
     */
    public void setPixels(int x, int y, int w, int h, 
           ColorModel model, int[] pixels, int offset, int scansize)
    {
      if (consumer != null)
	consumer.setPixels(x, y, w, h, model, pixels, offset, scansize);
    }

}

