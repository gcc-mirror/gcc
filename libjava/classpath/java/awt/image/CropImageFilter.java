/* CropImageFilter.java -- Java class for cropping image filter
   Copyright (C) 1999, 2004  Free Software Foundation, Inc.

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

import java.awt.Rectangle;
import java.util.Hashtable;

/**
 * Currently this filter does almost nothing and needs to be implemented.
 *
 * @author C. Brian Jones (cbj@gnu.org) 
 */
public class CropImageFilter extends ImageFilter
{
    int x;
    int y;
    int width;
    int height;

    /**
     * Construct a new <code>CropImageFilter</code> instance.
     *
     * @param x the x-coordinate location of the top-left of the cropped rectangle
     * @param y the y-coordinate location of the top-left of the cropped rectangle
     * @param width the width of the cropped rectangle
     * @param height the height of the cropped rectangle
     */
    public CropImageFilter(int x, int y, int width, int height) {
	this.x = x;
	this.y = y;
	this.width = width;
	this.height = height;
    }

    /**
     * An <code>ImageProducer</code> indicates the size of the image
     * being produced using this method.  This filter overrides this
     * method in order to set the dimentions to the size of the
     * cropped rectangle instead of the size of the image.
     * 
     * @param width the width of the image
     * @param height the height of the image 
     */
    public void setDimensions(int width, int height)
    {
      if (consumer != null)
	consumer.setDimensions(this.width, this.height);
    }

    /**
     * An <code>ImageProducer</code> can set a list of properties
     * associated with this image by using this method.
     * <br>
     * FIXME - What property is set for this class?
     *
     * @param props the list of properties associated with this image 
     */
    public void setProperties(Hashtable props)
    {
  	props.put("filters", "CropImageFilter");
	if (consumer != null)
	  consumer.setProperties(props);
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
	Rectangle filterBounds = new Rectangle(this.x, this.y,
	                                       this.width, this.height);
	Rectangle pixelBounds = new Rectangle(x, y, w, h);

	if (filterBounds.intersects(pixelBounds))
	{
	    Rectangle bounds = filterBounds.intersection(pixelBounds);

	    byte[] cropped = new byte[bounds.width * bounds.height];
	    for (int i = 0; i < bounds.height; i++)
	    {
		int start = (bounds.y - pixelBounds.y + i) * scansize + offset;

		for (int j = 0; j < bounds.width; j++)
		    cropped[i * bounds.width + j] = pixels[start + bounds.x + j];
	    }
	    
	    if (consumer != null)
	      consumer.setPixels(0, 0,
				 bounds.width, bounds.height,
				 model, cropped, 0, bounds.width);
	}
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
	Rectangle filterBounds = new Rectangle(this.x, this.y,
	                                       this.width, this.height);
	Rectangle pixelBounds = new Rectangle(x, y, w, h);

	if (filterBounds.intersects(pixelBounds))
	{
	    Rectangle bounds = filterBounds.intersection(pixelBounds);

	    int[] cropped = new int[bounds.width * bounds.height];
	    for (int i = 0; i < bounds.height; i++)
	    {
		int start = (bounds.y - pixelBounds.y + i) * scansize + offset;

		for (int j = 0; j < bounds.width; j++)
		    cropped[i * bounds.width + j] = pixels[start + bounds.x + j];
	    }
	    
	    if (consumer != null)
	      consumer.setPixels(0, 0,
				 bounds.width, bounds.height,
				 model, cropped, 0, bounds.width);
	}
    }

}

