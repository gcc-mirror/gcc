/* RGBImageFilter.java -- Java class for filtering Pixels by RGB values
   Copyright (C) 1999, 2005  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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
 * A filter designed to filter images in the default RGBColorModel regardless of 
 * the ImageProducer's ColorModel.
 *
 * @author Mark Benvenuto (mcb54@columbia.edu)
 */
public abstract class RGBImageFilter extends ImageFilter
{
    protected ColorModel origmodel;

    protected ColorModel newmodel;
    
    /**
       Specifies whether to apply the filter to the index entries of the 
       IndexColorModel. Subclasses should set this to true if the filter 
       does not depend on the pixel's coordinate.
     */
    protected boolean canFilterIndexColorModel = false;

    /**
       Construct new RGBImageFilter.
     */
    public RGBImageFilter() 
    {
    }

    /**
     * Sets the ColorModel used to filter with. If the specified ColorModel is IndexColorModel 
     * and canFilterIndexColorModel is true, we subsitute the ColorModel for a filtered one
     * here and in setPixels whenever the original one appears. Otherwise overrides the default
     * ColorModel of ImageProducer and specifies the default RGBColorModel
     *
     * @param model the color model to be used most often by setPixels
     * @see ColorModel */
    public void setColorModel(ColorModel model) 
    {
	origmodel = model;
	newmodel = model;

	if( ( model instanceof IndexColorModel) && canFilterIndexColorModel  ) {
		newmodel = filterIndexColorModel( (IndexColorModel) model );
		consumer.setColorModel(newmodel);
	    }
	else {
		consumer.setColorModel(ColorModel.getRGBdefault());
	}
    }
    
    /**
       Registers a new ColorModel to subsitute for the old ColorModel when 
       setPixels encounters the a pixel with the old ColorModel. The pixel 
       remains unchanged except for a new ColorModel.
       
       @param oldcm the old ColorModel
       @param newcm the new ColorModel
     */
    public void substituteColorModel(ColorModel oldcm,
				     ColorModel newcm)
    {
	origmodel = oldcm;
	newmodel = newcm;
    }

    /**
       Filters an IndexColorModel through the filterRGB function. Uses
       coordinates of -1 to indicate its filtering an index and not a pixel.

       @param icm an IndexColorModel to filter
     */
    public IndexColorModel filterIndexColorModel(IndexColorModel icm) 
    {
	int len = icm.getMapSize(), rgb;
	byte reds[] = new byte[len], greens[] = new byte[len], blues[] = new byte[len], alphas[]  = new byte[len];
	
	icm.getAlphas( alphas );
	icm.getReds( reds );
	icm.getGreens( greens );
	icm.getBlues( blues );

	for( int i = 0; i < len; i++ )
	    {
		rgb = filterRGB( -1, -1, makeColor ( alphas[i], reds[i], greens[i], blues[i] ) );
		alphas[i] = (byte)(( 0xff000000 & rgb ) >> 24);
		reds[i] = (byte)(( 0xff0000 & rgb ) >> 16);
		greens[i] = (byte)(( 0xff00 & rgb ) >> 8);
		blues[i] = (byte)(0xff & rgb);
	    }
	return new IndexColorModel( icm.getPixelSize(), len, reds, greens, blues, alphas );
    }

    private int makeColor( byte a, byte r, byte g, byte b )
    {
	return ( 0xff000000 & (a << 24) | 0xff0000 & (r << 16) | 0xff00 & (g << 8) | 0xff & b ); 
    }

    /**
       This functions filters a set of RGB pixels through filterRGB.

       @param x the x coordinate of the rectangle
       @param y the y coordinate of the rectangle
       @param w the width of the rectangle
       @param h the height of the rectangle
       @param pixels the array of pixel values
       @param offset the index of the first pixels in the <code>pixels</code> array
       @param scansize the width to use in extracting pixels from the <code>pixels</code> array
    */
    public void filterRGBPixels(int x, int y, int w, int h, int[] pixels,
				int offset, int scansize)
    {
      for (int xp = x; xp < (x + w); xp++)
	for (int yp = y; yp < (y + h); yp++)
	  {
	    pixels[offset] = filterRGB(xp, yp, pixels[offset]);
	    offset++;
	  }
    }


    /**
     * If the ColorModel is the same ColorModel which as already converted 
     * then it converts it the converted ColorModel. Otherwise it passes the 
     * array of pixels through filterRGBpixels.
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
                          ColorModel model, byte[] pixels,
                          int offset, int scansize)
    {
	if(model == origmodel && (model instanceof IndexColorModel) && canFilterIndexColorModel)
	{
	    consumer.setPixels(x, y, w, h, newmodel, pixels, offset, scansize);
	}
	else
	{
	    int intPixels[] =
		convertColorModelToDefault( x, y, w, h, model, pixels, offset, scansize );
	    filterRGBPixels( x, y, w, h, intPixels, offset, scansize );
	    consumer.setPixels(x, y, w, h, ColorModel.getRGBdefault(), intPixels, offset, scansize);
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
                          ColorModel model, int[] pixels,
                          int offset, int scansize)
    {
	if(model == origmodel && (model instanceof IndexColorModel) && canFilterIndexColorModel)
	{
	    consumer.setPixels(x, y, w, h, newmodel, pixels, offset, scansize);
	}
	else
	{
	    //FIXME: Store the filtered pixels in a separate temporary buffer?
	    convertColorModelToDefault( x, y, w, h, model, pixels, offset, scansize );
	    filterRGBPixels( x, y, w, h, pixels, offset, scansize );
	    consumer.setPixels(x, y, w, h, ColorModel.getRGBdefault(), pixels, offset, scansize);
	}
    }

    private int[] convertColorModelToDefault(int x, int y, int w, int h, 
                                            ColorModel model, byte pixels[],
                                            int offset, int scansize)
    {
	int intPixels[] = new int[pixels.length];
	for (int i = 0; i < pixels.length; i++)
	    intPixels[i] = makeColorbyDefaultCM(model, pixels[i]);
	return intPixels;
    }

    private void convertColorModelToDefault(int x, int y, int w, int h, 
                                            ColorModel model, int pixels[],
                                            int offset, int scansize)
    {
	for (int i = 0; i < pixels.length; i++)
	    pixels[i] = makeColorbyDefaultCM(model, pixels[i]);
    }

    private int makeColorbyDefaultCM(ColorModel model, byte rgb) 
    {
	return makeColor( model.getAlpha( rgb ) * 4, model.getRed( rgb ) * 4, model.getGreen( rgb ) * 4, model.getBlue( rgb ) * 4 );
    }

    private int makeColorbyDefaultCM(ColorModel model, int rgb) 
    {
	return makeColor( model.getAlpha( rgb ), model.getRed( rgb ), model.getGreen( rgb ), model.getBlue( rgb ) );
    }

    private int makeColor( int a, int r, int g, int b )
    {
	return (int)( 0xff000000 & (a << 24) | 0xff0000 & (r << 16) | 0xff00 & (g << 8) | 0xff & b ); 
    }


    /**
       Filters a single pixel from the default ColorModel.

       @param x x-coordinate
       @param y y-coordinate
       @param rgb color
     */
    public abstract int filterRGB(int x,
				  int y,
				  int rgb);
}
