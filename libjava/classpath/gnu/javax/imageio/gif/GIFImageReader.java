/* GIFImageReader.java --
   Copyright (C)  2006  Free Software Foundation, Inc.

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

package gnu.javax.imageio.gif;

import gnu.javax.imageio.IIOInputStream;

import java.io.IOException;
import java.io.InputStream;
import javax.imageio.*;
import javax.imageio.spi.*;
import javax.imageio.metadata.*;
import javax.imageio.stream.ImageInputStream;
import java.util.Iterator;
import java.awt.image.BufferedImage;
import java.awt.image.IndexColorModel;
import java.awt.image.SampleModel;
import java.awt.image.MultiPixelPackedSampleModel;
import java.awt.image.SinglePixelPackedSampleModel;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;
import java.awt.image.Raster;
import java.awt.image.WritableRaster;

public class GIFImageReader extends ImageReader 
{
  private GIFFile file;

  protected GIFImageReader(ImageReaderSpi originatingProvider)
  {
    super( originatingProvider );
    file = null;
  }
  
  private void readImage() throws IOException
  {
    if( file != null )
      return;

    try
      {
	if( input instanceof InputStream )
	  file = new GIFFile( (InputStream)input );
	else
	  file = new GIFFile( new IIOInputStream((ImageInputStream)input) );
      }
    catch(GIFFile.GIFException ge)
      {
	throw new IIOException(ge.getMessage());
      }
  }

  /**
   * Returns the Global/Local palette as an IndexColorModel
   */
  private IndexColorModel getPalette(int index)
  {
    GIFFile f = file.getImage( index );
    byte[] data = f.getRawPalette();
    int nc = f.getNColors();
    byte[] r = new byte[nc];
    byte[] g = new byte[nc];
    byte[] b = new byte[nc];

    for(int i = 0; i < nc; i ++ )
      {
	r[i] = data[ i * 3 ];
	g[i] = data[ i * 3 + 1 ];
	b[i] = data[ i * 3 + 2 ];
      }

    if( f.hasTransparency() )
      {
	byte[] a = new byte[nc];
	for(int i = 0; i < nc; i ++ )
	  a[i] = (byte)0xFF;
	a[f.getTransparentIndex()] = 0;
	return new IndexColorModel(8, nc, r, g, b, a);
      }
    
    return new IndexColorModel(8, nc, r, g, b);
  }

  private void validateIndex(int imageIndex) 
    throws IndexOutOfBoundsException 
  {
    if( imageIndex < 0 || imageIndex >= getNumImages(false) )
      throw new IndexOutOfBoundsException("Invalid image index.");
  }

  public void setInput(Object input) 
  {
    super.setInput(input);
  }

  public void setInput(Object input, 
		       boolean seekForwardOnly, 
		       boolean ignoreMetadata) 
  {
    super.setInput(input, seekForwardOnly, ignoreMetadata);
  }
	
  public void setInput(Object input, boolean isStreamable) 
  {
    super.setInput(input, isStreamable);
	
    if (!(input instanceof ImageInputStream) && 
	!(input instanceof InputStream))
      throw new IllegalArgumentException("Input not an ImageInputStream.");
  }

  private void checkStream() throws IOException 
  {
    if (!(input instanceof ImageInputStream) &&
	!(input instanceof InputStream))
      throw new IllegalStateException("Input not an ImageInputStream or InputStream.");

    if(input == null)
      throw new IllegalStateException("No input stream.");
  }

  public int getWidth(int imageIndex) throws IOException 
  {
    validateIndex( imageIndex );
    return file.getImage( imageIndex ).getWidth();
  }

  public int getHeight(int imageIndex) throws IOException 
  {
    validateIndex( imageIndex );
    return file.getImage( imageIndex ).getHeight();
  }

  public Iterator getImageTypes(int imageIndex)
  {
    validateIndex( imageIndex );
    return null;
  }

  /**
   * Returns the number of images. 
   */
  public int getNumImages(boolean allowSearch)
  {
    try // Image should be loaded here already. But just in case:
      {
	readImage();
      }
    catch(IOException ioe)
      {
	return 0; // Well, now we're in trouble. But return something anyway.
      }
    return file.nImages();
  }


  // FIXME: Support metadata
  public IIOMetadata getImageMetadata(int imageIndex)
  {
    validateIndex( imageIndex );
    return null;
  }

  // FIXME: Support metadata
  public IIOMetadata getStreamMetadata()
  {
    return null;
  }

  /**
   * Reads the image indexed by imageIndex and returns it as 
   * a complete BufferedImage, using a supplied ImageReadParam.
   */	      
  public BufferedImage read(int imageIndex, ImageReadParam param) 
    throws IOException, IIOException 
  {
    validateIndex( imageIndex );
    GIFFile f = file.getImage( imageIndex );
    int width = f.getWidth();
    int height = f.getHeight();
    SampleModel sm;
    switch( f.getNColors() )
      {
      case 16:
	sm = new MultiPixelPackedSampleModel(DataBuffer.TYPE_BYTE, 
					     width, height, 4);
	break;
      case 4:
	sm = new MultiPixelPackedSampleModel(DataBuffer.TYPE_BYTE, 
					     width, height, 2);
	break;
      case 2:
	sm = new MultiPixelPackedSampleModel(DataBuffer.TYPE_BYTE, 
					     width, height, 1);
	break;
      default:
	sm = new SinglePixelPackedSampleModel(DataBuffer.TYPE_BYTE, 
					      width, height, 
					      new int[] {0xFF});
	break;
      }
    DataBuffer db = new DataBufferByte(f.getRawImage(), width * height, 0);
    WritableRaster raster = Raster.createWritableRaster(sm, db, null);
    
    return new BufferedImage(getPalette( imageIndex ), raster, false, null);
  }
}
