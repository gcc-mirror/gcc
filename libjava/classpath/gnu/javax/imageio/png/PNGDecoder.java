/* PNGDecoder.java
   Copyright (C) 2006 Free Software Foundation

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

package gnu.javax.imageio.png;

import java.util.zip.Inflater;
import java.util.zip.DataFormatException;
import java.awt.image.ColorModel;
import java.awt.image.ComponentColorModel;
import java.awt.image.ComponentSampleModel;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;
import java.awt.image.DataBufferUShort;
import java.awt.image.IndexColorModel;
import java.awt.image.MultiPixelPackedSampleModel;
import java.awt.image.Raster;
import java.awt.image.SampleModel;
import java.awt.image.SinglePixelPackedSampleModel;
import java.awt.image.WritableRaster;
import java.awt.color.ColorSpace;

public class PNGDecoder
{
  private PNGHeader header;
  private byte[] raster;
  private byte[] scanline, lastScanline;
  private byte[] filterType;
  private int offset, length;
  private int currentScanline;
  private final int stride;
  private Inflater inflater;
  private boolean readFilter;
  private int bpp; // bytes per pixel

  /**
   * Constructs a filter object for
   */
  public PNGDecoder(PNGHeader header)
  {
    this.header = header;
    offset = 0;
    inflater = new Inflater();
    stride = header.getScanlineStride();
    length = stride * header.getHeight();

    // Allocate the output raster
    raster = new byte[ length ];
    scanline = new byte[ stride ];
    lastScanline = new byte[ stride ];
    currentScanline = 0;
    readFilter = true;
    bpp = header.bytesPerPixel();
    filterType = new byte[1];
    inflater = new Inflater();
  }

  private int getBytes( byte[] buf, int offset ) throws PNGException
  {
    try
      {
	return inflater.inflate( buf, offset, buf.length - offset);
      }
    catch(DataFormatException dfe)
      {
	throw new PNGException("Error inflating data.");
      }
  }

  /**
   * Decodes a data chunk.
   */
  public void addData( PNGData chunk ) throws PNGException
  {
    int n = 0;
    if( isFinished() )
      return;
    chunk.feedToInflater( inflater );
    do
      {
	if( readFilter )
	  if( getBytes( filterType, 0 ) < 1 )
	    return;

	n = getBytes( scanline, offset );

	if( offset + n < stride )
	  {
	    offset += n;
	    readFilter = false;
	  }
	else
	  {
	    scanline = PNGFilter.unFilterScanline( filterType[0], scanline,
						   lastScanline, bpp );
	    System.arraycopy( scanline, 0,
			      raster, currentScanline * stride, stride );
	    lastScanline = scanline;
	    scanline = new byte[scanline.length];
	    currentScanline++;
	    readFilter = true;
	    offset = 0;
	  }
      }
    while( n > 0 && currentScanline < header.getHeight() );
  }

  /**
   * Parse the appropriate color type and create an AWT raster for it.
   * @param raster - the file header.
   */
  public WritableRaster getRaster( PNGHeader header )
  {
    SampleModel sm = null;
    DataBuffer db = null;
    int t;
    int width = header.getWidth();
    int height = header.getHeight();
    int depth = header.getDepth();

    switch( header.getColorType() )
      {
      case PNGHeader.GRAYSCALE_WITH_ALPHA:
	if( depth == 8 )
	  {
	    t = DataBuffer.TYPE_BYTE;
	    db = getByteBuffer();
	  }
	else
	  { 
	    t = DataBuffer.TYPE_USHORT;
	    db = getShortBuffer();
	  }
	sm = new ComponentSampleModel(t, width, height, 2, width * 2,
				      new int[]{0, 1});
	break;

      case PNGHeader.GRAYSCALE:
	switch( depth )
	  {
	  case 16:
	    sm = new ComponentSampleModel(DataBuffer.TYPE_USHORT, 
					  width, height, 1, width,
					  new int[]{ 0 });
	    db = getShortBuffer();
	    break;
	    
	  case 8:
	    sm = new ComponentSampleModel(DataBuffer.TYPE_BYTE, 
					  width, height, 1, width,
					  new int[]{ 0 });
	    db = getByteBuffer();
	    break;
	    
	  default:
	    sm = new MultiPixelPackedSampleModel(DataBuffer.TYPE_BYTE, 
						 width, height, depth);
	    db = getByteBuffer();
	    break;
	  }
	break;

      case PNGHeader.RGB:
	if( depth == 8 )
	  {
	    t = DataBuffer.TYPE_BYTE;
	    db = getByteBuffer();
	  }
	else
	  { 
	    t = DataBuffer.TYPE_USHORT;
	    db = getShortBuffer();
	  }
	sm = new ComponentSampleModel(t, width, height, 3, 3 * width,
				      new int[]{0, 1, 2});
	break;

      case PNGHeader.RGB_WITH_ALPHA:
	if( depth == 8 )
	  {
	    t = DataBuffer.TYPE_BYTE;
	    db = getByteBuffer();
	  }
	else
	  { 
	    t = DataBuffer.TYPE_USHORT;
	    db = getShortBuffer();
	  }
	
	sm = new ComponentSampleModel(t, width, height, 4, width * 4,
				      new int[]{0, 1, 2, 3});
	break;
	
      case PNGHeader.INDEXED:
	if( depth == 8 )
	  sm = new SinglePixelPackedSampleModel(DataBuffer.TYPE_BYTE, 
						width, height, 
						new int[] {0xFF});
	else
	  sm = new MultiPixelPackedSampleModel(DataBuffer.TYPE_BYTE, 
					       width, height, depth);
	db = getByteBuffer();
	break;
      }

    return Raster.createWritableRaster(sm, db, null);
  }

  /**
   * Wrap the raster with a DataBufferUShort,
   * conversion is big-endian (PNG native).
   */
  private DataBuffer getShortBuffer()
  {
    short[] data = new short[(raster.length >> 1)];
    for( int i = 0; i < data.length; i++ )
      data[i] = (short)(((raster[i * 2] & 0xFF) << 8) | 
			(raster[i * 2 + 1] & 0xFF));
    return new DataBufferUShort( data, data.length );
  }

  /**
   * Wrap the raster with a DataBufferByte
   */
  private DataBuffer getByteBuffer()
  {
    return new DataBufferByte( raster, raster.length );
  }

  public ColorModel getColorModel( ColorSpace cs, 
				   int colorType, int depth )
  {
    int[] bits;
    boolean hasAlpha = false;
    int transferType;

    switch( colorType )
      {
      case PNGHeader.GRAYSCALE_WITH_ALPHA:
	if( cs == null )
	  cs = ColorSpace.getInstance( ColorSpace.CS_GRAY );
	hasAlpha = true;
	bits = new int[]{ depth, depth };
	break;
	
      case PNGHeader.RGB:
	bits = new int[]{ depth, depth, depth };
	break;

      case PNGHeader.RGB_WITH_ALPHA:
	hasAlpha = true;
	bits = new int[]{ depth, depth, depth, depth };
	break;

      case PNGHeader.GRAYSCALE:
	if( depth < 8 )
	  return grayPalette( depth );

	if( cs == null )
	  cs = ColorSpace.getInstance( ColorSpace.CS_GRAY );
	bits = new int[]{ depth };
	break;

      default:
      case PNGHeader.INDEXED:
	return null; // Handled by the palette chunk.
      }

    if( cs == null )
      cs = ColorSpace.getInstance( ColorSpace.CS_sRGB );


    return new ComponentColorModel(cs, bits, hasAlpha, false, 
				   (hasAlpha ? 
				    ComponentColorModel.TRANSLUCENT : 
				    ComponentColorModel.OPAQUE), 
				   ((depth == 16) ? DataBuffer.TYPE_USHORT : 
				    DataBuffer.TYPE_BYTE));
  }

  private IndexColorModel grayPalette(int depth)
  { 
    byte[] c = new byte[ (1 << depth) ];
    for(int i = 0; i < c.length; i++)
      c[i] = (byte)(255.0 * (((double)i) / ((double)c.length - 1.0)));
    return new IndexColorModel(8, c.length, c, c, c);
  }

  public byte[] getRaster()
  {
    return raster;
  }

  public boolean isFinished()
  {
    return currentScanline >= header.getHeight();
  }
}
