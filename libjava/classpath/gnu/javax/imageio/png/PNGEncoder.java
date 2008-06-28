/* PNGEncoder.java --
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

import java.util.Vector;
import java.util.zip.Deflater;
import java.awt.color.ColorSpace;
import java.awt.color.ICC_ColorSpace;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;
import java.awt.image.DataBufferUShort;
import java.awt.image.IndexColorModel;
import java.awt.image.WritableRaster;

public class PNGEncoder 
{
  /**
   * The default data chunk size. 8 kb.
   */
  private static final int defaultChunkSize = 8192;

  private PNGHeader header;
  private PNGPalette palette;
  private int stride, bpp;
  private byte[] rawData;
  private PNGICCProfile profile;

  public PNGEncoder( BufferedImage bi ) throws PNGException
  {
    ColorModel c = bi.getColorModel();
    int width = bi.getWidth(); 
    int height = bi.getHeight();
    int depth = 0;
    int colorType;
    boolean interlace = false;

    if( c instanceof IndexColorModel )
      {
	colorType = PNGHeader.INDEXED;
	int n = ((IndexColorModel)c).getMapSize();
	if( n <= 2 )
	  depth = 1;
	else if( n <= 4 )
	  depth = 2;
	else if( n <= 16 )
	  depth = 4;
	else if( n <= 256 )
	  depth = 8;
	else
	  throw new PNGException("Depth must be <= 8 bits for indexed color.");
	palette = new PNGPalette( ((IndexColorModel)c) );
      }
    else
      { 
	ColorSpace cs = c.getColorSpace();
	ColorSpace grayCS = ColorSpace.getInstance( ColorSpace.CS_GRAY );
	if( cs == grayCS || bi.getType() == BufferedImage.TYPE_BYTE_GRAY 
	    || bi.getType() == BufferedImage.TYPE_USHORT_GRAY )
	  colorType = c.hasAlpha() ? PNGHeader.GRAYSCALE_WITH_ALPHA : 
	    PNGHeader.GRAYSCALE;
	else
	  colorType = c.hasAlpha() ? PNGHeader.RGB_WITH_ALPHA : PNGHeader.RGB;
	// Figure out the depth
	int[] bits = c.getComponentSize();
	depth = bits[0];
	for(int i = 1; i < bits.length; i++ )
	  if( bits[i] > depth ) depth = bits[i];
	if( (cs != grayCS && !cs.isCS_sRGB()) && cs instanceof ICC_ColorSpace )
	  profile = new PNGICCProfile( ((ICC_ColorSpace)cs).getProfile() );
      }

    header = new PNGHeader(width, height, depth, colorType, interlace);

    stride = header.getScanlineStride(); // scanline stride 
    bpp = header.bytesPerPixel(); // bytes per pixel
    getRawData( bi );
  }

  /**
   * Returns the generated header.
   */ 
  public PNGHeader getHeader()
  {
    return header;
  }

  /**
   * Returns the generated palette.
   */ 
  public PNGPalette getPalette()
  {
    return palette;
  }

  /**
   * Returns the associated ICC profile, if any.
   */ 
  public PNGICCProfile getProfile()
  {
    return profile;
  }

  /**
   * Encodes the raster and returns a Vector of PNGData chunks.
   */
  public Vector encodeImage()
  {
    Deflater deflater = new Deflater(); // The deflater
    boolean useFilter = PNGFilter.useFilter( header );
    byte[] lastScanline = new byte[ stride ];

    byte[] data = new byte[ rawData.length + header.getHeight() ];

    byte filterByte = PNGFilter.FILTER_NONE;
    for( int i = 0; i < header.getHeight(); i++)
      {
	byte[] scanline = new byte[ stride ]; 
	System.arraycopy(rawData, (i * stride), scanline, 0, stride);
	if( useFilter && i > 0)
	  filterByte = PNGFilter.chooseFilter( scanline, lastScanline, bpp);
	
	byte[] filtered = PNGFilter.filterScanline( filterByte, scanline, 
						    lastScanline, bpp );
	data[i * (stride + 1)] = filterByte;
	System.arraycopy(filtered, 0, data, 1 + (i * (stride + 1)), stride);

	lastScanline = scanline;
      }

    deflater.setInput( data ); 
    deflater.finish();

    PNGData chunk;
    Vector chunks = new Vector();
    do
      {
	chunk = new PNGData( defaultChunkSize );
	chunk.deflateToChunk( deflater );
	chunks.add( chunk );
      }
    while( chunk.chunkFull() );
    chunk.shrink(); // Shrink the last chunk.
    return chunks;
  }

  /**
   * Get the image's raw data.
   * FIXME: This may need improving on.
   */
  private void getRawData( BufferedImage bi ) throws PNGException
  {
    WritableRaster raster = bi.getRaster();
    rawData = new byte[ stride * header.getHeight() ];
    if( header.isIndexed() )
      {
	DataBuffer db = raster.getDataBuffer();
	if( !( db instanceof DataBufferByte ) )
	  throw new PNGException("Unexpected DataBuffer for an IndexColorModel.");
	byte[] data = ((DataBufferByte)db).getData();
	for(int i = 0; i < header.getHeight(); i++ )
	  System.arraycopy( data, i * stride, rawData, i * stride, stride );
	return;
      }

    if( header.getDepth() == 16 )
      {
	DataBuffer db = raster.getDataBuffer();
	if( !( db instanceof DataBufferUShort ) )
	  throw new PNGException("Unexpected DataBuffer for 16-bit.");
	short[] data = ((DataBufferUShort)db).getData();
	for(int i = 0; i < header.getHeight(); i++ )
	  for(int j = 0; j < ( stride >> 1); j++)
	    {
	      rawData[ j * 2 + i * stride ] = (byte)((data[j + i * (stride >> 1 )] & 0xFF00) >> 8);
	      rawData[ j * 2 + i * stride + 1 ] = (byte)(data[j + i * (stride >> 1 )] & 0xFF);
	    }
	return;
      }

    int size = ( header.getColorType() == PNGHeader.RGB_WITH_ALPHA ) ? 4 : 3;
    int width = header.getWidth();
    int height = header.getHeight();
    int[] pixels = bi.getRGB( 0, 0, width, height, null, 0, width );

    for( int i = 0; i < width * height; i++ )
      {
	rawData[ i * size ] = (byte)((pixels[i] & 0xFF0000) >> 16);
	rawData[ i * size + 1 ] = (byte)((pixels[i] & 0xFF00) >> 8);
	rawData[ i * size + 2 ] = (byte)(pixels[i] & 0xFF);
      }

    if( size == 4 )
      for( int i = 0; i < width * height; i++ )
	rawData[ i * size + 3 ] = (byte)((pixels[i] & 0xFF000000) >> 24);
  }
}
