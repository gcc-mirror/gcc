/* PNGHeader.java -- PNG Header
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

/**
 * A PNG Header chunk.
 */
public class PNGHeader extends PNGChunk
{
  private int width, height, depth;
  private int colorType, compression, filter, interlace;

  /**
   * The valid interlace types.
   */
  public static final int INTERLACE_NONE = 0;
  public static final int INTERLACE_ADAM7 = 1;

  /**
   * The valid color types.
   */
  public static final int GRAYSCALE = 0;
  public static final int RGB = 2;
  public static final int INDEXED = 3;
  public static final int GRAYSCALE_WITH_ALPHA = 4;
  public static final int RGB_WITH_ALPHA = 6;

  /**
   * Parses a PNG Header chunk.
   */
  protected PNGHeader( int type, byte[] data, int crc ) throws PNGException
  {
    super( type, data, crc );
    if( data.length < 13 )
      throw new PNGException("Unexpectedly short header chunk. (" + data.length
                             + " bytes)");

    width = ((data[0] & 0xFF) << 24) | ( (data[1] & 0xFF) << 16 ) |
      ((data[2] & 0xFF) << 8) | (data[3] & 0xFF);
    height = ((data[4] & 0xFF) << 24) | ( (data[5] & 0xFF) << 16 ) |
      ((data[6] & 0xFF) << 8) | (data[7] & 0xFF);
    depth = (data[8] & 0xFF);
    colorType = (data[9] & 0xFF);
    compression = (data[10] & 0xFF);
    filter = (data[11] & 0xFF);
    interlace = (data[12] & 0xFF);
  }

  /**
   * Create a PNG header chunk.
   * Warning: This trusts that the parameters are valid.
   */
  public PNGHeader(int width, int height, int depth,
                   int colorType, boolean interlace)
  {
    super( TYPE_HEADER );
    data = new byte[ 13 ];

    this.width = width;
    this.height = height;
    this.depth = depth;
    compression = filter = 0;
    this.colorType = colorType;
    this.interlace = interlace ? 1 : 0;

    // Build the data chunk.
    byte[] a = getInt( width );
    byte[] b = getInt( height );
    data[0] = a[0]; data[1] = a[1]; data[2] = a[2]; data[3] = a[3];
    data[4] = b[0]; data[5] = b[1]; data[6] = b[2]; data[7] = b[3];
    data[8] = (byte)depth;
    data[9] = (byte)colorType;
    data[10] = (byte)compression;
    data[11] = (byte)filter;
    data[12] = (byte)this.interlace;
  }

  /**
   * Validates the header fields
   */
  public boolean isValidChunk()
  {
    if( !super.isValidChunk() )
      return false;

    // width and height must be nonzero
    if( width == 0 || height == 0 )
      return false;
    // colorType can be 0,2,3,4,6
    if( (colorType & 0xFFFFFFF8) != 0 || colorType == 5 || colorType == 1)
      return false;
    // Possible valid depths are 1,2,4,8,16
    if( !((depth == 1) || (depth == 2) || (depth == 4) ||
        (depth == 8) || (depth == 16)) )
      return false;
    if( colorType == INDEXED && depth == 16 )
      return false;
    if( ( colorType == RGB || colorType == GRAYSCALE_WITH_ALPHA ||
          colorType == RGB_WITH_ALPHA ) &&
        depth < 8 )
      return false;
    // Only compression and filter methods zero are defined
    if( compression != 0 || filter != 0 )
      return false;
    // Interlace methods, 0 and 1 are valid values.
    if( (interlace & 0xFFFFFFFE) != 0 )
      return false;

    return true;
  }

  /**
   * Returns <code>true</code> if this PNG is indexed-color
   */
  public boolean isIndexed()
  {
    return (colorType == INDEXED);
  }

  /**
   * Returns <code>true</code> if this PNG is grayscale
   */
  public boolean isGrayscale()
  {
    return ((colorType ==  GRAYSCALE) || (colorType == GRAYSCALE_WITH_ALPHA));
  }

  /**
   * Returns the color type of the image.
   */
  public int getColorType()
  {
    return colorType;
  }

  /**
   * Returns whether the image is interlaced or not.
   */
  public boolean isInterlaced()
  {
    return (interlace != 0);
  }

  /**
   * Returns the number of bytes per pixel.
   */
  public int bytesPerPixel()
  {
    switch( colorType )
      {
      case GRAYSCALE_WITH_ALPHA:
        return ((depth * 2) >> 3);
      case RGB:
        return ((depth * 3) >> 3);
      case RGB_WITH_ALPHA:
        return ((depth * 4) >> 3);

      default:
      case GRAYSCALE:
      case INDEXED:
        int i = (depth >> 3);
        if( i > 0 ) return i;
        return 1; // if bytes per pixel < 1, return 1 anyway.
      }
  }

  /**
   * Returns the stride of one scanline, in bytes.
   */
  public int getScanlineStride()
  {
    long nBits = 0; // bits per scanline - scanlines are on byte offsets.
    switch( colorType )
      {
      case GRAYSCALE:
        nBits = width * depth;
        break;
      case RGB:
        nBits = width * depth * 3;
        break;
      case INDEXED:
        nBits = depth * width;
        break;
      case GRAYSCALE_WITH_ALPHA:
        nBits = depth * width * 2;
        break;
      case RGB_WITH_ALPHA:
        nBits = depth * width * 4;
        break;
      }
    // Round up number of bits to the nearest byte
    if( (nBits & 0x07) != 0 )
      nBits += (8 - (nBits & 0x07));

    return (int)(nBits >> 3); // return # of bytes.
  }

  public int getWidth()
  {
    return width;
  }

  public int getHeight()
  {
    return height;
  }

  public int getDepth()
  {
    return depth;
  }

  /**
   * Debugging string.
   */
  public String toString()
  {
    return "Header Chunk. Image width:"+width+" height:"+height+
      " depth:"+depth+" color type:"+colorType+" compression type:"+
      compression+" filter type:"+ filter+" interlace:"+interlace;
  }
}
