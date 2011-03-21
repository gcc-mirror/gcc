/* PNGChunk.java -- Generic PNG chunk
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

import java.io.InputStream;
import java.io.OutputStream;
import java.io.IOException;

/**
 * Class to load and validate a generic PNG chunk.
 */
public class PNGChunk
{

  /**
   * CRC table and initialization code.
   */
  private static long[] crcTable;

  static
   {
     long c;
     crcTable = new long[256];

     for(int i = 0; i < 256; i++)
       {
         c = i;
         for(int j = 0; j < 8; j++)
           if( (c & 1) == 1 )
             c = 0xEDB88320L ^ (c >> 1);
           else
             c = c >> 1;
         crcTable[i] = c;
       }
   }

  /**
   * (recognized) PNG chunk types.
   */
  public static final int TYPE_HEADER = 0x49484452; // 'IHDR'
  public static final int TYPE_PALETTE = 0x504c5445;// 'PLTE'
  public static final int TYPE_DATA = 0x49444154;   // 'IDAT'
  public static final int TYPE_TIME = 0x74494d45;   // 'tIME'
  public static final int TYPE_END = 0x49454e44;    // 'IEND'
  public static final int TYPE_PHYS = 0x70485973;   // 'pHYS'
  public static final int TYPE_GAMMA = 0x67414d41;  // 'gAMA'
  public static final int TYPE_PROFILE = 0x69434350;  // 'iCCP'

  /**
   * The chunk type - Represented in the file as 4 ASCII bytes,
   */
  private int type;

  /**
   * The chunk data
   */
  protected byte[] data;

  /**
   * The chunk's crc
   */
  private int crc;

  /**
   * Constructor for reading a generic chunk.
   */
  protected PNGChunk( int type, byte[] data, int crc )
  {
    this.type = type;
    this.data = data;
    this.crc = crc;
  }

  /**
   * Constructor for creating new chunks.
   * (only used by subclasses - creating a generic chunk is rather useless)
   */
  protected PNGChunk( int type )
  {
    this.type = type;
  }

  /**
   * Loads a chunk from an InputStream. Does not perform validation,
   * but will throw an IOException if the read fails.
   * @param in - th einputstream to read from
   * @param strict - if true, a PNGException is thrown on all invalid chunks,
   * if false, only critical chunks will throw PNGExceptions.
   */
  public static PNGChunk readChunk(InputStream in, boolean strict)
    throws IOException, PNGException
  {
    byte data[] = new byte[4];
    if( in.read( data ) != 4 )
      throw new IOException("Could not read chunk length.");
    int length = ((data[0] & 0xFF) << 24) | ((data[1] & 0xFF) << 16 ) |
      ((data[2] & 0xFF) << 8) | (data[3] & 0xFF);

    if( in.read( data ) != 4 )
      throw new IOException("Could not read chunk type.");
    int type = ((data[0] & 0xFF) << 24) | ((data[1] & 0xFF) << 16 ) |
      ((data[2] & 0xFF) << 8) | (data[3] & 0xFF);

    byte[] chkdata = new byte[ length ];
    if( in.read( chkdata ) != length )
      throw new IOException("Could not read chunk data.");

    if( in.read( data ) != 4 )
      throw new IOException("Could not read chunk CRC.");

    int crc = ((data[0] & 0xFF) << 24) | ( (data[1] & 0xFF) << 16 ) |
      ((data[2] & 0xFF) << 8) | (data[3] & 0xFF);

    if( strict )
      return getChunk( type, chkdata, crc );
    else
      {
        try
          {
            return getChunk( type, chkdata, crc );
          }
        catch(PNGException pnge)
          {
            if( isEssentialChunk( type ) )
              throw pnge;
            return null;
          }
      }
  }

  /**
   * Returns a specialied object for a chunk, if we have one.
   */
  private static PNGChunk getChunk( int type, byte[] data, int crc )
    throws PNGException
  {
    switch( type )
      {
      case TYPE_HEADER:
        return new PNGHeader( type, data, crc );
      case TYPE_DATA:
        return new PNGData( type, data, crc );
      case TYPE_PALETTE:
        return new PNGPalette( type, data, crc );
      case TYPE_TIME:
        return new PNGTime( type, data, crc );
      case TYPE_PHYS:
        return new PNGPhys( type, data, crc );
      case TYPE_GAMMA:
        return new PNGGamma( type, data, crc );
      case TYPE_PROFILE:
        return new PNGICCProfile( type, data, crc );
      default:
        return new PNGChunk( type, data, crc );
      }
  }

  /**
   * Returns whether the chunk is essential or not
   */
  private static boolean isEssentialChunk( int type )
  {
    switch( type )
      {
      case TYPE_HEADER:
      case TYPE_DATA:
      case TYPE_PALETTE:
      case TYPE_END:
        return true;
      default:
        return false;
      }
  }

  /**
   * Validates the chunk
   */
  public boolean isValidChunk()
  {
    return (crc == calcCRC());
  }

  /**
   * Returns the chunk type.
   */
  public int getType()
  {
    return type;
  }

  /**
   * Writes a PNG chunk to an output stream,
   * performing the CRC calculation as well.
   */
  public void writeChunk(OutputStream out) throws IOException
  {
    out.write( getInt(data.length) );
    out.write( getInt(type) );
    out.write( data );
    out.write( getInt(calcCRC()) );
  }

  /**
   * Return whether the chunk contains any data.
   */
  public boolean isEmpty()
  {
    return ( data.length == 0 );
  }

  /**
   * Convenience method. Cast an int to four bytes (big endian).
   * (Now why doesn't java have a simple way of doing this?)
   */
  public static byte[] getInt(int intValue)
  {
    long i = (intValue & 0xFFFFFFFFL);
    byte[] b = new byte[4];
    b[0] = (byte)((i & 0xFF000000L) >> 24);
    b[1] = (byte)((i & 0x00FF0000L) >> 16);
    b[2] = (byte)((i & 0x0000FF00L) >> 8);
    b[3] = (byte)(i & 0x000000FFL);
    return b;
  }

  /**
   * Calculates this chunk's CRC value.
   */
  private int calcCRC()
  {
    long c = 0xFFFFFFFFL;
    byte[] t = getInt( type );
    for(int i = 0; i < 4; i++)
      c = crcTable[ (int)((c ^ t[i]) & 0xFF) ] ^ (c >> 8);

    for(int i = 0; i < data.length; i++)
      c = crcTable[ (int)((c ^ data[i]) & 0xFF) ] ^ (c >> 8);

    return (int)(c ^ 0xFFFFFFFFL);
  }

  public String toString()
  {
    return "PNG Chunk. Type: " + new String( getInt(type) ) + " , CRC: " +
      crc + " , calculated CRC: "+calcCRC();
  }

}
