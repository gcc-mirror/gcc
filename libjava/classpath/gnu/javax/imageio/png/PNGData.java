/* PNGData.java -- PNG IDAT chunk.
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
import java.util.zip.Deflater;

/**
 * A PNG IDAT (data) chunk.
 */
public class PNGData extends PNGChunk
{
  private int offset;

  protected PNGData( int type, byte[] data, int crc )
  {
    super( type, data, crc );
  }

  protected PNGData( int chunkSize )
  {
    super( PNGChunk.TYPE_DATA );
    data = new byte[ chunkSize ];
    offset = 0;
  }

  /**
   * Deflates the available data in def to the chunk.
   *
   * @return true if the chunk is filled and no more data can be written,
   * false otherwise.
   */
  public void deflateToChunk( Deflater def )
  {
    offset += def.deflate( data, offset, data.length - offset );
  }

  /**
   * Returns true if the chunk is filled.
   */
  public boolean chunkFull()
  {
    return (offset >= data.length);
  }

  /**
   * Shrink the chunk to offset size, used for the last chunk in a stream
   * (no trailing data!)
   */
  public void shrink()
  {
    byte[] newData = new byte[ offset ];
    System.arraycopy( data, 0, newData, 0, offset );
    data = newData;
  }

  /**
   * Feeds the data in the chunk to a ZIP inflater object.
   */
  public void feedToInflater( Inflater inf )
  {
    inf.setInput( data );
  }

  public String toString()
  {
    return "PNG Data chunk. Length = "+data.length;
  }
}
