/* FileChannel.java -- 
   Copyright (C) 2002 Free Software Foundation, Inc.

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

package java.nio.channels;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.MappedByteBuffer;
import java.nio.channels.spi.AbstractInterruptibleChannel;

/**
 * @author Michael Koch
 * @since 1.4
 */
public abstract class FileChannel extends AbstractInterruptibleChannel
  implements ByteChannel, GatheringByteChannel, ScatteringByteChannel
{
  public static class MapMode
  {
    public int m;

    public static MapMode READ_ONLY  = new MapMode(0);
    public static MapMode READ_WRITE = new MapMode(1);
    public static MapMode PRIVATE    = new MapMode(2);

    /**
     * Initializes the MapMode.
     */
    MapMode(int a)
    {
      m = a;
    }

    public String toString() 
    {
      return "" + m;
    }
  }

  /**
   * Initializes the channel.
   */
  protected FileChannel ()
  {
  }

  /**
   * Maps the file into the memory.
   *
   * @exception IOException If an error occurs.
   */
  public abstract MappedByteBuffer map(MapMode mode, long position, long size)
    throws IOException;

  /**
   * Return the size of the file thus far
   */
  public abstract long size() throws IOException;
  
  /**
   * Writes data to the channel.
   *
   * @exception IOException If an error occurs.
   */
  public long write (ByteBuffer[] srcs) throws IOException
  {
    long result = 0;
    
    for (int i = 0; i < srcs.length; i++)
      {
        result += write (srcs[i]);
      }
    
    return result;
  }
  
  /**
   * Writes data to the channel.
   */
  public abstract long write(ByteBuffer[] srcs, int offset, int length)
    throws IOException;
  
  /**
   * Reads data from the channel.
   */
  public abstract int read(ByteBuffer dst) throws IOException;
  
  /**
   * Closes the channel.
   *
   * This is called from @see close.
   *
   * @exception IOException If an error occurs.
   */
  protected abstract void implCloseChannel() throws IOException;

  /**
   * msync with the disk
   */
  public abstract void force(boolean metaData);    
}
