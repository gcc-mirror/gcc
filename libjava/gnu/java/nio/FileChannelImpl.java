/* FileChannelImpl.java -- 
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

package gnu.java.nio;

import java.io.EOFException;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.ByteBuffer;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;

/**
 * This file is not user visible !
 * But alas, Java does not have a concept of friendly packages
 * so this class is public. 
 * Instances of this class are created by invoking getChannel
 * Upon a Input/Output/RandomAccessFile object.
 */

public class FileChannelImpl extends FileChannel
{
  public long address;
  public int length;
  public int fd;
  public MappedByteBuffer buf;
  public Object file_obj; // just to keep it live...

  /**
   * This method came from java.io.RandomAccessFile
   * It is private there so we will repeat it here.
   */
//   private native long lengthInternal (int native_fd) throws IOException;
  private long lengthInternal (int native_fd) throws IOException
  {
    return 0;
  };

  public FileChannelImpl (int fd, Object obj)
  {
    this.fd = fd;
    this.file_obj = obj;
  }

  public long size () throws IOException
  {
    return lengthInternal (fd);
  }
    
  protected void implCloseChannel()  throws IOException
  {
    if (address != 0)
	    {
        nio_unmmap_file (fd, address, (int) length);
	    }

    // FIXME
    fd = 0;

    if (file_obj instanceof RandomAccessFile)
	    {
        RandomAccessFile o = (RandomAccessFile) file_obj;
        o.close();
	    }
    else if (file_obj instanceof FileInputStream)
	    {
        FileInputStream o = (FileInputStream) file_obj;
        o.close();
	    }
    else if (file_obj instanceof FileOutputStream)
	    {
        FileOutputStream o = (FileOutputStream) file_obj;
        o.close();
	    }
  }

  public int read (ByteBuffer dst) throws IOException
  {
    int w = 0;
    int s = (int)size();

    if (buf == null)
	    {
        throw new EOFException("file not mapped");
	    }

    for (int i=0; i<s; i++)
	    {
        dst.put( buf.get() );
	    }

    return s;
  }

  public long read (ByteBuffer[] dsts) throws IOException
  {
    return read (dsts, 0, dsts.length);
  }

  public long read (ByteBuffer[] dsts, int offset, int length)
    throws IOException
  {
    long result = 0;

    for (int i = offset; i < offset + length; i++)
	    {
        result += write (dsts[i]);
	    }

    return result;
  }

  public int write (ByteBuffer src) throws IOException
  {
    int w = 0;

    if (buf == null)
	    {
        throw new EOFException ("file not mapped");
	    }

    while (src.hasRemaining ())
	    {
        buf.put (src.get ());
        w++;
	    }

    return w;
  }
    
  public long write(ByteBuffer[] srcs, int offset, int length)
    throws IOException
  {
    long res = 0;

    for (int i = offset;i < offset + length;i++)
	    {
        res += write (srcs[i]);
	    }
	return res;
    }
				   
  public MappedByteBuffer map (FileChannel.MapMode mode, long position,
                               long size)
    throws IOException
  {
//     int cmode = mode.m;
//     address = nio_mmap_file (fd, position, size, cmode);
//     length = size;
//     buf = new MappedByteFileBuffer (this);
//     return buf;
    return null;
  }

  static MappedByteBuffer create_direct_mapped_buffer (long address,
                                                       long length)
  {
//     FileChannelImpl ch = new FileChannelImpl (-1, null);
//     ch.address = address;
//     ch.length = (int) length;
//     ch.buf = new MappedByteFileBuffer (ch);
//     return ch.buf;			 
    return null;
  }

  public long write (ByteBuffer[] srcs)
    throws IOException
  {
    return write (srcs, 0, srcs.length);
  }
				   
  /**
   * msync with the disk
   */
  public void force (boolean metaData)
  {
    nio_msync (fd, address, length);
  }

//   static native long nio_mmap_file (int fd, long pos, int size, int mode);

//   static native void nio_unmmap_file (int fd, long address, int size);

//   static native void nio_msync (int fd, long address, int length);

  static long nio_mmap_file (int fd, long pos, int size, int mode)
  {
    return 0;
  }

  static void nio_unmmap_file (int fd, long address, int size)
  {
  };

  static void nio_msync (int fd, long address, int length)
  {
  };
}
