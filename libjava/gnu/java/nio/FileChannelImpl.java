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
import java.nio.channels.ClosedChannelException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.channels.NonReadableChannelException;
import java.nio.channels.NonWritableChannelException;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.WritableByteChannel;

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
  private native long lengthInternal (int native_fd) throws IOException;

  public FileChannelImpl (int fd, Object obj)
  {
    this.fd = fd;
    this.file_obj = obj;
  }

  public long size () throws IOException
  {
    if (!isOpen ())
      throw new ClosedChannelException ();

    return lengthInternal (fd);
  }
    
  protected void implCloseChannel() throws IOException
  {
    if (address != 0)
      {
        nio_unmmap_file (fd, address, (int) length);
        address = 0;
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

  public int read (ByteBuffer dst, long position)
    throws IOException
  {
    if (position < 0)
      throw new IllegalArgumentException ();

    if (!isOpen ())
      throw new ClosedChannelException ();
    
    // FIXME: check for NonReadableChannelException

    throw new Error ("Not implemented");
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
    
  public int write (ByteBuffer src, long position)
    throws IOException
  {
    if (position < 0)
      throw new IllegalArgumentException ();

    if (!isOpen ())
      throw new ClosedChannelException ();
    
    // FIXME: check for NonWritableChannelException

    throw new Error ("Not implemented");
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
    if ((mode != MapMode.READ_ONLY
         && mode != MapMode.READ_WRITE
         && mode != MapMode.PRIVATE)
        || position < 0
        || size < 0
        || size > Integer.MAX_VALUE)
      throw new IllegalArgumentException ();
    
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
  public void force (boolean metaData) throws IOException
  {
    if (!isOpen ())
      throw new ClosedChannelException ();

    // FIXME: What to do with metaData ?
    
    nio_msync (fd, address, length);
  }

  public long transferTo (long position, long count, WritableByteChannel target)
    throws IOException
  {
    if (position < 0
        || count < 0)
      throw new IllegalArgumentException ();

    if (!isOpen ())
      throw new ClosedChannelException ();

    // FIXME: check for NonReadableChannelException
    // FIXME: check for NonWritableChannelException
    
    throw new Error ("Not implemented");
  }

  public long transferFrom (ReadableByteChannel src, long position, long count)
    throws IOException
  {
    if (position < 0
        || count < 0)
      throw new IllegalArgumentException ();

    if (!isOpen ())
      throw new ClosedChannelException ();

    // FIXME: check for NonReadableChannelException
    // FIXME: check for NonWritableChannelException
    
    throw new Error ("Not implemented");
  }

  public FileLock lock (long position, long size, boolean shared)
    throws IOException
  {
    if (position < 0
        || size < 0)
      throw new IllegalArgumentException ();

    if (!isOpen ())
      throw new ClosedChannelException ();

    // FIXME: check for NonReadableChannelException
    // FIXME: check for NonWritableChannelException
    
    throw new Error ("Not implemented");
  }
  
  public FileLock tryLock (long position, long size, boolean shared)
    throws IOException
  {
    if (position < 0
        || size < 0)
      throw new IllegalArgumentException ();

    if (!isOpen ())
      throw new ClosedChannelException ();

    throw new Error ("Not implemented");
  }

  public long position ()
    throws IOException
  {
    if (!isOpen ())
      throw new ClosedChannelException ();

    throw new Error ("not implemented");
  }
  
  public FileChannel position (long newPosition)
    throws IOException
  {
    if (newPosition < 0)
      throw new IllegalArgumentException ();

    if (!isOpen ())
      throw new ClosedChannelException ();

    throw new Error ("not implemented");
  }
  
  public FileChannel truncate (long size)
    throws IOException
  {
    if (size < 0)
      throw new IllegalArgumentException ();

    if (!isOpen ())
      throw new ClosedChannelException ();

    // FIXME: check for NonWritableChannelException

    throw new Error ("not implemented");
  }
  
  private static native long nio_mmap_file (int fd, long pos, int size, int mode);
  private static native void nio_unmmap_file (int fd, long address, int size);
  private static native void nio_msync (int fd, long address, int length);
}
