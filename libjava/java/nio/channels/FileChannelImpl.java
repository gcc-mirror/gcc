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


package java.nio.channels;

import java.io.EOFException;
import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.ByteBuffer;
import java.nio.MappedByteBuffer;
import java.nio.MappedByteBufferImpl;
import gnu.classpath.Configuration;
import gnu.gcj.RawData;

/**
 * This file is not user visible !
 * But alas, Java does not have a concept of friendly packages
 * so this class is public. 
 * Instances of this class are created by invoking getChannel
 * Upon a Input/Output/RandomAccessFile object.
 */

public class FileChannelImpl extends FileChannel
{
  static
  {
    // load the shared library needed for native methods.
    if (Configuration.INIT_LOAD_LIBRARY)
      {
        System.loadLibrary ("javanio");
      }
  }
  
  public RawData map_address;
  
  int length;
  FileDescriptor fd;
  MappedByteBuffer buf;
  Object file_obj; // just to keep it live...

  public FileChannelImpl (FileDescriptor fd, boolean write, Object obj)
  {
    if (!(obj instanceof RandomAccessFile)
        && !(obj instanceof FileInputStream)
        && !(obj instanceof FileOutputStream))
      throw new InternalError ();

    this.fd = fd;
    this.file_obj = obj;
  }

  public FileChannelImpl ()
  {
    this (new FileDescriptor (), true, null);
  }

  private native long implPosition ();
  private native FileChannel implPosition (long newPosition);
  private native FileChannel implTruncate (long size);
  
  private native RawData nio_mmap_file (long pos, long size, int mode);
  private native void nio_unmmap_file (RawData map_address, int size);
  private native void nio_msync (RawData map_address, int length);

  public native long size () throws IOException;
    
  protected void implCloseChannel() throws IOException
  {
    if (map_address != null)
      {
        nio_unmmap_file (map_address, (int) length);
        map_address = null;
      }

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
    // Check if file is mapped into memory.
    if (buf != null)
      {
	// FIXME: implement this
        throw new Error ("Accessing mapped buffers not implemented.");
      }

    // File not mapped, access it directly.
    return implRead (dst);
  }

  public int read (ByteBuffer dst, long position)
    throws IOException
  {
    if (position < 0)
      throw new IllegalArgumentException ();

    if (!isOpen ())
      throw new ClosedChannelException ();
   
    if (file_obj instanceof FileOutputStream)
      throw new NonReadableChannelException ();

    int result;
    long oldPosition;

    oldPosition = implPosition ();
    position (position);
    result = implRead (dst);
    implPosition (oldPosition);
    
    return result;
  }

  private int implRead (ByteBuffer dst) throws IOException
  {
    int result;
    byte[] buffer = new byte [dst.remaining ()];
    
    result = implRead (buffer, 0, buffer.length);

    if (result > 0)
      dst.put (buffer, 0, result);

    return result;
  }
  
  private native int implRead (byte[] buffer, int offset, int length)
    throws IOException;

  public long read (ByteBuffer[] dsts, int offset, int length)
    throws IOException
  {
    long result = 0;

    for (int i = offset; i < offset + length; i++)
      {
        result += read (dsts [i]);
      }

    return result;
  }

  public int write (ByteBuffer src) throws IOException
  {
    // Check if file is mapped into memory.
    if (buf != null)
      {
	// FIXME: implement this
        throw new Error ("Accessing mapped buffers not implemented.");
      }
    
    // File not mapped, access it directly.
    return implWrite (src);
  }
    
  public int write (ByteBuffer src, long position)
    throws IOException
  {
    if (position < 0)
      throw new IllegalArgumentException ();

    if (!isOpen ())
      throw new ClosedChannelException ();
    
    if (file_obj instanceof FileInputStream)
       throw new NonWritableChannelException ();

    int result;
    long oldPosition;

    oldPosition = implPosition ();
    position (position);
    result = implWrite (src);
    implPosition (oldPosition);
    
    return result;
  }

  private int implWrite (ByteBuffer src) throws IOException
  {
    byte[] buffer = new byte [src.remaining ()];
    
    src.get (buffer, 0, buffer.length);
    return implWrite (buffer, 0, buffer.length);
  }
  
  private native int implWrite (byte[] buffer, int offset, int length)
    throws IOException;
  
  public long write(ByteBuffer[] srcs, int offset, int length)
    throws IOException
  {
    long result = 0;

    for (int i = offset;i < offset + length;i++)
      {
        result += write (srcs[i]);
      }
    
    return result;
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
    
    // FIXME: Make this working.
    int cmode = mode.m;
    map_address = nio_mmap_file (position, size, cmode);
    length = (int) size;
    buf = new MappedByteBufferImpl (this);
    return buf;
  }

  static MappedByteBuffer create_direct_mapped_buffer (RawData map_address,
                                                       long length)
    throws IOException
  {
    FileChannelImpl ch = new FileChannelImpl ();
    ch.map_address = map_address;
    ch.length = (int) length;
    ch.buf = new MappedByteBufferImpl (ch);
    return ch.buf;			 
  }

  /**
   * msync with the disk
   */
  public void force (boolean metaData) throws IOException
  {
    if (!isOpen ())
      throw new ClosedChannelException ();

    // FIXME: What to do with metaData ?
    
    nio_msync (map_address, length);
  }

  public long transferTo (long position, long count, WritableByteChannel target)
    throws IOException
  {
    if (position < 0
        || count < 0)
      throw new IllegalArgumentException ();

    if (!isOpen ())
      throw new ClosedChannelException ();

    if (file_obj instanceof FileOutputStream)
       throw new NonReadableChannelException ();
   
    // XXX: count needs to be casted from long to int. Dataloss ?
    ByteBuffer buffer = ByteBuffer.allocate ((int) count);
    read (buffer, position);
    buffer.flip();
    return target.write (buffer);
  }

  public long transferFrom (ReadableByteChannel src, long position, long count)
    throws IOException
  {
    if (position < 0
        || count < 0)
      throw new IllegalArgumentException ();

    if (!isOpen ())
      throw new ClosedChannelException ();

    if (file_obj instanceof FileInputStream)
       throw new NonWritableChannelException ();

    // XXX: count needs to be casted from long to int. Dataloss ?
    ByteBuffer buffer = ByteBuffer.allocate ((int) count);
    src.read (buffer);
    buffer.flip();
    return write (buffer, position);
  }

  public FileLock lock (long position, long size, boolean shared)
    throws IOException
  {
    if (position < 0
        || size < 0)
      throw new IllegalArgumentException ();

    if (!isOpen ())
      throw new ClosedChannelException ();

    if (shared &&
        file_obj instanceof FileOutputStream)
      throw new NonReadableChannelException ();
	
    if (!shared &&
        file_obj instanceof FileInputStream)
      throw new NonWritableChannelException ();
	
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

    return implPosition ();
  }
  
  public FileChannel position (long newPosition)
    throws IOException
  {
    if (newPosition < 0)
      throw new IllegalArgumentException ();

    if (!isOpen ())
      throw new ClosedChannelException ();

    return implPosition (newPosition);
  }
  
  public FileChannel truncate (long size)
    throws IOException
  {
    if (size < 0)
      throw new IllegalArgumentException ();

    if (!isOpen ())
      throw new ClosedChannelException ();

    if (file_obj instanceof FileInputStream)
       throw new NonWritableChannelException ();

    return implTruncate (size);
  }
}
