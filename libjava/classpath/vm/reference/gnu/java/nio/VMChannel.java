/* VMChannel.java -- Native interface suppling channel operations.
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.java.nio;

import gnu.classpath.Configuration;
import gnu.java.net.PlainSocketImpl;
import gnu.java.nio.PipeImpl.SinkChannelImpl;
import gnu.java.nio.PipeImpl.SourceChannelImpl;
import gnu.java.nio.channels.FileChannelImpl;

import java.io.IOException;
import java.nio.ByteBuffer;

/**
 * Native interface to support configuring of channel to run in a non-blocking
 * manner and support scatter/gather io operations.
 * 
 * @author Michael Barker <mike@middlesoft.co.uk>
 *
 */
public class VMChannel
{
  private final int fd;
  
  private VMChannel(int fd)
  {
    this.fd = fd;
  }
  
  public static VMChannel getVMChannel(PlainSocketImpl socket)
  {
    return new VMChannel(socket.getNativeFD());
  }
  
  public static VMChannel getVMChannel(SourceChannelImpl source)
  {
    return new VMChannel(source.getNativeFD());
  }
  
  public static VMChannel getVMChannel(SinkChannelImpl sink)
  {
    return new VMChannel(sink.getNativeFD());
  }
  
  public static VMChannel getVMChannel(FileChannelImpl file)
  {
    return new VMChannel(file.getNativeFD());
  }

  static
  {
    // load the shared library needed for native methods.
    if (Configuration.INIT_LOAD_LIBRARY)
      {
        System.loadLibrary ("javanio");
      }
    initIDs();
  }
  
  /**
   * Set the file descriptor to have the required blocking
   * setting.
   * 
   * @param fd
   * @param blocking
   */
  public native void setBlocking(int fd, boolean blocking);
  
  public void setBlocking(boolean blocking)
  {
    setBlocking(fd, blocking);
  }
  

  /**
   * Reads a byte buffer directly using the supplied file descriptor.
   * Assumes that the buffer is a DirectBuffer.
   * 
   * @param fd Native file descriptor to read from.
   * @param dst Direct Byte Buffer to read to.
   * @return Number of bytes read.
   * @throws IOException If an error occurs or dst is not a direct buffers. 
   */
  native int read(int fd, ByteBuffer dst)
    throws IOException;
  
  public int read(ByteBuffer dst)
    throws IOException
  {
    return read(fd, dst);
  }
  
  /**
   * Reads into byte buffers directly using the supplied file descriptor.
   * Assumes that the buffer list contains DirectBuffers.  Will perform a
   * scattering read.
   * 
   * @param fd Native file descriptor to read from.
   * @param dsts An array direct byte buffers.
   * @param offset Index of the first buffer to read to.
   * @param length The number of buffers to read to.
   * @return Number of bytes read.
   * @throws IOException If an error occurs or the dsts are not direct buffers. 
   */
  native long readScattering(int fd, ByteBuffer[] dsts, int offset, int length)
    throws IOException;

  public long readScattering(ByteBuffer[] dsts, int offset, int length)
    throws IOException
  {
    if (offset + length > dsts.length)
      throw new IndexOutOfBoundsException("offset + length > dsts.length");
    
    return readScattering(fd, dsts, offset, length);
  }
  
  /**
   * Writes from a direct byte bufer using the supplied file descriptor.
   * Assumes the buffer is a DirectBuffer.
   * 
   * @param fd
   * @param src
   * @return Number of bytes written.
   * @throws IOException
   */
  native int write(int fd, ByteBuffer src)
    throws IOException;

  public int write(ByteBuffer src)
    throws IOException
  {
    return write(fd, src);
  }
  
  /**
   * Writes from byte buffers directly using the supplied file descriptor.
   * Assumes the that buffer list constains DirectBuffers.  Will perform
   * as gathering write.
   * 
   * @param fd
   * @param srcs
   * @param offset
   * @param length
   * @return Number of bytes written.
   * @throws IOException
   */
  native long writeGathering(int fd, ByteBuffer[] srcs, int offset, int length)
    throws IOException;
  
  public long writeGathering(ByteBuffer[] srcs, int offset, int length)
    throws IOException
  {
    if (offset + length > srcs.length)
      throw new IndexOutOfBoundsException("offset + length > srcs.length");
    
    return writeGathering(fd, srcs, offset, length);
  }
  
  private native static void initIDs();

}
