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
    int m;
    public static final MapMode READ_ONLY = new MapMode(0);
    public static final MapMode READ_WRITE = new MapMode(1);
    public static final MapMode PRIVATE = new MapMode(2);

    /**
     * Initializes the MapMode.
     */
    MapMode(int a)
    {
      m = a;
    }

    /**
     * Returns a string representation of the <code>MapMode</code> object.
     */
    public String toString()
    {
      if (this == READ_ONLY)
        return "READ_ONLY";
      else if (this == READ_WRITE)
        return "READ_WRITE";

      return "PRIVATE";
    }
  }

  /**
   * Initializes the channel.
   */
  protected FileChannel()
  {
  }

  /**
   * Maps the file into the memory.
   *
   * @exception IllegalArgumentException If the preconditions on the parameters
   * do not hold.
   * @exception IOException If an I/O error occurs.
   * @exception NonReadableChannelException If mode is READ_ONLY but this channel was
   * not opened for reading.
   * @exception NonWritableChannelException If mode is READ_WRITE or PRIVATE but this
   * channel was not opened for writing.
   */
  public abstract MappedByteBuffer map(MapMode mode, long position, long size)
    throws IOException;

  /**
   * Return the size of the file thus far
   *
   * @exception ClosedChannelException If this channel is closed.
   */
  public abstract long size() throws IOException;

  /**
   * Writes data to the channel.
   *
   * @exception IOException If an I/O error occurs.
   */
  public final long write(ByteBuffer[] srcs) throws IOException
  {
    return write(srcs, 0, srcs.length);
  }

  /**
   * Writes data to the channel.
   *
   * @exception IOException If an I/O error occurs.
   */
  public abstract int write(ByteBuffer src) throws IOException;

  /**
   * Writes data to the channel.
   *
   * @exception AsynchronousCloseException If another thread closes this channel
   * while the transfer is in progress.
   * @exception ClosedByInterruptException If another thread interrupts the
   * current thread while the transfer is in progress, thereby closing both
   * channels and setting the current thread's interrupt status.
   * @exception ClosedChannelException If this channel is closed.
   * @exception IllegalArgumentException If position is negative.
   * @exception IOException If an I/O error occurs.
   * @exception NonWritableChannelException If this channel was not opened for
   * writing.
   */
  public abstract int write(ByteBuffer srcs, long position)
    throws IOException;

  /**
   * Writes data to the channel.
   *
   * @exception IOException If an I/O error occurs.
   */
  public abstract long write(ByteBuffer[] srcs, int offset, int length)
    throws IOException;

  /**
   * Reads data from the channel.
   *
   * @exception IOException If an I/O error occurs.
   */
  public abstract long read(ByteBuffer[] dsts, int offset, int length)
    throws IOException;

  /**
   * Reads data from the channel.
   *
   * @exception IOException If an I/O error occurs.
   */
  public final long read(ByteBuffer[] dsts) throws IOException
  {
    return read(dsts, 0, dsts.length);
  }

  /**
   * Reads data from the channel.
   *
   * @exception IOException If an I/O error occurs.
   */
  public abstract int read(ByteBuffer dst) throws IOException;

  /**
   * Reads data from the channel.
   *
   * @exception AsynchronousCloseException If another thread closes this channel
   * while the transfer is in progress.
   * @exception ClosedByInterruptException If another thread interrupts the
   * current thread while the transfer is in progress, thereby closing both
   * channels and setting the current thread's interrupt status.
   * @exception ClosedChannelException If this channel is closed.
   * @exception IllegalArgumentException If position is negative.
   * @exception IOException If an I/O error occurs.
   * @exception NonReadableChannelException If this channel was not opened for
   * reading.
   */
  public abstract int read(ByteBuffer dst, long position)
    throws IOException;

  /**
   * Closes the channel.
   *
   * This is called from @see close.
   *
   * @exception IOException If an I/O error occurs.
   */
  protected abstract void implCloseChannel() throws IOException;

  /**
   * msync with the disk
   *
   * @exception ClosedChannelException If this channel is closed.
   * @exception IOException If an I/O error occurs.
   */
  public abstract void force(boolean metaData) throws IOException;

  /**
   * Creates a file lock for the whole associated file.
   *
   * @exception AsynchronousCloseException If another thread closes this channel
   * while the transfer is in progress.
   * @exception ClosedChannelException If this channel is closed.
   * @exception FileLockInterruptionException If the invoking thread is
   * interrupted while blocked in this method.
   * @exception IOException If an I/O error occurs.
   * @exception NonReadableChannelException If shared is true and this channel
   * was not opened for reading.
   * @exception NonWritableChannelException If shared is false and this channel
   * was not opened for writing.
   * @exception OverlappingFileLockException If a lock that overlaps the
   * requested region is already held by this Java virtual machine, or if
   * another thread is already blocked in this method and is attempting to lock
   * an overlapping region.
   */
  public final FileLock lock() throws IOException
  {
    return lock(0, Long.MAX_VALUE, false);
  }

  /**
   * Creates a file lock for a region of the associated file.
   *
   * @exception AsynchronousCloseException If another thread closes this channel
   * while the transfer is in progress.
   * @exception ClosedChannelException If this channel is closed.
   * @exception FileLockInterruptionException If the invoking thread is
   * interrupted while blocked in this method.
   * @exception IllegalArgumentException If the preconditions on the parameters
   * do not hold.
   * @exception IOException If an I/O error occurs.
   * @exception OverlappingFileLockException If a lock that overlaps the
   * requested region is already held by this Java virtual machine, or if
   * another thread is already blocked in this method and is attempting to lock
   * an overlapping region.
   * @exception NonReadableChannelException If shared is true and this channel
   * was not opened for reading.
   * @exception NonWritableChannelException If shared is false and this channel
   * was not opened for writing.
   */
  public abstract FileLock lock(long position, long size, boolean shared)
    throws IOException;

  /**
   * Tries to aqquire alock on the whole associated file.
   *
   * @exception ClosedChannelException If this channel is closed.
   * @exception IOException If an I/O error occurs.
   * @exception OverlappingFileLockException If a lock that overlaps the
   * requested region is already held by this Java virtual machine, or if
   * another thread is already blocked in this method and is attempting to lock
   * an overlapping region.
   */
  public final FileLock tryLock() throws IOException
  {
    return tryLock(0, Long.MAX_VALUE, false);
  }

  /**
   * Tries to aqquire a lock on a region of the associated file.
   *
   * @exception ClosedChannelException If this channel is closed.
   * @exception IllegalArgumentException If the preconditions on the parameters
   * do not hold.
   * @exception IOException If an I/O error occurs.
   * @exception OverlappingFileLockException If a lock that overlaps the
   * requested region is already held by this Java virtual machine, or if
   * another thread is already blocked in this method and is attempting to lock
   * an overlapping region.
   */
  public abstract FileLock tryLock(long position, long size, boolean shared)
    throws IOException;

  /**
   * Returns the current position on the file.
   *
   * @exception ClosedChannelException If this channel is closed.
   * @exception IOException If an I/O error occurs.
   */
  public abstract long position() throws IOException;

  /**
   * Sets the position of the channel on the assoziated file.
   *
   * @exception ClosedChannelException If this channel is closed.
   * @exception IllegalArgumentException If newPosition is negative.
   * @exception IOException If an I/O error occurs.
   */
  public abstract FileChannel position(long newPosition)
    throws IOException;

  /**
   * Transfers bytes from this channel's file to the given writable byte
   * channel.
   *
   * @exception AsynchronousCloseException If another thread closes this channel
   * while the transfer is in progress.
   * @exception ClosedByInterruptException If another thread interrupts the
   * current thread while the transfer is in progress, thereby closing both
   * channels and setting the current thread's interrupt status.
   * @exception ClosedChannelException If this channel is closed.
   * @exception IllegalArgumentException If the preconditions on the parameters
   * do not hold.
   * @exception IOException If an I/O error occurs.
   * @exception NonReadableChannelException If this channel was not opened for
   * reading.
   * @exception NonWritableChannelException If the target channel was not
   * opened for writing.
   */
  public abstract long transferTo(long position, long count,
                                  WritableByteChannel target)
    throws IOException;

  /**
   * Transfers bytes from the given readable channel into this channel.
   *
   * @exception AsynchronousCloseException If another thread closes this channel
   * while the transfer is in progress.
   * @exception ClosedByInterruptException If another thread interrupts the
   * current thread while the transfer is in progress, thereby closing both
   * channels and setting the current thread's interrupt status.
   * @exception ClosedChannelException If this channel is closed.
   * @exception IllegalArgumentException If the preconditions on the parameters
   * do not hold.
   * @exception IOException If an I/O error occurs.
   * @exception NonReadableChannelException If the source channel was not
   * opened for reading.
   * @exception NonWritableChannelException If this channel was not opened for
   * writing.
   */
  public abstract long transferFrom(ReadableByteChannel src, long position,
                                    long count) throws IOException;

  /**
   * Truncates the channel's file at <code>size</code>.
   *
   * @exception ClosedChannelException If this channel is closed.
   * @exception IllegalArgumentException If size is negative.
   * @exception IOException If an I/O error occurs.
   * @exception NonWritableChannelException If this channel was not opened for
   * writing.
   */
  public abstract FileChannel truncate(long size) throws IOException;
}
