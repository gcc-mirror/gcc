/* FileChannelImpl.java --
   Copyright (C) 2002, 2004, 2005, 2006  Free Software Foundation, Inc.

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
import gnu.java.nio.FileLockImpl;
import gnu.java.nio.VMChannel;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
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
public final class FileChannelImpl extends FileChannel
{
  // These are mode values for open().
  public static final int READ   = 1;
  public static final int WRITE  = 2;
  public static final int APPEND = 4;

  // EXCL is used only when making a temp file.
  public static final int EXCL   = 8;
  public static final int SYNC   = 16;
  public static final int DSYNC  = 32;

  public static final FileChannelImpl in;
  public static final FileChannelImpl out;
  public static final FileChannelImpl err;

  //private static native void init();

  static
  {
    if (Configuration.INIT_LOAD_LIBRARY)
      {
        System.loadLibrary("javanio");
      }

    //init();

    FileChannelImpl ch = null;
    try
      {
        ch = new FileChannelImpl(VMChannel.getStdin(), READ);
      }
    catch (IOException ioe)
      {
        throw new Error(ioe);
      }
    in = ch;

    ch = null;
    try
      {
        ch = new FileChannelImpl(VMChannel.getStdout(), WRITE);
      }
    catch (IOException ioe)
      {
        throw new Error(ioe);
      }
    out = ch;

    ch = null;
    try
      {
        ch = new FileChannelImpl(VMChannel.getStderr(), WRITE);
      }
    catch (IOException ioe)
      {
        throw new Error(ioe);
      }
    err = ch;
  }

  /**
   * This is the actual native file descriptor value
   */
  private VMChannel ch;

  private int mode;

  final String description;

  /* Open a file.  MODE is a combination of the above mode flags. */
  /* This is a static factory method, so that VM implementors can decide
   * substitute subclasses of FileChannelImpl. */
  public static FileChannelImpl create(File file, int mode)
    throws IOException
  {
    return new FileChannelImpl(file, mode);
  }

  private FileChannelImpl(File file, int mode)
    throws IOException
  {
    String path = file.getPath();
    description = path;
    this.mode = mode;
    this.ch = new VMChannel();
    ch.openFile(path, mode);

    // First open the file and then check if it is a a directory
    // to avoid race condition.
    if (file.isDirectory())
      {
        try
          {
            close();
          }
        catch (IOException e)
          {
            /* ignore it */
          }

        throw new FileNotFoundException(description + " is a directory");
      }
  }

  /**
   * Constructor for default channels in, out and err.
   *
   * Used by init() (native code).
   *
   * @param fd the file descriptor (0, 1, 2 for stdin, stdout, stderr).
   *
   * @param mode READ or WRITE
   */
  FileChannelImpl (VMChannel ch, int mode)
  {
    this.mode = mode;
    this.description = "descriptor(" + ch.getState() + ")";
    this.ch = ch;
  }

  public int available() throws IOException
  {
    return ch.available();
  }

  private long implPosition() throws IOException
  {
    return ch.position();
  }

  private void seek(long newPosition) throws IOException
  {
    ch.seek(newPosition);
  }

  private void implTruncate(long size) throws IOException
  {
    ch.truncate(size);
  }

  public void unlock(long pos, long len) throws IOException
  {
    ch.unlock(pos, len);
  }

  public long size () throws IOException
  {
    return ch.size();
  }

  protected void implCloseChannel() throws IOException
  {
    ch.close();
  }

  /**
   * Makes sure the Channel is properly closed.
   */
  protected void finalize() throws IOException
  {
    if (ch.getState().isValid())
      close();
  }

  public int read (ByteBuffer dst) throws IOException
  {
    return ch.read(dst);
  }

  public int read (ByteBuffer dst, long position)
    throws IOException
  {
    if (position < 0)
      throw new IllegalArgumentException ("position: " + position);
    long oldPosition = implPosition ();
    position (position);
    int result = read(dst);
    position (oldPosition);

    return result;
  }

  public int read() throws IOException
  {
    return ch.read();
  }

  public long read (ByteBuffer[] dsts, int offset, int length)
    throws IOException
  {
    return ch.readScattering(dsts, offset, length);
  }

  public int write (ByteBuffer src) throws IOException
  {
    return ch.write(src);
  }

  public int write (ByteBuffer src, long position)
    throws IOException
  {
    if (position < 0)
      throw new IllegalArgumentException ("position: " + position);

    if (!isOpen ())
      throw new ClosedChannelException ();

    if ((mode & WRITE) == 0)
       throw new NonWritableChannelException ();

    int result;
    long oldPosition;

    oldPosition = implPosition ();
    seek (position);
    result = write(src);
    seek (oldPosition);

    return result;
  }

  public void write (int b) throws IOException
  {
    ch.write(b);
  }

  public long write(ByteBuffer[] srcs, int offset, int length)
    throws IOException
  {
    return ch.writeGathering(srcs, offset, length);
  }

  public MappedByteBuffer map (FileChannel.MapMode mode,
                               long position, long size)
    throws IOException
  {
    char nmode = 0;
    if (mode == MapMode.READ_ONLY)
      {
        nmode = 'r';
        if ((this.mode & READ) == 0)
          throw new NonReadableChannelException();
      }
    else if (mode == MapMode.READ_WRITE || mode == MapMode.PRIVATE)
      {
        nmode = mode == MapMode.READ_WRITE ? '+' : 'c';
        if ((this.mode & WRITE) != WRITE)
          throw new NonWritableChannelException();
        if ((this.mode & READ) != READ)
          throw new NonReadableChannelException();
      }
    else
      throw new IllegalArgumentException ("mode: " + mode);

    if (position < 0 || size < 0 || size > Integer.MAX_VALUE)
      throw new IllegalArgumentException ("position: " + position
                                          + ", size: " + size);
    return ch.map(nmode, position, (int) size);
  }

  /**
   * msync with the disk
   */
  public void force (boolean metaData) throws IOException
  {
    if (!isOpen ())
      throw new ClosedChannelException ();

    ch.flush(metaData);
  }

  // like transferTo, but with a count of less than 2Gbytes
  private int smallTransferTo (long position, int count,
                               WritableByteChannel target)
    throws IOException
  {
    ByteBuffer buffer;
    try
      {
        // Try to use a mapped buffer if we can.  If this fails for
        // any reason we'll fall back to using a ByteBuffer.
        buffer = map (MapMode.READ_ONLY, position, count);
      }
    catch (IOException e)
      {
        buffer = ByteBuffer.allocate (count);
        read (buffer, position);
        buffer.flip();
      }

    return target.write (buffer);
  }

  public long transferTo (long position, long count,
                          WritableByteChannel target)
    throws IOException
  {
    if (position < 0
        || count < 0)
      throw new IllegalArgumentException ("position: " + position
                                          + ", count: " + count);

    if (!isOpen ())
      throw new ClosedChannelException ();

    if ((mode & READ) == 0)
       throw new NonReadableChannelException ();

    final int pageSize = 65536;
    long total = 0;

    while (count > 0)
      {
        int transferred
          = smallTransferTo (position, (int)Math.min (count, pageSize),
                             target);
        if (transferred < 0)
          break;
        total += transferred;
        position += transferred;
        count -= transferred;
      }

    return total;
  }

  // like transferFrom, but with a count of less than 2Gbytes
  private int smallTransferFrom (ReadableByteChannel src, long position,
                                 int count)
    throws IOException
  {
    ByteBuffer buffer = null;

    if (src instanceof FileChannel)
      {
        try
          {
            // Try to use a mapped buffer if we can.  If this fails
            // for any reason we'll fall back to using a ByteBuffer.
            buffer = ((FileChannel)src).map (MapMode.READ_ONLY, position,
                                             count);
          }
        catch (IOException e)
          {
          }
      }

    if (buffer == null)
      {
        buffer = ByteBuffer.allocate (count);
        src.read (buffer);
        buffer.flip();
      }

    return write (buffer, position);
  }

  public long transferFrom (ReadableByteChannel src, long position,
                            long count)
    throws IOException
  {
    if (position < 0
        || count < 0)
      throw new IllegalArgumentException ("position: " + position
                                          + ", count: " + count);

    if (!isOpen ())
      throw new ClosedChannelException ();

    if ((mode & WRITE) == 0)
       throw new NonWritableChannelException ();

    final int pageSize = 65536;
    long total = 0;

    while (count > 0)
      {
        int transferred = smallTransferFrom (src, position,
                                             (int)Math.min (count, pageSize));
        if (transferred < 0)
          break;
        total += transferred;
        position += transferred;
        count -= transferred;
      }

    return total;
  }

  // Shared sanity checks between lock and tryLock methods.
  private void lockCheck(long position, long size, boolean shared)
    throws IOException
  {
    if (position < 0
        || size < 0)
      throw new IllegalArgumentException ("position: " + position
                                          + ", size: " + size);

    if (!isOpen ())
      throw new ClosedChannelException();

    if (shared && ((mode & READ) == 0))
      throw new NonReadableChannelException();

    if (!shared && ((mode & WRITE) == 0))
      throw new NonWritableChannelException();
  }

  public FileLock tryLock (long position, long size, boolean shared)
    throws IOException
  {
    lockCheck(position, size, shared);

    boolean completed = false;
    try
      {
        begin();
        boolean lockable = ch.lock(position, size, shared, false);
        completed = true;
        return (lockable
                ? new FileLockImpl(this, position, size, shared)
                : null);
      }
    finally
      {
        end(completed);
      }
  }

  public FileLock lock (long position, long size, boolean shared)
    throws IOException
  {
    lockCheck(position, size, shared);

    boolean completed = false;
    try
      {
        boolean lockable = ch.lock(position, size, shared, true);
        completed = true;
        return (lockable
                ? new FileLockImpl(this, position, size, shared)
                : null);
      }
    finally
      {
        end(completed);
      }
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
      throw new IllegalArgumentException ("newPosition: " + newPosition);

    if (!isOpen ())
      throw new ClosedChannelException ();

    // FIXME note semantics if seeking beyond eof.
    // We should seek lazily - only on a write.
    seek (newPosition);
    return this;
  }

  public FileChannel truncate (long size)
    throws IOException
  {
    if (size < 0)
      throw new IllegalArgumentException ("size: " + size);

    if (!isOpen ())
      throw new ClosedChannelException ();

    if ((mode & WRITE) == 0)
       throw new NonWritableChannelException ();

    if (size < size ())
      implTruncate (size);

    return this;
  }

  public String toString()
  {
    return (super.toString()
            + "[ fd: " + ch.getState()
            + "; mode: " + Integer.toOctalString(mode)
            + "; " + description + " ]");
  }

  /**
   * @return The native file descriptor.
   * /
  public int getNativeFD()
  {
    return fd;
  }*/
}
