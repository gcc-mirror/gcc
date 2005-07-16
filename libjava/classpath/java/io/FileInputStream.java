/* FileInputStream.java -- An input stream that reads from disk files.
   Copyright (C) 1998, 2002, 2003, 2004, 2005  Free Software Foundation, Inc.

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


package java.io;

import gnu.java.nio.channels.FileChannelImpl;

import java.nio.channels.FileChannel;

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */
 
/**
 * This class is a stream that reads its bytes from a file. 
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Warren Levy (warrenl@cygnus.com)
 */
public class FileInputStream extends InputStream
{
  /**
   * This is the native file handle for the file this stream is reading from
   */
  private FileDescriptor fd;

  private FileChannelImpl ch;

  /**
   * This method initializes a <code>FileInputStream</code> to read from the
   * specified named file.  A security check is first made to determine
   * whether or not access to this file is allowed.  This is done by
   * calling the <code>checkRead()</code> method of the 
   * <code>SecurityManager</code>
   * (if one exists) with the name of this file.  An exception is thrown
   * if reading is not allowed.  If the file does not exist, an exception
   * is also thrown.
   *
   * @param name The name of the file this stream should read from
   *
   * @exception SecurityException If read access to the file is not allowed
   * @exception FileNotFoundException If the file does not exist 
   * or if it is a directory
   */
  public FileInputStream(String name) throws FileNotFoundException
  {
    this(new File(name));
  }

  /**
   * This method initializes a <code>FileInputStream</code> to read from the
   * specified <code>File</code> object.  A security check is first
   * made to determine
   * whether or not access to this file is allowed.  This is done by
   * calling the <code>checkRead()</code> method of the
   * <code>SecurityManager</code>
   * (if one exists) with the name of this file.  An exception is thrown
   * if reading is not allowed.  If the file does not exist, an exception
   * is also thrown.
   *
   * @param file The <code>File</code> object this stream should read from
   *
   * @exception SecurityException If read access to the file is not allowed
   * @exception FileNotFoundException If the file does not exist
   * or if it is a directory.
   */
  public FileInputStream(File file) throws FileNotFoundException
  {
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkRead(file.getPath());

    ch = FileChannelImpl.create(file, FileChannelImpl.READ);
  }

  /**
   * This method initializes a <code>FileInputStream</code> to read from the
   * specified <code>FileDescriptor</code> object.  A security
   * check is first made to
   * determine whether or not access to this file is allowed.  This is done by
   * calling the <code>checkRead()</code> method of the 
   * <code>SecurityManager</code>
   * (if one exists) with the specified <code>FileDescriptor</code>  
   * An exception is 
   * thrown if reading is not allowed.
   *
   * @param fdObj The <code>FileDescriptor</code> object this stream 
   * should read from
   *
   * @exception SecurityException If read access to the file is not allowed
   */
  public FileInputStream(FileDescriptor fdObj)
  {
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkRead(fdObj);

    fd = fdObj;
    ch = (FileChannelImpl) fdObj.channel;
  }

  FileInputStream(FileChannelImpl ch)
  {
    this.ch = ch;
  }

  /**
   * This method returns the number of bytes that can be read from this
   * stream before a read can block.  A return of 0 indicates that blocking
   * might (or might not) occur on the very next read attempt.
   * <p>
   * This method returns the number of unread bytes remaining in the file if
   * the descriptor being read from is an actual file.  If this method is
   * reading from a ''special'' file such a the standard input, this method
   * will return the appropriate value for the stream being read.
   * <p>
   * Be aware that reads on plain files that do not reside locally might
   * possibly block even if this method says they should not.  For example,
   * a remote server might crash, preventing an NFS mounted file from being
   * read.
   *
   * @return The number of bytes that can be read before blocking could occur
   *
   * @exception IOException If an error occurs
   */
  public int available() throws IOException
  {
    return ch.available();
  }

  /**
   * This method closes the stream.  Any futher attempts to read from the
   * stream will likely generate an IOException since the underlying file
   * will be closed.
   *
   * @exception IOException If an error occurs.
   */
  public void close() throws IOException
  {
    ch.close();
  }

  protected void finalize() throws IOException
  {
    // We don't actually need this, but we include it because it is
    // mentioned in the JCL.
  }

  /**
   * This method returns a <code>FileDescriptor</code> object representing the
   * underlying native file handle of the file this stream is reading
   * from
   *
   * @return A <code>FileDescriptor</code> for this stream
   *
   * @exception IOException If an error occurs
   */
  public final FileDescriptor getFD() throws IOException
  {
    synchronized (this)
      {
	if (fd == null)
	  fd = new FileDescriptor (ch);
	return fd;
      }
  }

  /**
   * This method reads an unsigned byte from the input stream and returns it
   * as an int in the range of 0-255.  This method also will return -1 if
   * the end of the stream has been reached.
   * <p>
   * This method will block until the byte can be read.
   *
   * @return The byte read or -1 if end of stream
   *
   * @exception IOException If an error occurs
   */
  public int read() throws IOException
  {
    return ch.read();
  }

  /**
   * This method reads bytes from a stream and stores them into a caller
   * supplied buffer.  This method attempts to completely fill the buffer,
   * but can return before doing so.  The actual number of bytes read is
   * returned as an int.  A -1 is returned to indicate the end of the stream.
   * <p>
   * This method will block until some data can be read.
   * <p>
   * This method operates by calling an overloaded read method like so:
   * <code>read(buf, 0, buf.length)</code>
   *
   * @param buf The buffer into which the bytes read will be stored.
   *
   * @return The number of bytes read or -1 if end of stream.
   *
   * @exception IOException If an error occurs.
   */
  public int read(byte[] buf) throws IOException
  {
    return read(buf, 0, buf.length);
  }

  /**
   * This method read bytes from a stream and stores them into a caller
   * supplied buffer.  It starts storing the data at index 
   * <code>offset</code> into
   * the buffer and attempts to read <code>len</code> bytes.  This method can
   * return before reading the number of bytes requested.  The actual number
   * of bytes read is returned as an int.  A -1 is returned to indicate the
   * end of the stream.
   * <p>
   * This method will block until some data can be read.
   *
   * @param buf The array into which the bytes read should be stored
   * @param offset The offset into the array to start storing bytes
   * @param len The requested number of bytes to read
   *
   * @return The actual number of bytes read, or -1 if end of stream.
   *
   * @exception IOException If an error occurs.
   */
  public int read(byte[] buf, int offset, int len) throws IOException
  {
    if (offset < 0
        || len < 0
        || offset + len > buf.length)
      throw new ArrayIndexOutOfBoundsException();

    return ch.read(buf, offset, len);
  }

  /**
   * This method skips the specified number of bytes in the stream.  It
   * returns the actual number of bytes skipped, which may be less than the
   * requested amount.
   * <p>
   * @param numBytes The requested number of bytes to skip
   *
   * @return The actual number of bytes skipped.
   *
   * @exception IOException If an error occurs
   */
  public synchronized long skip (long numBytes) throws IOException
  {
    if (numBytes < 0)
      throw new IllegalArgumentException ("Can't skip negative bytes: " + 
                                          numBytes);

    if (numBytes == 0)
      return 0;

    long oldPos = ch.position ();
    ch.position(oldPos + numBytes);
    return ch.position() - oldPos;
  }

  /**
   * This method creates a java.nio.channels.FileChannel.
   * Nio does not allow one to create a file channel directly.
   * A file channel must be created by first creating an instance of
   * Input/Output/RandomAccessFile and invoking the getChannel() method on it.
   */
  public synchronized FileChannel getChannel () 
  {
    return ch;
  }

} // class FileInputStream

