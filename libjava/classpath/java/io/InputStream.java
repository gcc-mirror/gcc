/* InputStream.java -- Base class for input
   Copyright (C) 1998, 1999, 2001, 2004, 2005 Free Software Foundation, Inc.

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

/**
  * This abstract class forms the base of the hierarchy of classes that read
  * input as a stream of bytes.  It provides a common set of methods for
  * reading bytes from streams.  Subclasses implement and extend these
  * methods to read bytes from a particular input source such as a file
  * or network connection.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Warren Levy (warrenl@cygnus.com)
  */
public abstract class InputStream implements Closeable
{
  /**
   * Default, no-arg, public constructor
   */
  public InputStream()
  {
  }

  /**
   * This method returns the number of bytes that can be read from this
   * stream before a read can block.  A return of 0 indicates that blocking
   * might (or might not) occur on the very next read attempt.
   * <p>
   * This method always returns 0 in this class
   *
   * @return The number of bytes that can be read before blocking could occur
   *
   * @exception IOException If an error occurs
   */
  public int available() throws IOException
  {
    return 0;
  }

  /**
   * This method closes the stream.  Any futher attempts to read from the
   * stream may generate an <code>IOException</code>
   * <p>
   * This method does nothing in this class, but subclasses may override
   * this method in order to provide additional functionality.
   *
   * @exception IOException If an error occurs, which can only happen
   * in a subclass
   */
  public void close() throws IOException
  {
    // Do nothing
  }

  /**
   * This method marks a position in the input to which the stream can
   * be "reset" by calling the <code>reset()</code> method.  The
   * parameter @code{readlimit} is the number of bytes that can be read
   * from the stream after setting the mark before the mark becomes
   * invalid.  For example, if <code>mark()</code> is called with a
   * read limit of 10, then when 11 bytes of data are read from the
   * stream before the <code>reset()</code> method is called, then the
   * mark is invalid and the stream object instance is not required to
   * remember the mark.
   * <p>
   * This method does nothing in this class, but subclasses may override it
   * to provide mark/reset functionality.
   *
   * @param readLimit The number of bytes that can be read before the
   *                  mark becomes invalid
   */
  public void mark(int readLimit)
  {
    // Do nothing
  }

  /**
   * This method returns a boolean that indicates whether the mark/reset
   * methods are supported in this class.  Those methods can be used to
   * remember a specific point in the stream and reset the stream to that
   * point.
   * <p>
   * This method always returns <code>false</code> in this class, but
   * subclasses can override this method to return <code>true</code>
   * if they support mark/reset functionality.
   *
   * @return <code>true</code> if mark/reset functionality is
   * supported, <code>false</code> otherwise 
   */
  public boolean markSupported()
  {
    return false;
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
  public abstract int read() throws IOException;

  /**
   * This method reads bytes from a stream and stores them into a caller
   * supplied buffer.  This method attempts to completely fill the buffer,
   * but can return before doing so.  The actual number of bytes read is
   * returned as an int.  A -1 is returned to indicate the end of the stream.
   * <p>
   * This method will block until some data can be read.
   * <p>
   * This method operates by calling an overloaded read method like so:
   * <code>read(b, 0, b.length)</code>
   *
   * @param b The buffer into which the bytes read will be stored.
   *
   * @return The number of bytes read or -1 if end of stream.
   *
   * @exception IOException If an error occurs.
   */
  public int read(byte[] b) throws IOException
  {
    return read(b, 0, b.length);
  }

  /**
   * This method read bytes from a stream and stores them into a
   * caller supplied buffer.  It starts storing the data at index
   * <code>off</code> into the buffer and attempts to read
   * <code>len</code> bytes.  This method can return before reading the
   * number of bytes requested.  The actual number of bytes read is
   * returned as an int.  A -1 is returned to indicate the end of the
   * stream.
   *  <p>
   * This method will block until some data can be read.
   * <p>
   * This method operates by calling the single byte <code>read()</code> method
   * in a loop until the desired number of bytes are read.  The read loop
   * stops short if the end of the stream is encountered or if an IOException
   * is encountered on any read operation except the first.  If the first
   * attempt to read a bytes fails, the IOException is allowed to propagate
   * upward.  And subsequent IOException is caught and treated identically
   * to an end of stream condition.  Subclasses can (and should if possible)
   * override this method to provide a more efficient implementation.
   *
   * @param b The array into which the bytes read should be stored
   * @param off The offset into the array to start storing bytes
   * @param len The requested number of bytes to read
   *
   * @return The actual number of bytes read, or -1 if end of stream.
   *
   * @exception IOException If an error occurs.
   */
  public int read(byte[] b, int off, int len) throws IOException
  {
    if (off < 0 || len < 0 || b.length - off < len)
      throw new IndexOutOfBoundsException();

    int i, ch;

    for (i = 0; i < len; ++i)
      try
	{
	  if ((ch = read()) < 0)
	    return i == 0 ? -1 : i;		// EOF
	  b[off + i] = (byte) ch;
	}
      catch (IOException ex)
	{
	  // Only reading the first byte should cause an IOException.
	  if (i == 0)
	    throw ex;
	  return i;
	}

    return i;
  }

  /**
   * This method resets a stream to the point where the
   * <code>mark()</code> method was called.  Any bytes that were read
   * after the mark point was set will be re-read during subsequent
   * reads.
   * <p>
   * This method always throws an IOException in this class, but subclasses
   * can override this method if they provide mark/reset functionality.
   *
   * @exception IOException Always thrown for this class
   */
  public void reset() throws IOException
  {
    throw new IOException("mark/reset not supported");
  }

  /**
   * This method skips the specified number of bytes in the stream.  It
   * returns the actual number of bytes skipped, which may be less than the
   * requested amount.
   * <p>
   * This method reads and discards bytes into a byte array until the
   * specified number of bytes were skipped or until either the end of stream
   * is reached or a read attempt returns a short count.  Subclasses can
   * override this metho to provide a more efficient implementation where
   * one exists.
   *
   * @param n The requested number of bytes to skip
   *
   * @return The actual number of bytes skipped.
   *
   * @exception IOException If an error occurs
   */
  public long skip(long n) throws IOException
  {
    // Throw away n bytes by reading them into a temp byte[].
    // Limit the temp array to 2Kb so we don't grab too much memory.
    final int buflen = n > 2048 ? 2048 : (int) n;
    byte[] tmpbuf = new byte[buflen];
    final long origN = n;

    while (n > 0L)
      {
	int numread = read(tmpbuf, 0, n > buflen ? buflen : (int) n);
	if (numread <= 0)
	  break;
	n -= numread;
      }

    return origN - n;
  }
}
