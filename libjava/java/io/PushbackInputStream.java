/* PushbackInputStream.java -- An input stream that can unread bytes
   Copyright (C) 1998, 1999, 2001, 2002, 2005  Free Software Foundation, Inc.

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

package java.io;

/**
  * This subclass of <code>FilterInputStream</code> provides the ability to 
  * unread data from a stream.  It maintains an internal buffer of unread
  * data that is supplied to the next read operation.  This is conceptually
  * similar to mark/reset functionality, except that in this case the 
  * position to reset the stream to does not need to be known in advance.
  * <p>
  * The default pushback buffer size one byte, but this can be overridden
  * by the creator of the stream.
  * <p>
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Warren Levy (warrenl@cygnus.com)
  */
public class PushbackInputStream extends FilterInputStream
{
  /**
   * This is the default buffer size
   */
  private static final int DEFAULT_BUFFER_SIZE = 1;

  /**
   * This is the buffer that is used to store the pushed back data
   */
  protected byte[] buf;

  /**
   * This is the position in the buffer from which the next byte will be
   * read.  Bytes are stored in reverse order in the buffer, starting from
   * <code>buf[buf.length - 1]</code> to <code>buf[0]</code>.  Thus when 
   * <code>pos</code> is 0 the buffer is full and <code>buf.length</code> when 
   * it is empty
   */
  protected int pos;

  /**
   * This method initializes a <code>PushbackInputStream</code> to
   * read from the specified subordinate <code>InputStream</code>
   * with a default pushback buffer size of 1.
   *
   * @param in The subordinate stream to read from
   */
  public PushbackInputStream(InputStream in)
  {
    this(in, DEFAULT_BUFFER_SIZE);
  }

  /**
   * This method initializes a <code>PushbackInputStream</code> to
   * read from the specified subordinate <code>InputStream</code> with
   * the specified buffer size
   *
   * @param in The subordinate <code>InputStream</code> to read from
   * @param size The pushback buffer size to use
   */
  public PushbackInputStream(InputStream in, int size)
  {
    super(in);
    if (size < 0)
      throw new IllegalArgumentException();
    buf = new byte[size];
    pos = buf.length;
  }

  /**
   * This method returns the number of bytes that can be read from this
   * stream before a read can block.  A return of 0 indicates that blocking
   * might (or might not) occur on the very next read attempt.
   * <p>
   * This method will return the number of bytes available from the
   * pushback buffer plus the number of bytes available from the 
   * underlying stream.
   *
   * @return The number of bytes that can be read before blocking could occur
   *
   * @exception IOException If an error occurs
   */
  public int available() throws IOException
  {
    return (buf.length - pos) + super.available();
  }

  /**
   * This method closes the stream and releases any associated resources.
   * 
   * @exception IOException If an error occurs.
   */
  public synchronized void close() throws IOException
  {
    buf = null;
    super.close();
  }

  /**
   * This method returns <code>false</code> to indicate that it does
   * not support mark/reset functionality.
   *
   * @return This method returns <code>false</code> to indicate that
   * this class does not support mark/reset functionality
   */
  public boolean markSupported()
  {
    return false;
  }

  /**
   * This method always throws an IOException in this class because
   * mark/reset functionality is not supported.
   *
   * @exception IOException Always thrown for this class
   */
  public void reset() throws IOException
  {
    throw new IOException("Mark not supported in this class");
  }

  /**
   * This method reads an unsigned byte from the input stream and returns it
   * as an int in the range of 0-255.  This method also will return -1 if
   * the end of the stream has been reached.  The byte returned will be read
   * from the pushback buffer, unless the buffer is empty, in which case
   * the byte will be read from the underlying stream.
   * <p>
   * This method will block until the byte can be read.
   *
   * @return The byte read or -1 if end of stream
   *
   * @exception IOException If an error occurs
   */
  public synchronized int read() throws IOException
  {
    if (pos < buf.length)
      return ((int) buf[pos++]) & 0xFF;

    return super.read();
  }

  /**
   * This method read bytes from a stream and stores them into a
   * caller supplied buffer.  It starts storing the data at index
   * <code>offset</code> into the buffer and attempts to read
   * <code>len</code> bytes.  This method can return before reading the
   * number of bytes requested.  The actual number of bytes read is
   * returned as an int.  A -1 is returned to indicate the end of the
   * stream.
   *  <p>
   * This method will block until some data can be read.
   * <p>
   * This method first reads bytes from the pushback buffer in order to 
   * satisfy the read request.  If the pushback buffer cannot provide all
   * of the bytes requested, the remaining bytes are read from the 
   * underlying stream.
   *
   * @param b The array into which the bytes read should be stored
   * @param off The offset into the array to start storing bytes
   * @param len The requested number of bytes to read
   *
   * @return The actual number of bytes read, or -1 if end of stream.
   *
   * @exception IOException If an error occurs.
   */
  public synchronized int read(byte[] b, int off, int len) throws IOException
  {
    int numBytes = Math.min(buf.length - pos, len);

    if (numBytes > 0)
      {
	System.arraycopy (buf, pos, b, off, numBytes);
	pos += numBytes;
	len -= numBytes;
	off += numBytes;
      }

    if (len > 0) 
      {
        len = super.read(b, off, len);
        if (len == -1) //EOF
	  return numBytes > 0 ? numBytes : -1;
	numBytes += len;
      }
    return numBytes;
  }

  /**
   * This method pushes a single byte of data into the pushback buffer.
   * The byte pushed back is the one that will be returned as the first byte
   * of the next read.
   * <p>
   * If the pushback buffer is full, this method throws an exception.
   * <p>
   * The argument to this method is an <code>int</code>.  Only the low
   * eight bits of this value are pushed back.
   *
   * @param b The byte to be pushed back, passed as an int
   *
   * @exception IOException If the pushback buffer is full.
   */
  public synchronized void unread(int b) throws IOException
  {
    if (pos <= 0)
      throw new IOException("Insufficient space in pushback buffer");

    buf[--pos] = (byte) b;
  }

  /**
   * This method pushes all of the bytes in the passed byte array into 
   * the pushback bfer.  These bytes are pushed in reverse order so that
   * the next byte read from the stream after this operation will be
   * <code>b[0]</code> followed by <code>b[1]</code>, etc.
   * <p>
   * If the pushback buffer cannot hold all of the requested bytes, an
   * exception is thrown.
   *
   * @param b The byte array to be pushed back
   *
   * @exception IOException If the pushback buffer is full
   */
  public synchronized void unread(byte[] b) throws IOException
  {
    unread(b, 0, b.length);
  }

  /**
   * This method pushed back bytes from the passed in array into the
   * pushback buffer.  The bytes from <code>b[offset]</code> to
   * <code>b[offset + len]</code> are pushed in reverse order so that
   * the next byte read from the stream after this operation will be
   * <code>b[offset]</code> followed by <code>b[offset + 1]</code>,
   * etc.
   * <p>
   * If the pushback buffer cannot hold all of the requested bytes, an
   * exception is thrown.
   *
   * @param b The byte array to be pushed back
   * @param off The index into the array where the bytes to be push start
   * @param len The number of bytes to be pushed.
   *
   * @exception IOException If the pushback buffer is full
   */
  public synchronized void unread(byte[] b, int off, int len)
    throws IOException
  {
    if (pos < len)
      throw new IOException("Insufficient space in pushback buffer");

    // Note the order that these bytes are being added is the opposite
    // of what would be done if they were added to the buffer one at a time.
    // See the Java Class Libraries book p. 1390.
    System.arraycopy(b, off, buf, pos - len, len);

    // Don't put this into the arraycopy above, an exception might be thrown
    // and in that case we don't want to modify pos.
    pos -= len;
  }

  /**
   * This method skips the specified number of bytes in the stream.  It
   * returns the actual number of bytes skipped, which may be less than the
   * requested amount.
   * <p>
   * This method first discards bytes from the buffer, then calls the
   * <code>skip</code> method on the underlying <code>InputStream</code> to 
   * skip additional bytes if necessary.
   *
   * @param n The requested number of bytes to skip
   *
   * @return The actual number of bytes skipped.
   *
   * @exception IOException If an error occurs
   *
   * @since 1.2
   */
  public synchronized long skip(long n) throws IOException
  {
    final long origN = n;

    if (n > 0L)
      {
	int numread = (int) Math.min((long) (buf.length - pos), n);
	pos += numread;
	n -= numread;
	if (n > 0)
	  n -= super.skip(n);
      }

    return origN - n;
  }
}
