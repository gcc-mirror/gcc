/* ByteArrayInputStream.java -- Read an array as a stream
   Copyright (C) 1998, 1999, 2001, 2005  Free Software Foundation, Inc.

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
  * This class permits an array of bytes to be read as an input stream.
  *
  * @author Warren Levy (warrenl@cygnus.com)
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */ 
public class ByteArrayInputStream extends InputStream
{
  /**
   * The array that contains the data supplied during read operations
   */
  protected byte[] buf;

  /**
   * The array index of the next byte to be read from the buffer
   * <code>buf</code>
   */
  protected int pos;

  /**
   * The currently marked position in the stream.  This defaults to 0, so a
   * reset operation on the stream resets it to read from array index 0 in
   * the buffer - even if the stream was initially created with an offset
   * greater than 0
   */
  protected int mark;

  /**
   * This indicates the maximum number of bytes that can be read from this
   * stream.  It is the array index of the position after the last valid
   * byte in the buffer <code>buf</code>
   */
  protected int count;

  /**
   * Create a new ByteArrayInputStream that will read bytes from the passed
   * in byte array.  This stream will read from the beginning to the end
   * of the array.  It is identical to calling an overloaded constructor
   * as <code>ByteArrayInputStream(buf, 0, buf.length)</code>.
   * <p>
   * Note that this array is not copied.  If its contents are changed 
   * while this stream is being read, those changes will be reflected in the
   * bytes supplied to the reader.  Please use caution in changing the 
   * contents of the buffer while this stream is open.
   *
   * @param buffer The byte array buffer this stream will read from.
   */
  public ByteArrayInputStream(byte[] buffer)
  {
    this(buffer, 0, buffer.length);
  }

  /**
   * Create a new ByteArrayInputStream that will read bytes from the
   * passed in byte array.  This stream will read from position
   * <code>offset</code> in the array for a length of
   * <code>length</code> bytes past <code>offset</code>.  If the
   * stream is reset to a position before <code>offset</code> then
   * more than <code>length</code> bytes can be read from the stream.
   * The <code>length</code> value should be viewed as the array index
   * one greater than the last position in the buffer to read.
   * <p>
   * Note that this array is not copied.  If its contents are changed 
   * while this stream is being read, those changes will be reflected in the
   * bytes supplied to the reader.  Please use caution in changing the 
   * contents of the buffer while this stream is open.
   *
   * @param buffer The byte array buffer this stream will read from.
   * @param offset The index into the buffer to start reading bytes from
   * @param length The number of bytes to read from the buffer
   */
  public ByteArrayInputStream(byte[] buffer, int offset, int length)
  {
    if (offset < 0  || length < 0 || offset > buffer.length)
      throw new IllegalArgumentException();

    buf = buffer;

    count = offset + length;
    if (count > buf.length)
      count = buf.length;

    pos = offset;
    mark = pos;
  }

  /**
   * This method returns the number of bytes available to be read from this
   * stream.  The value returned will be equal to <code>count - pos</code>.
   *
   * @return The number of bytes that can be read from this stream
   * before blocking, which is all of them 
   */
  public synchronized int available()
  {
    return count - pos;
  }

  /**
   * This method sets the mark position in this stream to the current
   * position.  Note that the <code>readlimit</code> parameter in this
   * method does nothing as this stream is always capable of
   * remembering all the bytes int it.
   * <p>
   * Note that in this class the mark position is set by default to
   * position 0 in the stream.  This is in constrast to some other
   * stream types where there is no default mark position.
   *
   * @param readLimit The number of bytes this stream must remember.
   * This parameter is ignored.
   */
  public synchronized void mark(int readLimit)
  {
    // readLimit is ignored per Java Class Lib. book, p.220.
    mark = pos;
  }

  /**
   * This method overrides the <code>markSupported</code> method in
   * <code>InputStream</code> in order to return <code>true</code> -
   * indicating that this stream class supports mark/reset
   * functionality.
   *
   * @return <code>true</code> to indicate that this class supports
   * mark/reset.
   */
  public boolean markSupported()
  {
    return true;
  }

  /**
   * This method reads one byte from the stream.  The <code>pos</code>
   * counter is advanced to the next byte to be read.  The byte read is
   * returned as an int in the range of 0-255.  If the stream position
   * is already at the end of the buffer, no byte is read and a -1 is
   * returned in order to indicate the end of the stream.
   *
   * @return The byte read, or -1 if end of stream
   */
  public synchronized int read()
  {
    if (pos < count)
      return ((int) buf[pos++]) & 0xFF;
    return -1;
  }

  /**
   * This method reads bytes from the stream and stores them into a
   * caller supplied buffer.  It starts storing the data at index
   * <code>offset</code> into the buffer and attempts to read
   * <code>len</code> bytes.  This method can return before reading
   * the number of bytes requested if the end of the stream is
   * encountered first.  The actual number of bytes read is returned.
   * If no bytes can be read because the stream is already at the end
   * of stream position, a -1 is returned.
   * <p>
   * This method does not block.
   *
   * @param buffer The array into which the bytes read should be stored.
   * @param offset The offset into the array to start storing bytes
   * @param length The requested number of bytes to read
   *
   * @return The actual number of bytes read, or -1 if end of stream.
   */
  public synchronized int read(byte[] buffer, int offset, int length)
  {
    if (pos >= count)
      return -1;

    int numBytes = Math.min(count - pos, length);
    System.arraycopy(buf, pos, buffer, offset, numBytes);
    pos += numBytes;
    return numBytes;
  }

  /**
   * This method sets the read position in the stream to the mark
   * point by setting the <code>pos</code> variable equal to the
   * <code>mark</code> variable.  Since a mark can be set anywhere in
   * the array, the mark/reset methods int this class can be used to
   * provide random search capabilities for this type of stream.
   */
  public synchronized void reset()
  {
    pos = mark;
  }

  /**
   * This method attempts to skip the requested number of bytes in the
   * input stream.  It does this by advancing the <code>pos</code>
   * value by the specified number of bytes.  It this would exceed the
   * length of the buffer, then only enough bytes are skipped to
   * position the stream at the end of the buffer.  The actual number
   * of bytes skipped is returned.
   *
   * @param num The requested number of bytes to skip
   *
   * @return The actual number of bytes skipped.
   */
  public synchronized long skip(long num)
  {
    // Even though the var numBytes is a long, in reality it can never
    // be larger than an int since the result of subtracting 2 positive
    // ints will always fit in an int.  Since we have to return a long
    // anyway, numBytes might as well just be a long.
    long numBytes = Math.min((long) (count - pos), num < 0 ? 0L : num);
    pos += numBytes;
    return numBytes;
  }
}
