/* StringBufferInputStream.java -- Read an String as a stream
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

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.  Deprecated in JDK 1.1.
 */
 
/**
  * This class permits a <code>String</code> to be read as an input stream.
  * The low eight bits of each character in the <code>String</code> are the
  * bytes that are returned. The high eight bits of each character are
  * discarded.
  * <p>
  * The mark/reset functionality in this class behaves differently than
  * normal.  The <code>mark()</code> method is always ignored and the 
  * <code>reset()</code> method always resets in stream to start reading from 
  * position 0 in the String.  Note that since this method does not override 
  * <code>markSupported()</code> in <code>InputStream</code>, calling that 
  * method will return <code>false</code>.
  * <p>
  * Note that this class is deprecated because it does not properly handle
  * 16-bit Java characters.  It is provided for backwards compatibility only
  * and should not be used for new development.  The <code>StringReader</code>
  * class should be used instead.
  *
  * @deprecated
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Warren Levy (warrenl@cygnus.com)
  */
public class StringBufferInputStream extends InputStream
{
  /** The String which is the input to this stream. */
  protected String buffer;

  /** Position of the next byte in buffer to be read. */
  protected int pos = 0;

  /** The length of the String buffer. */
  protected int count;

 /**
  * Create a new <code>StringBufferInputStream</code> that will read bytes
  * from the passed in <code>String</code>.  This stream will read from the
  * beginning to the end of the <code>String</code>.
  *
  * @param s The <code>String</code> this stream will read from.
  */
  public StringBufferInputStream(String s)
  {
    buffer = s;
    count = s.length();
  }

 /**
  * This method returns the number of bytes available to be read from this
  * stream.  The value returned will be equal to <code>count - pos</code>.
  *
  * @return The number of bytes that can be read from this stream before
  * blocking, which is all of them
  */
  public int available()
  {
    return count - pos;
  }

 /**
  * This method reads one byte from the stream.  The <code>pos</code> counter 
  * is advanced to the next byte to be read.  The byte read is returned as
  * an int in the range of 0-255.  If the stream position is already at the
  * end of the buffer, no byte is read and a -1 is returned in order to
  * indicate the end of the stream.
  *
  * @return The byte read, or -1 if end of stream
  */
  public int read()
  {
    if (pos >= count)
      return -1;	// EOF

    return ((int) buffer.charAt(pos++)) & 0xFF;
  }

/**
  * This method reads bytes from the stream and stores them into a caller
  * supplied buffer.  It starts storing the data at index <code>offset</code> 
  * into the buffer and attempts to read <code>len</code> bytes.  This method
  * can return before reading the number of bytes requested if the end of the
  * stream is encountered first.  The actual number of bytes read is 
  * returned.  If no bytes can be read because the stream is already at 
  * the end of stream position, a -1 is returned.
  * <p>
  * This method does not block.
  *
  * @param b The array into which the bytes read should be stored.
  * @param off The offset into the array to start storing bytes
  * @param len The requested number of bytes to read
  *
  * @return The actual number of bytes read, or -1 if end of stream.
  */
  public int read(byte[] b, int off, int len)
  {
    if (off < 0 || len < 0 || off + len > b.length)
      throw new ArrayIndexOutOfBoundsException();

    if (pos >= count)
      return -1;	// EOF

    int numRead = Math.min(len, count - pos);
    if (numRead < 0)
      return 0;

    buffer.getBytes(pos, pos + numRead, b, off);
    pos += numRead;
    return numRead;
  }

 /**
  * This method sets the read position in the stream to the beginning
  * setting the <code>pos</code> variable equal to 0.  Note that this differs
  * from the common implementation of the <code>reset()</code> method.
  */
  public void reset()
  {
    pos = 0;
  }

 /**
  * This method attempts to skip the requested number of bytes in the
  * input stream.  It does this by advancing the <code>pos</code> value by the
  * specified number of bytes.  It this would exceed the length of the
  * buffer, then only enough bytes are skipped to position the stream at
  * the end of the buffer.  The actual number of bytes skipped is returned.
  *
  * @param n The requested number of bytes to skip
  *
  * @return The actual number of bytes skipped.
  */
  public long skip(long n)
  {
    if (n < 0)
      return 0L;

    long actualSkip = Math.min(n, count - pos);
    pos += actualSkip;
    return actualSkip;
  }
}
