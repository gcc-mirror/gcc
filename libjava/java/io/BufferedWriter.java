/* BufferedWriter.java -- Buffer output into large blocks before writing
   Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

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

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * Status:  Complete to version 1.1.
 */

/**
  * This class accumulates chars written in a buffer instead of immediately
  * writing the data to the underlying output sink. The chars are instead
  * as one large block when the buffer is filled, or when the stream is
  * closed or explicitly flushed. This mode operation can provide a more
  * efficient mechanism for writing versus doing numerous small unbuffered
  * writes.
  *
  * @version 0.0
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Tom Tromey <tromey@cygnus.com>
  * @date September 25, 1998 
  */

public class BufferedWriter extends Writer
{
  /**
   * This method initializes a new <code>BufferedWriter</code> instance
   * that will write to the specified subordinate <code>Writer</code>
   * and which will use a default buffer size of 512 chars.
   *
   * @param out The underlying <code>Writer</code> to write data to
   */
  public BufferedWriter (Writer out)
  {
    this (out, DEFAULT_BUFFER_SIZE);
  }

  /**
   * This method initializes a new <code>BufferedWriter</code> instance
   * that will write to the specified subordinate <code>Writer</code>
   * and which will use the specified buffer size
   *
   * @param out The underlying <code>Writer</code> to write data to
   * @param size The size of the internal buffer
   */
  public BufferedWriter (Writer ox, int size)
  {
    super (ox);
    out = ox;
    buffer = new char[size];
    count = 0;
  }

  /**
   * This method flushes any remaining buffered chars then closes the 
   * underlying output stream.  Any further attempts to write to this stream
   * may throw an exception
   */
  public void close () throws IOException
  {
    synchronized (lock)
      {
	// It is safe to call localFlush even if the stream is already
	// closed.
	localFlush ();
	out.close();
	buffer = null;
      }
  }

  /**
   * This method causes any currently buffered chars to be immediately
   * written to the underlying output stream.
   *
   * @exception IOException If an error occurs
   */
  public void flush () throws IOException
  {
    synchronized (lock)
      {
	if (buffer == null)
	  throw new IOException ("Stream closed");
	localFlush ();
	out.flush();
      }
  }

  /**
   * This method writes out a system depedent line separator sequence.  The
   * actual value written is detemined from the <xmp>line.separator</xmp>
   * system property.
   *
   * @exception IOException If an error occurs
   */
  public void newLine () throws IOException
  {
    write (System.getProperty("line.separator"));
  }

  /**
   * This method writes a single char of data.  This will be written to the
   * buffer instead of the underlying data source.  However, if the buffer
   * is filled as a result of this write request, it will be flushed to the
   * underlying output stream.
   *
   * @param b The char of data to be written, passed as an int
   *
   * @exception IOException If an error occurs
   */
  public void write (int oneChar) throws IOException
  {
    synchronized (lock)
      {
	if (buffer == null)
	  throw new IOException ("Stream closed");
	buffer[count++] = (char) oneChar;
	if (count == buffer.length)
	  localFlush ();
      }
  }

  /**
   * This method writes <code>len</code> chars from the char array 
   * <code>buf</code> starting at position <code>offset</code> in the buffer. 
   * These chars will be written to the internal buffer.  However, if this
   * write operation fills the buffer, the buffer will be flushed to the
   * underlying output stream.
   *
   * @param buf The array of chars to write.
   * @param offset The index into the char array to start writing from.
   * @param len The number of chars to write.
   *
   * @exception IOException If an error occurs
   */
  public void write (char[] buf, int offset, int len) throws IOException
  {
    synchronized (lock)
      {
	if (buffer == null)
	  throw new IOException ("Stream closed");

	// Bypass buffering if there is too much incoming data.
	if (count + len > buffer.length)
	  {
	    localFlush ();
	    out.write(buf, offset, len);
	  }
	else
	  {
	    System.arraycopy(buf, offset, buffer, count, len);
	    count += len;
	    if (count == buffer.length)
	      localFlush ();
	  }
      }
  }

  /**
   * This method writes <code>len</code> chars from the <code>String</code>
   * <code>str</code> starting at position <code>offset</code> in the string. 
   * These chars will be written to the internal buffer.  However, if this
   * write operation fills the buffer, the buffer will be flushed to the
   * underlying output stream.
   *
   * @param str The <code>String</code> to write.
   * @param offset The index into the string to start writing from.
   * @param len The number of chars to write.
   *
   * @exception IOException If an error occurs
   */
  public void write (String str, int offset, int len) throws IOException
  {
    synchronized (lock)
      {
	if (buffer == null)
	  throw new IOException ("Stream closed");

	if (count + len > buffer.length)
	  {
	    localFlush ();
	    out.write(str, offset, len);
	  }
	else
	  {
	    str.getChars(offset, offset + len, buffer, count);
	    count += len;
	    if (count == buffer.length)
	      localFlush ();
	  }
      }
  }

  // This should only be called with the lock held.
  private final void localFlush () throws IOException
  {
    if (count > 0)
      {
	out.write(buffer, 0, count);
	count = 0;
      }
  }

  /**
   * This is the underlying <code>Writer</code> to which this object
   * sends its output.
   */
  private Writer out;

  /**
   * This is the internal char array used for buffering output before
   * writing it.
   */
  char[] buffer;

  /**
   * This is the number of chars that are currently in the buffer and
   * are waiting to be written to the underlying stream.  It always points to
   * the index into the buffer where the next char of data will be stored
   */
  int count;

  /**
   * This is the default buffer size
   */
  private static final int DEFAULT_BUFFER_SIZE = 8192;
}
