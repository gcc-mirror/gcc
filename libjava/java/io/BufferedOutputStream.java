/* BufferedOutputStream.java -- Buffer output into large blocks before writing
   Copyright (C) 1998, 2000, 2003 Free Software Foundation, Inc.

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
  * This class accumulates bytes written in a buffer instead of immediately
  * writing the data to the underlying output sink. The bytes are instead
  * as one large block when the buffer is filled, or when the stream is
  * closed or explicitly flushed. This mode operation can provide a more
  * efficient mechanism for writing versus doing numerous small unbuffered
  * writes.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class BufferedOutputStream extends FilterOutputStream
{
  /**
   * This is the default buffer size
   */
  private static final int DEFAULT_BUFFER_SIZE = 512;

  /**
   * This is the internal byte array used for buffering output before
   * writing it.
   */
  protected byte[] buf;

  /**
   * This is the number of bytes that are currently in the buffer and
   * are waiting to be written to the underlying stream.  It always points to
   * the index into the buffer where the next byte of data will be stored
   */
  protected int count;

  /**
   * This method initializes a new <code>BufferedOutputStream</code> instance
   * that will write to the specified subordinate <code>OutputStream</code>
   * and which will use a default buffer size of 512 bytes.
   *
   * @param out The underlying <code>OutputStream</code> to write data to
   */
  public BufferedOutputStream(OutputStream out)
  {
    this(out, DEFAULT_BUFFER_SIZE);
  }

  /**
   * This method initializes a new <code>BufferedOutputStream</code> instance
   * that will write to the specified subordinate <code>OutputStream</code>
   * and which will use the specified buffer size
   *
   * @param out The underlying <code>OutputStream</code> to write data to
   * @param size The size of the internal buffer
   */
  public BufferedOutputStream(OutputStream out, int size)
  {
    super(out);

    buf = new byte[size];
  }

  /**
   * This method causes any currently buffered bytes to be immediately
   * written to the underlying output stream.
   *
   * @exception IOException If an error occurs
   */
  public synchronized void flush() throws IOException
  {
    if (count == 0)
      return;

    out.write(buf, 0, count);
    count = 0;
    out.flush();
  }

  /**
   * This method flushes any remaining buffered bytes then closes the 
   * underlying output stream.  Any further attempts to write to this stream
   * may throw an exception
   *
  public synchronized void close() throws IOException
  {
    flush();
    out.close();
  }
  */

  /**
   * This method runs when the object is garbage collected.  It is 
   * responsible for ensuring that all buffered bytes are written and
   * for closing the underlying stream.
   *
   * @exception IOException If an error occurs (ignored by the Java runtime)
   *
  protected void finalize() throws IOException
  {
    close();
  }
  */

  /**
   * This method writes a single byte of data.  This will be written to the
   * buffer instead of the underlying data source.  However, if the buffer
   * is filled as a result of this write request, it will be flushed to the
   * underlying output stream.
   *
   * @param b The byte of data to be written, passed as an int
   *
   * @exception IOException If an error occurs
   */
  public synchronized void write(int b) throws IOException
  {
    if (count == buf.length)
      flush();

    buf[count] = (byte)(b & 0xFF);
    ++count;
  }

  /**
   * This method writes <code>len</code> bytes from the byte array 
   * <code>buf</code> starting at position <code>offset</code> in the buffer. 
   * These bytes will be written to the internal buffer.  However, if this
   * write operation fills the buffer, the buffer will be flushed to the
   * underlying output stream.
   *
   * @param buf The array of bytes to write.
   * @param offset The index into the byte array to start writing from.
   * @param len The number of bytes to write.
   *
   * @exception IOException If an error occurs
   */
  public synchronized void write(byte[] buf, int offset, int len) 
    throws IOException
  {
    // Buffer can hold everything.  Note that the case where LEN < 0
    // is automatically handled by the downstream write.
    if (len < (this.buf.length - count))
      {
        System.arraycopy(buf, offset, this.buf, count, len);
        count += len;
      }
    else
      {
        // The write was too big.  So flush the buffer and write the new
        // bytes directly to the underlying stream, per the JDK 1.2
        // docs.
        flush();
        out.write (buf, offset, len);
      }
  }

} // class BufferedOutputStream 

