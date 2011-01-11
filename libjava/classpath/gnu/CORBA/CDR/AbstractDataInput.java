/* AbstractDataInput.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package gnu.CORBA.CDR;

import java.io.IOException;

/**
 * Some data input stream that can be either Big or
 * Little Endian.
 *
 * This class reuses code from GNU Classpath DataInputStream.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 * @author Warren Levy (warrenl@cygnus.com)
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public interface AbstractDataInput
{
  /**
   * This method reads bytes from the underlying stream into the specified
   * byte array buffer.  It will attempt to fill the buffer completely, but
   * may return a short count if there is insufficient data remaining to be
   * read to fill the buffer.
   *
   * @param b The buffer into which bytes will be read.
   *
   * @return The actual number of bytes read, or -1 if end of stream reached
   * before reading any bytes.
   *
   * @exception IOException If an error occurs.
   */
  int read(byte[] b)
    throws IOException;

  /**
   * This method reads bytes from the underlying stream into the specified
   * byte array buffer.  It will attempt to read <code>len</code> bytes and
   * will start storing them at position <code>off</code> into the buffer.
   * This method can return a short count if there is insufficient data
   * remaining to be read to complete the desired read length.
   *
   * @param b The buffer into which bytes will be read.
   * @param off The offset into the buffer to start storing bytes.
   * @param len The requested number of bytes to read.
   *
   * @return The actual number of bytes read, or -1 if end of stream reached
   * before reading any bytes.
   *
   * @exception IOException If an error occurs.
   */
  int read(byte[] b, int off, int len)
    throws IOException;

  /**
   * This method reads a Java boolean value from an input stream.  It does
   * so by reading a single byte of data.  If that byte is zero, then the
   * value returned is <code>false</code>.  If the byte is non-zero, then
   * the value returned is <code>true</code>.
   * <p>
   * This method can read a <code>boolean</code> written by an object
   * implementing the <code>writeBoolean()</code> method in the
   * <code>DataOutput</code> interface.
   *
   * @return The <code>boolean</code> value read
   *
   * @exception EOFException If end of file is reached before reading
   * the boolean
   * @exception IOException If any other error occurs
   *
   * @see DataOutput#writeBoolean
   */
  boolean readBoolean()
               throws IOException;

  /**
   * This method reads a Java byte value from an input stream.  The value
   * is in the range of -128 to 127.
   * <p>
   * This method can read a <code>byte</code> written by an object
   * implementing the <code>writeByte()</code> method in the
   * <code>DataOutput</code> interface.
   *
   * @return The <code>byte</code> value read
   *
   * @exception EOFException If end of file is reached before reading the byte
   * @exception IOException If any other error occurs
   *
   * @see DataOutput#writeByte
   */
  byte readByte()
         throws IOException;

  /**
   * This method reads a Java <code>char</code> value from an input stream.
   * It operates by reading two bytes from the stream and converting them to
   * a single 16-bit Java <code>char</code>.  The two bytes are stored most
   * significant byte first (i.e., "big endian") regardless of the native
   * host byte ordering.
   * <p>
   * As an example, if <code>byte1</code> and <code>byte2</code>
   * represent the first and second byte read from the stream
   * respectively, they will be transformed to a <code>char</code> in
   * the following manner:
   * <p>
   * <code>(char)(((byte1 &amp; 0xFF) &lt;&lt; 8) | (byte2 &amp; 0xFF)</code>
   * <p>
   * This method can read a <code>char</code> written by an object
   * implementing the <code>writeChar()</code> method in the
   * <code>DataOutput</code> interface.
   *
   * @return The <code>char</code> value read
   *
   * @exception EOFException If end of file is reached before reading the char
   * @exception IOException If any other error occurs
   *
   * @see DataOutput#writeChar
   */
  char readChar()
         throws IOException;

  /**
   * This method reads a Java double value from an input stream.  It operates
   * by first reading a <code>long</code> value from the stream by calling the
   * <code>readLong()</code> method in this interface, then converts
   * that <code>long</code> to a <code>double</code> using the
   * <code>longBitsToDouble</code> method in the class
   * <code>java.lang.Double</code>
   * <p>
   * This method can read a <code>double</code> written by an object
   * implementing the <code>writeDouble()</code> method in the
   * <code>DataOutput</code> interface.
   *
   * @return The <code>double</code> value read
   *
   * @exception EOFException If end of file is reached before reading
   * the double
   * @exception IOException If any other error occurs
   *
   * @see DataOutput#writeDouble
   * @see java.lang.Double#longBitsToDouble
   */
  double readDouble()
             throws IOException;

  /**
   * This method reads a Java float value from an input stream.  It
   * operates by first reading an <code>int</code> value from the
   * stream by calling the <code>readInt()</code> method in this
   * interface, then converts that <code>int</code> to a
   * <code>float</code> using the <code>intBitsToFloat</code> method
   * in the class <code>java.lang.Float</code>
   * <p>
   * This method can read a <code>float</code> written by an object
   * implementing the <code>writeFloat()</code> method in the
   * <code>DataOutput</code> interface.
   *
   * @return The <code>float</code> value read
   *
   * @exception EOFException If end of file is reached before reading the float
   * @exception IOException If any other error occurs
   *
   * @see DataOutput#writeFloat
   * @see java.lang.Float#intBitsToFloat
   */
  float readFloat()
           throws IOException;

  /**
   * This method reads raw bytes into the passed array until the array is
   * full.  Note that this method blocks until the data is available and
   * throws an exception if there is not enough data left in the stream to
   * fill the buffer.  Note also that zero length buffers are permitted.
   * In this case, the method will return immediately without reading any
   * bytes from the stream.
   *
   * @param b The buffer into which to read the data
   *
   * @exception EOFException If end of file is reached before filling the
   * buffer
   * @exception IOException If any other error occurs
   */
  void readFully(byte[] b)
          throws IOException;

  /**
   * This method reads a Java <code>int</code> value from an input stream
   * It operates by reading four bytes from the stream and converting them to
   * a single Java <code>int</code>.  The bytes are stored most
   * significant byte first (i.e., "big endian") regardless of the native
   * host byte ordering.
   * <p>
   * As an example, if <code>byte1</code> through <code>byte4</code> represent
   * the first four bytes read from the stream, they will be
   * transformed to an <code>int</code> in the following manner:
   * <p>
   * <code>(int)(((byte1 &amp; 0xFF) &lt;&lt; 24) + ((byte2 &amp; 0xFF) &lt;&lt; 16) +
   * ((byte3 &amp; 0xFF)&lt;&lt; 8) + (byte4 &amp; 0xFF)))</code>
   * <p>
   * The value returned is in the range of -2147483648 to 2147483647.
   * <p>
   * This method can read an <code>int</code> written by an object
   * implementing the <code>writeInt()</code> method in the
   * <code>DataOutput</code> interface.
   *
   * @return The <code>int</code> value read
   *
   * @exception EOFException If end of file is reached before reading the int
   * @exception IOException If any other error occurs
   *
   * @see DataOutput#writeInt
   */
  int readInt()
       throws IOException;

  /**
   * This method reads a Java <code>long</code> value from an input stream
   * It operates by reading eight bytes from the stream and converting them to
   * a single Java <code>long</code>.  The bytes are stored most
   * significant byte first (i.e., "big endian") regardless of the native
   * host byte ordering.
   * <p>
   * As an example, if <code>byte1</code> through <code>byte8</code> represent
   * the first eight bytes read from the stream, they will be
   * transformed to an <code>long</code> in the following manner:
   * <p>
   * <code>(long)(((byte1 &amp; 0xFF) &lt;&lt; 56) + ((byte2 &amp; 0xFF) &lt;&lt; 48) +
   * ((byte3 &amp; 0xFF) &lt;&lt; 40) + ((byte4 &amp; 0xFF) &lt;&lt; 32) +
   * ((byte5 &amp; 0xFF) &lt;&lt; 24) + ((byte6 &amp; 0xFF) &lt;&lt; 16) +
   * ((byte7 &amp; 0xFF) &lt;&lt; 8) + (byte8 &amp; 0xFF)))
   * </code>
   * <p>
   * The value returned is in the range of -9223372036854775808 to
   * 9223372036854775807.
   * <p>
   * This method can read an <code>long</code> written by an object
   * implementing the <code>writeLong()</code> method in the
   * <code>DataOutput</code> interface.
   *
   * @return The <code>long</code> value read
   *
   * @exception EOFException If end of file is reached before reading the long
   * @exception IOException If any other error occurs
   *
   * @see DataOutput#writeLong
   */
  long readLong()
         throws IOException;

  /**
   * This method reads a signed 16-bit value into a Java in from the
   * stream.  It operates by reading two bytes from the stream and
   * converting them to a single 16-bit Java <code>short</code>.  The
   * two bytes are stored most significant byte first (i.e., "big
   * endian") regardless of the native host byte ordering.
   * <p>
   * As an example, if <code>byte1</code> and <code>byte2</code>
   * represent the first and second byte read from the stream
   * respectively, they will be transformed to a <code>short</code>. in
   * the following manner:
   * <p>
   * <code>(short)(((byte1 &amp; 0xFF) &lt;&lt; 8) | (byte2 &amp; 0xFF))</code>
   * <p>
   * The value returned is in the range of -32768 to 32767.
   * <p>
   * This method can read a <code>short</code> written by an object
   * implementing the <code>writeShort()</code> method in the
   * <code>DataOutput</code> interface.
   *
   * @return The <code>short</code> value read
   *
   * @exception EOFException If end of file is reached before reading the value
   * @exception IOException If any other error occurs
   *
   * @see DataOutput#writeShort
   */
  short readShort()
           throws IOException;

  /**
   * This method reads 8 unsigned bits into a Java <code>int</code>
   * value from the stream. The value returned is in the range of 0 to
   * 255.
   * <p>
   * This method can read an unsigned byte written by an object
   * implementing the <code>writeUnsignedByte()</code> method in the
   * <code>DataOutput</code> interface.
   *
   * @return The unsigned bytes value read as a Java <code>int</code>.
   *
   * @exception EOFException If end of file is reached before reading the value
   * @exception IOException If any other error occurs
   *
   * @see DataOutput#writeByte
   */
  int readUnsignedByte()
                throws IOException;

  /**
   * This method reads 16 unsigned bits into a Java int value from the stream.
   * It operates by reading two bytes from the stream and converting them to
   * a single Java <code>int</code>  The two bytes are stored most
   * significant byte first (i.e., "big endian") regardless of the native
   * host byte ordering.
   * <p>
   * As an example, if <code>byte1</code> and <code>byte2</code>
   * represent the first and second byte read from the stream
   * respectively, they will be transformed to an <code>int</code> in
   * the following manner:
   * <p>
   * <code>(int)(((byte1 &amp; 0xFF) &lt;&lt; 8) + (byte2 &amp; 0xFF))</code>
   * <p>
   * The value returned is in the range of 0 to 65535.
   * <p>
   * This method can read an unsigned short written by an object
   * implementing the <code>writeUnsignedShort()</code> method in the
   * <code>DataOutput</code> interface.
   *
   * @return The unsigned short value read as a Java <code>int</code>
   *
   * @exception EOFException If end of file is reached before reading the value
   * @exception IOException If any other error occurs
   *
   * @see DataOutput#writeShort
   */
  int readUnsignedShort()
                 throws IOException;

  /**
   * Read a single byte.
   *
   * @return a byte, extracted from the stream or -1 if
   * EOF has been reached.
   * @throws IOException
   */
  public int read()
           throws IOException;

  /**
   * This method attempts to skip and discard the specified number of bytes
   * in the input stream.  It may actually skip fewer bytes than requested.
   * This method will not skip any bytes if passed a negative number of bytes
   * to skip.
   *
   * @param n The requested number of bytes to skip.
   *
   * @return The requested number of bytes to skip.
   *
   * @exception IOException If an error occurs.
   * @specnote The JDK docs claim that this returns the number of bytes
   *  actually skipped. The JCL claims that this method can throw an
   *  EOFException. Neither of these appear to be true in the JDK 1.3's
   *  implementation. This tries to implement the actual JDK behaviour.
   */
  int skipBytes(int n)
         throws IOException;
}
