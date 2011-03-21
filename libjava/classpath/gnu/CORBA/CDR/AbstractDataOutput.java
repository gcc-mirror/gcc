/* AbstractDataOutput.java --
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
 * An abstract data output stream that could write data in either
 * Big Endian or Little Endian format.
 *
 * This class reuses code from GNU Classpath DataOutputStream.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 * @author Warren Levy (warrenl@cygnus.com)
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public interface AbstractDataOutput
{
  /**
   * This method flushes any unwritten bytes to the underlying stream.
   *
   * @exception IOException If an error occurs.
   */
  void flush()
      throws IOException;

  /**
   * This method writes the specified byte (passed as an <code>int</code>)
   * to the underlying output stream.
   *
   * @param value The <code>byte</code> to write, passed as an <code>int</code>.
   *
   * @exception IOException If an error occurs.
   */
  void write(int value)
      throws IOException;

  /**
   * This method writes <code>len</code> bytes from the specified byte array
   * <code>buf</code> starting at position <code>offset</code> into the
   * buffer to the underlying output stream.
   *
   * @param buf The byte array to write from.
   * @param offset The index into the byte array to start writing from.
   * @param len The number of bytes to write.
   *
   * @exception IOException If an error occurs.
   */
  void write(byte[] buf, int offset, int len)
      throws IOException;

  /**
   * Write the complete byte array.
   * @throws IOException
   */
  void write(byte[] buf)
      throws IOException;

  /**
   * This method writes a Java boolean value to an output stream.  If
   * <code>value</code> is <code>true</code>, a byte with the value of
   * 1 will be written, otherwise a byte with the value of 0 will be
   * written.
   *
   * The value written can be read using the <code>readBoolean</code>
   * method in <code>DataInput</code>.
   *
   * @param value The <code>boolean</code> value to write to the stream
   *
   * @exception IOException If an error occurs
   */
  void writeBoolean(boolean value)
             throws IOException;

  /**
   * This method writes a Java byte value to an output stream.  The
   * byte to be written will be in the lowest 8 bits of the
   * <code>int</code> value passed.
   *
   * The value written can be read using the <code>readByte</code> or
   * <code>readUnsignedByte</code> methods in <code>DataInput</code>.
   *
   * @param value The <code>byte</code> to write to the stream, passed as
   * the low eight bits of an <code>int</code>.
   *
   * @exception IOException If an error occurs
   */
  void writeByte(int value)
          throws IOException;

  /**
   * This method writes a Java short value to an output stream.  The
   * char to be written will be in the lowest 16 bits of the <code>int</code>
   * value passed.
   *
   * @exception IOException If an error occurs
   */
  void writeShort(int value)
           throws IOException;

  /**
   * This method writes a Java char value to an output stream.  The
   * char to be written will be in the lowest 16 bits of the <code>int</code>
   * value passed.
   *
   * @exception IOException If an error occurs
   */
  void writeChar(int value)
          throws IOException;

  /**
   * This method writes a Java int value to an output stream.
   *
   * @param value The <code>int</code> value to write to the stream
   *
   * @exception IOException If an error occurs
   */
  void writeInt(int value)
         throws IOException;

  /**
   * This method writes a Java long value to an output stream.
   *
   * @param value The <code>long</code> value to write to the stream
   *
   * @exception IOException If an error occurs
   */
  void writeLong(long value)
          throws IOException;

  /**
   * This method writes a Java <code>float</code> value to the stream.
   * @param value The <code>float</code> value to write to the stream
   *
   * @exception IOException If an error occurs
   */
  void writeFloat(float value)
           throws IOException;

  /**
   * This method writes a Java <code>double</code> value to the stream.
   *
   * @param value The <code>double</code> value to write to the stream
   *
   * @exception IOException If an error occurs
   */
  void writeDouble(double value)
            throws IOException;
}
