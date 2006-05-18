/* ImageOutputStream.java
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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


package javax.imageio.stream;

import java.io.DataOutput;
import java.io.IOException;


/**
 * An output stream for use by {@link javax.imageio.ImageWriter
 * ImageWriters}.
 *
 * @since 1.4
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public interface ImageOutputStream
  extends ImageInputStream, DataOutput
{
  /**
   * @param position
   *
   * @throws IOException if an errror occurs
   */
  void flushBefore(long position) throws IOException;

  /**
   * Writes an array into the stream.
   *
   * @param data the data to be written
   *
   * @throws IOException if an errror occurs
   */
  void write(byte[] data) throws IOException;

  /**
   * Writes a region of data from an array into the stream.
   *
   * @param data the data to be written
   * @param offset the offset in the array
   * @param len the length in the array
   *
   * @throws IOException if an errror occurs
   */
  void write(byte[] data, int offset, int len) throws IOException;

  /**
   * Writes an <code>int</code> into the stream.
   *
   * @param data the data to be written
   *
   * @throws IOException if an errror occurs
   */
  void write(int data) throws IOException;

  /**
   * Writes a bit value to the stream.
   *
   * @throws IOException if an error occurs
   */
  void writeBit(int bit) throws IOException;

  /**
   * Writes a number of bit values to the stream.
   *
   * @throws IOException if an errror occurs
   */
  void writeBits(long bits, int numBits) throws IOException;

  /**
   * Writes a <code>boolean</code> value into the stream.
   *
   * @param data the data to be written
   *
   * @throws IOException if an errror occurs
   */
  void writeBoolean(boolean data) throws IOException;

  /**
   * Writes a <code>byte</code> value into the stream.
   *
   * @param data the data to be written
   *
   * @throws IOException if an errror occurs
   */
  void writeByte(int data) throws IOException;

  /**
   * @param data the data to be written
   *
   * @throws IOException if an errror occurs
   */
  void writeBytes(String data) throws IOException;

  /**
   * Writes a character into the stream.
   *
   * @param data the data to be written
   *
   * @throws IOException if an errror occurs
   */
  void writeChar(int data) throws IOException;

  /**
   * Writes characters to the stream.
   *
   * @param data the data to be written
   * @param offset the offset in the array
   * @param len the lenth in the array
   *
   * @throws IOException if an errror occurs
   */
  void writeChars(char[] data, int offset, int len) throws IOException;

  /**
   * Writes characters from a given <code>String</code> into the stream.
   *
   * @param data the data to be written
   *
   * @throws IOException if an errror occurs
   */
  void writeChars(String data) throws IOException;

  /**
   * Writes a <code>double</code> into the stream.
   *
   * @param data the data to be written
   *
   * @throws IOException if an errror occurs
   */
  void writeDouble(double data) throws IOException;

  /**
   * Writes an array of <code>double</code> into the stream.
   *
   * @param data the data to be written
   * @param offset the offset in the array
   * @param len the lenth in the array
   *
   * @throws IOException if an errror occurs
   */
  void writeDoubles(double[] data, int offset, int len)
    throws IOException;

  /**
   * Writes a <code>float</code> into the stream.
   *
   * @param data the data to be written
   *
   * @throws IOException if an errror occurs
   */
  void writeFloat(float data) throws IOException;

  /**
   * Writes an array of <code>float</code> into the stream.
   *
   * @param data the data to be written
   * @param offset the offset in the array
   * @param len the lenth in the array
   *
   * @throws IOException if an errror occurs
   */
  void writeFloats(float[] data, int offset, int len) throws IOException;

  /**
   * Writes a <code>int</code> into the stream.
   *
   * @param data the data to be written
   *
   * @throws IOException if an errror occurs
   */
  void writeInt(int data) throws IOException;

  /**
   * Writes an array of <code>int</code> into the stream.
   *
   * @param data the data to be written
   * @param offset the offset in the array
   * @param len the lenth in the array
   *
   * @throws IOException if an errror occurs
   */
  void writeInts(int[] data, int offset, int len) throws IOException;

  /**
   * Writes a <code>long</code> into the stream.
   *
   * @param data the data to be written
   *
   * @throws IOException if an errror occurs
   */
  void writeLong(long data) throws IOException;

  /**
   * Writes an array of <code>long</code> into the stream.
   *
   * @param data the data to be written
   * @param offset the offset in the array
   * @param len the lenth in the array
   *
   * @throws IOException if an errror occurs
   */
  void writeLongs(long[] data, int offset, int len) throws IOException;

  /**
   * Writes a <code>short</code> into the stream.
   *
   * @param data the data to be written
   *
   * @throws IOException if an errror occurs
   */
  void writeShort(int data) throws IOException;

  /**
   * Writes an array of <code>short</code> into the stream.
   *
   * @param data the data to be written
   * @param offset the offset in the array
   * @param len the lenth in the array
   *
   * @throws IOException if an errror occurs
   */
  void writeShorts(short[] data, int offset, int len) throws IOException;

  /**
   * Writes a <code>String</code> into the stream.
   *
   * @param data the data to be written
   *
   * @throws IOException if an errror occurs
   */
  void writeUTF(String data) throws IOException;
}
