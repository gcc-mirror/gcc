/* Buffers.java --
   Copyright (C) 2000, 2002, 2004  Free Software Foundation

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


package gnu.java.awt;

import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;
import java.awt.image.DataBufferDouble;
import java.awt.image.DataBufferFloat;
import java.awt.image.DataBufferInt;
import java.awt.image.DataBufferShort;
import java.awt.image.DataBufferUShort;

/** 
 * Utility class for creating and accessing data buffers of arbitrary
 * data types.
 */
public final class Buffers
{
  /**
   * Create a data buffer of a particular type.
   *
   * @param dataType the desired data type of the buffer.
   * @param data an array containing data, or null
   * @param size the size of the data buffer bank
   */
  public static DataBuffer createBuffer(int dataType, Object data,
					int size)
  {
    if (data == null) return createBuffer(dataType, size, 1);

    return createBufferFromData(dataType, data, size);
  }


  /**
   * Create a data buffer of a particular type.
   *
   * @param dataType the desired data type of the buffer.
   * @param size the size of the data buffer bank
   */
  public static DataBuffer createBuffer(int dataType, int size) {
    return createBuffer(dataType, size, 1);
  }

  /**
   * Create a data buffer of a particular type.
   *
   * @param dataType the desired data type of the buffer.
   * @param size the size of the data buffer bank
   * @param numBanks the number of banks the buffer should have
   */
  public static DataBuffer createBuffer(int dataType, int size, int numBanks)
  {
    switch (dataType)
      {
      case DataBuffer.TYPE_BYTE:
	return new DataBufferByte(size, numBanks);
      case DataBuffer.TYPE_SHORT:
	return new DataBufferShort(size, numBanks);
      case DataBuffer.TYPE_USHORT:
	return new DataBufferUShort(size, numBanks);
      case DataBuffer.TYPE_INT:
	return new DataBufferInt(size, numBanks);
      case DataBuffer.TYPE_FLOAT:
	return new DataBufferFloat(size, numBanks);
      case DataBuffer.TYPE_DOUBLE:
	return new DataBufferDouble(size, numBanks);
      default:
	throw new UnsupportedOperationException();
      }
  }
  
  /**
   * Create a data buffer of a particular type.
   *
   * @param dataType the desired data type of the buffer
   * @param data an array containing the data
   * @param size the size of the data buffer bank
   */
  public static DataBuffer createBufferFromData(int dataType, Object data,
						int size)
  {
    switch (dataType)
      {
      case DataBuffer.TYPE_BYTE:
	return new DataBufferByte((byte[]) data, size);
      case DataBuffer.TYPE_SHORT:
	return new DataBufferShort((short[]) data, size);
      case DataBuffer.TYPE_USHORT:
	return new DataBufferUShort((short[]) data, size);
      case DataBuffer.TYPE_INT:
	return new DataBufferInt((int[]) data, size);
      case DataBuffer.TYPE_FLOAT:
	return new DataBufferFloat((float[]) data, size);
      case DataBuffer.TYPE_DOUBLE:
	return new DataBufferDouble((double[]) data, size);
      default:
	throw new UnsupportedOperationException();
      }
  }

  /** 
   * Return the data array of a data buffer, regardless of the data
   * type.
   *
   * @return an array of primitive values. The actual array type
   * depends on the data type of the buffer.
   */
  public static Object getData(DataBuffer buffer)
  {
    if (buffer instanceof DataBufferByte)
      return ((DataBufferByte) buffer).getData();

    if (buffer instanceof DataBufferShort)
      return ((DataBufferShort) buffer).getData();

    if (buffer instanceof DataBufferUShort)
      return ((DataBufferUShort) buffer).getData();

    if (buffer instanceof DataBufferInt)
      return ((DataBufferInt) buffer).getData();

    if (buffer instanceof DataBufferFloat)
      return ((DataBufferFloat) buffer).getData();

    if (buffer instanceof DataBufferDouble)
      return ((DataBufferDouble) buffer).getData();

    throw new ClassCastException("Unknown data buffer type");
  }

    
  /**
   * Copy data from array contained in data buffer, much like
   * System.arraycopy. Create a suitable destination array if the
   * given destination array is null.
   */
  public static Object getData(DataBuffer src, int srcOffset,
			       Object dest,  int destOffset,
			       int length)
  {
    Object from;
    if (src instanceof DataBufferByte)
      {
	from = ((DataBufferByte) src).getData();
	if (dest == null) dest = new byte[length+destOffset];
      }
    else if (src instanceof DataBufferShort)
      {
	from = ((DataBufferShort) src).getData();
	if (dest == null) dest = new short[length+destOffset];
      }
    else if (src instanceof DataBufferUShort)
      {
	from = ((DataBufferUShort) src).getData();
	if (dest == null) dest = new short[length+destOffset];
      }
    else if (src instanceof DataBufferInt)
      {
	from = ((DataBufferInt) src).getData();
	if (dest == null) dest = new int[length+destOffset];
      }
    else if (src instanceof DataBufferFloat)
      {
	from = ((DataBufferFloat) src).getData();
	if (dest == null) dest = new float[length+destOffset];
      }
    else if (src instanceof DataBufferDouble)
      {
	from = ((DataBufferDouble) src).getData();
	if (dest == null) dest = new double[length+destOffset];
      }
    else
      {
	throw new ClassCastException("Unknown data buffer type");
      }
    
    System.arraycopy(from, srcOffset, dest, destOffset, length);
    return dest;
  }
  
  /**
   * @param bits the width of a data element measured in bits
   *
   * @return the smallest data type that can store data elements of
   * the given number of bits, without any truncation.
   */
  public static int smallestAppropriateTransferType(int bits)
  {
    if (bits <= 8)
      {
	return DataBuffer.TYPE_BYTE;
      }
    else if (bits <= 16)
      {
	return DataBuffer.TYPE_USHORT;
      } 
    else if (bits <= 32)
      {
	return DataBuffer.TYPE_INT;
      }
    else
      {
	return DataBuffer.TYPE_UNDEFINED;
      }
  }
}
