/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.awt;

import java.awt.image.*;

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
      case DataBuffer.TYPE_USHORT:
	return new DataBufferUShort(size, numBanks);
      case DataBuffer.TYPE_INT:
	return new DataBufferInt(size, numBanks);
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
      case DataBuffer.TYPE_USHORT:
	return new DataBufferUShort((short[]) data, size);
      case DataBuffer.TYPE_INT:
	return new DataBufferInt((int[]) data, size);
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
    if (buffer instanceof DataBufferUShort)
      return ((DataBufferUShort) buffer).getData();
    if (buffer instanceof DataBufferInt)
      return ((DataBufferInt) buffer).getData();
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
