/* Copyright (C) 2000, 2002, 2005  Free Software Foundation

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

package java.awt.image;

/**
 * Class that manages arrays of data elements. A data buffer consists
 * of one or more banks.  A bank is a continuous region of data
 * elements.
 *
 * @author Rolf W. Rasmussen (rolfwr@ii.uib.no)
 */
public abstract class DataBuffer
{
  /**
   * A constant representing a data type that uses <code>byte</code> primitives
   * as the storage unit.
   */
  public static final int TYPE_BYTE      =  0;

  /**
   * A constant representing a data type that uses <code>short</code>
   * primitives as the storage unit.
   */
  public static final int TYPE_USHORT    =  1;

  /**
   * A constant representing a data type that uses <code>short</code>
   * primitives as the storage unit.
   */
  public static final int TYPE_SHORT     =  2;

  /**
   * A constant representing a data type that uses <code>int</code>
   * primitives as the storage unit.
   */
  public static final int TYPE_INT       =  3;

  /**
   * A constant representing a data type that uses <code>float</code>
   * primitives as the storage unit.
   */
  public static final int TYPE_FLOAT     =  4;

  /**
   * A constant representing a data type that uses <code>double</code>
   * primitives as the storage unit.
   */
  public static final int TYPE_DOUBLE    =  5;

  /**
   * A constant representing an undefined data type.
   */
  public static final int TYPE_UNDEFINED = 32;

  /** The type of the data elements stored in the data buffer.  */
  protected int dataType;

  /** The number of banks in this buffer.  */
  protected int banks = 1;

  /** Offset into the default (0'th) bank). */
  protected int offset; // FIXME: Is offsets[0] always mirrored in offset?

  /** The size of the banks.  */
  protected int size;

  /** Offset into each bank.  */
  protected int[] offsets;

  /**
   * Creates a new <code>DataBuffer</code> with the specified data type and
   * size.  The <code>dataType</code> should be one of the constants
   * {@link #TYPE_BYTE}, {@link #TYPE_SHORT}, {@link #TYPE_USHORT},
   * {@link #TYPE_INT}, {@link #TYPE_FLOAT} and {@link #TYPE_DOUBLE}.
   * <p>
   * The physical (array-based) storage is allocated by a subclass.
   *
   * @param dataType the data type.
   * @param size the number of elements in the buffer.
   */
  protected DataBuffer(int dataType, int size)
  {
    this(dataType, size, 1);
  }

  /**
   * Creates a new <code>DataBuffer</code> with the specified data type,
   * size and number of banks.  The <code>dataType</code> should be one of
   * the constants {@link #TYPE_BYTE}, {@link #TYPE_SHORT},
   * {@link #TYPE_USHORT}, {@link #TYPE_INT}, {@link #TYPE_FLOAT} and
   * {@link #TYPE_DOUBLE}.
   * <p>
   * The physical (array-based) storage is allocated by a subclass.
   *
   * @param dataType the data type.
   * @param size the number of elements in the buffer.
   * @param numBanks the number of data banks.
   */
  protected DataBuffer(int dataType, int size, int numBanks) {
    this(dataType, size, numBanks, 0);
  }

  /**
   * Creates a new <code>DataBuffer</code> with the specified data type,
   * size and number of banks.  An offset (which applies to all banks) is
   * also specified.  The <code>dataType</code> should be one of
   * the constants {@link #TYPE_BYTE}, {@link #TYPE_SHORT},
   * {@link #TYPE_USHORT}, {@link #TYPE_INT}, {@link #TYPE_FLOAT} and
   * {@link #TYPE_DOUBLE}.
   * <p>
   * The physical (array-based) storage is allocated by a subclass.
   *
   * @param dataType the data type.
   * @param size the number of elements in the buffer.
   * @param numBanks the number of data banks.
   * @param offset the offset to the first element for all banks.
   */
  protected DataBuffer(int dataType, int size, int numBanks, int offset) {
    banks = numBanks;
    this.dataType = dataType;
    this.size = size;
    this.offset = offset;

    offsets = new int[ numBanks ];
    for(int i = 0; i < numBanks; i++ )
      offsets[i] = offset;
  }

  /**
   * Creates a new <code>DataBuffer</code> with the specified data type,
   * size and number of banks.  An offset (which applies to all banks) is
   * also specified.  The <code>dataType</code> should be one of
   * the constants {@link #TYPE_BYTE}, {@link #TYPE_SHORT},
   * {@link #TYPE_USHORT}, {@link #TYPE_INT}, {@link #TYPE_FLOAT} and
   * {@link #TYPE_DOUBLE}.
   * <p>
   * The physical (array-based) storage is allocated by a subclass.
   *
   * @param dataType the data type.
   * @param size the number of elements in the buffer.
   * @param numBanks the number of data banks.
   * @param offsets the offsets to the first element for all banks.
   *
   * @throws ArrayIndexOutOfBoundsException if
   *         <code>numBanks != offsets.length</code>.
   */
  protected DataBuffer(int dataType, int size, int numBanks, int[] offsets) {
    if (numBanks != offsets.length)
      throw new ArrayIndexOutOfBoundsException();

    this.dataType = dataType;
    this.size = size;
    banks = numBanks;
    this.offsets = offsets;

    offset = offsets[0];
  }

  /**
   * Returns the size (number of bits) of the specified data type. Valid types
   * are defined by the constants {@link #TYPE_BYTE}, {@link #TYPE_SHORT},
   * {@link #TYPE_USHORT}, {@link #TYPE_INT}, {@link #TYPE_FLOAT} and
   * {@link #TYPE_DOUBLE}.
   *
   * @param dataType the data type.
   * @return The number of bits for the specified data type.
   * @throws IllegalArgumentException if <code>dataType < 0</code> or
   *         <code>dataType > TYPE_DOUBLE</code>.
   */
  public static int getDataTypeSize(int dataType) {
    // Maybe this should be a lookup table instead.
    switch (dataType)
      {
      case TYPE_BYTE:
        return 8;
      case TYPE_USHORT:
      case TYPE_SHORT:
        return 16;
      case TYPE_INT:
      case TYPE_FLOAT:
        return 32;
      case TYPE_DOUBLE:
        return 64;
      default:
        throw new IllegalArgumentException();
      }
  }

  /**
   * Returns the type of the data elements in the data buffer.  Valid types
   * are defined by the constants {@link #TYPE_BYTE}, {@link #TYPE_SHORT},
   * {@link #TYPE_USHORT}, {@link #TYPE_INT}, {@link #TYPE_FLOAT} and
   * {@link #TYPE_DOUBLE}.
   *
   * @return The type.
   */
  public int getDataType()
  {
    return dataType;
  }

  /**
   * Returns the size of the data buffer.
   *
   * @return The size.
   */
  public int getSize()
  {
    return size;
  }

  /**
   * Returns the element offset for the first data bank.
   *
   * @return The element offset.
   */
  public int getOffset()
  {
    return offset;
  }

  /**
   * Returns the offsets for all the data banks used by this
   * <code>DataBuffer</code>.
   *
   * @return The offsets.
   */
  public int[] getOffsets()
  {
    if (offsets == null)
    {
      // is this necessary?
      offsets = new int[1];
      offsets[0] = offset;
    }
    return offsets;
  }

  /**
   * Returns the number of data banks for this <code>DataBuffer</code>.
   * @return The number of data banks.
   */
  public int getNumBanks()
  {
    return banks;
  }

  /**
   * Returns an element from the first data bank.  The offset (specified in
   * the constructor) is added to <code>i</code> before accessing the
   * underlying data array.
   *
   * @param i the element index.
   * @return The element.
   */
  public int getElem(int i)
  {
    return getElem(0, i);
  }

  /**
   * Returns an element from a particular data bank.  The offset (specified in
   * the constructor) is added to <code>i</code> before accessing the
   * underlying data array.
   *
   * @param bank the bank index.
   * @param i the element index.
   * @return The element.
   */
  public abstract int getElem(int bank, int i);

  /**
   * Sets an element in the first data bank.  The offset (specified in the
   * constructor) is added to <code>i</code> before updating the underlying
   * data array.
   *
   * @param i the element index.
   * @param val the new element value.
   */
  public void setElem(int i, int val)
  {
    setElem(0, i, val);
  }

  /**
   * Sets an element in a particular data bank.  The offset (specified in the
   * constructor) is added to <code>i</code> before updating the underlying
   * data array.
   *
   * @param bank the data bank index.
   * @param i the element index.
   * @param val the new element value.
   */
  public abstract void setElem(int bank, int i, int val);

  /**
   * Returns an element from the first data bank, converted to a
   * <code>float</code>.  The offset (specified in the constructor) is added
   * to <code>i</code> before accessing the underlying data array.
   *
   * @param i the element index.
   * @return The element.
   */
  public float getElemFloat(int i)
  {
    return getElem(i);
  }

  /**
   * Returns an element from a particular data bank, converted to a
   * <code>float</code>.  The offset (specified in the constructor) is
   * added to <code>i</code> before accessing the underlying data array.
   *
   * @param bank the bank index.
   * @param i the element index.
   * @return The element.
   */
  public float getElemFloat(int bank, int i)
  {
    return getElem(bank, i);
  }

  /**
   * Sets an element in the first data bank.  The offset (specified in the
   * constructor) is added to <code>i</code> before updating the underlying
   * data array.
   *
   * @param i the element index.
   * @param val the new element value.
   */
  public void setElemFloat(int i, float val)
  {
    setElem(i, (int) val);
  }

  /**
   * Sets an element in a particular data bank.  The offset (specified in the
   * constructor) is added to <code>i</code> before updating the underlying
   * data array.
   *
   * @param bank the data bank index.
   * @param i the element index.
   * @param val the new element value.
   */
  public void setElemFloat(int bank, int i, float val)
  {
    setElem(bank, i, (int) val);
  }

  /**
   * Returns an element from the first data bank, converted to a
   * <code>double</code>.  The offset (specified in the constructor) is added
   * to <code>i</code> before accessing the underlying data array.
   *
   * @param i the element index.
   * @return The element.
   */
  public double getElemDouble(int i)
  {
    return getElem(i);
  }

  /**
   * Returns an element from a particular data bank, converted to a
   * <code>double</code>.  The offset (specified in the constructor) is
   * added to <code>i</code> before accessing the underlying data array.
   *
   * @param bank the bank index.
   * @param i the element index.
   * @return The element.
   */
  public double getElemDouble(int bank, int i)
  {
    return getElem(bank, i);
  }

  /**
   * Sets an element in the first data bank.  The offset (specified in the
   * constructor) is added to <code>i</code> before updating the underlying
   * data array.
   *
   * @param i the element index.
   * @param val the new element value.
   */
  public void setElemDouble(int i, double val)
  {
    setElem(i, (int) val);
  }

  /**
   * Sets an element in a particular data bank.  The offset (specified in the
   * constructor) is added to <code>i</code> before updating the underlying
   * data array.
   *
   * @param bank the data bank index.
   * @param i the element index.
   * @param val the new element value.
   */
  public void setElemDouble(int bank, int i, double val)
  {
    setElem(bank, i, (int) val);
  }
}
