/* Copyright (C) 2000, 2002  Free Software Foundation

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */

package java.awt.image;

/** 
 * Class that manages arrays of data elements. A data buffer consists
 * of one or more banks.  A bank is a continuous region of data
 * elements.
 *
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public abstract class DataBuffer
{
  public static final int TYPE_BYTE      =  0;
  public static final int TYPE_USHORT    =  1;
  public static final int TYPE_SHORT     =  2;
  public static final int TYPE_INT       =  3;
  public static final int TYPE_FLOAT     =  4;
  public static final int TYPE_DOUBLE    =  5;
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
  
  protected DataBuffer(int dataType, int size)
  {
    this.dataType = dataType;
    this.size = size;
  }

  protected DataBuffer(int dataType, int size, int numBanks) {
    this(dataType, size);
    banks = numBanks;
    offsets = new int[numBanks];
  }

  protected DataBuffer(int dataType, int size, int numBanks, int offset) {
    this(dataType, size, numBanks);
    
    java.util.Arrays.fill(offsets, offset);          
    
    this.offset = offset;
  }

  protected DataBuffer(int dataType, int size, int numBanks, int[] offsets) {
    this(dataType, size);
    if (numBanks != offsets.length) 
      throw new ArrayIndexOutOfBoundsException();
    
    banks = numBanks;
    this.offsets = offsets;
    
    offset = offsets[0];
  }
  
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

  public int getDataType()
  {
    return dataType;
  }
  
  public int getSize()
  {
    return size;
  }
  
  public int getOffset()
  {
    return offset;
  }
  
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

  public int getNumBanks()
  {
    return banks;
  }

  public int getElem(int i)
  {
    return getElem(0, i);
  }

  public abstract int getElem(int bank, int i);
  
  public void setElem(int i, int val)
  {
    setElem(0, i, val);
  }

  public abstract void setElem(int bank, int i, int val);
  
  public float getElemFloat(int i)
  {
    return getElem(i);
  }
    
  public float getElemFloat(int bank, int i)
  {
    return getElem(bank, i);
  }

  public void setElemFloat(int i, float val)
  {
    setElem(i, (int) val);
  }

  public void setElemFloat(int bank, int i, float val)
  {
    setElem(bank, i, (int) val);
  }

  public double getElemDouble(int i)
  {
    return getElem(i);
  }
    
  public double getElemDouble(int bank, int i)
  {
    return getElem(bank, i);
  }

  public void setElemDouble(int i, double val)
  {
    setElem(i, (int) val);
  }

  public void setElemDouble(int bank, int i, double val)
  {
    setElem(bank, i, (int) val);
  }
}
