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

/* This is one of several classes that are nearly identical. Maybe we
   should have a central template and generate all these files. This
   is one of the cases where templates or macros would have been
   useful to have in Java.

   This file has been created using search-replace. My only fear is
   that these classes will grow out-of-sync as of a result of changes
   that are not propagated to the other files. As always, mirroring
   code is a maintenance nightmare.  */

/**
 * @author Rolf W. Rasmussen (rolfwr@ii.uib.no)
 */
public final class DataBufferByte extends DataBuffer
{
  private byte[] data;
  private byte[][] bankData;
  
  /**
   * Creates a new data buffer with a single data bank containing the 
   * specified number of <code>byte</code> elements.
   * 
   * @param size the number of elements in the data bank.
   */
  public DataBufferByte(int size)
  {
    super(TYPE_BYTE, size, 1, 0);
    bankData = new byte[1][];
    data = new byte[size];
    bankData[0] = data;
  }

  /**
   * Creates a new data buffer with the specified number of data banks, 
   * each containing the specified number of <code>byte</code> elements.
   * 
   * @param size the number of elements in the data bank.
   * @param numBanks the number of data banks.
   */
  public DataBufferByte(int size, int numBanks)
  {
    super(TYPE_BYTE, size, numBanks);
    bankData = new byte[numBanks][size];
    data = bankData[0];
  }

  /**
   * Creates a new data buffer backed by the specified data bank.
   * <p>
   * Note: there is no exception when <code>dataArray</code> is 
   * <code>null</code>, but in that case an exception will be thrown
   * later if you attempt to access the data buffer.
   * 
   * @param dataArray the data bank.
   * @param size the number of elements in the data bank.
   */
  public DataBufferByte(byte[] dataArray, int size)
  {
    super(TYPE_BYTE, size, 1, 0);
    bankData = new byte[1][];
    data = dataArray;
    bankData[0] = data;
  }
    
  /**
   * Creates a new data buffer backed by the specified data bank, with
   * the specified offset to the first element.
   * <p>
   * Note: there is no exception when <code>dataArray</code> is 
   * <code>null</code>, but in that case an exception will be thrown
   * later if you attempt to access the data buffer.
   * 
   * @param dataArray the data bank.
   * @param size the number of elements in the data bank.
   * @param offset the offset to the first element in the array.
   */
  public DataBufferByte(byte[] dataArray, int size, int offset)
  {
    super(TYPE_BYTE, size, 1, offset);
    bankData = new byte[1][];
    data = dataArray;
    bankData[0] = data;
  }

  /**
   * Creates a new data buffer backed by the specified data banks.
   * 
   * @param dataArray the data banks.
   * @param size the number of elements in the data bank.
   * 
   * @throws NullPointerException if <code>dataArray</code> is 
   *         <code>null</code>.
   */
  public DataBufferByte(byte[][] dataArray, int size)
  {
    super(TYPE_BYTE, size, dataArray.length);
    bankData = dataArray;
    data = bankData[0];
  }

  /**
   * Creates a new data buffer backed by the specified data banks, with
   * the specified offsets to the first element in each bank.
   * 
   * @param dataArray the data banks.
   * @param size the number of elements in the data bank.
   * @param offsets the offsets to the first element in each data bank.
   * 
   * @throws NullPointerException if <code>dataArray</code> is 
   *         <code>null</code>.
   */
  public DataBufferByte(byte[][] dataArray, int size, int[] offsets)
  {
    super(TYPE_BYTE, size, dataArray.length, offsets);
    bankData = dataArray;
    data = bankData[0];
  }

  /**
   * Returns the first data bank.
   * 
   * @return The first data bank.
   */
  public byte[] getData()
  {
    return data;
  }
    
  /**
   * Returns a data bank.
   * 
   * @param bank the bank index.
   * @return A data bank.
   */
  public byte[] getData(int bank) 
  {
    return bankData[bank];
  }
    
  /**
   * Returns the array underlying this <code>DataBuffer</code>.
   * 
   * @return The data banks.
   */
  public byte[][] getBankData()
  {
    return bankData;
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
    return data[i+offset] & 0xff; // get unsigned byte as int
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
  public int getElem(int bank, int i)
  {
    // get unsigned byte as int
    return bankData[bank][i+offsets[bank]] & 0xff;
  }

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
    data[i+offset] = (byte) val;
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
  public void setElem(int bank, int i, int val)
  {
    bankData[bank][i+offsets[bank]] = (byte) val;
  }
}
