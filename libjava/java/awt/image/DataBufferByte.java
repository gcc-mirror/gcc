/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

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
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public class DataBufferByte extends DataBuffer
{
  private byte[] data;
  private byte[][] bankData;
  
  public DataBufferByte(int size)
  {
    super(TYPE_BYTE, size);
    data = new byte[size];
  }

  public DataBufferByte(int size, int numBanks)
  {
    super(TYPE_BYTE, size, numBanks);
    bankData = new byte[numBanks][size];
    data = bankData[0];
  }

  public DataBufferByte(byte[] dataArray, int size)
  {
    super(TYPE_BYTE, size);
    data = dataArray;
  }
    
  public DataBufferByte(byte[] dataArray, int size, int offset)
  {
    super(TYPE_BYTE, size, 1, offset);
    data = dataArray;
  }

  public DataBufferByte(byte[][] dataArray, int size)
  {
    super(TYPE_BYTE, size, dataArray.length);
    bankData = dataArray;
    data = bankData[0];
  }

  public DataBufferByte(byte[][] dataArray, int size, int[] offsets)
  {
    super(TYPE_BYTE, size, dataArray.length, offsets);
    bankData = dataArray;
    data = bankData[0];
  }

  public byte[] getData()
  {
    return data;
  }
    
  public byte[] getData(int bank) 
  {
    return bankData[bank];
  }
    
  public byte[][] getBankData()
  {
    return bankData;
  }
  
  public int getElem(int i)
  {
    return data[i+offset] & 0xff; // get unsigned byte as int
  }
  
  public int getElem(int bank, int i)
  {
    // get unsigned byte as int
    return bankData[bank][i+offsets[bank]] & 0xff;
  }

  public void setElem(int i, int val)
  {
    data[i+offset] = (byte) val;
  }

  public void setElem(int bank, int i, int val)
  {
    bankData[bank][i+offsets[bank]] = (byte) val;
  }
}
