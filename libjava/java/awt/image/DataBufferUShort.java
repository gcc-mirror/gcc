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
public class DataBufferUShort extends DataBuffer
{
  private short[] data;
  private short[][] bankData;
  
  public DataBufferUShort(int size)
  {
    super(TYPE_USHORT, size);
    data = new short[size];
  }

  public DataBufferUShort(int size, int numBanks)
  {
    super(TYPE_USHORT, size, numBanks);
    bankData = new short[numBanks][size];
    data = bankData[0];
  }

  public DataBufferUShort(short[] dataArray, int size)
  {
    super(TYPE_USHORT, size);
    data = dataArray;
  }
    
  public DataBufferUShort(short[] dataArray, int size, int offset)
  {
    super(TYPE_USHORT, size, 1, offset);
    data = dataArray;
  }

  public DataBufferUShort(short[][] dataArray, int size)
  {
    super(TYPE_USHORT, size, dataArray.length);
    bankData = dataArray;
    data = bankData[0];
  }

  public DataBufferUShort(short[][] dataArray, int size, int[] offsets)
  {
    super(TYPE_USHORT, size, dataArray.length, offsets);
    bankData = dataArray;
    data = bankData[0];
  }

  public short[] getData()
  {
    return data;
  }
    
  public short[] getData(int bank)
  {
    return bankData[bank];
  }
    
  public short[][] getBankData()
  {
    return bankData;
  }
  
  public int getElem(int i)
  {
    return data[i+offset] & 0xffff; // get unsigned short as int
  }

  public int getElem(int bank, int i)
  {
    // get unsigned short as int
    return bankData[bank][i+offsets[bank]] & 0xffff;
  }

  public void setElem(int i, int val)
  {
    data[i+offset] = (short) val;
  }

  public void setElem(int bank, int i, int val)
  {
    bankData[bank][i+offsets[bank]] = (short) val;
  }
}
