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
