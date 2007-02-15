/* SymbolLookup.java -- Finds class names by analyzing memory.
   Copyright (C) 2007  Free Software Foundation

   This file is part of libgcj.

   This software is copyrighted work licensed under the terms of the
   Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
   details.  */

package gnu.gcj.tools.gc_analyze;

import java.io.BufferedReader;
import java.io.IOException;

class SymbolLookup
{
  MemoryMap memoryMap;

  public SymbolLookup(BufferedReader reader,
                      String rawFileName)
    throws IOException
  {
    memoryMap = new MemoryMap(reader, rawFileName);
  }

  public String decodeUTF8(long address) throws IOException
  {
    return decodeUTF8(address, -1);
  }
  
  public String decodeUTF8(long address, int limit) throws IOException
  {
    if (address == 0)
      return null;

    BytePtr utf8 = memoryMap.getBytePtr(address, 64);

    if (utf8 == null)
      return null;

    int len = utf8.getShort(1);
    int hash16 = utf8.getShort(0) & 0xffff;

    if (len <= 0 || (limit > 0 && len > (limit - 4)))
      return null;
    
    if (len > utf8.getsize() + 4)
      utf8 = memoryMap.getBytePtr(address, len + 4);

    if (utf8 == null)
      return null;
    
    StringBuilder sb = new StringBuilder(len);
    int pos = 4;
    len += 4;
    
    while (pos < len)
      {
        int f = utf8.getByte(pos++);
        if ((f & 0x80) == 0)
          {
            sb.append((char)f);
          }
        else if ((f & 0xe0) == 0xc0)
          {
            int s = utf8.getByte(pos++);
            char c = (char)(((f & 0x1f) << 6) | (s & 0x80));
            sb.append(c);
          }
        else if ((f & 0xe0) == 0xe0)
          {
            int s = utf8.getByte(pos++);
            int t = utf8.getByte(pos++);
            char c = (char)(((f & 0x0f) << 12)
                            | ((s & 0x80) << 6) | (t & 0x80));
            sb.append(c);
          }
        else 
          break;  // Bad utf8
      }
    String rv = sb.toString();
    if (hash16 == (rv.hashCode() & 0xffff))
      return rv;
    else
      return null;
  }

  public String getSymbolViaVtable(long address) throws IOException
  {
    return memoryMap.getSymbol(address);
  }

  public String getSymbol(long address) throws IOException
  {
    String symbol = memoryMap.getSymbol(address);
    if (null != symbol)
      return symbol;
    
    BytePtr klass = memoryMap.getBytePtr(address, 3 * memoryMap.wordSize);
    if (klass == null)
      return null;
    
    long nameUTF8p = klass.getWord(2);
    
    return decodeUTF8(nameUTF8p);
  }

  BytePtr getBytePtr(long addr, int length) throws IOException
  {
    return memoryMap.getBytePtr(addr, length);
  }
}
