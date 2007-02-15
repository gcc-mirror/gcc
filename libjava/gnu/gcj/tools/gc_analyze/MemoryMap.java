/* MemoryMap.java -- Maps address ranges to their data.
   Copyright (C) 2007  Free Software Foundation

   This file is part of libgcj.

   This software is copyrighted work licensed under the terms of the
   Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
   details.  */

package gnu.gcj.tools.gc_analyze;

import java.io.BufferedReader;
import java.io.EOFException;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.FileChannel;
import java.util.Comparator;
import java.util.HashMap;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Reads /proc/self/maps output from dump file.
 * Creates map of <filename> to Range.
 *
 * Returns filename given address.
 * Returns offset given address.
 * Returns BytePtr given address.
 *
 */
class MemoryMap
{
  static class RangeComparator implements Comparator<Range>
  {
    public int compare(Range r1, Range r2)
    {
      if (r2.end == 0 && r1.end != 0)
        return -compare(r2, r1);
      
      if (r1.begin < r2.begin)
        return -1;
      else if (r1.begin >= r2.end)
        return 1;
      else
        return 0;
    }
  }
  
  static class Range
  {
    long begin;
    long end;

    long offset;
    String filename;
    Range()
    {
    }
    
    Range(long b, long e, String s, long o)
    {
      begin = b;
      end = e;
      filename = s;
      offset = o;
    }
  }

  /**
   * Parse the string as an unsigned hexadecimal number.  This is
   * similar to Long.parseInt(s,16), but without the restriction that
   * values that have the sign bit set not being allowed.
   *
   * @param s the number as a String.
   * @return the number.
   */
  static long parseHexLong(String s)
  {
    if (s.length() > 16)
      throw new NumberFormatException();
    long r = 0;
    for (int i = 0; i < s.length(); i++)
      {
        int digit = 0;
        char c = s.charAt(i);
        switch (c)
          {
          case '0':
          case '1':
          case '2':
          case '3':
          case '4':
          case '5':
          case '6':
          case '7':
          case '8':
          case '9':
            digit = c - '0';
            break;
          case 'a':
          case 'b':
          case 'c':
          case 'd':
          case 'e':
          case 'f':
            digit = 10 + c - 'a';
            break;
          case 'A':
          case 'B':
          case 'C':
          case 'D':
          case 'E':
          case 'F':
            digit = 10 + c - 'A';
            break;
          default:
            throw new NumberFormatException();
          }
        r = (r << 4) + digit;
      }
    return r;
  }
  
  // String filename -> Range
  TreeSet<Range> map = new TreeSet<Range>(new RangeComparator());
  HashMap<String, SymbolTable> symbolTables =
    new HashMap<String, SymbolTable>();
  ByteOrder byteOrder;
  int wordSize;

  public MemoryMap(BufferedReader reader,
                   String rawFileName) throws IOException
  {
    FileChannel raw = (new RandomAccessFile(rawFileName, "r")).getChannel();
    ByteBuffer buf = ByteBuffer.allocate(8);
    raw.read(buf);
    if (buf.hasRemaining())
      {
        raw.close();
        throw new EOFException();
      }
    buf.flip();
    wordSize = buf.get();
    
    if (wordSize == 8 || wordSize == 4)
      byteOrder = ByteOrder.LITTLE_ENDIAN;
    else
      {
        byteOrder = ByteOrder.BIG_ENDIAN;
        buf.rewind();
        wordSize = buf.getInt();
        if (0 == wordSize)
          wordSize = buf.getInt();
      }
    switch (wordSize)
      {
      case 4:
      case 8:
        break;
      default:
        throw new IOException("Bad .bytes file header");
      }
    buf = ByteBuffer.allocate(3 * wordSize);
    buf.order(byteOrder);
    raw.position(0L);

    for(;;)
      {
        // Read the block header.
        buf.clear();
        if (-1 == raw.read(buf))
          {
            //EOF
            raw.close();
            break;
          }
        if (buf.hasRemaining())
          {
            raw.close();
            throw new EOFException();
          }
        buf.flip();
        long dummy
          = (wordSize == 4) ? (buf.getInt() & 0xffffffffL) : buf.getLong();
        if (dummy != wordSize)
          throw new IOException("Bad .bytes file header");
        long start
          = wordSize == 4 ? (buf.getInt() & 0xffffffffL) : buf.getLong();
        long length
          = wordSize == 4 ? (buf.getInt() & 0xffffffffL) : buf.getLong();
        if (length < 0L)
          throw new IOException("Bad .bytes file header");
      
        long currentPos = raw.position();
        raw.position(currentPos + length);
    
        Range range = new Range(start, start + length,
                                rawFileName, currentPos);
        map.add(range);
      }

    for (;;)
      {
        String s = reader.readLine();
        if (s == null)
          break;
        if (s.indexOf("Begin address map") >= 0)
          {
            for (;;)
              {
                s = reader.readLine();
                if (s.indexOf("End address map") >= 0)
                  {
                    dump();
                    return;
                  }
                int endOfAddress = s.indexOf('-');
                long address = parseHexLong(s.substring(0, endOfAddress));
                int endOfAddress2 = s.indexOf(' ', endOfAddress + 1);
                long address2 = parseHexLong(s.substring(endOfAddress + 1,
                                                         endOfAddress2));
                int endOfOffset = s.indexOf(' ', endOfAddress2 + 6);
                long offset;
                try
                  {
                    offset = parseHexLong(s.substring(endOfAddress2 + 6,
                                                      endOfOffset));
                  }
                catch (Exception e)
                  {
                    offset = 0;
                  }
                int end = s.indexOf('/');

                if (end > 0)
                  {
                    String file = s.substring(end);
                    if (file.startsWith("/dev/"))
                      continue;

                    Range r = new Range(address, address2, file, offset);
                    if (offset == 0)
                      {
                        // Read the file's symbol table
                        try
                          {
                            File f = ToolPrefix.fileForName(file);
                            if (f != null)
                              {
                                SymbolTable st = new SymbolTable(f.getPath());
                                if (st.loadAddr != address)
                                  st.relocation = address - st.loadAddr;
                                symbolTables.put(file, st);
                              }
                          }
                        catch (Exception ex)
                          {
                            ex.printStackTrace();
                          }
                      }
                    map.add(r);
                  }
              } // inner loop
          } // started inner loop
      } // outer loop - finding begin
  } // memoryMap

  
  public void dump()
  {
    System.out.println("MemoryMap:");
    for (Range r : map)
      {
        System.out.println(Long.toHexString(r.begin) + "-"
                           + Long.toHexString(r.end) + " -> "
                           + r.filename + " offset "
                           + Long.toHexString(r.offset));
      }
  }

  Range getRange(long addr)
  {
    Range r = new Range();
    r.begin = addr;
    SortedSet<Range> t = map.tailSet(r);
    if (t.isEmpty())
      return null;
    Range c = t.first();
    if (c.begin <= addr && addr < c.end)
      return c;
    return null;
  }
  
  String getFile(long addr)
  {
    Range r = getRange(addr);
    if (null != r)
      return r.filename;
    return null;
  }

  long getOffset(long addr)
  {
    Range r = getRange(addr);
    if (null != r)
      return r.offset;
    return 0L;
  }

  /**
   * @return BytePtr which includes given address.
   */
  BytePtr getBytePtr(long addr, int length) throws IOException
  {
    Range r = getRange(addr);
    
    if (null == r)
      return null;

    File f = ToolPrefix.fileForName(r.filename);
    if (null == f)
      return null;
    
    if (addr + length > r.end)
      length = (int)(r.end - addr);
    
    ByteBuffer b = ByteBuffer.allocate(length);
    b.order(byteOrder);
    
    FileChannel fc = (new RandomAccessFile(f, "r")).getChannel();
    fc.position(r.offset + addr - r.begin);
    int nr = fc.read(b);
    fc.close();
    if (nr != length)
      return null;
    b.flip();
    return new BytePtr(b, wordSize);
  }
  
  public String getSymbol(long addr)
  {
    Range r = getRange(addr);
    
    if (r == null)
      return null;
    
    SymbolTable st = symbolTables.get(r.filename);
    if (st == null)
      return null;
    
    // Apply relocation
    addr -= st.relocation;
    
    return st.getSymbol(addr);
  }
}
