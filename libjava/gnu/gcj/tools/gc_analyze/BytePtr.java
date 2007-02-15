/* BytePtr.java -- Container for bytes from a memory image.
   Copyright (C) 2007  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.tools.gc_analyze;

import java.nio.ByteBuffer;

public class BytePtr
{
  ByteBuffer content;
  int wordSize;

  BytePtr(ByteBuffer b, int ws)
  {
    content = b;
    wordSize = ws;
  }
  
  public int getsize()
  {
    return content.limit();
  }

  public int getByte(int offset)
  {
    return content.get(offset);
  }

  public int getInt(int n)
  {
    return content.getInt(n * 4);
  }

  public int getShort(int n)
  {
    return content.getShort(n * 2);
  }
  
  public long getWord(int n)
  {
    if (4 == wordSize)
      return 0xffffffffL & content.getInt(n * 4);
    else
      return content.getLong(n * 8);
  }
  
  public int intsPerWord()
  {
    return (4 == wordSize) ? 1 : 2;
  }

  public BytePtr getRegion(int offset, int size)
  {
    int oldLimit = content.limit();
    content.position(offset);
    content.limit(offset + size);
    ByteBuffer n = content.slice();
    content.position(0);
    content.limit(oldLimit);
    
    return new BytePtr(n, wordSize);
  }

  public void setInt(int a, int n)
  {
    content.putInt(a * 4, n);
  }

  public void dump()
  {
    // 38 5a f4 2a 50 bd 04 10 10 00 00 00 0e 00 00 00   8Z.*P...........
    int i;
    StringBuilder b = new StringBuilder(67);
    for (i = 0; i < 66; i++)
      b.append(' ');
    b.append('\n');

    i = 0;
    do
      {
        for (int j = 0; j < 16; j++)
          {
            int k = i + j;

            if (k < content.limit())
              {
                int v = 0xff & getByte(k);
                // hex
                int v1 = v/16;
                b.setCharAt(j * 3 + 0,
                            (char)(v1 >= 10 ? 'a' - 10 + v1 : v1 + '0'));
                v1 = v % 16;
                b.setCharAt(j * 3 + 1,
                            (char)(v1 >= 10 ? 'a' - 10 + v1 : v1 + '0'));
                // ascii
                b.setCharAt(j + 50, (char)((v >= 32 && v <= 127) ? v: '.'));
              }
            else
              {
                b.setCharAt(j * 3 + 0, ' ');
                b.setCharAt(j * 3 + 1, ' ');
                b.setCharAt(j + 50, ' ');
              }
          }
        i += 16;
        System.out.print(b);
      } while (i < content.limit());
  }
}
