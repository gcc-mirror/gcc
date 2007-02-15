/* BlockMap.java -- Container for information on GC maintained memory blocks.
   Copyright (C) 2007  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.tools.gc_analyze;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Map;
import java.util.TreeMap;

class BlockMap
{
  static final int HBLKSIZE = 4096;

  class SizeKind implements Comparable<SizeKind>
  {
    int size;
    int kind;

    public SizeKind(int size, int kind)
    {
      this.size = size;
      this.kind = kind;
    }

    public int compareTo(SizeKind b)
    {
      if (this.size != b.size)
        return this.size - b.size;
      return this.kind - b.kind;
    }
  }

  class PtrMarks
  {
    long ptr;
    int marks;
  
    public PtrMarks(long ptr, int marks)
    {
      this.ptr = ptr;
      this.marks = marks;
    }
  }

  private TreeMap<SizeKind, ArrayList<PtrMarks>> map =
    new TreeMap<SizeKind, ArrayList<PtrMarks>>();

  public BlockMap(BufferedReader reader) throws IOException
  {
    for (;;)
      {
        String s = reader.readLine();
        if (s == null)
          break;
        if (s.charAt(0) == '#')
          continue;
        if (s.indexOf("Begin block map") >= 0)
          {
            for (;;)
              {
                s = reader.readLine();
                if (s.charAt(0) == '#')
                  continue;
                if (s.indexOf("End block map") >= 0)
                  return;
                String[] items = s.split(",");
                long ptr = 0;
                int kind = 0, size = 0, marks = 0;
                for (int i=0; i<items.length; i++)
                  {
                    String[] x = items[i].split(" ");
                    String last = x[x.length - 1];
                    switch (i)
                      {
                      case 0:
                        ptr = MemoryMap.parseHexLong(last.substring(2));
                        break;
                      case 1:
                        kind = Integer.parseInt(last);
                        break;
                      case 2:
                        size = Integer.parseInt(last);
                        break;
                      case 3:
                        marks = Integer.parseInt(last);
                        break;
                      }
                  }
                SizeKind sk = new SizeKind(size, kind);
                ArrayList<PtrMarks> m = map.get(sk);
                if (m == null)
                    {
                        m = new ArrayList<PtrMarks>();
                        map.put(sk, m);
                    }
                PtrMarks pm = new PtrMarks(ptr, marks);
                m.add(pm);
              } // inner loop
          } // started inner loop
      } // outer loop - finding begin
  } // memoryMap

  public void dump()
  {
    System.out.println();
    System.out.println();
    System.out.println("*** Used Blocks ***\n");
    System.out.println();
    System.out.println("  Size     Kind            Blocks     Used       Free       Wasted");
    System.out.println("-------  -------------    ------- ---------- ----------    -------");

    int total_blocks = 0, total_used = 0, total_free = 0, total_wasted = 0;

    for (Map.Entry<SizeKind, ArrayList<PtrMarks>> me : map.entrySet())
      {
        SizeKind sk = me.getKey();

        System.out.println(MemoryAnalyze.format(sk.size, 7) + "  "
                           + MemoryAnalyze.kindToName(sk.kind));

        int sub_blocks = 0, sub_used = 0, sub_free = 0, sub_wasted = 0;
        int sub_count = 0;

        ArrayList<PtrMarks> v = me.getValue();

        for (PtrMarks pm : v)
          {
            int bytes = sk.size;
            int blocks = (sk.size + HBLKSIZE - 1) / HBLKSIZE;
            int used;
            int free;
            int wasted;

            if (bytes < HBLKSIZE)
              {
                used = bytes * pm.marks;
                free = bytes * (HBLKSIZE / bytes - pm.marks);
                wasted = HBLKSIZE - HBLKSIZE / bytes * bytes;
              }
            else
              {
                if (pm.marks != 0)
                  {
                    used = bytes;
                    free = 0;
                    wasted = (bytes + HBLKSIZE - 1)
                      / HBLKSIZE * HBLKSIZE - used;
                  }
                else
                  {
                    used = 0;
                    free = bytes;
                    wasted = 0;
                  }
              }

            StringBuilder sb = new StringBuilder();
            sb.append("                            ");
            sb.append(MemoryAnalyze.format(blocks, 5));
            sb.append("  ");
            sb.append(MemoryAnalyze.format(used, 9));
            sb.append("  ");
            sb.append(MemoryAnalyze.format(free, 9));
            sb.append("  ");
            sb.append(MemoryAnalyze.format(wasted, 9));
            System.out.println(sb);

            sub_blocks += blocks;
            sub_used += used;
            sub_free += free;
            sub_wasted += wasted;
            sub_count++;

            total_blocks += blocks;
            total_used += used;
            total_free += free;
            total_wasted += wasted;
          } // blocks with size/kind
        if (sub_count > 1)
          {
            System.out.println(
                               "                          ------- ---------- ----------    -------");
            StringBuilder sb = new StringBuilder();
            sb.append("                            ");
            sb.append(MemoryAnalyze.format(sub_blocks, 5));
            sb.append("  ");
            sb.append(MemoryAnalyze.format(sub_used, 9));
            sb.append("  ");
            sb.append(MemoryAnalyze.format(sub_free, 9));
            sb.append("  ");
            sb.append(MemoryAnalyze.format(sub_wasted, 9));
            System.out.println(sb);
          }
      } // size/kind

    System.out.println("-------  -------------    ------- ---------- ----------    -------");
    StringBuilder sb = new StringBuilder();
    sb.append("                            ");
    sb.append(MemoryAnalyze.format(total_blocks, 5));
    sb.append("  ");
    sb.append(MemoryAnalyze.format(total_used, 9));
    sb.append("  ");
    sb.append(MemoryAnalyze.format(total_free, 9));
    sb.append("  ");
    sb.append(MemoryAnalyze.format(total_wasted, 9));
    System.out.println(sb);
    System.out.println("Total bytes = "
                       + MemoryAnalyze.format(total_blocks * HBLKSIZE, 10));
  }
}
