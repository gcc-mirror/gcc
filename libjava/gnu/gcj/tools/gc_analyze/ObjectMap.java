/* ObjectMap.java -- Contains a map of all objects keyed by their addresses.
   Copyright (C) 2007  Free Software Foundation

   This file is part of libgcj.

   This software is copyrighted work licensed under the terms of the
   Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
   details.  */

package gnu.gcj.tools.gc_analyze;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

class ObjectMap implements Iterable<Map.Entry<Long, ObjectMap.ObjectItem>>
{

  class ObjectItem
  {
    int used;
    int size;
    int kind;
    long klass;
    long data;
    long ptr;
    String typeName;
    String string; // only for string objects
    boolean stringData; // character array pointed to by a string
    ObjectItem reference; // object at reference points to this

    ItemList points_to = new ItemList();
    ItemList pointed_by = new ItemList();
  }

  private TreeMap<Long, ObjectItem> map = new TreeMap<Long, ObjectItem>();

  public Iterator<Map.Entry<Long, ObjectItem>> iterator()
  {
    return map.entrySet().iterator();
  }

  public ObjectItem get(long ptr)
  {
    ObjectItem item = map.get(ptr);
    return item;
  }

  public ObjectMap(BufferedReader reader) throws IOException
  {
    outer_loop:
    for (;;)
      {
        String s = reader.readLine();
        if (s == null)
          break;
        if (s.indexOf("Begin object map") >= 0)
          {
            for (;;)
              {
                s = reader.readLine();
                if (s.indexOf("End object map") >= 0)
                  break outer_loop;
                String[] items = s.split(",");
                ObjectItem item = new ObjectItem();
                long ptr = 0;
                for (int i=0; i<items.length; i++)
                  {
                    String[] x = items[i].split(" ");
                    String last = x[x.length-1];
                    switch (i)
                      {
                      case 0:
                        item.used = Integer.parseInt(last);
                        break;
                      case 1:
                        ptr = MemoryMap.parseHexLong(last.substring(2));
                        break;
                      case 2:
                        item.size = Integer.parseInt(last);
                        break;
                      case 3:
                        item.kind = Integer.parseInt(last);
                        break;
                      case 4:
                        if (last.length() > 1)
                          item.klass =
                            MemoryMap.parseHexLong(last.substring(2));
                        else
                          item.klass  = Integer.parseInt(last,16);
                        break;
                      case 5:
                        try
                          {
                            item.data =
                              Integer.parseInt(last.substring(2), 16);
                          }
                        catch (Exception e)
                          {
                            item.data = 0;
                          }
                        break;
                      }
                  }
                item.ptr = ptr;
                map.put(ptr, item);
              } // inner loop
          } // started inner loop
      } // outer loop - finding begin
    for (Map.Entry<Long, ObjectItem> me : this)
      {
        ObjectItem item = me.getValue();
        if (item.data != 0)
          {
            // see if data is a pointer to a block
            ObjectItem referenced = map.get(item.data);
            if (referenced != null)
              {
                referenced.reference = item;
              }
          }
      }
  } // memoryMap

  public void dump()
  {
    for (Map.Entry<Long, ObjectItem> me : this)
      {
        long ptr = me.getKey();
        ObjectItem item = me.getValue();
        System.out.println("ptr = " + Long.toHexString(ptr)
                           + ", size = " + item.size
                           + ", klass = " + Long.toHexString(item.klass)
                           + ", kind = " + item.kind
                           + ", data = " + item.data);
      }
  }
}
