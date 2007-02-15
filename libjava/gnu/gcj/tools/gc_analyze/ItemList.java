/* ItemList.java -- Maps all objects keyed by their addresses.
   Copyright (C) 2007  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.tools.gc_analyze;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;

class ItemList
{
  public ItemList()
  {
  }

  private TreeMap<Long, HashMap<ObjectMap.ObjectItem, Integer>> map;

  public void add(ObjectMap.ObjectItem item)
  {
    if (map == null)
      map = new TreeMap<Long, HashMap<ObjectMap.ObjectItem, Integer>>();
    Long x = new Long(item.klass);
    HashMap<ObjectMap.ObjectItem, Integer> list = map.get(x);
    if (list == null)
      {
        list = new HashMap<ObjectMap.ObjectItem, Integer>();
        map.put(x, list);
      }
    Integer count = list.get(item);
    if (count == null)
      list.put(item, new Integer(1));
    else
      list.put(item, new Integer(count.intValue() + 1));
  }

  void dump(String title, SymbolLookup lookup) throws IOException
  {
    if (map == null)
      return;
    System.out.println(title);
    for (Map.Entry<Long, HashMap<ObjectMap.ObjectItem, Integer>> me :
           map.entrySet())
      {
        HashMap<ObjectMap.ObjectItem, Integer> list = me.getValue();
        boolean first = true;

        for (Map.Entry<ObjectMap.ObjectItem, Integer> me2 : list.entrySet())
          {
            ObjectMap.ObjectItem item = me2.getKey();
            Integer count = me2.getValue();
            if (first)
              {
                String name =
                  MemoryAnalyze.getSymbolPretty(lookup, item, false);
                System.out.println("    " + name + ":");
                first = false;
              }
            System.out.print("        0x" + Long.toHexString(item.ptr));
            if (count.intValue() != 1)
              System.out.print(" * " + count);
            System.out.println();
          }
      }
  }
}
