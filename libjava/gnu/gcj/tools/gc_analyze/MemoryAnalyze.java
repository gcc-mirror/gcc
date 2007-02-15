/* MemoryAnalyze.java -- Analyzes a libgcj heap dump.
   Copyright (C) 2007  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.tools.gc_analyze;

import gnu.classpath.tools.getopt.FileArgumentCallback;
import gnu.classpath.tools.getopt.Option;
import gnu.classpath.tools.getopt.OptionException;
import gnu.classpath.tools.getopt.Parser;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

class MemoryAnalyze
{
  public MemoryAnalyze()
  {
  }

  private static NumberFormat numberFormat;
  private static boolean verbose;
  static String format(long number, int digits)
  {
    if (numberFormat == null)
      {
        numberFormat = NumberFormat.getNumberInstance();
        numberFormat.setGroupingUsed(true);
      }
    String temp = numberFormat.format(number);
    int spaces = digits - temp.length();
    if (spaces < 0)
      spaces = 0;
    return "                                ".substring(0,spaces) + temp;
  }

  static void sorted_report(String description,
                            int total_space,
                            ArrayList<String> list,
                            Comparator<String> comparator)
  {
    System.out.println("*** " + description + " ***");
    System.out.println();
    System.out.println("  Total Size       Count       Size    Description");
    System.out.println("--------------     -----    --------   -----------------------------------");
    Collections.sort(list, comparator);
    for (Iterator it = list.iterator(); it.hasNext(); )
      {
        String v = (String)it.next();
        System.out.println(stripend(v));
      }
    System.out.println("--------------     -----    --------   -----------------------------------");
    System.out.println(format(total_space, 14));
    System.out.println();
    System.out.println();
  }

  private static String stripend(String s)
  {
    int n = s.lastIndexOf(" /");
    if (n > 0)
      return s.substring(0,n);
    return s;
  }

  static  class SubstringComparator implements Comparator<String>
  {
    private int begin, end;
    private boolean reverse;

    SubstringComparator(int begin, int end, boolean reverse)
    {
      this.begin = begin;
      this.end = end;
      this.reverse = reverse;
    }

    public int compare(String s1, String s2)
    {
      if (end == 0)
        s1 = s1.substring(begin);
      else
        s1 = s1.substring(begin, end);

      if (end == 0)
        s2 = s2.substring(begin);
      else
        s2 = s2.substring(begin, end);
      int i = s1.compareTo(s2);
      if (reverse)
        return -i;
      return i;
    }
  }

  static class OptionParser extends Parser
  {
    int filesFound;
	  
    OptionParser()
    {
      super("gc-analyze",
            "gc-analyze (" + System.getProperty("java.vm.version") + ")");

      add(new Option('d',
                     "Directory containing runtime objects",
                     "directory")
        {
          public void parsed(String argument) throws OptionException
          {
            ToolPrefix.pathPrefix = argument;			
          }
        });

      add(new Option('p',
                     "Binary tool prefix, prepended to nm and readelf to "
                     + "obtain target specific versions of these commands",
                     "prefix")
        {
          public void parsed(String argument) throws OptionException
          {
            ToolPrefix.toolPrefix = argument;			
          }
        });

      add(new Option("verbose", 'v',
                     "Verbose output; requires filename.bytes")
        {
          public void parsed(String argument) throws OptionException
          {
            verbose = true;			
          }
        });

      setHeader("usage: gc-analyze [-v] [-p tool-prefix] [-d <directory>] "
                + "filename");
    }
	  
    protected void validate() throws OptionException
    {
      if (filesFound != 1)
        throw new OptionException("Must specify exactly one filename");
    }
	  
    public String[] parse(String[] inArgs)
    {
      final ArrayList<String> fileResult = new ArrayList<String>();
      parse(inArgs, new FileArgumentCallback()
        {
          public void notifyFile(String fileArgument)
          {
            filesFound++;
            fileResult.add(fileArgument);
          }
        });
      return fileResult.toArray(new String[1]);
    }
  }
  
  public static void main(String[] args)
  {
    class Info
    {
      int size;
      int count;
    }
    int total_space = 0;

    Parser optionParser = new OptionParser();
    
    String rest[] = optionParser.parse(args);
    
    String filename = rest[0];
    
    try
      {
        BufferedReader reader =
          new BufferedReader(new InputStreamReader(new FileInputStream(filename)));
        SymbolLookup lookup = new SymbolLookup(reader, filename + ".bytes");
        ObjectMap objectMap = new ObjectMap(reader);
        BlockMap blockMap = new BlockMap(reader);
        reader.close();

        // add info to item(s)
        // add item.klass
        for (Map.Entry<Long, ObjectMap.ObjectItem> me : objectMap)
        {
            ObjectMap.ObjectItem item = me.getValue();

            // try to get a klass (happens with intern'ed strings...)
            if (item.klass==0)
              {
                BytePtr p = lookup.getBytePtr(item.ptr, item.size);
                if (p!=null)
                  {
                    long vtable = p.getWord(0);
                    String sym =
                        lookup.getSymbolViaVtable(vtable - 2 * lookup.memoryMap.wordSize);
                    if (sym != null)
                      {
                        item.typeName = SymbolTable.demangleVTName(sym);
                      }
                    else if (vtable != 0)
                      {
                        // get klass from vtable
                        p = lookup.getBytePtr(vtable,
                                              lookup.memoryMap.wordSize);
                        if (p != null)
                          {
                            long klass = p.getWord(0);
                            item.klass = klass;
                          }
                      }
                  }
              }

            // figure out strings
            String class_name;
            if (null == item.typeName)
              {
                class_name =
                  MemoryAnalyze.getSymbolPretty(lookup, item, false);
                item.typeName = class_name;
              }
            else
              {
                class_name = item.typeName;
              }
            System.out.print("class_name=[" + class_name + "]");

            if (class_name.compareTo("_ZTVN4java4lang6StringE")==0
                || class_name.compareTo("java.lang.String")==0)
              {
                BytePtr p = lookup.getBytePtr(item.ptr, item.size);
                long data = p.getWord(1); 
                int boffset = p.getInt(2 * p.intsPerWord());
                int count = p.getInt(1 + 2 * p.intsPerWord());
                int hash = p.getInt(2 + 2 * p.intsPerWord());
                BytePtr chars = lookup.getBytePtr(data+boffset, count * 2);
                StringBuffer sb = new StringBuffer(count);
                for (int qq = 0; qq<count; qq++)
                  sb.append((char)chars.getShort(qq));
                int newhash = sb.toString().hashCode();
                if (newhash!=hash)
                  {
                    p.setInt(4, newhash);
                  }

                item.string = sb.toString();
                System.out.println(" value = \"" + item.string + "\"");
                if (data != item.ptr)
                  {
                    ObjectMap.ObjectItem next = objectMap.get(data);
                    if (next != null)
                      next.stringData = true;
                    else
                      System.out.println("String [" + item.string + "] at "
                                         + Long.toHexString(item.ptr)
                                         + " can't find array at " 
                                         + Long.toHexString(data));
                  }
              }
            else if (null != item.string)
              System.out.println(" value = \"" + item.string + "\"");
            else
              System.out.println();
          }


        HashMap<String, Info> map = new HashMap<String, Info>();
        for (Map.Entry<Long, ObjectMap.ObjectItem> me : objectMap)
          {
            ObjectMap.ObjectItem item = me.getValue();
            String name = getSymbolPretty(lookup, item, true);
            Info info = map.get(name);
            if (info == null)
              {
                info = new Info();
                info.count = 0;
                info.size = item.size;
                map.put(name, info);
              }
            info.count++;
            total_space += item.size;
          }

        ArrayList<String> list = new ArrayList<String>();
        for (Iterator it = map.entrySet().iterator(); it.hasNext(); )
          {
            Map.Entry me = (Map.Entry)it.next();
            String name = (String)me.getKey();
            Info info = (Info)me.getValue();

            StringBuffer sb = new StringBuffer();
            sb.append(format(info.count * info.size * 100 / total_space,
                             3));
            sb.append("%");
            sb.append(format(info.count * info.size, 10));
            sb.append(" = ");
            sb.append(format(info.count, 7));
            sb.append(" * ");
            sb.append(format(info.size, 9));
            sb.append(" - ");
            sb.append(name);
            list.add(sb.toString());
          }

        sorted_report("Memory Usage Sorted by Total Size",
                      total_space, list, new SubstringComparator(5,14,true));
        sorted_report("Memory Usage Sorted by Description",
                      total_space, list, new SubstringComparator(39,0,false));
        sorted_report("Memory Usage Sorted by Count",
                      total_space, list, new SubstringComparator(17,25,true));
        sorted_report("Memory Usage Sorted by Size",
                      total_space, list, new SubstringComparator(28,37,true));

        blockMap.dump();

        // dump raw memory
        if (verbose)
          {
            // analyze references
            for (Map.Entry<Long, ObjectMap.ObjectItem> me : objectMap)
              {
                long ptr = me.getKey();
                ObjectMap.ObjectItem item = me.getValue();
                BytePtr p = lookup.getBytePtr(ptr, item.size);
                if (p == null)
                  System.out.println("can't find ptr 0x"
                                     + Long.toHexString(ptr));
                else if (item.kind != 0) // not GC_PTRFREE
                  for (int i = 1;
                       i < item.size / lookup.memoryMap.wordSize; i++)
                    {
                      long maybe_ptr = p.getWord(i);
                      ObjectMap.ObjectItem item2 = objectMap.get(maybe_ptr);
                      if (item2 != null)
                        {
                          item2.pointed_by.add(item);
                          item.points_to.add(item2);
                        }
                    }
              }
            System.out.println();
            System.out.println("*** All Objects ***");
            System.out.println();

            for (Map.Entry<Long, ObjectMap.ObjectItem> me : objectMap)
            {
              long ptr = me.getKey();
              ObjectMap.ObjectItem item = me.getValue();
              String name = getSymbolPretty(lookup, item, false);
              System.out.print("0x" + Long.toHexString(ptr) + " - " + name
                               + " (" + item.size + ")");
              if (item.string != null)
        	System.out.println(" \"" + item.string + "\"");
              else
        	System.out.println();

              BytePtr p = lookup.getBytePtr(ptr, item.size);

              if (p == null)
                System.out.println(
                  "can't find memory; recently allocated from free list?");
              else
                p.dump();

              item.points_to.dump("  points to:", lookup);
              item.pointed_by.dump("  pointed to by:", lookup);
              System.out.println();
            }
          }
      }
    catch (IOException e)
      {
        e.printStackTrace();
      }
  }

  public static String kindToName(int kind)
  {
    String name;
    switch (kind)
      {
      case 0:
        name = "GC_PTRFREE";
        break;
      case 1:
        name = "GC_NORMAL";
        break;
      case 2:
        name = "GC_UNCOLLECTABLE";
        break;
      case 3:
        name = "GC_AUUNCOLLCTABLE";
        break;
      case 4:
        name = "(Java)";
        break;
      case 5:
        name = "(Java Debug)";
        break;
      case 6:
        name = "(Java Array)";
        break;
      default:
        name = "(Kind " + kind + ")";
        break;
      }
    return name;
  }

  public static String getSymbolPretty(SymbolLookup lookup,
                                       ObjectMap.ObjectItem item,
                                       boolean bsize)
    throws IOException
  {
    
    String name = item.typeName;
    
    if (name == null)
      name = lookup.getSymbol(item.klass);
    
    if (name == null)
      {
      	String v = lookup.decodeUTF8(item.ptr, item.size);
      	if (null != v)
      	  {
      	    name = "UTF8Const";
      	    item.string = v;
      	  }
      }
    
    if (name == null)
      {
        name = kindToName(item.kind);
      }
    if (item.kind==6)
      name += "[" + format(item.data, 0) + "]";
    if (bsize)
      name = name + " / " + format(item.size, 7);
    return name;
  }
}
