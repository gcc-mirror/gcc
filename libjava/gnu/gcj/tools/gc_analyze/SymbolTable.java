/* SymbolTable.java -- Maintains a mapping of addresses to names.
   Copyright (C) 2007  Free Software Foundation

   This file is part of libgcj.

   This software is copyrighted work licensed under the terms of the
   Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
   details.  */

package gnu.gcj.tools.gc_analyze;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

class SymbolTable
{
  // Long address->String name
  private HashMap<Long, String> map = new HashMap<Long, String>();

  // Reverse
  // String name -> Long address
  // used for RelocateImage
  private HashMap<String, Long> reverse = new HashMap<String, Long>();
  
  long loadAddr;
  long relocation;

  static Matcher interestingSymbol =
    Pattern.compile("^([0-9a-fA-F]+)\\s+\\S+\\s+(_Z\\S+)").matcher("");
  static Matcher readelfLoadMatcher =
    Pattern.compile("^\\s+LOAD\\s+(\\S+)\\s+(\\S+)\\s.*").matcher("");
 
  public SymbolTable(String filename) throws IOException
  {
    Process p = Runtime.getRuntime().exec(ToolPrefix.toolPrefix
                                          + "nm " + filename);
    InputStream es = p.getErrorStream();
    InputStream is = p.getInputStream();

    BufferedReader reader = new BufferedReader(new InputStreamReader(is));
    int count = 0;

    String line;
    while ((line = reader.readLine()) != null)
      {
        interestingSymbol.reset(line);
        if (interestingSymbol.matches())
          {
            try
              {
                String name = interestingSymbol.group(2);
                String addr = interestingSymbol.group(1);
                if (name.startsWith("_ZTVN") || name.endsWith("6class$E"))
                  {
                    long address = MemoryMap.parseHexLong(addr);
                    Long l = new Long(address);
                    map.put(l, name);
                    count++;
                    reverse.put(name, l);
                  }
              }
            catch (NumberFormatException e)
              {
                // ignore it
              }
          }
      }
    es.close();
    is.close();
    p.destroy();
    
    if (count > 0)
      {
        // Assume nm read some symbols from it and that
        // readelf can tell us something about how it is loaded.
        p = Runtime.getRuntime().exec(ToolPrefix.toolPrefix
                                      + "readelf -l " + filename);
        es = p.getErrorStream();
        is = p.getInputStream();

        reader = new BufferedReader(new InputStreamReader(is));
        while ((line = reader.readLine()) != null)
          {
            readelfLoadMatcher.reset(line);
            if (readelfLoadMatcher.matches())
              {
                loadAddr
                  = Long.decode(readelfLoadMatcher.group(2)).longValue();
                break;
              }
          }
        es.close();
        is.close();
        p.destroy();
      }
    
    System.out.println(ToolPrefix.toolPrefix + "nm " + filename
                       + " -> " + count + " symbols");
  }

  public static void main(String args[])
  {
    try
      {
        SymbolTable st = new SymbolTable(args[0]);
        st.dump();
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
      }
  }

  public static String demangleVTName(String n)
  {
    if (n.startsWith("_ZTVN") && n.endsWith("E"))
      return demangle(n.substring(5, n.length() - 1));
    else
      return null;
  }

  public void dump()
  {
    for (Map.Entry<Long, String> me : map.entrySet())
      {
        long address = me.getKey();
        String symbol = me.getValue();
        System.out.println(Long.toHexString(address) + " -> " + symbol);
        if (symbol.startsWith("_ZN") && symbol.endsWith("6class$E"))
          {
            System.out.println("  Class: "
                               + demangle(symbol.substring(3, symbol.length()
                                                           - 8)));
          }
        else if (symbol.startsWith("_ZTVN") && symbol.endsWith("E"))
          {
            System.out.println("  VT: "
                               + demangle(symbol.substring(5, symbol.length()
                                                           - 1)));
          }
      }
  }

  private static String demangle(String symbol)
  {
    StringBuilder sb = new StringBuilder();
    for (int i=0; i<symbol.length(); )
      {
        int l = 0;
        while (i < symbol.length())
          {
            int d = symbol.charAt(i);
            if (d < '0' || d > '9')
              break;
            l = 10 * l + (d - '0');
            i++;
          }
        if (l == 0)
          break; 
        // copy
        if (sb.length() > 0)
          sb.append('.');
        while (l > 0 && i < symbol.length())
          {
            sb.append(symbol.charAt(i));
            l--;
            i++;
          }
      }
    return sb.toString();
  }

  public String getSymbol(long address)
  {
    String symbol = map.get(address);
    if (symbol == null)
      return null;

    if (symbol.startsWith("_ZN") && symbol.endsWith("6class$E"))
      symbol = demangle(symbol.substring(3, symbol.length() - 8));
    return symbol;
  }

  // will return -1 if not found
  public long getAddress(String symbol)
  {
    Long address = reverse.get(symbol);
    if (address == null)
      return -1;
    return address.longValue();
  }
}
