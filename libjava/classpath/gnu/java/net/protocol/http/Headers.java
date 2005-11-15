/* Headers.java --
   Copyright (C) 2004 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package gnu.java.net.protocol.http;

import gnu.java.net.LineInputStream;

import java.io.IOException;
import java.io.InputStream;
import java.text.DateFormat;
import java.text.ParseException;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

/**
 * A collection of HTTP header names and associated values.
 * Retrieval of values is case insensitive. An iteration over the keys
 * returns the header names in the order they were received.
 *
 * @author Chris Burdess (dog@gnu.org)
 */
public class Headers
  extends LinkedHashMap
{

  static final DateFormat dateFormat = new HTTPDateFormat();

  static class Header
  {

    final String name;

    Header(String name)
    {
      if (name == null || name.length() == 0)
        {
          throw new IllegalArgumentException(name);
        }
      this.name = name;
    }

    public int hashCode()
    {
      return name.toLowerCase().hashCode();
    }

    public boolean equals(Object other)
    {
      if (other instanceof Header)
        {
          return ((Header) other).name.equalsIgnoreCase(name);
        }
      return false;
    }

    public String toString()
    {
      return name;
    }
    
  }

  static class HeaderEntry
    implements Map.Entry
  {

    final Map.Entry entry;

    HeaderEntry(Map.Entry entry)
    {
      this.entry = entry;
    }

    public Object getKey()
    {
      return ((Header) entry.getKey()).name;
    }

    public Object getValue()
    {
      return entry.getValue();
    }

    public Object setValue(Object value)
    {
      return entry.setValue(value);
    }

    public int hashCode()
    {
      return entry.hashCode();
    }

    public boolean equals(Object other)
    {
      return entry.equals(other);
    }

    public String toString()
    {
      return getKey().toString() + "=" + getValue();
    }
    
  }

  public Headers()
  {
  }

  public boolean containsKey(Object key)
  {
    return super.containsKey(new Header((String) key));
  }

  public Object get(Object key)
  {
    return super.get(new Header((String) key));
  }

  /**
   * Returns the value of the specified header as a string.
   */
  public String getValue(String header)
  {
    return (String) super.get(new Header(header));
  }

  /**
   * Returns the value of the specified header as an integer,
   * or -1 if the header is not present or not an integer.
   */
  public int getIntValue(String header)
  {
    String val = getValue(header);
    if (val == null)
      {
        return -1;
      }
    try
      {
        return Integer.parseInt(val);
      }
    catch (NumberFormatException e)
      {
      }
    return -1;
  }

  /**
   * Returns the value of the specified header as a long, or -1 if the
   * header is not present or cannot be parsed as a long.
   */
  public long getLongValue(String header)
  {
    String val = getValue(header);
    if (val == null)
      {
        return -1;
      }
    try
      {
        return Long.parseLong(val);
      }
    catch (NumberFormatException e)
      {
      }
    return -1;
  }

  /**
   * Returns the value of the specified header as a date,
   * or <code>null</code> if the header is not present or not a date.
   */
  public Date getDateValue(String header)
  {
    String val = getValue(header);
    if (val == null)
      {
        return null;
      }
    try
      {
        return dateFormat.parse(val);
      }
    catch (ParseException e)
      {
        return null;
      }
  }

  public Object put(Object key, Object value)
  {
    return super.put(new Header((String) key), value);
  }

  public Object remove(Object key)
  {
    return super.remove(new Header((String) key));
  }

  public void putAll(Map t)
  {
    for (Iterator i = t.keySet().iterator(); i.hasNext(); )
      {
        String key = (String) i.next();
        String value = (String) t.get(key);
        put(key, value);
      }
  }
  
  public Set keySet()
  {
    Set keys = super.keySet();
    Set ret = new LinkedHashSet();
    for (Iterator i = keys.iterator(); i.hasNext(); )
      {
        ret.add(((Header) i.next()).name);
      }
    return ret;
  }

  public Set entrySet()
  {
    Set entries = super.entrySet();
    Set ret = new LinkedHashSet();
    for (Iterator i = entries.iterator(); i.hasNext(); )
      {
        Map.Entry entry = (Map.Entry) i.next();
        ret.add(new HeaderEntry(entry));
      }
    return ret;
  }

  /**
   * Parse the specified input stream, adding headers to this collection.
   */
  public void parse(InputStream in)
    throws IOException
  {
    LineInputStream lin = (in instanceof LineInputStream) ?
      (LineInputStream) in : new LineInputStream(in);
    
    String name = null;
    StringBuilder value = new StringBuilder();
    while (true)
      {
        String line = lin.readLine();
        if (line == null)
          {
            if (name != null)
              {
                addValue(name, value.toString());
              }
            break;
          }
        int len = line.length();
        if (len < 2)
          {
            if (name != null)
              {
                addValue(name, value.toString());
              }
            break;
          }
        char c1 = line.charAt(0);
        if (c1 == ' ' || c1 == '\t')
          {
            // Continuation
	    int last = len - 1;
	    if (line.charAt(last) != '\r')
	      ++last;
            value.append(line.substring(0, last));
          }
        else
          {
            if (name != null)
              {
                addValue(name, value.toString());
              }
            
            int di = line.indexOf(':');
            name = line.substring(0, di);
            value.setLength(0);
            do
              {
                di++;
              }
            while (di < len && line.charAt(di) == ' ');
	    int last = len - 1;
	    if (line.charAt(last) != '\r')
	      ++last;
            value.append(line.substring(di, last));
          }
      }
  }
  
  private void addValue(String name, String value)
  {
    Header key = new Header(name);
    String old = (String) super.get(key);
    if (old == null)
      {
        super.put(key, value);
      }
    else
      {
        super.put(key, old + ", " + value);
      }
  }
  
}

