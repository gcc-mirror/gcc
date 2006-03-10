/* Headers.java --
   Copyright (C) 2004, 2006 Free Software Foundation, Inc.

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
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

/**
 * A collection of HTTP header names and associated values.  The
 * values are {@link ArrayList ArrayLists} of Strings.  Retrieval of
 * values is case insensitive. An iteration over the collection
 * returns the header names in the order they were received.
 *
 * @author Chris Burdess (dog@gnu.org)
 * @author David Daney (ddaney@avtrex.com)
 */
class Headers
{
  /**
   * A list of HeaderElements
   *
   */
  private final ArrayList headers = new ArrayList();
  
  static final DateFormat dateFormat = new HTTPDateFormat();

  static class HeaderElement
  {
    String name;
    String value;

    HeaderElement(String name, String value)
    {
      this.name = name;
      this.value = value;
    }
  }

  public Headers()
  {
  }

  /**
   * Return an Iterator over this collection of headers.
   * Iterator.getNext() returns objects of type {@link HeaderElement}.
   *
   * @return the Iterator.
   */
  Iterator iterator()
  {
    return headers.iterator();
  }
  
  /**
   * Returns the value of the specified header as a string.  If
   * multiple values are present, the last one is returned.
   */
  public String getValue(String header)
  {
    for (int i = headers.size() - 1; i >= 0; i--)
      {
        HeaderElement e = (HeaderElement)headers.get(i);
        if (e.name.equalsIgnoreCase(header))
          {
            return e.value;
          }
      }
    return null;
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

  /**
   * Add a header to this set of headers.  If there is an existing
   * header with the same name, it is discarded.
   *
   * @param name the header name
   * @param value the header value
   *
   * @see #addValue
   */
  public void put(String name, String value)
  {
    remove(name);
    headers.add(headers.size(), new HeaderElement(name, value));
  }

  /**
   * Add all headers from a set of headers to this set.  If any of the
   * headers to be added have the same name as existing headers, the
   * existing headers will be discarded.
   *
   * @param o the headers to be added
   */
  public void putAll(Headers o)
  {
    for (Iterator it = o.iterator(); it.hasNext(); )
      {
        HeaderElement e = (HeaderElement)it.next();
        remove(e.name);
      }
    for (Iterator it = o.iterator(); it.hasNext(); )
      {
        HeaderElement e = (HeaderElement)it.next();
        addValue(e.name, e.value);
      }
  }

  /**
   * Remove a header from this set of headers.  If there is more than
   * one instance of a header of the given name, they are all removed.
   *
   * @param name the header name
   */
  public void remove(String name)
  {
    for (Iterator it = headers.iterator(); it.hasNext(); )
      {
        HeaderElement e = (HeaderElement)it.next();
        if (e.name.equalsIgnoreCase(name))
          it.remove();
      }
  }

  /**
   * Parse the specified InputStream, adding headers to this collection.
   *
   * @param in the InputStream.
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
  

  /**
   * Add a header to this set of headers.  If there is an existing
   * header with the same name, it is not effected.
   *
   * @param name the header name
   * @param value the header value
   *
   * @see #put
   */
  public void addValue(String name, String value)
  {
    headers.add(headers.size(), new HeaderElement(name, value));
  }

  /**
   * Get a new Map containing all the headers.  The keys of the Map
   * are Strings (the header names).  The values of the Map are
   * unmodifiable Lists containing Strings (the header values).
   *
   * <p>
   * 
   * The returned map is modifiable.  Changing it will not effect this
   * collection of Headers in any way.
   *
   * @return a Map containing all the headers.
   */
  public Map getAsMap()
  {
    LinkedHashMap m = new LinkedHashMap();
    for (Iterator it = headers.iterator(); it.hasNext(); )
      {
        HeaderElement e = (HeaderElement)it.next();
        ArrayList l = (ArrayList)m.get(e.name);
        if (l == null)
          {
            l = new ArrayList(1);
            l.add(e.value);
            m.put(e.name, l);
          }
        else
          l.add(0, e.value);
      }
    for (Iterator it = m.entrySet().iterator(); it.hasNext(); )
      {
        Map.Entry me = (Map.Entry)it.next();
        ArrayList l = (ArrayList)me.getValue();
        me.setValue(Collections.unmodifiableList(l));
      }
    return m;
  }
  
  /**
   * Get the name of the Nth header.
   *
   * @param i the header index.
   *
   * @return the header name.
   *
   * @see #getHeaderValue
   */
  public String getHeaderName(int i)
  {
    if (i >= headers.size() || i < 0)
      return null;
    
    return ((HeaderElement)headers.get(i)).name;
  }

  /**
   * Get the value of the Nth header.
   *
   * @param i the header index.
   *
   * @return the header value.
   *
   * @see #getHeaderName
   */
  public String getHeaderValue(int i)
  {
    if (i >= headers.size() || i < 0)
      return null;
    
    return ((HeaderElement)headers.get(i)).value;
  }
  
}

