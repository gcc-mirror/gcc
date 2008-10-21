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

import gnu.java.lang.CPStringBuilder;

import gnu.java.net.LineInputStream;

import java.io.IOException;
import java.io.InputStream;
import java.lang.Iterable;
import java.text.DateFormat;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * A collection of HTTP header names and associated values.  The
 * values are {@link ArrayList ArrayLists} of Strings.  Retrieval of
 * values is case insensitive. An iteration over the collection
 * returns the header names in the order they were received.
 *
 * @author Chris Burdess (dog@gnu.org)
 * @author David Daney (ddaney@avtrex.com)
 */
class Headers implements Iterable<Headers.HeaderElement>
{
  /**
   * A list of HeaderElements
   */
  private final ArrayList<HeaderElement> headers
    = new ArrayList<HeaderElement>();
  
  /**
   * The HTTP dateformat used to parse date header fields.
   */
  private static final DateFormat dateFormat = new HTTPDateFormat();

  /**
   * Class for a Header element consisting of
   * a name and value String.
   */
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

  /**
   * Default constructor.
   */
  public Headers()
  {
    // nothing to do
  }

  /**
   * Return an Iterator over this collection of headers.
   * Iterator.getNext() returns objects of type {@link HeaderElement}.
   *
   * @return the Iterator.
   */
  public Iterator<HeaderElement> iterator()
  {
    return headers.iterator();
  }
  
  /**
   * Returns the value of the specified header as a string. If
   * multiple values are present, the last one is returned.
   * 
   * @param header the header name (case insensitive search)
   * @return The header value or <code>null</code> if not found.
   */
  public String getValue(String header)
  {
    for (int i = headers.size() - 1; i >= 0; i--)
      {
        HeaderElement e = headers.get(i);
        if (e.name.equalsIgnoreCase(header))
          {
            return e.value;
          }
      }
    return null;
  }

  /**
   * Returns the value of the specified header as an integer. If
   * multiple values are present, the last one is returned.
   * 
   * @param header the header name (case insensitive search)
   * @return The header value or <code>-1</code> if not present or
   * not an integer value.
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
        // fall through
      }
    return -1;
  }

  /**
   * Returns the value of the specified header as a long. If
   * multiple values are present, the last one is returned.
   * 
   * @param header the header name (case insensitive search)
   * @return The header value or <code>-1</code> if not present or
   * not a long value.
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
        // fall through
      }
    return -1;
  }

  /**
   * Returns the value of the specified header as a date. If
   * multiple values are present, the last one is returned.
   * 
   * @param header the header name (case insensitive search)
   * @return The header value or <code>null</code> if not present or
   * not a date value.
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
   * header with the same name it's value is replaced with the new value.
   * If multiple headers of the same name exist only the last one's value 
   * is replaced.
   *
   * @param name the header name
   * @param value the header value
   *
   * @see #addValue(String, String)
   */
  public void put(String name, String value)
  {    
    for (int i = headers.size() - 1; i >= 0; i--)
      {
        HeaderElement e = headers.get(i);
        if (e.name.equalsIgnoreCase(name))
          {
            e.value = value;
            return;
          }
      }
    
    // nothing was replaced so add it as new HeaderElement
    addValue(name, value);
  }
  
  /**
   * Add all headers from a set of headers to this set. Any existing header 
   * with the same (case insensitive) name as one of the new headers will 
   * be overridden.
   *
   * @param o the headers to be added
   */
  public void putAll(Headers o)
  {
    for (Iterator<HeaderElement> it = o.iterator(); it.hasNext(); )
      {
        HeaderElement e = it.next();
        remove(e.name);
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
    for (Iterator<HeaderElement> it = headers.iterator(); it.hasNext(); )
      {
        HeaderElement e = it.next();
        if (e.name.equalsIgnoreCase(name))
          it.remove();
      }
  }

  /**
   * Parse the specified InputStream, adding headers to this collection.
   *
   * @param in the InputStream.
   * @throws IOException if I/O error occured.
   */
  public void parse(InputStream in)
    throws IOException
  {
    LineInputStream lin = (in instanceof LineInputStream) ?
      (LineInputStream) in : new LineInputStream(in);
    
    String name = null;
    CPStringBuilder value = new CPStringBuilder();
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
   * @see #put(String, String)
   */
  public void addValue(String name, String value)
  {
    headers.add(headers.size(), new HeaderElement(name, value));
  }

  /**
   * Get a new Map containing all the headers.  The keys of the Map
   * are Strings (the header names). The headers will be included 
   * case-sensitive in the map so that querying must be done with the
   * correct case of the needed header name. The values of the Map are
   * unmodifiable Lists containing Strings (the header values).
   *
   * <p> 
   * The returned map is modifiable. Changing it will not effect this
   * collection of Headers in any way.</p>
   *
   * @return a Map containing all the headers.
   */
  public Map<String,List<String>> getAsMap()
  {
    LinkedHashMap<String,List<String>> m = new LinkedHashMap<String,List<String>>();
    for (Iterator<HeaderElement> it = headers.iterator(); it.hasNext(); )
      {
        HeaderElement e = it.next();
        ArrayList<String> l = (ArrayList<String>)m.get(e.name);
        if (l == null)
          {
            l = new ArrayList<String>(1);
            l.add(e.value);
            m.put(e.name, l);
          }
        else
          l.add(0, e.value);
      }
    for (Iterator<Map.Entry<String,List<String>>> it = m.entrySet().iterator(); it.hasNext(); )
      {
        Map.Entry<String,List<String>> me = it.next();
        List<String> l = me.getValue();
        me.setValue(Collections.unmodifiableList(l));
      }
    return m;
  }
  
  /**
   * Get the name of the Nth header.
   *
   * @param i the header index.
   *
   * @return The header name, or <code>null</code> if index outside of range.
   *
   * @see #getHeaderValue(int)
   */
  public String getHeaderName(int i)
  {
    if (i >= headers.size() || i < 0)
      return null;
    
    return headers.get(i).name;
  }

  /**
   * Get the value of the Nth header.
   *
   * @param i the header index.
   *
   * @return the header value, or <code>null</code> if index outside of range.
   *
   * @see #getHeaderName(int)
   */
  public String getHeaderValue(int i)
  {
    if (i >= headers.size() || i < 0)
      return null;
    
    return headers.get(i).value;
  }
}
