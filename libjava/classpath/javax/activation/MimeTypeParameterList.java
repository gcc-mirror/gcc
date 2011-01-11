/* MimeTypeParameterList.java -- Handle a list of MIME type parameters.
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

package javax.activation;

import gnu.java.lang.CPStringBuilder;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * A list of MIME type parameters, as specified in RFCs 2045 and 2046.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 * @version 1.1
 */
public class MimeTypeParameterList
{

  private final List<String> parameterNames;
  private final Map<String,String> parameterValues;

  /**
   * Constructor for an empty parameter list.
   */
  public MimeTypeParameterList()
  {
    parameterNames = new ArrayList<String>();
    parameterValues = new HashMap<String,String>();
  }

  /**
   * Constructor that parses the specified MIME parameter data.
   * @param parameterList a MIME parameter list string representation
   */
  public MimeTypeParameterList(String parameterList)
    throws MimeTypeParseException
  {
    this();
    parse(parameterList);
  }

  /**
   * Parses the specified MIME parameter data, storing the results in this
   * object.
   * @param parameterList a MIME parameter list string representation
   */
  protected void parse(String parameterList)
    throws MimeTypeParseException
  {
    if (parameterList == null)
      {
        return;
      }
    // Tokenize list into parameters
    char[] chars = parameterList.toCharArray();
    int len = chars.length;
    boolean inQuotedString = false;
    CPStringBuilder buffer = new CPStringBuilder();
    List<String> params = new ArrayList<String>();
    for (int i = 0; i < len; i++)
      {
        char c = chars[i];
        if (c == ';' && !inQuotedString)
          {
            String param = buffer.toString().trim();
            if (param.length() > 0)
              {
                params.add(param);
              }
            buffer.setLength(0);
          }
        else
          {
            if (c == '"')
              {
                inQuotedString = !inQuotedString;
              }
            buffer.append(c);
          }
      }
    String param = buffer.toString().trim();
    if (param.length() > 0)
      {
        params.add(param);
      }

    // Tokenize each parameter into name + value
    for (Iterator<String> i = params.iterator(); i.hasNext();)
      {
        param = i.next();
        int ei = param.indexOf('=');
        if (ei == -1)
          {
            throw new MimeTypeParseException("Couldn't find the '=' that " +
                                             "separates a parameter name " +
                                             "from its value.");
          }
        String name = param.substring(0, ei).trim();
        MimeType.checkValidity(name, "Parameter name is invalid");
        String value = param.substring(ei + 1).trim();
        len = value.length();
        if (len > 1 && value.charAt(0) == '"' &&
            value.charAt(len - 1) == '"')
          {
            value = unquote(value);
          }
        else
          {
            MimeType.checkValidity(name, "Parameter value is invalid");
          }

        parameterNames.add(name);
        parameterValues.put(name.toLowerCase(), value);
      }
  }

  /**
   * Returns the number of parameters.
   */
  public synchronized int size()
  {
    return parameterNames.size();
  }

  /**
   * Indicates if there are no parameters.
   */
  public synchronized boolean isEmpty()
  {
    return parameterNames.isEmpty();
  }

  /**
   * Returns the value for the specified parameter name.
   * @param name the parameter name
   */
  public synchronized String get(String name)
  {
    name = name.trim();
    return parameterValues.get(name.toLowerCase());
  }

  /**
   * Sets the value for the specified parameter name.
   * @param name the parameter name
   * @param value the parameter value
   */
  public synchronized void set(String name, String value)
  {
    name = name.trim();
    boolean exists = false;
    for (String pname : parameterNames)
      {
        if (name.equalsIgnoreCase(pname))
          {
            exists = true;
          }
      }
    if (!exists)
      {
        parameterNames.add(name);
      }
    parameterValues.put(name.toLowerCase(), value);
  }

  /**
   * Removes the parameter identified by the specified name.
   * @param name the parameter name
   */
  public synchronized void remove(String name)
  {
    name = name.trim();
    for (Iterator<String> i = parameterNames.iterator();i.hasNext();)
      {
        String pname = i.next();
        if (name.equalsIgnoreCase(pname))
          {
            i.remove();
          }
      }
    parameterValues.remove(name.toLowerCase());
  }

  /**
   * Returns an enumeration of all the parameter names.
   */
  // Raw type is forced by public spec.
  @SuppressWarnings("unchecked")
  public synchronized Enumeration getNames()
  {
    return new IteratorEnumeration(parameterNames.iterator());
  }

  /**
   * Returns an RFC 2045-compliant string representation of this parameter
   * list.
   */
  public synchronized String toString()
  {
    CPStringBuilder buffer = new CPStringBuilder();
    for (String name : parameterNames)
      {
        String value = parameterValues.get(name.toLowerCase());

        buffer.append(';');
        buffer.append(' ');
        buffer.append(name);
        buffer.append('=');
        buffer.append(quote(value));
      }
    return buffer.toString();
  }

  private static String quote(String value)
  {
    boolean needsQuoting = false;
    int len = value.length();
    for (int i = 0; i < len; i++)
      {
        if (!MimeType.isValidChar(value.charAt(i)))
          {
            needsQuoting = true;
            break;
          }
      }

    if (needsQuoting)
      {
        CPStringBuilder buffer = new CPStringBuilder();
        buffer.append('"');
        for (int i = 0; i < len; i++)
          {
            char c = value.charAt(i);
            if (c == '\\' || c == '"')
              {
                buffer.append('\\');
              }
            buffer.append(c);
          }
        buffer.append('"');
        return buffer.toString();
      }
    return value;
  }

  private static String unquote(String value)
  {
    int len = value.length();
    CPStringBuilder buffer = new CPStringBuilder();
    for (int i = 1; i < len - 1; i++)
      {
        char c = value.charAt(i);
        if (c == '\\')
          {
            i++;
            if (i < len - 1)
              {
                c = value.charAt(i);
                if (c != '\\' && c != '"')
                  {
                    buffer.append('\\');
                  }
              }
          }
        buffer.append(c);
      }
    return buffer.toString();
  }

  /**
   * Enumeration proxy for an Iterator.
   */
  static class IteratorEnumeration
    implements Enumeration<String>
  {

    final Iterator<String> iterator;

    IteratorEnumeration(Iterator<String> iterator)
    {
      this.iterator = iterator;
    }

    public boolean hasMoreElements()
    {
      return iterator.hasNext();
    }

    public String nextElement()
    {
      return iterator.next();
    }

  }

}
