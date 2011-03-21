/* HeaderFieldHelper.java -- Helps manage headers fields
   Copyright (C) 1998, 2003 Free Software Foundation, Inc.

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


package gnu.java.net;

import java.util.HashMap;
import java.util.Map;
import java.util.Vector;

/**
 * This class manages header field keys and values.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public class HeaderFieldHelper
{
  private Vector<String> headerFieldKeys;
  private Vector<String> headerFieldValues;

  public HeaderFieldHelper()
  {
    this (10);
  }

  public HeaderFieldHelper (int size)
  {
    headerFieldKeys = new Vector<String> (size);
    headerFieldValues = new Vector<String> (size);
  }

  public void addHeaderField (String key, String value)
  {
    headerFieldKeys.addElement (key);
    headerFieldValues.addElement (value);
  }

  public String getHeaderFieldKeyByIndex (int index)
  {
    String key = null;

    try
      {
        key = headerFieldKeys.elementAt (index);
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
      }

    return key;
  }

  public String getHeaderFieldValueByIndex(int index)
  {
    String value = null;

    try
      {
        value = headerFieldValues.elementAt (index);
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
      }

    return value;
  }

  public String getHeaderFieldValueByKey(String key)
  {
    String value = null;

    try
      {
        value = headerFieldValues.elementAt(headerFieldKeys.indexOf(key));
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
      }

    return value;
  }

  public Map<String, String> getHeaderFields()
  {
    HashMap<String, String> headers = new HashMap<String, String>();
    int max = headerFieldKeys.size();

    for (int index = 0; index < max; index++)
      {
        headers.put(headerFieldKeys.elementAt(index),
                    headerFieldValues.elementAt(index));
      }

    return headers;
  }

  public int getNumberOfEntries()
  {
    return headerFieldKeys.size();
  }

} // class HeaderFieldHelper
