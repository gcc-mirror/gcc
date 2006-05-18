/* RequestedAttributes.java -- 
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.javax.print.ipp.attribute;

import java.util.ArrayList;
import java.util.List;

import javax.print.attribute.Attribute;

/**
 * <code>RequestedAttributes</code> specifies the requested
 * attributes in an IPP request operation.
 * 
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class RequestedAttributes implements Attribute
{
  private ArrayList attributes;

  /**
   * Creates a <code>RequestedAttributes</code> object with 
   * the initial value.
   *
   * @param value the string for the ipp name
   *
   * @exception NullPointerException if value is null
   */
  public RequestedAttributes(String value)
  {
    if (value == null)
      throw new NullPointerException();
    
    attributes = new ArrayList();      
    attributes.add(value);
  }
  
  /**
   * Adds the IPP name value to the set.
   * 
   * @param value the string for the ipp name
   */
  public void addValue(String value)
  {
    attributes.add(value);
  }
  
  /**
   * Returns the values.
   * 
   * @return The values as list.
   */
  public List getValues() 
  {
    return attributes;    
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>DocumentFormat</code> itself.
   */
  public Class getCategory()
  {
    return RequestedAttributes.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "requested-attributes".
   */
  public String getName()
  {
    return "requested-attributes";
  }

  /**
   * Returns the string representation for this object.
   *
   * @return The string representation.
   */
  public String toString()
  {
    StringBuffer b = new StringBuffer();
    
    if (attributes.size() > 0)
      b.append(attributes.get(0));
    
    for (int i=1; i < attributes.size(); i++)
      b.append(", " + attributes.get(i));
    
    return b.toString();
  }  
}
