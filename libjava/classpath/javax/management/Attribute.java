/* Attribute.java --
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

package javax.management;

import java.io.Serializable;

/**
 * Represents an MBean attribute, having the name and the assigned value. The
 * MBean objects use this class to get and set attributes values.
 * 
 * @since 1.5
 * 
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class Attribute
  implements Serializable
{
  /**
   * The attribute name.
   */
  final String m_name;

  /**
   * The attribute value.
   */
  final Object m_value;

  /**
   * Create the attribute with the given name and value.
   * 
   * @param name the attribute name
   * @param value the attribute value
   */
  public Attribute(String name, Object value)
  {
    m_name = name;
    m_value = value;
  }

  /**
   * Compares the attribute with another attribute.
   * 
   * @param other the other object to compare with
   * 
   * @return true if both value and object are equal, false otherwise.
   */
  public boolean equals(Object other)
  {
    if (other instanceof Attribute)
      {
        Attribute oa = (Attribute) other;
        boolean n, v;
        if (oa.m_name == null || m_name == null)
          n = oa.m_name == m_name;
        else
          n = oa.m_name.equals(m_name);

        if (oa.m_value == null || m_value == null)
          v = oa.m_value == m_value;
        else
          v = oa.m_value.equals(m_value);
        
        return n && v;

      }
    else
      return false;
  }

  /**
   * Returns the attribute name.
   * 
   * @return the attribute name
   */
  public String getName()
  {
    return m_name;
  }

  /**
   * Returns the attribute value.
   * 
   * @return the attribute value.
   */
  public Object getValue()
  {
    return m_value;
  }

  /**
   * Need to override as {@link #equals} is overridden.
   * 
   * @return the expression, dependent of the object and name hashcodes.
   */
  public int hashCode()
  {
    int n = m_name == null ? 0 : m_name.hashCode();
    int v = m_value == null ? 0 : m_value.hashCode();
    
    return n ^ v;
  }

}
