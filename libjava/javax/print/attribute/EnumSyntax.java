/* EnumSyntax.java -- 
   Copyright (C) 2003 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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

package javax.print.attribute;

import java.io.Serializable;

/**
 * @author Michael Koch
 */
public abstract class EnumSyntax implements Cloneable, Serializable
{
  private static final long serialVersionUID = -2739521845085831642L;
  
  private int value;

  /**
   * Creates a <code>EnumSyntax</code> object.
   *
   * @param value the value to set
   */
  protected EnumSyntax(int value)
  {
    this.value = value;
  }

  /**
   * Returns the value of this object.
   *
   * @return the value
   */
  public int getValue()
  {
    return value;
  }

  /**
   * Clones this object.
   *
   * @return a clone of this object
   */
  public Object clone()
  {
    try
      {
        return super.clone();
      }
    catch (CloneNotSupportedException e)
      {
        // Cannot happen as we implement java.lang.Cloneable.
        return null;
      }
  }

  /**
   * Returns the hashcode for this object.
   *
   * @return the hashcode
   */
  public int hashCode()
  {
    return value;
  }

  /**
   * Returns the string representation for this object.
   *
   * @return the string representation
   */
  public String toString()
  {
    int index = value - getOffset();
    String[] table = getStringTable();

    if (table != null
        && index >= 0
        && index < table.length)
      return table[index];
    
    return "" + value;
  }

  /**
   * Returns a table with the enumeration values represented as strings
   * for this object.
   *
   * The default implementation just returns null.
   *
   * @return the enumeration values as strings
   */
  protected String[] getStringTable()
  {
    return null;
  }

  /**
   * Returns a table with the enumeration values for this object.
   *
   * The default implementation just returns null.
   *
   * @return the enumeration values
   */
  protected EnumSyntax[] getEnumValueTable()
  {
    return null;
  }

  public int getOffset()
  {
    return 0;
  }
}
