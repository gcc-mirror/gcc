/* EnumSyntax.java --
   Copyright (C) 2003, 2005 Free Software Foundation, Inc.

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

package javax.print.attribute;

import java.io.InvalidObjectException;
import java.io.ObjectStreamException;
import java.io.Serializable;

/**
 * <code>EnumSyntax</code> is the abstract base class of all enumeration
 * classes in the Java Print Service API.
 * <p>
 * Every enumeration class which extends from EnumSyntax provides several
 * enumeration objects as singletons of its class.
 * </p>
 * <p>
 * Notes for implementing subclasses:
 * <ul>
 *   <li>
 *     The values of all enumeration singelton instances have to be in a
 *     sequence which may start at any value. See: {@link #getOffset()}
 *   </li>
 *   <li>
 *     Subclasses have to override {@link #getEnumValueTable()} and should
 *     override {@link #getStringTable()} for correct serialization.
 *   </li>
 * </ul>
 * </p>
 * Example:
 * <pre>
 * public class PrinterState extends EnumSyntax
 * {
 *   public static final PrinterState IDLE = new PrinterState(1);
 *   public static final PrinterState PROCESSING = new PrinterState(2);
 *   public static final PrinterState STOPPED = new PrinterState(3);
 *
 *   protected PrinterState(int value)
 *   {
 *     super(value);
 *   }
 *
 *   // Overridden because values start not at zero !
 *   protected int getOffset()
 *   {
 *     return 1;
 *   }
 *
 *   private static final String[] stringTable = { "idle", "processing",
 *                                                 "stopped" };
 *
 *   protected String[] getStringTable()
 *   {
 *     return stringTable;
 *   }
 *
 *   private static final PrinterState[] enumValueTable = { IDLE,
 *                                             PROCESSING, STOPPED};
 *
 *   protected EnumSyntax[] getEnumValueTable()
 *   {
 *     return enumValueTable;
 *   }
 * }
 * </pre>
 *
 * @author Michael Koch (konqueror@gmx.de)
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public abstract class EnumSyntax implements Cloneable, Serializable
{
  private static final long serialVersionUID = -2739521845085831642L;

  private int value;

  /**
   * Creates a <code>EnumSyntax</code> object.
   *
   * @param value the value to set.
   */
  protected EnumSyntax(int value)
  {
    this.value = value;
  }

  /**
   * Returns the value of this enumeration object.
   *
   * @return The value.
   */
  public int getValue()
  {
    return value;
  }

  /**
   * Clones this object.
   *
   * @return A clone of this object.
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
   * The hashcode is the value of this enumeration object.
   *
   * @return The hashcode.
   */
  public int hashCode()
  {
    return value;
  }

  /**
   * Returns the string representation for this object.
   * The string value from <code>getStringTable()</code> method is returned
   * if subclasses override this method. Otherwise the value of this object
   * as a string is returned.
   *
   * @return The string representation.
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
   * The default implementation just returns null. Subclasses should
   * override this method.
   *
   * @return The enumeration values as strings.
   */
  protected String[] getStringTable()
  {
    return null;
  }

  /**
   * Needed for singelton semantics during deserialisation.
   *
   * Subclasses must not override this class. Subclasses have to override
   * <code>getEnumValueTable()</code> and should override
   * <code>getStringTable()</code> for correct serialization.
   *
   * @return The Object at index <code>value - getOffset()</code>
   *         in getEnumValueTable.
   * @throws ObjectStreamException if getEnumValueTable() returns null.
   */
  protected Object readResolve() throws ObjectStreamException
  {
    EnumSyntax[] table = getEnumValueTable();
    if (table == null)
      throw new InvalidObjectException("Null enumeration value table "
                                       + "for class "
                                       + this.getClass().toString());

    return table[value - getOffset()];
  }

  /**
   * Returns a table with the enumeration values for this object.
   *
   * The default implementation just returns null. Subclasses have to
   * to override this method for serialization.
   *
   * @return The enumeration values.
   */
  protected EnumSyntax[] getEnumValueTable()
  {
    return null;
  }

  /**
   * Returns the lowest used value by the enumerations of this class.
   *
   * The default implementation returns 0. This is enough if enumerations
   * start with a zero value. Otherwise subclasses need to override this
   * method for serialization and return the lowest value they use.
   * .
   * @return The lowest used value used.
   */
  protected int getOffset()
  {
    return 0;
  }
}
