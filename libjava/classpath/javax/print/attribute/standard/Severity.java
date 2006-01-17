/* Severity.java --
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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

package javax.print.attribute.standard;

import javax.print.attribute.Attribute;
import javax.print.attribute.EnumSyntax;


/**
 * The <code>Severity</code> printing attribute specifies the severity
 * for a <code>PrinterStateReason</code> attribute.
 * <p>
 * This attribute does not appear in the attribute set of a print service 
 * itself. Its used inside the <code>PrinterStateReasons</code> 
 * attribute which contains <code>PrinterStateReason</code> objects which 
 * informs about the print service's status.
 * </p>
 * <p>
 * <b>IPP Compatibility:</b> Severity is not an IPP attribute on its own
 * but used in the PrinterStateReason attribute to indicate the severity.
 * </p>
 * 
 * @author Michael Koch (konqueror@gmx.de)
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class Severity extends EnumSyntax
  implements Attribute
{
  private static final long serialVersionUID = 8781881462717925380L;

  /**
   * Indicates that the reason is a report.
   */
  public static final Severity REPORT = new Severity(0);
  
  /**
   * Indicates that the reason is a warning.
   */
  public static final Severity WARNING = new Severity(1);
  
  /**
   * Indicates that the reason is an error.
   */
  public static final Severity ERROR = new Severity(2);

  private static final String[] stringTable = { "report", "warning", "error" };
  
  private static final Severity[] enumValueTable = { REPORT, WARNING, ERROR };
  
  /**
   * Constructs a <code>Severity</code> object.
   *
   * @param value the enum value.
   */
  protected Severity(int value)
  {
    super(value);
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>Severity</code> itself.
   */
  public Class getCategory()
  {
    return Severity.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "severity".
   */
  public String getName()
  {
    return "severity";
  }
  
  /**
   * Returns a table with the enumeration values represented as strings
   * for this object.
   *
   * @return The enumeration values as strings.
   */
  protected String[] getStringTable()
  {
    return stringTable;
  }

  /**
   * Returns a table with the enumeration values for this object.
   *
   * @return The enumeration values.
   */
  protected EnumSyntax[] getEnumValueTable()
  {
    return enumValueTable;
  }
}
