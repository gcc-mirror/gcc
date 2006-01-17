/* Chromaticity.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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

import javax.print.attribute.DocAttribute;
import javax.print.attribute.EnumSyntax;
import javax.print.attribute.PrintJobAttribute;
import javax.print.attribute.PrintRequestAttribute;

/**
 * The <code>Chromaticity</code> printing attribute specifies if print data
 * should be printed in monochrome or color.
 * <p>
 * The attribute interacts with the document to be printed. If the document
 * to be printed is a monochrome document it will be printed monochrome 
 * regardless of the value of this attribute category. However if it is a
 * color document supplying the attribute value <code>MONOCHROME</code>
 * will prepare the document to be printed in monochrome instead of color.
 * </p>
 * <p>
 * This printing attribute has nothing to do with the capabilities of the
 * printer device. To check if a specific printer service supports printing
 * in color you have to use the attribute
 * {@link javax.print.attribute.standard.ColorSupported}
 * </p>
 * <p>
 * <b>IPP Compatibility:</b> Chromaticity is not an IPP 1.1 attribute.
 * </p>
 *  
 * @author Michael Koch (konqueror@gmx.de)
 */
public final class Chromaticity extends EnumSyntax
  implements DocAttribute, PrintRequestAttribute, PrintJobAttribute
{
  private static final long serialVersionUID = 4660543931355214012L;
  
  /** Specifies monochrome printing. */
  public static final Chromaticity MONOCHROME = new Chromaticity(0);
  
  /** Specifies color printing. */
  public static final Chromaticity COLOR = new Chromaticity(1);
  
  private static final String[] stringTable = { "monochrome", "color" };
  private static final Chromaticity[] enumValueTable = { MONOCHROME, COLOR };

  /**
   * Creates a <code>Chromaticity</code> object.
   *
   * @param value the enum value
   */
  protected Chromaticity(int value)
  {
    super(value);
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>Chromaticity</code> itself.
   */
  public Class getCategory()
  {
    return Chromaticity.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "chromaticity".
   */
  public String getName()
  {
    return "chromaticity";
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
