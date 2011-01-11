/* OrientationRequested.java --
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
import javax.print.attribute.DocAttribute;
import javax.print.attribute.EnumSyntax;
import javax.print.attribute.PrintJobAttribute;
import javax.print.attribute.PrintRequestAttribute;


/**
 * The <code>OrientationRequested</code> printing attribute specifies
 * the desired orientation of the print data on the media sheet.
 * <p>
 * The effect of this attribute may depend on the document format as
 * some document formats (e.g. postscript) contains the orientation
 * inside the print data. However for other formats like e.g. plain
 * text this attribute will have an effect on the orientation.
 * </p>
 * <p>
 * <b>IPP Compatibility:</b> OrientationRequested is an IPP 1.1 attribute.
 * </p>
 *
 * @author Michael Koch (konqueror@gmx.de)
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class OrientationRequested extends EnumSyntax
  implements DocAttribute, PrintRequestAttribute, PrintJobAttribute
{
  private static final long serialVersionUID = -4447437289862822276L;

  /**
   * Orientation as portrait.
   */
  public static final OrientationRequested PORTRAIT =
    new OrientationRequested(3);

  /**
   * Orientation as landscape.
   */
  public static final OrientationRequested LANDSCAPE =
    new OrientationRequested(4);

  /**
   * Orientation as reversed landscape.
   */
  public static final OrientationRequested REVERSE_LANDSCAPE =
    new OrientationRequested(5);

  /**
   * Orientation as reversed portrait.
   */
  public static final OrientationRequested REVERSE_PORTRAIT =
    new OrientationRequested(6);


  private static final String[] stringTable = { "portrait", "landscape",
                                                "reverse-landscape",
                                                "reverse-portrait" };

  private static final OrientationRequested[]
      enumValueTable = { PORTRAIT, LANDSCAPE,
                         REVERSE_LANDSCAPE, REVERSE_PORTRAIT };

  /**
   * Constructs a <code>OrientationRequested</code> object.
   *
   * @param value the value
   */
  protected OrientationRequested(int value)
  {
    super(value);
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>OrientationRequested</code> itself.
   */
  public Class< ? extends Attribute> getCategory()
  {
    return OrientationRequested.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "orientation-requested".
   */
  public String getName()
  {
    return "orientation-requested";
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

  /**
   * Returns the lowest used value by the enumerations of this class.
   * .
   * @return The lowest value used.
   */
  protected int getOffset()
  {
    return 3;
  }
}
