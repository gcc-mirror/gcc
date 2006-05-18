/* SidesSupported.java --
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

package gnu.javax.print.ipp.attribute.supported;

import javax.print.attribute.EnumSyntax;
import javax.print.attribute.SupportedValuesAttribute;


/**
 * <code>SidesSupported</code> provides the
 * supported values for the sides attribute.
 *  
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class SidesSupported extends EnumSyntax
  implements SupportedValuesAttribute
{
  
  /** Specifies that each page should be printed on one sheet. */
  public static final SidesSupported ONE_SIDED = new SidesSupported(0);
  
  /** 
   * Specifies that two following pages should be printed on the 
   * front and back of one sheet for binding on the long edge.
   */
  public static final SidesSupported TWO_SIDED_LONG_EDGE = 
    new SidesSupported(1);
  
  /** 
   * Specifies that two following pages should be printed on the 
   * front and back of one sheet for binding on the short edge.
   */
  public static final SidesSupported TWO_SIDED_SHORT_EDGE = 
    new SidesSupported(2);
  
  /** An alias constant for "two sided long edge". */
  public static final SidesSupported DUPLEX = new SidesSupported(1);
  
  /** An alias constant for "two sided short edge". */
  public static final SidesSupported TUMBLE = new SidesSupported(2);

  private static final String[] stringTable = { "one-sided", 
                                                "two-sided-long-edge",
                                                "two-sided-short-edge" };
  
  private static final SidesSupported[] 
      enumValueTable = { ONE_SIDED, TWO_SIDED_LONG_EDGE, 
                         TWO_SIDED_SHORT_EDGE };
  
  
  /**
   * Creates a <code>SidesSupported</code> object.
   *
   * @param value the value of the enum
   */
  protected SidesSupported(int value)
  {
    super(value);
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>SidesSupported</code> itself.
   */
  public Class getCategory()
  {
    return SidesSupported.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "sides-supported".
   */
  public String getName()
  {
    return "sides-supported";
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
