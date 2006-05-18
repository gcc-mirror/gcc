/* SidesDefault.java --
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

package gnu.javax.print.ipp.attribute.defaults;

import gnu.javax.print.ipp.IppUtilities;
import gnu.javax.print.ipp.attribute.DefaultValueAttribute;

import javax.print.attribute.Attribute;
import javax.print.attribute.EnumSyntax;


/**
 * <code>SidesDefault</code> provides the
 * default for the sides attribute.
 *  
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class SidesDefault extends EnumSyntax
  implements DefaultValueAttribute
{
  
  /** Specifies that each page should be printed on one sheet. */
  public static final SidesDefault ONE_SIDED = new SidesDefault(0);
  
  /** 
   * Specifies that two following pages should be printed on the 
   * front and back of one sheet for binding on the long edge.
   */
  public static final SidesDefault TWO_SIDED_LONG_EDGE = 
    new SidesDefault(1);
  
  /** 
   * Specifies that two following pages should be printed on the 
   * front and back of one sheet for binding on the short edge.
   */
  public static final SidesDefault TWO_SIDED_SHORT_EDGE = 
    new SidesDefault(2);
  
  /** An alias constant for "two sided long edge". */
  public static final SidesDefault DUPLEX = new SidesDefault(1);
  
  /** An alias constant for "two sided short edge". */
  public static final SidesDefault TUMBLE = new SidesDefault(2);

  private static final String[] stringTable = { "one-sided", 
                                                "two-sided-long-edge",
                                                "two-sided-short-edge" };
  
  private static final SidesDefault[] enumValueTable = { ONE_SIDED, 
                                                         TWO_SIDED_LONG_EDGE, 
                                                         TWO_SIDED_SHORT_EDGE };
  
  
  /**
   * Creates a <code>SidesDefault</code> object.
   *
   * @param value the value of the enum
   */
  protected SidesDefault(int value)
  {
    super(value);
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>SidesDefault</code> itself.
   */
  public Class getCategory()
  {
    return SidesDefault.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "sides-default".
   */
  public String getName()
  {
    return "sides-default";
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
   * Returns the equally enum of the standard attribute class
   * of this DefaultValuesAttribute enum.
   * 
   * @return The enum of the standard attribute class.
   */
  public Attribute getAssociatedAttribute() 
  {
    return IppUtilities.getEnumAttribute("sides", new Integer(getValue()));
  }
}
