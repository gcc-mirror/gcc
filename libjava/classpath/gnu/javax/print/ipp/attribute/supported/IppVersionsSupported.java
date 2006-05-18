/* IppVersionsSupported.java -- 
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
 * IppVersionsSupported attribute as described in RFC 2911 section
 * 4.4.14 provides the value(s) (implemented as EnumSyntax)
 * of the supported IPP versions.
 * 
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class IppVersionsSupported extends EnumSyntax 
  implements SupportedValuesAttribute
{
  
  // a keyword based attribute in IPP - int values just starting at 0
  
  /** IPP version 1.0 */
  public static final IppVersionsSupported V_1_0 = 
    new IppVersionsSupported(0);
  
  /** IPP version 1.1 */
  public static final IppVersionsSupported V_1_1 = 
    new IppVersionsSupported(1);
  
  private static final String[] stringTable = { "1.0", "1.1" };
  
  private static final IppVersionsSupported[] enumValueTable = { V_1_0, 
                                                                 V_1_1 };

  /**
   * Constructs a <code>IppVersionsSupported</code> object.
   * 
   * @param value the enum value
   */
  public IppVersionsSupported(int value)
  {
    super(value);
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>IppVersionsSupported</code> itself.
   */
  public Class getCategory()
  {
    return IppVersionsSupported.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "ipp-versions-supported".
   */
  public String getName()
  {
    return "ipp-versions-supported";
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
