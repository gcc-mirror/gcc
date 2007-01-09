/* SheetCollate.java --
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
 * The <code>SheetCollate</code> printing attribute specifies 
 * whether or not the sheets of each copy in a print job have to be
 * in sequence.
 * <p>
 * The attribute only makes sense if multiple copies are specified through
 * the <code>Copies</code> printing attribute. If <code>UNCOLLATED</code>
 * is specified every page of a print job is printed for all requested
 * copies before the next page is processed. <code>COLLATED</code> means
 * that for every copy requested the pages have to be printed in sequence.
 * </p>
 * <p>
 * <b>IPP Compatibility:</b> SheetCollate is not an IPP 1.1 attribute.
 * </p>
 * 
 * @author Michael Koch (konqueror@gmx.de)
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class SheetCollate extends EnumSyntax
  implements DocAttribute, PrintRequestAttribute, PrintJobAttribute
{
  private static final long serialVersionUID = 7080587914259873003L;

  /**
   * The sheets of the different copies are uncollated.
   */
  public static final SheetCollate UNCOLLATED = new SheetCollate(0);
  
  /**
   * The sheets of the different copies are collated.
   */
  public static final SheetCollate COLLATED = new SheetCollate(1);


  private static final String[] stringTable = { "uncollated", "collated" };
  
  private static final SheetCollate[] enumValueTable = { UNCOLLATED, 
                                                         COLLATED };  
  
  /**
   * Creates a <code>SheetCollate</code> object.
   *
   * @param value the enum value.
   */
  protected SheetCollate(int value)
  {
    super(value);
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>SheetCollate</code> itself.
   */
  public Class< ? extends Attribute> getCategory()
  {
    return SheetCollate.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "sheet-collate".
   */
  public String getName()
  {
    return "sheet-collate";
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
