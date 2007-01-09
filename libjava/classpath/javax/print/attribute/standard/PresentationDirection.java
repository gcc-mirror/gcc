/* PresentationDirection.java --
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
import javax.print.attribute.PrintJobAttribute;
import javax.print.attribute.PrintRequestAttribute;


/**
 * The <code>PresentationDirection</code> attribute specifies
 * a value to be used together with the <code>NumberUp</code> attribute 
 * to indicate the layout of multiple pages on a single media sheet.
 * <p>
 * <b>IPP Compatibility:</b> PresentationDirection is not an IPP 1.1 
 * attribute.
 * </p>
 * 
 * @author Michael Koch (konqueror@gmx.de)
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class PresentationDirection extends EnumSyntax
  implements PrintRequestAttribute, PrintJobAttribute
{
  private static final long serialVersionUID = 8294728067230931780L;

  /**
   * The single pages are arranged on the media in columns starting 
   * at the top left towards the bottom left.
   */
  public static final PresentationDirection TOBOTTOM_TORIGHT =
    new PresentationDirection(0);
  
  /**
   * The single pages are arranged on the media in columns starting 
   * at the top right towards the bottom left.
   */
  public static final PresentationDirection TOBOTTOM_TOLEFT =
    new PresentationDirection(1);
  
  /**
   * The single pages are arranged on the media in columns starting 
   * at the bottom left towards the top right.
   */
  public static final PresentationDirection TOTOP_TORIGHT =
    new PresentationDirection(2);
  
  /**
   * The single pages are arranged on the media in columns starting 
   * at the bottom right towards the top left.
   */
  public static final PresentationDirection TOTOP_TOLEFT =
    new PresentationDirection(3);
  
  /**
   * The single pages are arranged on the media in rows starting 
   * at the top left towards the right bottom.
   */
  public static final PresentationDirection TORIGHT_TOBOTTOM =
    new PresentationDirection(4);
  
  /**
   * The single pages are arranged on the media in rows starting 
   * at the bottom left towards the right top.
   */
  public static final PresentationDirection TORIGHT_TOTOP =
    new PresentationDirection(5);
  
  /**
   * The single pages are arranged on the media in rows starting 
   * at the top right towards the left bottom.
   */
  public static final PresentationDirection TOLEFT_TOBOTTOM =
    new PresentationDirection(6);
  
  /**
   * The single pages are arranged on the media in rows starting 
   * at the bottom right towards the left top.
   */
  public static final PresentationDirection TOLEFT_TOTOP =
    new PresentationDirection(7);

  private static final String[] stringTable = { "tobottom-toright", 
    "tobottom-toleft", "totop-toright", "totop-toleft", "toright-tobottom", 
    "toright-totop", "toleft-tobottom", "toleft-totop" };

  private static final PresentationDirection[] enumValueTable = 
    { TOBOTTOM_TORIGHT, TOBOTTOM_TOLEFT, TOTOP_TORIGHT, TOTOP_TOLEFT, 
      TORIGHT_TOBOTTOM, TORIGHT_TOTOP, TOLEFT_TOBOTTOM, TOLEFT_TOTOP };
  
  /**
   * Constructs a <code>PresentationDirection</code> object.
   * 
   * @param value the enum value.
   */
  private PresentationDirection(int value)
  {
    super(value);
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>PresentationDirection</code> itself.
   */
  public Class< ? extends Attribute> getCategory()
  {
    return PresentationDirection.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "presentation-direction".
   */
  public String getName()
  {
    return "presentation-direction";
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
