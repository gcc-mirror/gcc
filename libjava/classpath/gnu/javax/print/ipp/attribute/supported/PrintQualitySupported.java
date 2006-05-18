/* PrintQualitySupported.java --
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

import gnu.javax.print.ipp.IppUtilities;

import java.util.Iterator;
import java.util.Set;

import javax.print.attribute.EnumSyntax;
import javax.print.attribute.SupportedValuesAttribute;
import javax.print.attribute.standard.PrintQuality;


/**
 * <code>PrintQualitySupported</code> provides the
 * supported values for the print-quality attribute.
 *  
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class PrintQualitySupported extends EnumSyntax
  implements SupportedValuesAttribute
{
  /** Draft quality of the printer. */
  public static final PrintQualitySupported DRAFT = 
    new PrintQualitySupported(3);
  
  /** Normal quality of the printer. */
  public static final PrintQualitySupported NORMAL = 
    new PrintQualitySupported(4);
  
  /** High quality of the printer. */
  public static final PrintQualitySupported HIGH = 
    new PrintQualitySupported(5);

  private static final String[] stringTable = { "draft", "normal", "high" };
  
  private static final PrintQualitySupported[] enumValueTable = { DRAFT, 
                                                                  NORMAL, 
                                                                  HIGH };
  
  /**
   * Constructs a <code>PrintQualitySupported</code> object.
   * 
   * @param value the value of the enum
   */
  protected PrintQualitySupported(int value)
  {
    super(value);
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>PrintQualitySupported</code> itself.
   */
  public Class getCategory()
  {
    return PrintQualitySupported.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "print-quality-supported".
   */
  public String getName()
  {
    return "print-quality-supported";
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
  
  /**
   * Returns the equally enum of the standard attribute class
   * of this SupportedValuesAttribute enum.
   * 
   * @return The enum of the standard attribute class.
   */
  public PrintQuality getAssociatedAttribute() 
  {
    return (PrintQuality) IppUtilities.getEnumAttribute(
                          "print-quality", new Integer(getValue()));
  }
  
  /**
   * Constructs an array from a set of -supported attributes.
   * @param set set to process
   * @return The constructed array.
   * 
   * @see #getAssociatedAttribute()
   */
  public static PrintQuality[] getAssociatedAttributeArray(Set set) 
  {
    PrintQualitySupported tmp;
    PrintQuality[] result = new PrintQuality[set.size()];      
    Iterator it = set.iterator();
    int j = 0;
    while (it.hasNext())
      {
        tmp = (PrintQualitySupported) it.next();
        result[j] = tmp.getAssociatedAttribute();
        j++;
      }            
    return result;
  }
}
