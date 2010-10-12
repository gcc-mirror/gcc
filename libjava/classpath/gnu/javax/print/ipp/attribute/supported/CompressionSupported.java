/* CompressionSupported.java --
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

import javax.print.attribute.Attribute;
import javax.print.attribute.EnumSyntax;
import javax.print.attribute.SupportedValuesAttribute;
import javax.print.attribute.standard.Compression;


/**
 * <code>CompressionSupported</code> provides the values which are
 * supported for the compression attribute.
 *
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class CompressionSupported extends EnumSyntax
  implements SupportedValuesAttribute
{

  /** The print data is not compressed. */
  public static final CompressionSupported NONE = new CompressionSupported(0);

  /** The print data is ZIP compressed. */
  public static final CompressionSupported DEFLATE = new CompressionSupported(1);

  /** The print data is GNU Zip compressed. */
  public static final CompressionSupported GZIP = new CompressionSupported(2);

  /** The print data is UNIX compressed. */
  public static final CompressionSupported COMPRESS = new CompressionSupported(3);

  private static final String[] stringTable = { "none", "deflate",
                                                "gzip", "compress" };

  private static final CompressionSupported[] enumValueTable = { NONE, DEFLATE,
                                                        GZIP, COMPRESS };

  /**
   * Constructs a <code>CompressionSupported</code> object.
   *
   * @param value the enum value
   */
  protected CompressionSupported(int value)
  {
    super(value);
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>CompressionSupported</code> itself.
   */
  public Class<? extends Attribute> getCategory()
  {
    return CompressionSupported.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "compression-supported".
   */
  public String getName()
  {
    return "compression-supported";
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
   * of this SupportedValuesAttribute enum.
   *
   * @return The enum of the standard attribute class.
   */
  public Compression getAssociatedAttribute()
  {
    return (Compression) IppUtilities.getEnumAttribute(
            "compression", new Integer(getValue()));
  }

  /**
   * Constructs an array from a set of -supported attributes.
   * @param set set to process
   * @return The constructed array.
   *
   * @see #getAssociatedAttribute()
   */
  public static Compression[]
    getAssociatedAttributeArray(Set<Attribute> set)
  {
    Compression[] result = new Compression[set.size()];
    int j = 0;
    for (Attribute tmp : set)
      {
        result[j] = ((CompressionSupported) tmp).getAssociatedAttribute();
        j++;
      }
    return result;
  }
}
