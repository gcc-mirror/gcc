/* Compression.java --
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

import javax.print.attribute.DocAttribute;
import javax.print.attribute.EnumSyntax;


/**
 * The <code>Compression</code> printing attribute specifies if and how the
 * supplied print data is compressed.
 * <p>
 * If this attribute is ommitted from the attributes set of the print
 * data it is assumed that no compression is done.
 * </p>
 * <p>
 * <b>IPP Compatibility:</b> Compression is an IPP 1.1 attribute.
 * </p>
 * 
 * @author Michael Koch (konqueror@gmx.de)
 */
public class Compression extends EnumSyntax
  implements DocAttribute
{
  private static final long serialVersionUID = -5716748913324997674L;

  /** The print data is not compressed. */
  public static final Compression NONE = new Compression(0);
  
  /** The print data is ZIP compressed. */
  public static final Compression DEFLATE = new Compression(1);
  
  /** The print data is GNU Zip compressed. */
  public static final Compression GZIP = new Compression(2);
  
  /** The print data is UNIX compressed. */
  public static final Compression COMPRESS = new Compression(3);
  
  private static final String[] stringTable = { "none", "deflate", 
                                                "gzip", "compress" };
  private static final Compression[] enumValueTable = { NONE, DEFLATE, 
                                                        GZIP, COMPRESS };

  /**
   * Constructs a <code>Compression</code> object.
   * 
   * @param value the enum value
   */
  protected Compression(int value)
  {
    super(value);
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>Compression</code> itself.
   */
  public Class getCategory()
  {
    return Compression.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "compression".
   */
  public String getName()
  {
    return "compression";
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
