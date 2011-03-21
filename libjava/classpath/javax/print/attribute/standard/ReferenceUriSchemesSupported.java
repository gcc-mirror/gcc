/* ReferenceUriSchemesSupported.java --
   Copyright (C) 2004, 2005, 2006 Free Software Foundation, Inc.

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


/**
 * The <code>ReferenceUriSchemesSupported</code> attribute provides
 * the supported URI schemes (e.g. ftp) which are supported by the
 * printer service to be used as uri reference for document data.
 * <p>
 * <b>IPP Compatibility:</b> ReferenceUriSchemesSupported is an IPP 1.1
 * attribute.
 * </p>
 *
 * @author Michael Koch (konqueror@gmx.de)
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public class ReferenceUriSchemesSupported extends EnumSyntax
  implements Attribute
{
  private static final long serialVersionUID = -8989076942813442805L;

  /**
   * The file transfer protocol (FTP).
   */
  public static final ReferenceUriSchemesSupported FTP =
    new ReferenceUriSchemesSupported(0);

  /**
   * The hyper text transfer protocol (HTTP).
   */
  public static final ReferenceUriSchemesSupported HTTP =
    new ReferenceUriSchemesSupported(1);

  /**
   * The secure hyper text transfer protocol (HTTPS).
   */
  public static final ReferenceUriSchemesSupported HTTPS =
    new ReferenceUriSchemesSupported(2);

  /**
   * The gopher protocol.
   */
  public static final ReferenceUriSchemesSupported GOPHER =
    new ReferenceUriSchemesSupported(3);

  /**
   * The USENET news - RFC 1738.
   */
  public static final ReferenceUriSchemesSupported NEWS =
    new ReferenceUriSchemesSupported(4);

  /**
   * The network news transfer protocol (NNTP) - RFC 1738.
   */
  public static final ReferenceUriSchemesSupported NNTP =
    new ReferenceUriSchemesSupported(5);

  /**
   * The wide area information server protocol (WAIS) - RFC 4156.
   */
  public static final ReferenceUriSchemesSupported WAIS =
    new ReferenceUriSchemesSupported(6);

  /**
   * A filename specific to the host.
   */
  public static final ReferenceUriSchemesSupported FILE =
    new ReferenceUriSchemesSupported(7);

  private static final String[] stringTable = { "ftp", "http", "https",
                                                "gopher", "news", "nntp",
                                                "wais", "file" };

  private static final ReferenceUriSchemesSupported[] enumValueTable =
    { FTP, HTTP, HTTPS, GOPHER, NEWS, NNTP, WAIS, FILE };

  /**
   * Constructs a <code>ReferenceUriSchemeSupported</code> object.
   *
   * @param value the enum value.
   */
  protected ReferenceUriSchemesSupported(int value)
  {
    super(value);
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>ReferenceUriSchemesSupported</code> itself.
   */
  public Class< ? extends Attribute> getCategory()
  {
    return ReferenceUriSchemesSupported.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "reference-uri-schemes-supported".
   */
  public final String getName()
  {
    return "reference-uri-schemes-supported";
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
