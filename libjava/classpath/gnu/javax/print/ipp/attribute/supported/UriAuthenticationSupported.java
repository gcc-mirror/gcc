/* UriAuthenticationSupported.java --
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

import javax.print.attribute.Attribute;
import javax.print.attribute.EnumSyntax;
import javax.print.attribute.SupportedValuesAttribute;

/**
 * UriAuthenticationSupported attribute as described in RFC 2911 section
 * 4.4.2 provides the keywords (implemented as EnumSyntax) which
 * authentication methods are supported by the printer object. This
 * includes a value of none.
 *
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class UriAuthenticationSupported extends EnumSyntax
  implements SupportedValuesAttribute
{

  // a keyword based attribute in IPP - int values just starting at 0

  /** Supports no authentication - assumes anonymous process */
  public static final UriAuthenticationSupported NONE =
    new UriAuthenticationSupported(0);

  /**
   * The authenticated user assumed is the value of the
   * "requesting-user-name" operation attribute supplied
   * with the operation.
   */
  public static final UriAuthenticationSupported REQUESTING_USER_NAME =
    new UriAuthenticationSupported(1);

  /** Supports HTTP basic authentication (RFC 2617) */
  public static final UriAuthenticationSupported BASIC =
    new UriAuthenticationSupported(2);

  /** Supports HTTP digest authentication (RFC 2617) */
  public static final UriAuthenticationSupported DIGEST =
    new UriAuthenticationSupported(3);

  /** Supports authentication through a client provided certificate */
  public static final UriAuthenticationSupported CERTIFICATE =
    new UriAuthenticationSupported(4);

  private static final String[] stringTable = { "none",
                                                "requesting-user-name",
                                                "basic", "digest",
                                                "certificate" };

  private static final UriAuthenticationSupported[] enumValueTable =
    { NONE, REQUESTING_USER_NAME, BASIC, DIGEST, CERTIFICATE };

  /**
   * Constructs a <code>UriAuthenticationSupported</code> object.
   *
   * @param value the enum value
   */
  public UriAuthenticationSupported(int value)
  {
    super(value);
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>UriAuthenticationSupported</code> itself.
   */
  public Class<? extends Attribute> getCategory()
  {
    return UriAuthenticationSupported.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "uri-authentication-supported".
   */
  public String getName()
  {
    return "uri-authentication-supported";
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
