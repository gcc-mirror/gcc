/* FormatUtil.java -- Encoding and decoding format utility methods
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


package gnu.java.security.util;

import gnu.java.security.Registry;

/**
 * Encoding and decoding format utility methods.
 */
public class FormatUtil
{
  /** Trivial constructor to enforce Singleton pattern. */
  private FormatUtil()
  {
    super();
  }

  /**
   * Returns the fully qualified name of the designated encoding ID.
   * 
   * @param formatID the unique identifier of the encoding format.
   * @return the fully qualified name of the designated format. Returns
   *         <code>null</code> if no such encoding format is known.
   */
  public static final String getEncodingName(int formatID)
  {
    String result = null;
    switch (formatID)
      {
      case Registry.RAW_ENCODING_ID:
        result = Registry.RAW_ENCODING;
        break;
      case Registry.X509_ENCODING_ID:
        result = Registry.X509_ENCODING;
        break;
      case Registry.PKCS8_ENCODING_ID:
        result = Registry.PKCS8_ENCODING;
        break;
      case Registry.ASN1_ENCODING_ID:
        result = Registry.ASN1_ENCODING;
        break;
      }

    return result;
  }

  /**
   * Returns the short name of the designated encoding ID. This is used by the
   * JCE Adapters.
   * 
   * @param formatID the unique identifier of the encoding format.
   * @return the short name of the designated format. Returns <code>null</code>
   *         if no such encoding format is known.
   */
  public static final String getEncodingShortName(int formatID)
  {
    String result = null;
    switch (formatID)
      {
      case Registry.RAW_ENCODING_ID:
        result = Registry.RAW_ENCODING_SHORT_NAME;
        break;
      case Registry.X509_ENCODING_ID:
        result = Registry.X509_ENCODING_SORT_NAME;
        break;
      case Registry.PKCS8_ENCODING_ID:
        result = Registry.PKCS8_ENCODING_SHORT_NAME;
        break;
      case Registry.ASN1_ENCODING_ID:
        result = Registry.ASN1_ENCODING_SHORT_NAME;
        break;
      }

    return result;
  }

  /**
   * Returns the identifier of the encoding format given its short name.
   * 
   * @param name the case-insensitive canonical short name of an encoding
   *          format.
   * @return the identifier of the designated encoding format, or <code>0</code>
   *         if the name does not correspond to any known format.
   */
  public static final int getFormatID(String name)
  {
    if (name == null)
      return 0;

    name = name.trim();
    if (name.length() == 0)
      return 0;

    int result = 0;
    if (name.equalsIgnoreCase(Registry.RAW_ENCODING_SHORT_NAME))
      result = Registry.RAW_ENCODING_ID;
    else if (name.equalsIgnoreCase(Registry.X509_ENCODING_SORT_NAME))
      result = Registry.X509_ENCODING_ID;
    else if (name.equalsIgnoreCase(Registry.PKCS8_ENCODING_SHORT_NAME))
      result = Registry.PKCS8_ENCODING_ID;
    
    return result;
  }
}
