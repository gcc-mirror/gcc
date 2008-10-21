/* Util.java -- Miscellaneous utility methods.
   Copyright (C) 2004, 2006  Free Software Foundation, Inc.

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


package gnu.java.security.x509;

import gnu.java.lang.CPStringBuilder;

/**
 * A collection of useful class methods.
 *
 * @author Casey Marshall (rsdio@metastatic.org)
 */
public final class Util
{

  // Constants.
  // -------------------------------------------------------------------------

  public static final String HEX = "0123456789abcdef";

  // Class methods.
  // -------------------------------------------------------------------------

  /**
   * Convert a byte array to a hexadecimal string, as though it were a
   * big-endian arbitrarily-sized integer.
   *
   * @param buf The bytes to format.
   * @param off The offset to start at.
   * @param len The number of bytes to format.
   * @return A hexadecimal representation of the specified bytes.
   */
  public static String toHexString(byte[] buf, int off, int len)
  {
    CPStringBuilder str = new CPStringBuilder();
    for (int i = 0; i < len; i++)
      {
        str.append(HEX.charAt(buf[i+off] >>> 4 & 0x0F));
        str.append(HEX.charAt(buf[i+off] & 0x0F));
      }
    return str.toString();
  }

  /**
   * See {@link #toHexString(byte[],int,int)}.
   */
  public static String toHexString(byte[] buf)
  {
    return Util.toHexString(buf, 0, buf.length);
  }

  /**
   * Convert a byte array to a hexadecimal string, separating octets
   * with the given character.
   *
   * @param buf The bytes to format.
   * @param off The offset to start at.
   * @param len The number of bytes to format.
   * @param sep The character to insert between octets.
   * @return A hexadecimal representation of the specified bytes.
   */
  public static String toHexString(byte[] buf, int off, int len, char sep)
  {
    CPStringBuilder str = new CPStringBuilder();
    for (int i = 0; i < len; i++)
      {
        str.append(HEX.charAt(buf[i+off] >>> 4 & 0x0F));
        str.append(HEX.charAt(buf[i+off] & 0x0F));
        if (i < len - 1)
          str.append(sep);
      }
    return str.toString();
  }

  /**
   * See {@link #toHexString(byte[],int,int,char)}.
   */
  public static String toHexString(byte[] buf, char sep)
  {
    return Util.toHexString(buf, 0, buf.length, sep);
  }

  /**
   * Create a representation of the given byte array similar to the
   * output of `hexdump -C', which is
   *
   * <p><pre>OFFSET  SIXTEEN-BYTES-IN-HEX  PRINTABLE-BYTES</pre>
   *
   * <p>The printable bytes show up as-is if they are printable and
   * not a newline character, otherwise showing as '.'.
   *
   * @param buf The bytes to format.
   * @param off The offset to start at.
   * @param len The number of bytes to encode.
   * @return The formatted string.
   */
  public static String hexDump(byte[] buf, int off, int len, String prefix)
  {
    String nl = System.getProperty("line.separator");
    CPStringBuilder str = new CPStringBuilder();
    int i = 0;
    while (i < len)
      {
        str.append(prefix);
        str.append(Util.formatInt(i+off, 16, 8));
        str.append("  ");
        String s = Util.toHexString(buf, i+off, Math.min(16, len-i), ' ');
        str.append(s);
        for (int j = 56 - (56 - s.length()); j < 56; j++)
          str.append(" ");
        for (int j = 0; j < Math.min(16, len - i); j++)
          {
            if ((buf[i+off+j] & 0xFF) < 0x20 || (buf[i+off+j] & 0xFF) > 0x7E)
              str.append('.');
            else
              str.append((char) (buf[i+off+j] & 0xFF));
          }
        str.append(nl);
        i += 16;
      }
    return str.toString();
  }

  /**
   * See {@link #hexDump(byte[],int,int,String)}.
   */
  public static String hexDump(byte[] buf, String prefix)
  {
    return hexDump(buf, 0, buf.length, prefix);
  }

  /**
   * Format an integer into the specified radix, zero-filled.
   *
   * @param i The integer to format.
   * @param radix The radix to encode to.
   * @param len The target length of the string. The string is
   *   zero-padded to this length, but may be longer.
   * @return The formatted integer.
   */
  public static String formatInt(int i, int radix, int len)
  {
    String s = Integer.toString(i, radix);
    CPStringBuilder buf = new CPStringBuilder();
    for (int j = 0; j < len - s.length(); j++)
      buf.append("0");
    buf.append(s);
    return buf.toString();
  }

  /**
   * Convert a hexadecimal string into its byte representation.
   *
   * @param hex The hexadecimal string.
   * @return The converted bytes.
   */
  public static byte[] toByteArray(String hex)
  {
    hex = hex.toLowerCase();
    byte[] buf = new byte[hex.length() / 2];
    int j = 0;
    for (int i = 0; i < buf.length; i++)
      {
        buf[i] = (byte) ((Character.digit(hex.charAt(j++), 16) << 4) |
                          Character.digit(hex.charAt(j++), 16));
      }
    return buf;
  }
}
