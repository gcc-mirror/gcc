/* ByteArray.java -- wrapper around a byte array, with nice toString output.
   Copyright (C) 2005  Free Software Foundation, Inc.

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

import gnu.java.lang.CPStringBuilder;

import java.io.PrintWriter;
import java.io.StringWriter;

public final class ByteArray
{
  private final byte[] value;

  public ByteArray (final byte[] value)
  {
    this.value = value;
  }

  public byte[] getValue ()
  {
    return value;
  }

  public String toString ()
  {
    StringWriter str = new StringWriter ();
    PrintWriter out = new PrintWriter (str);
    int i = 0;
    int len = value.length;
    while (i < len)
      {
        out.print (formatInt (i, 16, 8));
        out.print ("  ");
        int l = Math.min (16, len - i);
        String s = toHexString (value, i, l, ' ');
        out.print (s);
        for (int j = 56 - (56 - s.length ()); j < 56; j++)
          out.print (" ");
        for (int j = 0; j < l; j++)
          {
            byte b = value[i+j];
            if ((b & 0xFF) < 0x20 || (b & 0xFF) > 0x7E)
              out.print (".");
            else
              out.print ((char) (b & 0xFF));
          }
        out.println ();
        i += 16;
      }
    return str.toString ();
  }

  public static String toHexString (byte[] buf, int off, int len, char sep)
  {
    CPStringBuilder str = new CPStringBuilder();
    for (int i = 0; i < len; i++)
      {
        str.append (Character.forDigit (buf[i+off] >>> 4 & 0x0F, 16));
        str.append (Character.forDigit (buf[i+off] & 0x0F, 16));
        if (i < len - 1)
          str.append(sep);
      }
    return str.toString();
  }

  public static String formatInt (int value, int radix, int len)
  {
    String s = Integer.toString (value, radix);
    CPStringBuilder buf = new CPStringBuilder ();
    for (int j = 0; j < len - s.length(); j++)
      buf.append ("0");
    buf.append (s);
    return buf.toString();
  }
}
