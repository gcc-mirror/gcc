/* BigDecimalHelper.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package gnu.CORBA;

import gnu.java.lang.CPStringBuilder;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import java.math.BigDecimal;
import java.math.BigInteger;

import org.omg.CORBA.TypeCodePackage.BadKind;

/**
 * Reads and writes BigDecimal as CORBA <code>fixed</code>.
 * The format, described in CORBA specification, requires to store
 * data in hexadecimal format, two digits per byte (oceted), most
 * significant digit first. The last half-byte in the representation
 * stores the sign, being 0xD for negative numbers and 0xC for
 * zero and positive numbers. To have the even number of half bytes,
 * 0x0 is appended to the beginning, if required. The position of the
 * decimal point is not stored.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class BigDecimalHelper
{
  {
  }

  /**
   * @todo remove from the release version.
   */
  public static void main(String[] args)
  {
    try
      {
        ByteArrayOutputStream b = new ByteArrayOutputStream();
        BigDecimal d = new BigDecimal("12234.54689");

        write(b, d);

        byte[] a = b.toByteArray();

        for (int i = 0; i < a.length; i++)
          {
            int k = a [ i ] & 0xFF;
            System.out.print(Integer.toHexString(k) + " ");
          }

        System.out.println("Now reading");

        ByteArrayInputStream bin = new ByteArrayInputStream(a);

        BigDecimal r = read(bin, d.scale());

        System.out.println(r);
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
      }
  }

  /**
   * Read the CORBA fixed, autodetecting the number of bytes
   * and assuming the given scale.
   */
  public static BigDecimal read(java.io.InputStream in, int scale)
                         throws IOException
  {
    ByteArrayOutputStream bout = new ByteArrayOutputStream();

    int f;

    do
      {
        f = in.read();
        if (f >= 0)
          bout.write(f);
      }
    // The last byte has 0xC or 0xD in the last halfbyte.  
    while ((f & 0xF) <= 0x9);

    return createFixed(scale, bout.toByteArray());
  }

  /**
   * Write the big decimal as CORBA <code>fixed<.code>.
   * The scale will not be stored.
   * 
   * @param out a stream to write into.
   * @param x a big decimal to write.
   *
   * @throws IOException if the stream write method throws one.
   * @throws BadKind if this BigDecimal has more digits than
   * specified.
   */
  public static void write(java.io.OutputStream out, BigDecimal x)
                    throws IOException, BadKind
  {
    CPStringBuilder v = new CPStringBuilder(x.unscaledValue().toString());

    boolean negative = v.charAt(0) == '-';

    if (negative)
      v = v.deleteCharAt(0);

    if ( (v.length() & 1) == 0)
      v.insert(0, '0');

    int c;

    for (int i = 0; i < v.length() - 1; i = i + 2)
      {
        c = ((v.charAt(i) - '0') << 4) | (v.charAt(i + 1) - '0');
        out.write(c);
      }

    c = ((v.charAt(v.length() - 1) - '0') << 4) | (negative ? 0xD : 0xC);

    out.write(c);
  }

  /**
   * Convert the loaded byte array, representing
   * CORBA <code>fixed</code>, into an instance of
   * the {@link BigDecimal}
   */
  private static BigDecimal createFixed(int scale, byte[] d)
  {
    CPStringBuilder s = new CPStringBuilder(2 * d.length);

    int last = d.length - 1;

    if ((d [ last ] & 0xF) == 0xD)
      s.append('-');

    if (last > 0)
      for (int i = 0; i < last; i++)
        {
          s.append((char) (((d [ i ] >> 4) & 0xF) + '0'));
          s.append((char) (((d [ i ]) & 0xF) + '0'));
        }

    s.append((char) (((d [ last ] >> 4) & 0xF) + '0'));

    BigInteger b = new BigInteger(s.toString());
    BigDecimal dec = new BigDecimal(b, scale);

    return dec;
  }
}
