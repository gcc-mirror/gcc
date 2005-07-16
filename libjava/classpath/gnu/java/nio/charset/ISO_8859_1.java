/* ISO_8859_1.java -- 
   Copyright (C) 2002, 2004, 2005 Free Software Foundation, Inc.

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

package gnu.java.nio.charset;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CoderResult;

/**
 * ISO-8859-1 charset.
 *
 * @author Jesse Rosenstock
 */
final class ISO_8859_1 extends Charset
{
  ISO_8859_1 ()
  {
    /* Canonical charset name chosen according to:
     * http://java.sun.com/j2se/1.5.0/docs/guide/intl/encoding.doc.html
     */
    super ("ISO-8859-1", new String[] {
        /* These names are provided by 
         * http://www.iana.org/assignments/character-sets
         */
        "iso-ir-100",
        "ISO_8859-1",
        "latin1",
        "l1",
        "IBM819",
        "CP819",
        "csISOLatin1",
        "8859_1",
        /* These names are provided by
         * http://oss.software.ibm.com/cgi-bin/icu/convexp?s=ALL
         */
        "ISO8859_1", "ISO_8859_1", "ibm-819", "ISO_8859-1:1987",
        "819"
        });

  }

  public boolean contains (Charset cs)
  {
    return cs instanceof US_ASCII || cs instanceof ISO_8859_1;
  }

  public CharsetDecoder newDecoder ()
  {
    return new Decoder (this);
  }

  public CharsetEncoder newEncoder ()
  {
    return new Encoder (this);
  }

  private static final class Decoder extends CharsetDecoder
  {
    // Package-private to avoid a trampoline constructor.
    Decoder (Charset cs)
    {
      super (cs, 1.0f, 1.0f);
    }

    protected CoderResult decodeLoop (ByteBuffer in, CharBuffer out)
    {
      // TODO: Optimize this in the case in.hasArray() / out.hasArray()
      while (in.hasRemaining ())
      {
        byte b = in.get ();

        if (!out.hasRemaining ())
          {
            in.position (in.position () - 1);
            return CoderResult.OVERFLOW;
          }

        out.put ((char) (b & 0xFF));
      }

      return CoderResult.UNDERFLOW;
    }
  }

  private static final class Encoder extends CharsetEncoder
  {
    // Package-private to avoid a trampoline constructor.
    Encoder (Charset cs)
    {
      super (cs, 1.0f, 1.0f);
    }

    protected CoderResult encodeLoop (CharBuffer in, ByteBuffer out)
    {
      // TODO: Optimize this in the case in.hasArray() / out.hasArray()
      while (in.hasRemaining ())
      {
        char c = in.get ();

        if (c > 0xFF)
          {
            in.position (in.position () - 1);
            return CoderResult.unmappableForLength (1);
          }
        if (!out.hasRemaining ())
          {
            in.position (in.position () - 1);
            return CoderResult.OVERFLOW;
          }

        out.put ((byte) c);
      }

      return CoderResult.UNDERFLOW;
    }
  }
}
