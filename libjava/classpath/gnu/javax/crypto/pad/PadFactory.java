/* PadFactory.java -- 
   Copyright (C) 2001, 2002, 2003, 2004, 2006 Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


package gnu.javax.crypto.pad;

import gnu.java.security.Registry;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * A Factory to instantiate padding schemes.
 */
public class PadFactory
    implements Registry
{
  /** Collection of padding algorithm names --cached for speed. */
  private static Set names;

  /** Trivial constructor to enforce Singleton pattern. */
  private PadFactory()
  {
    super();
  }

  /**
   * Returns an instance of a padding algorithm given its name.
   * 
   * @param pad the case-insensitive name of the padding algorithm.
   * @return an instance of the padding algorithm, operating with a given block
   *         size, or <code>null</code> if none found.
   * @throws InternalError if the implementation does not pass its self-test.
   */
  public static final IPad getInstance(String pad)
  {
    if (pad == null)
      return null;

    pad = pad.trim().toLowerCase();
    if (pad.endsWith("padding"))
      pad = pad.substring(0, pad.length() - "padding".length());
    IPad result = null;
    if (pad.equals(PKCS7_PAD) || pad.equals(PKCS5_PAD))
      result = new PKCS7();
    else if (pad.equals(TBC_PAD))
      result = new TBC();
    else if (pad.equals(EME_PKCS1_V1_5_PAD))
      result = new PKCS1_V1_5();
    else if (pad.equals(SSL3_PAD))
      result = new SSL3();
    else if (pad.equals(TLS1_PAD))
      result = new TLS1();
    else if (pad.equals(ISO10126_PAD))
      result = new ISO10126();

    if (result != null && ! result.selfTest())
      throw new InternalError(result.name());

    return result;
  }

  /**
   * Returns a {@link Set} of names of padding algorithms supported by this
   * <i>Factory</i>.
   * 
   * @return a {@link Set} of padding algorithm names (Strings).
   */
  public static final Set getNames()
  {
    if (names == null)
      {
        HashSet hs = new HashSet();
        hs.add(PKCS5_PAD);
        hs.add(PKCS7_PAD);
        hs.add(TBC_PAD);
        hs.add(EME_PKCS1_V1_5_PAD);
        hs.add(SSL3_PAD);
        hs.add(TLS1_PAD);
        hs.add(ISO10126_PAD);
        names = Collections.unmodifiableSet(hs);
      }
    return names;
  }
}
