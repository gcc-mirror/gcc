/* ARCFour.java -- 
   Copyright (C) 2002, 2003, 2006 Free Software Foundation, Inc.

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


package gnu.javax.crypto.prng;

import gnu.java.security.Registry;
import gnu.java.security.prng.BasePRNG;
import gnu.java.security.prng.LimitReachedException;

import java.util.Map;

/**
 * RC4 is a stream cipher developed by Ron Rivest. Until 1994 RC4 was a trade
 * secret of RSA Data Security, Inc., when it was released anonymously to a
 * mailing list. This version is a descendent of that code, and since there is
 * no proof that the leaked version was in fact RC4 and because "RC4" is a
 * trademark, it is called "ARCFOUR", short for "Allegedly RC4".
 * <p>
 * This class only implements the <i>keystream</i> of ARCFOUR. To use this as a
 * stream cipher, one would say:
 * <pre>
 * out = in &circ; arcfour.nextByte();
 * </pre>
 * <p>
 * This operation works for encryption and decryption.
 * <p>
 * References:
 * <ol>
 * <li>Schneier, Bruce: <i>Applied Cryptography: Protocols, Algorithms, and
 * Source Code in C, Second Edition.</i> (1996 John Wiley and Sons), pp.
 * 397--398. ISBN 0-471-11709-9</li>
 * <li>K. Kaukonen and R. Thayer, "A Stream Cipher Encryption Algorithm
 * 'Arcfour'", Internet Draft (expired), <a
 * href="http://www.mozilla.org/projects/security/pki/nss/draft-kaukonen-cipher-arcfour-03.txt">draft-kaukonen-cipher-arcfour-03.txt</a></li>
 * </ol>
 */
public class ARCFour
    extends BasePRNG
    implements Cloneable
{
  /** The attributes property name for the key bytes. */
  public static final String ARCFOUR_KEY_MATERIAL = "gnu.crypto.prng.arcfour.key-material";
  /** The size of the internal S-box. */
  public static final int ARCFOUR_SBOX_SIZE = 256;
  /** The S-box. */
  private byte[] s;
  private byte m, n;

  /** Default 0-arguments constructor. */
  public ARCFour()
  {
    super(Registry.ARCFOUR_PRNG);
  }

  public void setup(Map attributes)
  {
    byte[] kb = (byte[]) attributes.get(ARCFOUR_KEY_MATERIAL);
    if (kb == null)
      throw new IllegalArgumentException("ARCFOUR needs a key");
    s = new byte[ARCFOUR_SBOX_SIZE];
    m = n = 0;
    byte[] k = new byte[ARCFOUR_SBOX_SIZE];
    for (int i = 0; i < ARCFOUR_SBOX_SIZE; i++)
      s[i] = (byte) i;
    if (kb.length > 0)
      for (int i = 0, j = 0; i < ARCFOUR_SBOX_SIZE; i++)
        {
          k[i] = kb[j++];
          if (j >= kb.length)
            j = 0;
        }
    for (int i = 0, j = 0; i < ARCFOUR_SBOX_SIZE; i++)
      {
        j = j + s[i] + k[i];
        byte temp = s[i];
        s[i] = s[j & 0xff];
        s[j & 0xff] = temp;
      }
    buffer = new byte[ARCFOUR_SBOX_SIZE];
    try
      {
        fillBlock();
      }
    catch (LimitReachedException wontHappen)
      {
      }
  }

  public void fillBlock() throws LimitReachedException
  {
    for (int i = 0; i < buffer.length; i++)
      {
        m++;
        n = (byte)(n + s[m & 0xff]);
        byte temp = s[m & 0xff];
        s[m & 0xff] = s[n & 0xff];
        s[n & 0xff] = temp;
        temp = (byte)(s[m & 0xff] + s[n & 0xff]);
        buffer[i] = s[temp & 0xff];
      }
  }
}
