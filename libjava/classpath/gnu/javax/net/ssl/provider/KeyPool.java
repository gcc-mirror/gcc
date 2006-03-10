/* KeyPool.java -- A set of ephemeral key pairs.
   Copyright (C) 2001, 2002, 2003, 2006  Free Software Foundation, Inc.

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


package gnu.javax.net.ssl.provider;

import java.math.BigInteger;
import java.security.KeyPair;
import java.security.SecureRandom;
import java.security.Security;
import java.util.LinkedList;
import javax.crypto.spec.DHParameterSpec;

import gnu.java.security.hash.HashFactory;
import gnu.java.security.hash.IMessageDigest;
import gnu.java.security.prng.IRandom;
import gnu.java.security.prng.LimitReachedException;
import gnu.java.security.util.Prime2;

final class KeyPool
{

  // Fields.
  // -------------------------------------------------------------------------

  private static final BigInteger ONE = BigInteger.ONE;
  private static final BigInteger TWO = BigInteger.valueOf(2L);
  private static final BigInteger E = BigInteger.valueOf(65537L);
  private static final SecureRandom RANDOM = new SecureRandom ();

  // Constructor.
  // -------------------------------------------------------------------------

  private KeyPool()
  {
  }

  // Class methods.
  // -------------------------------------------------------------------------

  /**
   * Generate an export-class (512 bit) RSA key pair.
   *
   * @return The new key pair.
   */
  static KeyPair generateRSAKeyPair()
  {
    BigInteger p, q, n, d;

    // Simplified version of GNU Crypto's RSAKeyPairGenerator.

    int M = 256;
    BigInteger lower = TWO.pow(255);
    BigInteger upper = TWO.pow(256).subtract(ONE);
    byte[] kb = new byte[32];
    while (true)
      {
        nextBytes(kb);
        p = new BigInteger(1, kb).setBit(0);
        if (p.compareTo(lower) >= 0 && p.compareTo(upper) <= 0 &&
            Prime2.isProbablePrime(p) && p.gcd(E).equals(ONE))
          break;
      }

    while (true)
      {
        nextBytes(kb);
        q = new BigInteger(1, kb).setBit(0);
        n = q.multiply(p);
        if (n.bitLength() == 512 && Prime2.isProbablePrime(q) &&
            q.gcd(E).equals(ONE))
          break;
      }

    d = E.modInverse(p.subtract(ONE).multiply(q.subtract(ONE)));

    return new KeyPair(new JessieRSAPublicKey(n, E),
                       new JessieRSAPrivateKey(n, d));
  }

  private static void nextBytes(byte[] buf)
  {
    RANDOM.nextBytes (buf);
  }
}
