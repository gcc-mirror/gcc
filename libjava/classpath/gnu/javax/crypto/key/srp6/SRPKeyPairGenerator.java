/* SRPKeyPairGenerator.java --
   Copyright (C) 2003, 2006 Free Software Foundation, Inc.

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


package gnu.javax.crypto.key.srp6;

import gnu.java.security.Configuration;
import gnu.java.security.Registry;
import gnu.java.security.key.IKeyPairGenerator;
import gnu.java.security.util.PRNG;

import java.math.BigInteger;
import java.security.KeyPair;
import java.security.SecureRandom;
import java.util.Map;
import java.util.logging.Logger;

/**
 * Reference:
 * <ol>
 * <li><a href="http://srp.stanford.edu/design.html">SRP Protocol Design</a><br>
 * Thomas J. Wu.</li>
 * </ol>
 */
public class SRPKeyPairGenerator
    implements IKeyPairGenerator
{
  private static final Logger log = Logger.getLogger(SRPKeyPairGenerator.class.getName());
  private static final BigInteger ZERO = BigInteger.ZERO;
  private static final BigInteger ONE = BigInteger.ONE;
  private static final BigInteger TWO = BigInteger.valueOf(2L);
  private static final BigInteger THREE = BigInteger.valueOf(3L);
  /** Property name of the length (Integer) of the modulus (N) of an SRP key. */
  public static final String MODULUS_LENGTH = "gnu.crypto.srp.L";
  /** Property name of the Boolean indicating wether or not to use defaults. */
  public static final String USE_DEFAULTS = "gnu.crypto.srp.use.defaults";
  /** Property name of the modulus (N) of an SRP key. */
  public static final String SHARED_MODULUS = "gnu.crypto.srp.N";
  /** Property name of the generator (g) of an SRP key. */
  public static final String GENERATOR = "gnu.crypto.srp.g";
  /** Property name of the user's verifier (v) for a Server SRP key. */
  public static final String USER_VERIFIER = "gnu.crypto.srp.v";
  /**
   * Property name of an optional {@link SecureRandom} instance to use. The
   * default is to use a classloader singleton from {@link PRNG}.
   */
  public static final String SOURCE_OF_RANDOMNESS = "gnu.crypto.srp.prng";
  /** Default value for the modulus length. */
  private static final int DEFAULT_MODULUS_LENGTH = 1024;
  /** The optional {@link SecureRandom} instance to use. */
  private SecureRandom rnd = null;
  /** Bit length of the shared modulus. */
  private int l;
  /** The shared public modulus. */
  private BigInteger N;
  /** The Field generator. */
  private BigInteger g;
  /** The user's verifier MPI. */
  private BigInteger v;
  /** Our default source of randomness. */
  private PRNG prng = null;

  // implicit 0-arguments constructor

  public String name()
  {
    return Registry.SRP_KPG;
  }

  public void setup(Map attributes)
  {
    // do we have a SecureRandom, or should we use our own?
    rnd = (SecureRandom) attributes.get(SOURCE_OF_RANDOMNESS);
    N = (BigInteger) attributes.get(SHARED_MODULUS);
    if (N != null)
      {
        l = N.bitLength();
        g = (BigInteger) attributes.get(GENERATOR);
        if (g == null)
          g = TWO;
        SRPAlgorithm.checkParams(N, g);
      }
    else
      { // generate or use default values for N and g
        Boolean useDefaults = (Boolean) attributes.get(USE_DEFAULTS);
        if (useDefaults == null)
          useDefaults = Boolean.TRUE;
        Integer L = (Integer) attributes.get(MODULUS_LENGTH);
        l = DEFAULT_MODULUS_LENGTH;
        if (useDefaults.equals(Boolean.TRUE))
          {
            if (L != null)
              {
                l = L.intValue();
                switch (l)
                  {
                  case 512:
                    N = SRPAlgorithm.N_512;
                    break;
                  case 640:
                    N = SRPAlgorithm.N_640;
                    break;
                  case 768:
                    N = SRPAlgorithm.N_768;
                    break;
                  case 1024:
                    N = SRPAlgorithm.N_1024;
                    break;
                  case 1280:
                    N = SRPAlgorithm.N_1280;
                    break;
                  case 1536:
                    N = SRPAlgorithm.N_1536;
                    break;
                  case 2048:
                    N = SRPAlgorithm.N_2048;
                    break;
                  default:
                    throw new IllegalArgumentException(
                        "unknown default shared modulus bit length");
                  }
                g = TWO;
                l = N.bitLength();
              }
          }
        else // generate new N and g
          {
            if (L != null)
              {
                l = L.intValue();
                if ((l % 256) != 0 || l < 512 || l > 2048)
                  throw new IllegalArgumentException(
                      "invalid shared modulus bit length");
              }
          }
      }
    // are we using this generator on the server side, or the client side?
    v = (BigInteger) attributes.get(USER_VERIFIER);
  }

  public KeyPair generate()
  {
    if (N == null)
      {
        BigInteger[] params = generateParameters();
        BigInteger q = params[0];
        N = params[1];
        g = params[2];
        if (Configuration.DEBUG)
          {
            log.fine("q: " + q.toString(16));
            log.fine("N: " + N.toString(16));
            log.fine("g: " + g.toString(16));
          }
      }
    return (v != null ? hostKeyPair() : userKeyPair());
  }

  private synchronized BigInteger[] generateParameters()
  {
    // N A large safe prime (N = 2q+1, where q is prime)
    // g A generator modulo N
    BigInteger q, p, g;
    byte[] qBytes = new byte[l / 8];
    do
      {
        do
          {
            nextRandomBytes(qBytes);
            q = new BigInteger(1, qBytes);
            q = q.setBit(0).setBit(l - 2).clearBit(l - 1);
          }
        while (! q.isProbablePrime(80));
        p = q.multiply(TWO).add(ONE);
      }
    while (p.bitLength() != l || ! p.isProbablePrime(80));
    // compute g. from FIPS-186, Appendix 4: e == 2
    BigInteger p_minus_1 = p.subtract(ONE);
    g = TWO;
    // Set h = any integer, where 1 < h < p - 1 and
    // h differs from any value previously tried
    for (BigInteger h = TWO; h.compareTo(p_minus_1) < 0; h = h.add(ONE))
      {
        // Set g = h**2 mod p
        g = h.modPow(TWO, p);
        // If g = 1, go to step 3
        if (! g.equals(ONE))
          break;
      }
    return new BigInteger[] { q, p, g };
  }

  private KeyPair hostKeyPair()
  {
    byte[] bBytes = new byte[(l + 7) / 8];
    BigInteger b, B;
    do
      {
        do
          {
            nextRandomBytes(bBytes);
            b = new BigInteger(1, bBytes);
          }
        while (b.compareTo(ONE) <= 0 || b.compareTo(N) >= 0);
        B = THREE.multiply(v).add(g.modPow(b, N)).mod(N);
      }
    while (B.compareTo(ZERO) == 0 || B.compareTo(N) >= 0);
    KeyPair result = new KeyPair(new SRPPublicKey(new BigInteger[] { N, g, B }),
                                 new SRPPrivateKey(new BigInteger[] { N, g, b, v }));
    return result;
  }

  private KeyPair userKeyPair()
  {
    byte[] aBytes = new byte[(l + 7) / 8];
    BigInteger a, A;
    do
      {
        do
          {
            nextRandomBytes(aBytes);
            a = new BigInteger(1, aBytes);
          }
        while (a.compareTo(ONE) <= 0 || a.compareTo(N) >= 0);
        A = g.modPow(a, N);
      }
    while (A.compareTo(ZERO) == 0 || A.compareTo(N) >= 0);
    KeyPair result = new KeyPair(new SRPPublicKey(new BigInteger[] { N, g, A }),
                                 new SRPPrivateKey(new BigInteger[] { N, g, a }));
    return result;
  }

  private void nextRandomBytes(byte[] buffer)
  {
    if (rnd != null)
      rnd.nextBytes(buffer);
    else
      getDefaultPRNG().nextBytes(buffer);
  }

  private PRNG getDefaultPRNG()
  {
    if (prng == null)
      prng = PRNG.getInstance();

    return prng;
  }
}
