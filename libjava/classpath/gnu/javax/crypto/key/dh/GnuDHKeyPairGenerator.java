/* GnuDHKeyPairGenerator.java --
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


package gnu.javax.crypto.key.dh;

import gnu.java.security.Configuration;
import gnu.java.security.Registry;
import gnu.java.security.hash.Sha160;
import gnu.java.security.key.IKeyPairGenerator;
import gnu.java.security.util.PRNG;

import java.math.BigInteger;
import java.security.KeyPair;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.SecureRandom;
import java.util.Map;
import java.util.logging.Logger;

import javax.crypto.spec.DHGenParameterSpec;
import javax.crypto.spec.DHParameterSpec;

/**
 * An implementation of a Diffie-Hellman keypair generator.
 * <p>
 * Reference:
 * <ol>
 * <li><a href="http://www.ietf.org/rfc/rfc2631.txt">Diffie-Hellman Key
 * Agreement Method</a><br>
 * Eric Rescorla.</li>
 * </ol>
 */
public class GnuDHKeyPairGenerator
    implements IKeyPairGenerator
{
  private static final Logger log = Logger.getLogger(GnuDHKeyPairGenerator.class.getName());
  /**
   * Property name of an optional {@link SecureRandom} instance to use. The
   * default is to use a classloader singleton from {@link PRNG}.
   */
  public static final String SOURCE_OF_RANDOMNESS = "gnu.crypto.dh.prng";
  /**
   * Property name of an optional {@link DHGenParameterSpec} or
   * {@link DHParameterSpec} instance to use for this generator.
   */
  public static final String DH_PARAMETERS = "gnu.crypto.dh.params";
  /** Property name of the size in bits (Integer) of the public prime (p). */
  public static final String PRIME_SIZE = "gnu.crypto.dh.L";
  /** Property name of the size in bits (Integer) of the private exponent (x). */
  public static final String EXPONENT_SIZE = "gnu.crypto.dh.m";
  /**
   * Property name of the preferred encoding format to use when externalizing
   * generated instance of key-pairs from this generator. The property is taken
   * to be an {@link Integer} that encapsulates an encoding format identifier.
   */
  public static final String PREFERRED_ENCODING_FORMAT = "gnu.crypto.dh.encoding";
  /** Default value for the size in bits of the public prime (p). */
  public static final int DEFAULT_PRIME_SIZE = 512;
  /** Default value for the size in bits of the private exponent (x). */
  public static final int DEFAULT_EXPONENT_SIZE = 160;
  /** Default encoding format to use when none was specified. */
  private static final int DEFAULT_ENCODING_FORMAT = Registry.RAW_ENCODING_ID;
  /** The SHA instance to use. */
  private Sha160 sha = new Sha160();
  /** The optional {@link SecureRandom} instance to use. */
  private SecureRandom rnd = null;
  /** The desired size in bits of the public prime (p). */
  private int l;
  /** The desired size in bits of the private exponent (x). */
  private int m;
  private BigInteger seed;
  private BigInteger counter;
  private BigInteger q;
  private BigInteger p;
  private BigInteger j;
  private BigInteger g;
  /** Our default source of randomness. */
  private PRNG prng = null;
  /** Preferred encoding format of generated keys. */
  private int preferredFormat;

  // default 0-arguments constructor

  public String name()
  {
    return Registry.DH_KPG;
  }

  public void setup(Map attributes)
  {
    // do we have a SecureRandom, or should we use our own?
    rnd = (SecureRandom) attributes.get(SOURCE_OF_RANDOMNESS);
    // are we given a set of Diffie-Hellman generation parameters or we shall
    // use our own?
    Object params = attributes.get(DH_PARAMETERS);
    // find out the desired sizes
    if (params instanceof DHGenParameterSpec)
      {
        DHGenParameterSpec jceSpec = (DHGenParameterSpec) params;
        l = jceSpec.getPrimeSize();
        m = jceSpec.getExponentSize();
      }
    else if (params instanceof DHParameterSpec)
      {
        // FIXME: I'm not sure this is correct. It seems to behave the
        // same way as Sun's RI, but I don't know if this behavior is
        // documented anywhere.
        DHParameterSpec jceSpec = (DHParameterSpec) params;
        p = jceSpec.getP();
        g = jceSpec.getG();
        l = p.bitLength();
        m = jceSpec.getL();
        // If no exponent size was given, generate an exponent as
        // large as the prime.
        if (m == 0)
          m = l;
      }
    else
      {
        Integer bi = (Integer) attributes.get(PRIME_SIZE);
        l = (bi == null ? DEFAULT_PRIME_SIZE : bi.intValue());
        bi = (Integer) attributes.get(EXPONENT_SIZE);
        m = (bi == null ? DEFAULT_EXPONENT_SIZE : bi.intValue());
      }
    if ((l % 256) != 0 || l < DEFAULT_PRIME_SIZE)
      throw new IllegalArgumentException("invalid modulus size");
    if ((m % 8) != 0 || m < DEFAULT_EXPONENT_SIZE)
      throw new IllegalArgumentException("invalid exponent size");
    if (m > l)
      throw new IllegalArgumentException("exponent size > modulus size");
    // what is the preferred encoding format
    Integer formatID = (Integer) attributes.get(PREFERRED_ENCODING_FORMAT);
    preferredFormat = formatID == null ? DEFAULT_ENCODING_FORMAT
                                       : formatID.intValue();
  }

  public KeyPair generate()
  {
    if (p == null)
      {
        BigInteger[] params = new RFC2631(m, l, rnd).generateParameters();
        seed = params[RFC2631.DH_PARAMS_SEED];
        counter = params[RFC2631.DH_PARAMS_COUNTER];
        q = params[RFC2631.DH_PARAMS_Q];
        p = params[RFC2631.DH_PARAMS_P];
        j = params[RFC2631.DH_PARAMS_J];
        g = params[RFC2631.DH_PARAMS_G];
        if (Configuration.DEBUG)
          {
            log.fine("seed: 0x" + seed.toString(16));
            log.fine("counter: " + counter.intValue());
            log.fine("q: 0x" + q.toString(16));
            log.fine("p: 0x" + p.toString(16));
            log.fine("j: 0x" + j.toString(16));
            log.fine("g: 0x" + g.toString(16));
          }
      }
    // generate a private number x of length m such as: 1 < x < q - 1
    BigInteger q_minus_1 = null;
    if (q != null)
      q_minus_1 = q.subtract(BigInteger.ONE);
    // We already check if m is modulo 8 in `setup.' This could just
    // be m >>> 3.
    byte[] mag = new byte[(m + 7) / 8];
    BigInteger x;
    while (true)
      {
        nextRandomBytes(mag);
        x = new BigInteger(1, mag);
        if (x.bitLength() == m && x.compareTo(BigInteger.ONE) > 0
            && (q_minus_1 == null || x.compareTo(q_minus_1) < 0))
          break;
      }
    BigInteger y = g.modPow(x, p);
    PrivateKey secK = new GnuDHPrivateKey(preferredFormat, q, p, g, x);
    PublicKey pubK = new GnuDHPublicKey(preferredFormat, q, p, g, y);
    return new KeyPair(pubK, secK);
  }

  /**
   * Fills the designated byte array with random data.
   *
   * @param buffer the byte array to fill with random data.
   */
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
