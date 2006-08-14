/* DSSKeyPairGenerator.java -- 
   Copyright 2001, 2002, 2003, 2006 Free Software Foundation, Inc.

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


package gnu.java.security.key.dss;

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
import java.security.spec.DSAParameterSpec;
import java.util.Map;
import java.util.logging.Logger;

/**
 * A key-pair generator for asymetric keys to use in conjunction with the DSS
 * (Digital Signature Standard).
 * <p>
 * References:
 * <p>
 * <a href="http://www.itl.nist.gov/fipspubs/fip186.htm">Digital Signature
 * Standard (DSS)</a>, Federal Information Processing Standards Publication
 * 186. National Institute of Standards and Technology.
 */
public class DSSKeyPairGenerator
    implements IKeyPairGenerator
{
  private static final Logger log = Logger.getLogger(DSSKeyPairGenerator.class.getName());

  /** The BigInteger constant 2. */
  private static final BigInteger TWO = BigInteger.valueOf(2L);

  /** Property name of the length (Integer) of the modulus (p) of a DSS key. */
  public static final String MODULUS_LENGTH = "gnu.crypto.dss.L";

  /**
   * Property name of the Boolean indicating wether or not to use default pre-
   * computed values of <code>p</code>, <code>q</code> and <code>g</code>
   * for a given modulus length. The ultimate behaviour of this generator with
   * regard to using pre-computed parameter sets will depend on the value of
   * this property and of the following one {@link #STRICT_DEFAULTS}:
   * <ol>
   * <li>If this property is {@link Boolean#FALSE} then this generator will
   * accept being setup for generating parameters for any modulus length
   * provided the modulus length is between <code>512</code> and
   * <code>1024</code>, and is of the form <code>512 + 64 * n</code>. In
   * addition, a new paramter set will always be generated; i.e. no pre-
   * computed values are used.</li>
   * <li>If this property is {@link Boolean#TRUE} and the value of
   * {@link #STRICT_DEFAULTS} is also {@link Boolean#TRUE} then this generator
   * will only accept being setup for generating parameters for modulus lengths
   * of <code>512</code>, <code>768</code> and <code>1024</code>. Any
   * other value, of the modulus length, even if between <code>512</code> and
   * <code>1024</code>, and of the form <code>512 + 64 * n</code>, will
   * cause an {@link IllegalArgumentException} to be thrown. When those modulus
   * length (<code>512</code>, <code>768</code>, and <code>1024</code>)
   * are specified, the paramter set is always the same.</li>
   * <li>Finally, if this property is {@link Boolean#TRUE} and the value of
   * {@link #STRICT_DEFAULTS} is {@link Boolean#FALSE} then this generator will
   * behave as in point 1 above, except that it will use pre-computed values
   * when possible; i.e. the modulus length is one of <code>512</code>,
   * <code>768</code>, or <code>1024</code>.</li>
   * </ol>
   * The default value of this property is {@link Boolean#TRUE}.
   */
  public static final String USE_DEFAULTS = "gnu.crypto.dss.use.defaults";

  /**
   * Property name of the Boolean indicating wether or not to generate new
   * parameters, even if the modulus length <i>L</i> is not one of the pre-
   * computed defaults (value {@link Boolean#FALSE}), or throw an exception
   * (value {@link Boolean#TRUE}) -- the exception in this case is an
   * {@link IllegalArgumentException}. The default value for this property is
   * {@link Boolean#FALSE}. The ultimate behaviour of this generator will
   * depend on the values of this and {@link #USE_DEFAULTS} properties -- see
   * {@link #USE_DEFAULTS} for more information.
   */
  public static final String STRICT_DEFAULTS = "gnu.crypto.dss.strict.defaults";

  /**
   * Property name of an optional {@link SecureRandom} instance to use. The
   * default is to use a classloader singleton from {@link PRNG}.
   */
  public static final String SOURCE_OF_RANDOMNESS = "gnu.crypto.dss.prng";

  /**
   * Property name of an optional {@link DSAParameterSpec} instance to use for
   * this generator's <code>p</code>, <code>q</code>, and <code>g</code>
   * values. The default is to generate these values or use pre-computed ones,
   * depending on the value of the <code>USE_DEFAULTS</code> attribute.
   */
  public static final String DSS_PARAMETERS = "gnu.crypto.dss.params";

  /**
   * Property name of the preferred encoding format to use when externalizing
   * generated instance of key-pairs from this generator. The property is taken
   * to be an {@link Integer} that encapsulates an encoding format identifier.
   */
  public static final String PREFERRED_ENCODING_FORMAT = "gnu.crypto.dss.encoding";

  /** Default value for the modulus length. */
  public static final int DEFAULT_MODULUS_LENGTH = 1024;

  /** Default encoding format to use when none was specified. */
  private static final int DEFAULT_ENCODING_FORMAT = Registry.RAW_ENCODING_ID;

  /** Initial SHS context. */
  private static final int[] T_SHS = new int[] {
      0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476, 0xC3D2E1F0
  };

  // from jdk1.3.1/docs/guide/security/CryptoSpec.html#AppB
  public static final DSAParameterSpec KEY_PARAMS_512 = new DSAParameterSpec(
      new BigInteger(
          "fca682ce8e12caba26efccf7110e526db078b05edecbcd1eb4a208f3ae1617ae"
        + "01f35b91a47e6df63413c5e12ed0899bcd132acd50d99151bdc43ee737592e17", 16),
      new BigInteger("962eddcc369cba8ebb260ee6b6a126d9346e38c5", 16),
      new BigInteger(
          "678471b27a9cf44ee91a49c5147db1a9aaf244f05a434d6486931d2d14271b9e"
        + "35030b71fd73da179069b32e2935630e1c2062354d0da20a6c416e50be794ca4", 16));
  public static final DSAParameterSpec KEY_PARAMS_768 = new DSAParameterSpec(
      new BigInteger(
          "e9e642599d355f37c97ffd3567120b8e25c9cd43e927b3a9670fbec5d8901419"
        + "22d2c3b3ad2480093799869d1e846aab49fab0ad26d2ce6a22219d470bce7d77"
        + "7d4a21fbe9c270b57f607002f3cef8393694cf45ee3688c11a8c56ab127a3daf", 16),
      new BigInteger("9cdbd84c9f1ac2f38d0f80f42ab952e7338bf511", 16),
      new BigInteger(
          "30470ad5a005fb14ce2d9dcd87e38bc7d1b1c5facbaecbe95f190aa7a31d23c4"
        + "dbbcbe06174544401a5b2c020965d8c2bd2171d3668445771f74ba084d2029d8"
        + "3c1c158547f3a9f1a2715be23d51ae4d3e5a1f6a7064f316933a346d3f529252", 16));
  public static final DSAParameterSpec KEY_PARAMS_1024 = new DSAParameterSpec(
      new BigInteger(
          "fd7f53811d75122952df4a9c2eece4e7f611b7523cef4400c31e3f80b6512669"
        + "455d402251fb593d8d58fabfc5f5ba30f6cb9b556cd7813b801d346ff26660b7"
        + "6b9950a5a49f9fe8047b1022c24fbba9d7feb7c61bf83b57e7c6a8a6150f04fb"
        + "83f6d3c51ec3023554135a169132f675f3ae2b61d72aeff22203199dd14801c7", 16),
      new BigInteger("9760508f15230bccb292b982a2eb840bf0581cf5", 16),
      new BigInteger(
          "f7e1a085d69b3ddecbbcab5c36b857b97994afbbfa3aea82f9574c0b3d078267"
        + "5159578ebad4594fe67107108180b449167123e84c281613b7cf09328cc8a6e1"
        + "3c167a8b547c8d28e0a3ae1e2bb3a675916ea37f0bfa213562f1fb627a01243b"
        + "cca4f1bea8519089a883dfe15ae59f06928b665e807b552564014c3bfecf492a", 16));

  private static final BigInteger TWO_POW_160 = TWO.pow(160);

  /** The length of the modulus of DSS keys generated by this instance. */
  private int L;

  /** The optional {@link SecureRandom} instance to use. */
  private SecureRandom rnd = null;

  private BigInteger seed;

  private BigInteger counter;

  private BigInteger p;

  private BigInteger q;

  private BigInteger e;

  private BigInteger g;

  private BigInteger XKEY;

  /** Our default source of randomness. */
  private PRNG prng = null;

  /** Preferred encoding format of generated keys. */
  private int preferredFormat;

  public String name()
  {
    return Registry.DSS_KPG;
  }

  /**
   * Configures this instance.
   * 
   * @param attributes the map of name/value pairs to use.
   * @exception IllegalArgumentException if the designated MODULUS_LENGTH value
   *              is not greater than 512, less than 1024 and not of the form
   *              <code>512 + 64j</code>.
   */
  public void setup(Map attributes)
  {
    // find out the modulus length
    Integer l = (Integer) attributes.get(MODULUS_LENGTH);
    L = (l == null ? DEFAULT_MODULUS_LENGTH : l.intValue());
    if ((L % 64) != 0 || L < 512 || L > 1024)
      throw new IllegalArgumentException(MODULUS_LENGTH);

    // should we use the default pre-computed params?
    Boolean useDefaults = (Boolean) attributes.get(USE_DEFAULTS);
    if (useDefaults == null)
      useDefaults = Boolean.TRUE;

    Boolean strictDefaults = (Boolean) attributes.get(STRICT_DEFAULTS);
    if (strictDefaults == null)
      strictDefaults = Boolean.FALSE;

    // are we given a set of DSA params or we shall use/generate our own?
    DSAParameterSpec params = (DSAParameterSpec) attributes.get(DSS_PARAMETERS);
    if (params != null)
      {
        p = params.getP();
        q = params.getQ();
        g = params.getG();
      }
    else if (useDefaults.equals(Boolean.TRUE))
      {
        switch (L)
          {
          case 512:
            p = KEY_PARAMS_512.getP();
            q = KEY_PARAMS_512.getQ();
            g = KEY_PARAMS_512.getG();
            break;
          case 768:
            p = KEY_PARAMS_768.getP();
            q = KEY_PARAMS_768.getQ();
            g = KEY_PARAMS_768.getG();
            break;
          case 1024:
            p = KEY_PARAMS_1024.getP();
            q = KEY_PARAMS_1024.getQ();
            g = KEY_PARAMS_1024.getG();
            break;
          default:
            if (strictDefaults.equals(Boolean.TRUE))
              throw new IllegalArgumentException(
                  "Does not provide default parameters for " + L
                  + "-bit modulus length");
            else
              {
                p = null;
                q = null;
                g = null;
              }
          }
      }
    else
      {
        p = null;
        q = null;
        g = null;
      }
    // do we have a SecureRandom, or should we use our own?
    rnd = (SecureRandom) attributes.get(SOURCE_OF_RANDOMNESS);
    // what is the preferred encoding format
    Integer formatID = (Integer) attributes.get(PREFERRED_ENCODING_FORMAT);
    preferredFormat = formatID == null ? DEFAULT_ENCODING_FORMAT
                                       : formatID.intValue();
    // set the seed-key
    byte[] kb = new byte[20]; // we need 160 bits of randomness
    nextRandomBytes(kb);
    XKEY = new BigInteger(1, kb).setBit(159).setBit(0);
  }

  public KeyPair generate()
  {
    if (p == null)
      {
        BigInteger[] params = new FIPS186(L, rnd).generateParameters();
        seed = params[FIPS186.DSA_PARAMS_SEED];
        counter = params[FIPS186.DSA_PARAMS_COUNTER];
        q = params[FIPS186.DSA_PARAMS_Q];
        p = params[FIPS186.DSA_PARAMS_P];
        e = params[FIPS186.DSA_PARAMS_E];
        g = params[FIPS186.DSA_PARAMS_G];
        if (Configuration.DEBUG)
          {
            log.fine("seed: " + seed.toString(16));
            log.fine("counter: " + counter.intValue());
            log.fine("q: " + q.toString(16));
            log.fine("p: " + p.toString(16));
            log.fine("e: " + e.toString(16));
            log.fine("g: " + g.toString(16));
          }
      }
    BigInteger x = nextX();
    BigInteger y = g.modPow(x, p);
    PublicKey pubK = new DSSPublicKey(preferredFormat, p, q, g, y);
    PrivateKey secK = new DSSPrivateKey(preferredFormat, p, q, g, x);
    return new KeyPair(pubK, secK);
  }

  /**
   * This method applies the following algorithm described in 3.1 of FIPS-186:
   * <ol>
   * <li>XSEED = optional user input.</li>
   * <li>XVAL = (XKEY + XSEED) mod 2<sup>b</sup>.</li>
   * <li>x = G(t, XVAL) mod q.</li>
   * <li>XKEY = (1 + XKEY + x) mod 2<sup>b</sup>.</li>
   * </ol>
   * <p>
   * Where <code>b</code> is the length of a secret b-bit seed-key (XKEY).
   * <p>
   * Note that in this implementation, XSEED, the optional user input, is always
   * zero.
   */
  private synchronized BigInteger nextX()
  {
    byte[] xk = XKEY.toByteArray();
    byte[] in = new byte[64]; // 512-bit block for SHS
    System.arraycopy(xk, 0, in, 0, xk.length);
    int[] H = Sha160.G(T_SHS[0], T_SHS[1], T_SHS[2], T_SHS[3], T_SHS[4], in, 0);
    byte[] h = new byte[20];
    for (int i = 0, j = 0; i < 5; i++)
      {
        h[j++] = (byte)(H[i] >>> 24);
        h[j++] = (byte)(H[i] >>> 16);
        h[j++] = (byte)(H[i] >>> 8);
        h[j++] = (byte) H[i];
      }
    BigInteger result = new BigInteger(1, h).mod(q);
    XKEY = XKEY.add(result).add(BigInteger.ONE).mod(TWO_POW_160);
    return result;
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
