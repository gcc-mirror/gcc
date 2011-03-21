/* DSSSignature.java --
   Copyright (C) 2001, 2002, 2003, 2006 Free Software Foundation, Inc.

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


package gnu.java.security.sig.dss;

import gnu.java.security.Registry;
import gnu.java.security.hash.IMessageDigest;
import gnu.java.security.hash.Sha160;
import gnu.java.security.prng.IRandom;
import gnu.java.security.sig.BaseSignature;
import gnu.java.security.sig.ISignature;

import java.math.BigInteger;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.interfaces.DSAPrivateKey;
import java.security.interfaces.DSAPublicKey;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;

/**
 * The DSS (Digital Signature Standard) algorithm makes use of the following
 * parameters:
 * <ol>
 * <li>p: A prime modulus, where
 * <code>2<sup>L-1</sup> &lt; p &lt; 2<sup>L</sup> </code> for <code>512 &lt;= L
 * &lt;= 1024</code> and <code>L</code> a multiple of <code>64</code>.</li>
 * <li>q: A prime divisor of <code>p - 1</code>, where <code>2<sup>159</sup>
 *    &lt; q &lt; 2<sup>160</sup></code>.</li>
 * <li>g: Where <code>g = h<sup>(p-1)</sup>/q mod p</code>, where
 * <code>h</code> is any integer with <code>1 &lt; h &lt; p - 1</code> such
 * that <code>h<sup> (p-1)</sup>/q mod p > 1</code> (<code>g</code> has order
 * <code>q mod p</code>).</li>
 * <li>x: A randomly or pseudorandomly generated integer with <code>0 &lt; x
 *    &lt; q</code>.</li>
 * <li>y: <code>y = g<sup>x</sup> mod p</code>.</li>
 * <li>k: A randomly or pseudorandomly generated integer with <code>0 &lt; k
 *    &lt; q</code>.</li>
 * </ol>
 * <p>
 * The integers <code>p</code>, <code>q</code>, and <code>g</code> can be
 * public and can be common to a group of users. A user's private and public
 * keys are <code>x</code> and <code>y</code>, respectively. They are
 * normally fixed for a period of time. Parameters <code>x</code> and
 * <code>k</code> are used for signature generation only, and must be kept
 * secret. Parameter <code>k</code> must be regenerated for each signature.
 * <p>
 * The signature of a message <code>M</code> is the pair of numbers
 * <code>r</code> and <code>s</code> computed according to the equations below:
 * <ul>
 * <li><code>r = (g<sup>k</sup> mod p) mod q</code> and</li>
 * <li><code>s = (k<sup>-1</sup>(SHA(M) + xr)) mod q</code>.</li>
 * </ul>
 * <p>
 * In the above, <code>k<sup>-1</sup></code> is the multiplicative inverse of
 * <code>k</code>, <code>mod q</code>; i.e., <code>(k<sup>-1</sup> k) mod q =
 * 1</code> and <code>0 &lt; k-1 &lt; q</code>. The value of <code>SHA(M)</code>
 * is a 160-bit string output by the Secure Hash Algorithm specified in FIPS
 * 180. For use in computing <code>s</code>, this string must be converted to
 * an integer.
 * <p>
 * As an option, one may wish to check if <code>r == 0</code> or <code>s == 0
 * </code>.
 * If either <code>r == 0</code> or <code>s == 0</code>, a new value of
 * <code>k</code> should be generated and the signature should be recalculated
 * (it is extremely unlikely that <code>r == 0</code> or <code>s == 0</code> if
 * signatures are generated properly).
 * <p>
 * The signature is transmitted along with the message to the verifier.
 * <p>
 * References:
 * <ol>
 * <li><a href="http://www.itl.nist.gov/fipspubs/fip186.htm">Digital Signature
 * Standard (DSS)</a>, Federal Information Processing Standards Publication
 * 186. National Institute of Standards and Technology.</li>
 * </ol>
 */
public class DSSSignature
    extends BaseSignature
{
  /** Trivial 0-arguments constructor. */
  public DSSSignature()
  {
    super(Registry.DSS_SIG, new Sha160());
  }

  /** Private constructor for cloning purposes. */
  private DSSSignature(DSSSignature that)
  {
    this();

    this.publicKey = that.publicKey;
    this.privateKey = that.privateKey;
    this.md = (IMessageDigest) that.md.clone();
  }

  public static final BigInteger[] sign(final DSAPrivateKey k, final byte[] h)
  {
    final DSSSignature sig = new DSSSignature();
    final Map attributes = new HashMap();
    attributes.put(ISignature.SIGNER_KEY, k);
    sig.setupSign(attributes);
    return sig.computeRS(h);
  }

  public static final BigInteger[] sign(final DSAPrivateKey k, final byte[] h,
                                        Random rnd)
  {
    final DSSSignature sig = new DSSSignature();
    final Map attributes = new HashMap();
    attributes.put(ISignature.SIGNER_KEY, k);
    if (rnd != null)
      attributes.put(ISignature.SOURCE_OF_RANDOMNESS, rnd);

    sig.setupSign(attributes);
    return sig.computeRS(h);
  }

  public static final BigInteger[] sign(final DSAPrivateKey k, final byte[] h,
                                        IRandom irnd)
  {
    final DSSSignature sig = new DSSSignature();
    final Map attributes = new HashMap();
    attributes.put(ISignature.SIGNER_KEY, k);
    if (irnd != null)
      attributes.put(ISignature.SOURCE_OF_RANDOMNESS, irnd);

    sig.setupSign(attributes);
    return sig.computeRS(h);
  }

  public static final boolean verify(final DSAPublicKey k, final byte[] h,
                                     final BigInteger[] rs)
  {
    final DSSSignature sig = new DSSSignature();
    final Map attributes = new HashMap();
    attributes.put(ISignature.VERIFIER_KEY, k);
    sig.setupVerify(attributes);
    return sig.checkRS(rs, h);
  }

  public Object clone()
  {
    return new DSSSignature(this);
  }

  protected void setupForVerification(PublicKey k)
      throws IllegalArgumentException
  {
    if (! (k instanceof DSAPublicKey))
      throw new IllegalArgumentException();

    this.publicKey = k;
  }

  protected void setupForSigning(PrivateKey k) throws IllegalArgumentException
  {
    if (! (k instanceof DSAPrivateKey))
      throw new IllegalArgumentException();

    this.privateKey = k;
  }

  protected Object generateSignature() throws IllegalStateException
  {
    final BigInteger[] rs = computeRS(md.digest());
    return encodeSignature(rs[0], rs[1]);
  }

  protected boolean verifySignature(Object sig) throws IllegalStateException
  {
    final BigInteger[] rs = decodeSignature(sig);
    return checkRS(rs, md.digest());
  }

  /**
   * Returns the output of a signature generation phase.
   *
   * @return an object encapsulating the DSS signature pair <code>r</code> and
   *         <code>s</code>.
   */
  private Object encodeSignature(BigInteger r, BigInteger s)
  {
    return new BigInteger[] { r, s };
  }

  /**
   * Returns the output of a previously generated signature object as a pair of
   * {@link java.math.BigInteger}.
   *
   * @return the DSS signature pair <code>r</code> and <code>s</code>.
   */
  private BigInteger[] decodeSignature(Object signature)
  {
    return (BigInteger[]) signature;
  }

  private BigInteger[] computeRS(final byte[] digestBytes)
  {
    final BigInteger p = ((DSAPrivateKey) privateKey).getParams().getP();
    final BigInteger q = ((DSAPrivateKey) privateKey).getParams().getQ();
    final BigInteger g = ((DSAPrivateKey) privateKey).getParams().getG();
    final BigInteger x = ((DSAPrivateKey) privateKey).getX();
    final BigInteger m = new BigInteger(1, digestBytes);
    BigInteger k, r, s;
    final byte[] kb = new byte[20]; // we'll use 159 bits only
    while (true)
      {
        this.nextRandomBytes(kb);
        k = new BigInteger(1, kb);
        k.clearBit(159);
        r = g.modPow(k, p).mod(q);
        if (r.equals(BigInteger.ZERO))
          continue;

        s = m.add(x.multiply(r)).multiply(k.modInverse(q)).mod(q);
        if (s.equals(BigInteger.ZERO))
          continue;

        break;
      }
    return new BigInteger[] { r, s };
  }

  private boolean checkRS(final BigInteger[] rs, final byte[] digestBytes)
  {
    final BigInteger r = rs[0];
    final BigInteger s = rs[1];
    final BigInteger g = ((DSAPublicKey) publicKey).getParams().getG();
    final BigInteger p = ((DSAPublicKey) publicKey).getParams().getP();
    final BigInteger q = ((DSAPublicKey) publicKey).getParams().getQ();
    final BigInteger y = ((DSAPublicKey) publicKey).getY();
    final BigInteger w = s.modInverse(q);
    final BigInteger u1 = w.multiply(new BigInteger(1, digestBytes)).mod(q);
    final BigInteger u2 = r.multiply(w).mod(q);
    final BigInteger v = g.modPow(u1, p).multiply(y.modPow(u2, p)).mod(p).mod(q);
    return v.equals(r);
  }
}
