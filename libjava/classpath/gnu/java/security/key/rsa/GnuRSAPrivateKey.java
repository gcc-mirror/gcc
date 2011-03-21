/* GnuRSAPrivateKey.java --
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


package gnu.java.security.key.rsa;

import gnu.java.lang.CPStringBuilder;

import gnu.java.security.Configuration;
import gnu.java.security.action.GetPropertyAction;
import gnu.java.security.Registry;
import gnu.java.security.key.IKeyPairCodec;

import java.math.BigInteger;
import java.security.AccessController;
import java.security.PrivateKey;
import java.security.interfaces.RSAPrivateCrtKey;
import java.security.interfaces.RSAPrivateKey;

/**
 * An object that embodies an RSA private key.
 * <p>
 * References:
 * <ol>
 * <li><a
 * href="http://www.cosic.esat.kuleuven.ac.be/nessie/workshop/submissions/rsa-pss.zip">
 * RSA-PSS Signature Scheme with Appendix, part B.</a><br>
 * Primitive specification and supporting documentation.<br>
 * Jakob Jonsson and Burt Kaliski.</li>
 * </ol>
 */
public class GnuRSAPrivateKey
    extends GnuRSAKey
    implements PrivateKey, RSAPrivateCrtKey
{
  /** The first prime divisor of the modulus. */
  private final BigInteger p;

  /** The second prime divisor of the modulus. */
  private final BigInteger q;

  /** The private exponent of an RSA private key. */
  private final BigInteger d;

  /** The first factor's exponent. */
  private final BigInteger dP;

  /** The second factor's exponent. */
  private final BigInteger dQ;

  /** The CRT (Chinese Remainder Theorem) coefficient. */
  private final BigInteger qInv;

  /** String representation of this key. Cached for speed. */
  private transient String str;

  /**
   * Convenience constructor. Calls the constructor with 5 arguments passing
   * {@link Registry#RAW_ENCODING_ID} as the identifier of the preferred
   * encoding format.
   *
   * @param p the modulus first prime divisor.
   * @param q the modulus second prime divisor.
   * @param e the public exponent.
   * @param d the private exponent.
   */
  public GnuRSAPrivateKey(BigInteger p, BigInteger q, BigInteger e, BigInteger d)
  {
    this(Registry.RAW_ENCODING_ID, p, q, e, d);
  }

  /**
   * Constructs a new instance of a <code>GnuRSAPrivateKey</code> given the
   * designated arguments.
   *
   * @param preferredFormat the indetifier of the preferred encoding format to
   *          use when externalizing this key.
   * @param p the modulus first prime divisor.
   * @param q the modulus second prime divisor.
   * @param e the public exponent.
   * @param d the private exponent.
   */
  public GnuRSAPrivateKey(int preferredFormat, BigInteger p, BigInteger q,
                          BigInteger e, BigInteger d)
  {
    this(preferredFormat,
         p.multiply(q),
         e, d, p, q,
         e.modInverse(p.subtract(BigInteger.ONE)),
         e.modInverse(q.subtract(BigInteger.ONE)),
         q.modInverse(p));
  }

  /**
   * Constructs a new instance of a <code>GnuRSAPrivateKey</code> given the
   * designated arguments.
   *
   * @param preferredFormat the indetifier of the preferred encoding format to
   *          use when externalizing this key.
   * @param n the public modulus, which is also the product of <code>p</code>
   *          and <code>q</code>.
   * @param e the public exponent.
   * @param d the private exponent.
   * @param p the modulus first prime divisor.
   * @param q the modulus second prime divisor.
   * @param dP the first prime's exponen. A positive integer less than
   *          <code>p</code> and <code>q</code>, satisfying
   *          <code>e * dP = 1 (mod p-1)</code>.
   * @param dQ the second prime's exponent. A positive integer less than
   *          <code>p</code> and <code>q</code>, satisfying
   *          <code>e * dQ = 1 (mod p-1)</code>.
   * @param qInv the Chinese Remainder Theorem coefiicient. A positive integer
   *          less than <code>p</code>, satisfying
   *          <code>q * qInv = 1 (mod p)</code>.
   */
  public GnuRSAPrivateKey(int preferredFormat, BigInteger n, BigInteger e,
                          BigInteger d, BigInteger p, BigInteger q,
                          BigInteger dP, BigInteger dQ, BigInteger qInv)
  {
    super(preferredFormat == Registry.ASN1_ENCODING_ID ? Registry.PKCS8_ENCODING_ID
                                                       : preferredFormat,
          n, e);
    this.d = d;
    this.p = p;
    this.q = q;
    // the exponents dP and dQ are positive integers less than p and q
    // respectively satisfying
    // e * dP = 1 (mod p-1);
    // e * dQ = 1 (mod q-1),
    this.dP = dP;
    this.dQ = dQ;
    // the CRT coefficient qInv is a positive integer less than p satisfying
    // q * qInv = 1 (mod p).
    this.qInv = qInv;
  }

  /**
   * A class method that takes the output of the <code>encodePrivateKey()</code>
   * method of an RSA keypair codec object (an instance implementing
   * {@link IKeyPairCodec} for RSA keys, and re-constructs an instance of this
   * object.
   *
   * @param k the contents of a previously encoded instance of this object.
   * @throws ArrayIndexOutOfBoundsException if there is not enough bytes, in
   *           <code>k</code>, to represent a valid encoding of an instance
   *           of this object.
   * @throws IllegalArgumentException if the byte sequence does not represent a
   *           valid encoding of an instance of this object.
   */
  public static GnuRSAPrivateKey valueOf(final byte[] k)
  {
    // try RAW codec
    if (k[0] == Registry.MAGIC_RAW_RSA_PRIVATE_KEY[0])
      try
        {
          return (GnuRSAPrivateKey) new RSAKeyPairRawCodec().decodePrivateKey(k);
        }
      catch (IllegalArgumentException ignored)
        {
        }
    // try PKCS#8 codec
    return (GnuRSAPrivateKey) new RSAKeyPairPKCS8Codec().decodePrivateKey(k);
  }

  public BigInteger getPrimeP()
  {
    return p;
  }

  public BigInteger getPrimeQ()
  {
    return q;
  }

  public BigInteger getPrimeExponentP()
  {
    return dP;
  }

  public BigInteger getPrimeExponentQ()
  {
    return dQ;
  }

  public BigInteger getCrtCoefficient()
  {
    return qInv;
  }

  public BigInteger getPrivateExponent()
  {
    return d;
  }

  /**
   * Returns the encoded form of this private key according to the designated
   * format.
   *
   * @param format the desired format identifier of the resulting encoding.
   * @return the byte sequence encoding this key according to the designated
   *         format.
   * @throws IllegalArgumentException if the format is not supported.
   * @see RSAKeyPairRawCodec
   * @see RSAKeyPairPKCS8Codec
   */
  public byte[] getEncoded(int format)
  {
    final byte[] result;
    switch (format)
      {
      case IKeyPairCodec.RAW_FORMAT:
        result = new RSAKeyPairRawCodec().encodePrivateKey(this);
        break;
      case IKeyPairCodec.PKCS8_FORMAT:
        result = new RSAKeyPairPKCS8Codec().encodePrivateKey(this);
        break;
      default:
        throw new IllegalArgumentException("Unsupported encoding format: "
                                           + format);
      }
    return result;
  }

  /**
   * Returns <code>true</code> if the designated object is an instance of this
   * class and has the same RSA parameter values as this one.
   *
   * @param obj the other non-null RSA key to compare to.
   * @return <code>true</code> if the designated object is of the same type
   *         and value as this one.
   */
  public boolean equals(final Object obj)
  {
    if (obj == null)
      return false;

    if (obj instanceof RSAPrivateKey)
      {
        final RSAPrivateKey that = (RSAPrivateKey) obj;
        return super.equals(that) && d.equals(that.getPrivateExponent());
      }
    if (obj instanceof RSAPrivateCrtKey)
      {
        final RSAPrivateCrtKey that = (RSAPrivateCrtKey) obj;
        return super.equals(that) && p.equals(that.getPrimeP())
               && q.equals(that.getPrimeQ())
               && dP.equals(that.getPrimeExponentP())
               && dQ.equals(that.getPrimeExponentQ())
               && qInv.equals(that.getCrtCoefficient());
      }
    return false;
  }

  public String toString()
  {
    if (str == null)
      {
        String ls = (String) AccessController.doPrivileged
            (new GetPropertyAction("line.separator"));
        str = new CPStringBuilder(this.getClass().getName()).append("(")
            .append(super.toString()).append(",").append(ls)
            .append("d=0x").append(Configuration.DEBUG ? d.toString(16)
                                                       : "**...*").append(ls)
            .append("p=0x").append(Configuration.DEBUG ? p.toString(16)
                                                       : "**...*").append(ls)
            .append("q=0x").append(Configuration.DEBUG ? q.toString(16)
                                                       : "**...*").append(ls)
            .append("dP=0x").append(Configuration.DEBUG ? dP.toString(16)
                                                        : "**...*").append(ls)
            .append("dQ=0x").append(Configuration.DEBUG ? dQ.toString(16)
                                                        : "**...*").append(ls)
            .append("qInv=0x").append(Configuration.DEBUG ? qInv.toString(16)
                                                          : "**...*").append(ls)
            .append(")")
            .toString();
      }
    return str;
  }
}
