/* GnuDHPrivateKey.java -- 
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

import gnu.java.security.Registry;
import gnu.java.security.key.IKeyPairCodec;

import java.math.BigInteger;

import javax.crypto.interfaces.DHPrivateKey;

/**
 * <p>An implementation of the Diffie-Hellman private key.</p>
 *
 * <p>Reference:</p>
 * <ol>
 *    <li><a href="http://www.ietf.org/rfc/rfc2631.txt">Diffie-Hellman Key
 *    Agreement Method</a><br>
 *    Eric Rescorla.</li>
 * </ol>
 */
public class GnuDHPrivateKey extends GnuDHKey implements DHPrivateKey
{

  // Constants and variables
  // -------------------------------------------------------------------------

  /** The private exponent. */
  private final BigInteger x;

  // Constructor(s)
  // -------------------------------------------------------------------------

  /**
   * Convenience constructor. Calls the constructor with five arguments passing
   * {@link Registry#RAW_ENCODING_ID} as the value of its first argument.
   * 
   * @param q a prime divisor of p-1.
   * @param p the public prime.
   * @param g the generator of the group.
   * @param x the private value x.
   */
  public GnuDHPrivateKey(BigInteger q, BigInteger p, BigInteger g, BigInteger x)
  {
    this(Registry.RAW_ENCODING_ID, q, p, g, x);
  }

  /**
   * Constructs a new instance of <code>GnuDHPrivateKey</code> given the
   * designated parameters.
   * 
   * @param preferredFormat the identifier of the encoding format to use by
   *          default when externalizing the key.
   * @param q a prime divisor of p-1.
   * @param p the public prime.
   * @param g the generator of the group.
   * @param x the private value x.
   */
  public GnuDHPrivateKey(int preferredFormat,
                         BigInteger q, BigInteger p, BigInteger g, BigInteger x)
  {
    super(preferredFormat == Registry.ASN1_ENCODING_ID ? Registry.PKCS8_ENCODING_ID
                                                       : preferredFormat,
          q, p, g);

    this.x = x;
  }

  // Class methods
  // -------------------------------------------------------------------------

  /**
   * <p>A class method that takes the output of the <code>encodePrivateKey()</code>
   * method of a DH keypair codec object (an instance implementing
   * {@link IKeyPairCodec} for DH keys, and re-constructs an instance of this
   * object.</p>
   *
   * @param k the contents of a previously encoded instance of this object.
   * @exception ArrayIndexOutOfBoundsException if there is not enough bytes,
   * in <code>k</code>, to represent a valid encoding of an instance of
   * this object.
   * @exception IllegalArgumentException if the byte sequence does not
   * represent a valid encoding of an instance of this object.
   */
  public static GnuDHPrivateKey valueOf(byte[] k)
  {
    // try RAW codec
    if (k[0] == Registry.MAGIC_RAW_DH_PRIVATE_KEY[0])
      try
        {
          return (GnuDHPrivateKey) new DHKeyPairRawCodec().decodePrivateKey(k);
        }
      catch (IllegalArgumentException ignored)
        {
        }

    // try PKCS#8 codec
    return (GnuDHPrivateKey) new DHKeyPairPKCS8Codec().decodePrivateKey(k);
  }

  // Instance methods
  // -------------------------------------------------------------------------

  // javax.crypto.interfaces.DHPrivateKey interface implementation -----------

  public BigInteger getX()
  {
    return x;
  }

  // other methods -----------------------------------------------------------

  /**
   * <p>Returns the encoded form of this private key according to the
   * designated format.</p>
   *
   * @param format the desired format identifier of the resulting encoding.
   * @return the byte sequence encoding this key according to the designated
   * format.
   * @exception IllegalArgumentException if the format is not supported.
   * @see gnu.crypto.key.dh.DHKeyPairRawCodec
   */
  public byte[] getEncoded(int format)
  {
    byte[] result;
    switch (format)
      {
      case IKeyPairCodec.RAW_FORMAT:
        result = new DHKeyPairRawCodec().encodePrivateKey(this);
        break;
      case IKeyPairCodec.PKCS8_FORMAT:
        result = new DHKeyPairPKCS8Codec().encodePrivateKey(this);
        break;
      default:
        throw new IllegalArgumentException("Unsupported encoding format: "
                                           + format);
      }
    return result;
  }

  /**
   * Returns <code>true</code> if the designated object is an instance of
   * {@link DHPrivateKey} and has the same parameter values as this one.
   * 
   * @param obj the other non-null DH key to compare to.
   * @return <code>true</code> if the designated object is of the same type
   *         and value as this one.
   */
  public boolean equals(Object obj)
  {
    if (obj == null)
      return false;

    if (! (obj instanceof DHPrivateKey))
      return false;

    DHPrivateKey that = (DHPrivateKey) obj;
    return super.equals(that) && x.equals(that.getX());
  }
}
