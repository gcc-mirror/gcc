/* GnuDHPublicKey.java -- 
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
import gnu.java.security.action.GetPropertyAction;
import gnu.java.security.key.IKeyPairCodec;

import java.math.BigInteger;
import java.security.AccessController;

import javax.crypto.interfaces.DHPublicKey;

/**
 * An implementation of the Diffie-Hellman public key.
 * <p>
 * Reference:
 * <ol>
 * <li><a href="http://www.ietf.org/rfc/rfc2631.txt">Diffie-Hellman Key
 * Agreement Method</a><br>
 * Eric Rescorla.</li>
 * </ol>
 */
public class GnuDHPublicKey
    extends GnuDHKey
    implements DHPublicKey
{
  private BigInteger y;
  /** String representation of this key. Cached for speed. */
  private transient String str;

  /**
   * Convenience constructor. Calls the constructor with five arguments passing
   * {@link Registry#RAW_ENCODING_ID} as the value of its first argument.
   * 
   * @param q a prime divisor of p-1.
   * @param p the public prime.
   * @param g the generator of the group.
   * @param y the public value y.
   */
  public GnuDHPublicKey(BigInteger q, BigInteger p, BigInteger g, BigInteger y)
  {
    this(Registry.RAW_ENCODING_ID, q, p, g, y);
  }

  /**
   * Constructs a new instance of <code>GnuDHPublicKey</code> given the
   * designated parameters.
   * 
   * @param preferredFormat the identifier of the encoding format to use by
   *          default when externalizing the key.
   * @param q a prime divisor of p-1.
   * @param p the public prime.
   * @param g the generator of the group.
   * @param y the public value y.
   */
  public GnuDHPublicKey(int preferredFormat, BigInteger q, BigInteger p,
                        BigInteger g, BigInteger y)
  {
    super(preferredFormat == Registry.ASN1_ENCODING_ID ? Registry.X509_ENCODING_ID
                                                       : preferredFormat,
          q, p, g);
    this.y = y;
  }

  /**
   * A class method that takes the output of the <code>encodePublicKey()</code>
   * method of a DH keypair codec object (an instance implementing
   * {@link IKeyPairCodec} for DSS keys, and re-constructs an instance of this
   * object.
   * 
   * @param k the contents of a previously encoded instance of this object.
   * @exception ArrayIndexOutOfBoundsException if there is not enough bytes, in
   *              <code>k</code>, to represent a valid encoding of an
   *              instance of this object.
   * @exception IllegalArgumentException if the byte sequence does not represent
   *              a valid encoding of an instance of this object.
   */
  public static GnuDHPublicKey valueOf(byte[] k)
  {
    // try RAW codec
    if (k[0] == Registry.MAGIC_RAW_DH_PUBLIC_KEY[0])
      try
        {
          return (GnuDHPublicKey) new DHKeyPairRawCodec().decodePublicKey(k);
        }
      catch (IllegalArgumentException ignored)
        {
        }
    // try X.509 codec
    return (GnuDHPublicKey) new DHKeyPairX509Codec().decodePublicKey(k);
  }

  public BigInteger getY()
  {
    return y;
  }

  /**
   * Returns the encoded form of this public key according to the designated
   * format.
   * 
   * @param format the desired format identifier of the resulting encoding.
   * @return the byte sequence encoding this key according to the designated
   *         format.
   * @exception IllegalArgumentException if the format is not supported.
   */
  public byte[] getEncoded(int format)
  {
    byte[] result;
    switch (format)
      {
      case IKeyPairCodec.RAW_FORMAT:
        result = new DHKeyPairRawCodec().encodePublicKey(this);
        break;
      case IKeyPairCodec.X509_FORMAT:
        result = new DHKeyPairX509Codec().encodePublicKey(this);
        break;
      default:
        throw new IllegalArgumentException("Unsupported encoding format: "
                                           + format);
      }
    return result;
  }

  /**
   * Returns <code>true</code> if the designated object is an instance of
   * {@link DHPublicKey} and has the same parameter values as this one.
   * 
   * @param obj the other non-null DH key to compare to.
   * @return <code>true</code> if the designated object is of the same type
   *         and value as this one.
   */
  public boolean equals(Object obj)
  {
    if (obj == null)
      return false;

    if (! (obj instanceof DHPublicKey))
      return false;

    DHPublicKey that = (DHPublicKey) obj;
    return super.equals(that) && y.equals(that.getY());
  }

  public String toString()
  {
    if (str == null)
      {
        String ls = (String) AccessController.doPrivileged
            (new GetPropertyAction("line.separator"));
        str = new StringBuilder(this.getClass().getName()).append("(")
            .append(super.toString()).append(",").append(ls)
            .append("y=0x").append(y.toString(16)).append(ls)
            .append(")")
            .toString();
      }
    return str;
  }
}
