/* GnuRSAPublicKey.java --
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

import gnu.java.security.Registry;
import gnu.java.security.action.GetPropertyAction;
import gnu.java.security.key.IKeyPairCodec;

import java.math.BigInteger;
import java.security.AccessController;
import java.security.PublicKey;
import java.security.interfaces.RSAPublicKey;

/**
 * An object that encapsulates an RSA public key.
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
public class GnuRSAPublicKey
    extends GnuRSAKey
    implements PublicKey, RSAPublicKey
{
  /** String representation of this key. Cached for speed. */
  private transient String str;

  /**
   * Conveience constructor. Calls the constructor with 3 arguments passing
   * {@link Registry#RAW_ENCODING_ID} as the identifier of the preferred
   * encoding format.
   *
   * @param n the modulus.
   * @param e the public exponent.
   */
  public GnuRSAPublicKey(final BigInteger n, final BigInteger e)
  {
    this(Registry.RAW_ENCODING_ID, n, e);
  }

  /**
   * Constructs a new instance of <code>GnuRSAPublicKey</code> given the
   * designated arguments.
   *
   * @param preferredFormat the identifier of the preferred encoding format to
   *          use when externalizing this key.
   * @param n the modulus.
   * @param e the public exponent.
   */
  public GnuRSAPublicKey(int preferredFormat, BigInteger n, BigInteger e)
  {
    super(preferredFormat == Registry.ASN1_ENCODING_ID ? Registry.X509_ENCODING_ID
                                                       : preferredFormat,
          n, e);
  }

  /**
   * A class method that takes the output of the <code>encodePublicKey()</code>
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
  public static GnuRSAPublicKey valueOf(final byte[] k)
  {
    // try RAW codec
    if (k[0] == Registry.MAGIC_RAW_RSA_PUBLIC_KEY[0])
      try
        {
          return (GnuRSAPublicKey) new RSAKeyPairRawCodec().decodePublicKey(k);
        }
      catch (IllegalArgumentException ignored)
        {
        }
    // try X.509 codec
    return (GnuRSAPublicKey) new RSAKeyPairX509Codec().decodePublicKey(k);
  }

  /**
   * Returns the encoded form of this public key according to the designated
   * format.
   *
   * @param format the desired format identifier of the resulting encoding.
   * @return the byte sequence encoding this key according to the designated
   *         format.
   * @throws IllegalArgumentException if the format is not supported.
   * @see RSAKeyPairRawCodec
   */
  public byte[] getEncoded(final int format)
  {
    final byte[] result;
    switch (format)
      {
      case IKeyPairCodec.RAW_FORMAT:
        result = new RSAKeyPairRawCodec().encodePublicKey(this);
        break;
      case IKeyPairCodec.X509_FORMAT:
        result = new RSAKeyPairX509Codec().encodePublicKey(this);
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

    if (! (obj instanceof RSAPublicKey))
      return false;

    final RSAPublicKey that = (RSAPublicKey) obj;
    return super.equals(that)
           && getPublicExponent().equals(that.getPublicExponent());
  }

  public String toString()
  {
    if (str == null)
      {
        String ls = (String) AccessController.doPrivileged
            (new GetPropertyAction("line.separator"));
        str = new CPStringBuilder(this.getClass().getName()).append("(")
            .append(super.toString()).append(",").append(ls)
            .append(")")
            .toString();
      }
    return str;
  }
}
