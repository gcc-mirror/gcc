/* RSAKeyPairRawCodec.java -- 
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

import gnu.java.security.Registry;
import gnu.java.security.key.IKeyPairCodec;

import java.io.ByteArrayOutputStream;
import java.math.BigInteger;
import java.security.PrivateKey;
import java.security.PublicKey;

/**
 * An object that implements the {@link IKeyPairCodec} interface for the <i>Raw</i>
 * format to use with RSA keypairs.
 */
public class RSAKeyPairRawCodec
    implements IKeyPairCodec
{
  // implicit 0-arguments constructor

  public int getFormatID()
  {
    return RAW_FORMAT;
  }

  /**
   * Returns the encoded form of the designated RSA public key according to the
   * <i>Raw</i> format supported by this library.
   * <p>
   * The <i>Raw</i> format for an RSA public key, in this implementation, is a
   * byte sequence consisting of the following:
   * <ol>
   * <li>4-byte magic consisting of the value of the literal
   * {@link Registry#MAGIC_RAW_RSA_PUBLIC_KEY},</li>
   * <li>1-byte version consisting of the constant: 0x01,</li>
   * <li>4-byte count of following bytes representing the RSA parameter
   * <code>n</code> (the modulus) in internet order,</li>
   * <li>n-bytes representation of a {@link BigInteger} obtained by invoking
   * the <code>toByteArray()</code> method on the RSA parameter <code>n</code>,
   * </li>
   * <li>4-byte count of following bytes representing the RSA parameter
   * <code>e</code> (the public exponent) in internet order,</li>
   * <li>n-bytes representation of a {@link BigInteger} obtained by invoking
   * the <code>toByteArray()</code> method on the RSA parameter <code>e</code>.
   * </li>
   * </ol>
   *
   * @param key the key to encode.
   * @return the <i>Raw</i> format encoding of the designated key.
   * @exception IllegalArgumentException if the designated key is not an RSA
   *                                     one.
   */
  public byte[] encodePublicKey(PublicKey key)
  {
    if (! (key instanceof GnuRSAPublicKey))
      throw new IllegalArgumentException("key");

    GnuRSAPublicKey rsaKey = (GnuRSAPublicKey) key;
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    // magic
    baos.write(Registry.MAGIC_RAW_RSA_PUBLIC_KEY[0]);
    baos.write(Registry.MAGIC_RAW_RSA_PUBLIC_KEY[1]);
    baos.write(Registry.MAGIC_RAW_RSA_PUBLIC_KEY[2]);
    baos.write(Registry.MAGIC_RAW_RSA_PUBLIC_KEY[3]);
    // version
    baos.write(0x01);
    // n
    byte[] buffer = rsaKey.getModulus().toByteArray();
    int length = buffer.length;
    baos.write(length >>> 24);
    baos.write((length >>> 16) & 0xFF);
    baos.write((length >>> 8) & 0xFF);
    baos.write(length & 0xFF);
    baos.write(buffer, 0, length);
    // e
    buffer = rsaKey.getPublicExponent().toByteArray();
    length = buffer.length;
    baos.write(length >>> 24);
    baos.write((length >>> 16) & 0xFF);
    baos.write((length >>> 8) & 0xFF);
    baos.write(length & 0xFF);
    baos.write(buffer, 0, length);
    return baos.toByteArray();
  }

  public PublicKey decodePublicKey(byte[] k)
  {
    // magic
    if (k[0] != Registry.MAGIC_RAW_RSA_PUBLIC_KEY[0]
        || k[1] != Registry.MAGIC_RAW_RSA_PUBLIC_KEY[1]
        || k[2] != Registry.MAGIC_RAW_RSA_PUBLIC_KEY[2]
        || k[3] != Registry.MAGIC_RAW_RSA_PUBLIC_KEY[3])
      throw new IllegalArgumentException("magic");

    // version
    if (k[4] != 0x01)
      throw new IllegalArgumentException("version");

    int i = 5;
    int l;
    byte[] buffer;
    // n
    l =  k[i++]         << 24
      | (k[i++] & 0xFF) << 16
      | (k[i++] & 0xFF) << 8
      | (k[i++] & 0xFF);
    buffer = new byte[l];
    System.arraycopy(k, i, buffer, 0, l);
    i += l;
    BigInteger n = new BigInteger(1, buffer);
    // e
    l =  k[i++]         << 24
      | (k[i++] & 0xFF) << 16
      | (k[i++] & 0xFF) << 8
      | (k[i++] & 0xFF);
    buffer = new byte[l];
    System.arraycopy(k, i, buffer, 0, l);
    i += l;
    BigInteger e = new BigInteger(1, buffer);
    return new GnuRSAPublicKey(n, e);
  }

  /**
   * Returns the encoded form of the designated RSA private key according to the
   * <i>Raw</i> format supported by this library.
   * <p>
   * The <i>Raw</i> format for an RSA private key, in this implementation, is a
   * byte sequence consisting of the following:
   * <ol>
   * <li>4-byte magic consisting of the value of the literal
   * {@link Registry#MAGIC_RAW_RSA_PRIVATE_KEY},</li>
   * <li>1-byte version consisting of the constant: 0x01,</li>
   * <li>4-byte count of following bytes representing the RSA parameter
   * <code>p</code> (the first prime factor of the modulus) in internet order,
   * </li>
   * <li>n-bytes representation of a {@link BigInteger} obtained by invoking
   * the <code>toByteArray()</code> method on the RSA parameter <code>p</code>,
   * </li>
   * <li>4-byte count of following bytes representing the RSA parameter
   * <code>q</code> (the second prime factor of the modulus) in internet
   * order,</li>
   * <li>n-bytes representation of a {@link BigInteger} obtained by invoking
   * the <code>toByteArray()</code> method on the RSA parameter <code>q</code>,
   * </li>
   * <li>4-byte count of following bytes representing the RSA parameter
   * <code>e</code> (the public exponent) in internet order,</li>
   * <li>n-bytes representation of a {@link BigInteger} obtained by invoking
   * the <code>toByteArray()</code> method on the RSA parameter <code>e</code>,
   * </li>
   * <li>4-byte count of following bytes representing the RSA parameter
   * <code>d</code> (the private exponent) in internet order,</li>
   * <li>n-bytes representation of a {@link BigInteger} obtained by invoking
   * the <code>toByteArray()</code> method on the RSA parameter <code>d</code>,
   * </li>
   * </ol>
   * 
   * @param key the key to encode.
   * @return the <i>Raw</i> format encoding of the designated key.
   */
  public byte[] encodePrivateKey(PrivateKey key)
  {
    if (! (key instanceof GnuRSAPrivateKey))
      throw new IllegalArgumentException("key");

    GnuRSAPrivateKey rsaKey = (GnuRSAPrivateKey) key;
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    // magic
    baos.write(Registry.MAGIC_RAW_RSA_PRIVATE_KEY[0]);
    baos.write(Registry.MAGIC_RAW_RSA_PRIVATE_KEY[1]);
    baos.write(Registry.MAGIC_RAW_RSA_PRIVATE_KEY[2]);
    baos.write(Registry.MAGIC_RAW_RSA_PRIVATE_KEY[3]);
    // version
    baos.write(0x01);
    // p
    byte[] buffer = rsaKey.getPrimeP().toByteArray();
    int length = buffer.length;
    baos.write(length >>> 24);
    baos.write((length >>> 16) & 0xFF);
    baos.write((length >>> 8) & 0xFF);
    baos.write(length & 0xFF);
    baos.write(buffer, 0, length);
    // q
    buffer = rsaKey.getPrimeQ().toByteArray();
    length = buffer.length;
    baos.write(length >>> 24);
    baos.write((length >>> 16) & 0xFF);
    baos.write((length >>> 8) & 0xFF);
    baos.write(length & 0xFF);
    baos.write(buffer, 0, length);
    // e
    buffer = rsaKey.getPublicExponent().toByteArray();
    length = buffer.length;
    baos.write(length >>> 24);
    baos.write((length >>> 16) & 0xFF);
    baos.write((length >>> 8) & 0xFF);
    baos.write(length & 0xFF);
    baos.write(buffer, 0, length);
    // d
    buffer = rsaKey.getPrivateExponent().toByteArray();
    length = buffer.length;
    baos.write(length >>> 24);
    baos.write((length >>> 16) & 0xFF);
    baos.write((length >>> 8) & 0xFF);
    baos.write(length & 0xFF);
    baos.write(buffer, 0, length);
    return baos.toByteArray();
  }

  public PrivateKey decodePrivateKey(byte[] k)
  {
    // magic
    if (k[0] != Registry.MAGIC_RAW_RSA_PRIVATE_KEY[0]
        || k[1] != Registry.MAGIC_RAW_RSA_PRIVATE_KEY[1]
        || k[2] != Registry.MAGIC_RAW_RSA_PRIVATE_KEY[2]
        || k[3] != Registry.MAGIC_RAW_RSA_PRIVATE_KEY[3])
      throw new IllegalArgumentException("magic");

    // version
    if (k[4] != 0x01)
      throw new IllegalArgumentException("version");

    int i = 5;
    int l;
    byte[] buffer;
    // p
    l =  k[i++]         << 24
      | (k[i++] & 0xFF) << 16
      | (k[i++] & 0xFF) << 8
      | (k[i++] & 0xFF);
    buffer = new byte[l];
    System.arraycopy(k, i, buffer, 0, l);
    i += l;
    BigInteger p = new BigInteger(1, buffer);
    // q
    l =  k[i++]         << 24
      | (k[i++] & 0xFF) << 16
      | (k[i++] & 0xFF) << 8
      | (k[i++] & 0xFF);
    buffer = new byte[l];
    System.arraycopy(k, i, buffer, 0, l);
    i += l;
    BigInteger q = new BigInteger(1, buffer);
    // e
    l =  k[i++]         << 24
      | (k[i++] & 0xFF) << 16
      | (k[i++] & 0xFF) << 8
      | (k[i++] & 0xFF);
    buffer = new byte[l];
    System.arraycopy(k, i, buffer, 0, l);
    i += l;
    BigInteger e = new BigInteger(1, buffer);
    // d
    l =  k[i++]         << 24
      | (k[i++] & 0xFF) << 16
      | (k[i++] & 0xFF) << 8
      | (k[i++] & 0xFF);
    buffer = new byte[l];
    System.arraycopy(k, i, buffer, 0, l);
    i += l;
    BigInteger d = new BigInteger(1, buffer);
    return new GnuRSAPrivateKey(p, q, e, d);
  }
}
