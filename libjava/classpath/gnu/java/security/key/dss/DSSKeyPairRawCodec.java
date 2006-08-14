/* DSSKeyPairRawCodec.java -- 
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

import gnu.java.security.Registry;
import gnu.java.security.key.IKeyPairCodec;

import java.io.ByteArrayOutputStream;
import java.math.BigInteger;
import java.security.PrivateKey;
import java.security.PublicKey;

/**
 * An object that implements the {@link IKeyPairCodec} operations for the
 * <i>Raw</i> format to use with DSS keypairs.
 */
public class DSSKeyPairRawCodec
    implements IKeyPairCodec
{
  // implicit 0-arguments constructor

  public int getFormatID()
  {
    return RAW_FORMAT;
  }

  /**
   * Returns the encoded form of the designated DSS (Digital Signature Standard)
   * public key according to the <i>Raw</i> format supported by this library.
   * <p>
   * The <i>Raw</i> format for a DSA public key, in this implementation, is a
   * byte sequence consisting of the following:
   * <ol>
   * <li>4-byte magic consisting of the value of the literal
   * {@link Registry#MAGIC_RAW_DSS_PUBLIC_KEY},
   * <li>
   * <li>1-byte version consisting of the constant: 0x01,</li>
   * <li>4-byte count of following bytes representing the DSA parameter
   * <code>p</code> in internet order,</li>
   * <li>n-bytes representation of a {@link BigInteger} obtained by invoking
   * the <code>toByteArray()</code> method on the DSA parameter <code>p</code>,
   * </li>
   * <li>4-byte count of following bytes representing the DSA parameter
   * <code>q</code>,</li>
   * <li>n-bytes representation of a {@link BigInteger} obtained by invoking
   * the <code>toByteArray()</code> method on the DSA parameter <code>q</code>,
   * </li>
   * <li>4-byte count of following bytes representing the DSA parameter
   * <code>g</code>,</li>
   * <li>n-bytes representation of a {@link BigInteger} obtained by invoking
   * the <code>toByteArray()</code> method on the DSA parameter <code>g</code>,
   * </li>
   * <li>4-byte count of following bytes representing the DSA parameter
   * <code>y</code>,</li>
   * <li>n-bytes representation of a {@link BigInteger} obtained by invoking
   * the <code>toByteArray()</code> method on the DSA parameter <code>y</code>,
   * </li>
   * </ol>
   * 
   * @param key the key to encode.
   * @return the <i>Raw</i> format encoding of the designated key.
   * @throws IllegalArgumentException if the designated key is not a DSS
   *           (Digital Signature Standard) one.
   * @see Registry#MAGIC_RAW_DSS_PUBLIC_KEY
   */
  public byte[] encodePublicKey(PublicKey key)
  {
    if (! (key instanceof DSSPublicKey))
      throw new IllegalArgumentException("key");

    DSSPublicKey dssKey = (DSSPublicKey) key;
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    // magic
    baos.write(Registry.MAGIC_RAW_DSS_PUBLIC_KEY[0]);
    baos.write(Registry.MAGIC_RAW_DSS_PUBLIC_KEY[1]);
    baos.write(Registry.MAGIC_RAW_DSS_PUBLIC_KEY[2]);
    baos.write(Registry.MAGIC_RAW_DSS_PUBLIC_KEY[3]);
    // version
    baos.write(0x01);
    // p
    byte[] buffer = dssKey.getParams().getP().toByteArray();
    int length = buffer.length;
    baos.write(length >>> 24);
    baos.write((length >>> 16) & 0xFF);
    baos.write((length >>> 8) & 0xFF);
    baos.write(length & 0xFF);
    baos.write(buffer, 0, length);
    // q
    buffer = dssKey.getParams().getQ().toByteArray();
    length = buffer.length;
    baos.write(length >>> 24);
    baos.write((length >>> 16) & 0xFF);
    baos.write((length >>> 8) & 0xFF);
    baos.write(length & 0xFF);
    baos.write(buffer, 0, length);
    // g
    buffer = dssKey.getParams().getG().toByteArray();
    length = buffer.length;
    baos.write(length >>> 24);
    baos.write((length >>> 16) & 0xFF);
    baos.write((length >>> 8) & 0xFF);
    baos.write(length & 0xFF);
    baos.write(buffer, 0, length);
    // y
    buffer = dssKey.getY().toByteArray();
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
    if (k[0] != Registry.MAGIC_RAW_DSS_PUBLIC_KEY[0]
        || k[1] != Registry.MAGIC_RAW_DSS_PUBLIC_KEY[1]
        || k[2] != Registry.MAGIC_RAW_DSS_PUBLIC_KEY[2]
        || k[3] != Registry.MAGIC_RAW_DSS_PUBLIC_KEY[3])
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
    // g
    l =  k[i++]         << 24
      | (k[i++] & 0xFF) << 16
      | (k[i++] & 0xFF) << 8
      | (k[i++] & 0xFF);
    buffer = new byte[l];
    System.arraycopy(k, i, buffer, 0, l);
    i += l;
    BigInteger g = new BigInteger(1, buffer);
    // y
    l =  k[i++]         << 24
      | (k[i++] & 0xFF) << 16
      | (k[i++] & 0xFF) << 8
      | (k[i++] & 0xFF);
    buffer = new byte[l];
    System.arraycopy(k, i, buffer, 0, l);
    i += l;
    BigInteger y = new BigInteger(1, buffer);
    return new DSSPublicKey(p, q, g, y);
  }

  /**
   * Returns the encoded form of the designated DSS (Digital Signature Standard)
   * private key according to the <i>Raw</i> format supported by this library.
   * <p>
   * The <i>Raw</i> format for a DSA private key, in this implementation, is a
   * byte sequence consisting of the following:
   * <ol>
   * <li>4-byte magic consisting of the value of the literal
   * {@link Registry#MAGIC_RAW_DSS_PRIVATE_KEY},
   * <li>
   * <li>1-byte version consisting of the constant: 0x01,</li>
   * <li>4-byte count of following bytes representing the DSA parameter
   * <code>p</code> in internet order,</li>
   * <li>n-bytes representation of a {@link BigInteger} obtained by invoking
   * the <code>toByteArray()</code> method on the DSA parameter <code>p</code>,
   * </li>
   * <li>4-byte count of following bytes representing the DSA parameter
   * <code>q</code>,</li>
   * <li>n-bytes representation of a {@link BigInteger} obtained by invoking
   * the <code>toByteArray()</code> method on the DSA parameter <code>q</code>,
   * </li>
   * <li>4-byte count of following bytes representing the DSA parameter
   * <code>g</code>,</li>
   * <li>n-bytes representation of a {@link BigInteger} obtained by invoking
   * the <code>toByteArray()</code> method on the DSA parameter <code>g</code>,
   * </li>
   * <li>4-byte count of following bytes representing the DSA parameter
   * <code>x</code>,</li>
   * <li>n-bytes representation of a {@link BigInteger} obtained by invoking
   * the <code>toByteArray()</code> method on the DSA parameter <code>x</code>,
   * </li>
   * </ol>
   * 
   * @param key the key to encode.
   * @return the <i>Raw</i> format encoding of the designated key.
   * @throws IllegalArgumentException if the designated key is not a DSS
   *           (Digital Signature Standard) one.
   */
  public byte[] encodePrivateKey(PrivateKey key)
  {
    if (! (key instanceof DSSPrivateKey))
      throw new IllegalArgumentException("key");

    DSSPrivateKey dssKey = (DSSPrivateKey) key;
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    // magic
    baos.write(Registry.MAGIC_RAW_DSS_PRIVATE_KEY[0]);
    baos.write(Registry.MAGIC_RAW_DSS_PRIVATE_KEY[1]);
    baos.write(Registry.MAGIC_RAW_DSS_PRIVATE_KEY[2]);
    baos.write(Registry.MAGIC_RAW_DSS_PRIVATE_KEY[3]);
    // version
    baos.write(0x01);
    // p
    byte[] buffer = dssKey.getParams().getP().toByteArray();
    int length = buffer.length;
    baos.write(length >>> 24);
    baos.write((length >>> 16) & 0xFF);
    baos.write((length >>> 8) & 0xFF);
    baos.write(length & 0xFF);
    baos.write(buffer, 0, length);
    // q
    buffer = dssKey.getParams().getQ().toByteArray();
    length = buffer.length;
    baos.write(length >>> 24);
    baos.write((length >>> 16) & 0xFF);
    baos.write((length >>> 8) & 0xFF);
    baos.write(length & 0xFF);
    baos.write(buffer, 0, length);
    // g
    buffer = dssKey.getParams().getG().toByteArray();
    length = buffer.length;
    baos.write(length >>> 24);
    baos.write((length >>> 16) & 0xFF);
    baos.write((length >>> 8) & 0xFF);
    baos.write(length & 0xFF);
    baos.write(buffer, 0, length);
    // x
    buffer = dssKey.getX().toByteArray();
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
    if (k[0] != Registry.MAGIC_RAW_DSS_PRIVATE_KEY[0]
        || k[1] != Registry.MAGIC_RAW_DSS_PRIVATE_KEY[1]
        || k[2] != Registry.MAGIC_RAW_DSS_PRIVATE_KEY[2]
        || k[3] != Registry.MAGIC_RAW_DSS_PRIVATE_KEY[3])
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
    // g
    l =  k[i++]         << 24
      | (k[i++] & 0xFF) << 16
      | (k[i++] & 0xFF) << 8
      | (k[i++] & 0xFF);
    buffer = new byte[l];
    System.arraycopy(k, i, buffer, 0, l);
    i += l;
    BigInteger g = new BigInteger(1, buffer);
    // x
    l =  k[i++]         << 24
      | (k[i++] & 0xFF) << 16
      | (k[i++] & 0xFF) << 8
      | (k[i++] & 0xFF);
    buffer = new byte[l];
    System.arraycopy(k, i, buffer, 0, l);
    i += l;
    BigInteger x = new BigInteger(1, buffer);
    return new DSSPrivateKey(p, q, g, x);
  }
}
