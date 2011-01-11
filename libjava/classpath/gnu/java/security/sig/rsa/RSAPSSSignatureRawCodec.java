/* RSAPSSSignatureRawCodec.java --
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


package gnu.java.security.sig.rsa;

import gnu.java.security.Registry;
import gnu.java.security.sig.ISignatureCodec;

import java.io.ByteArrayOutputStream;

/**
 * An object that implements the {@link ISignatureCodec} operations for the
 * <i>Raw</i> format to use with RSA-PSS signatures.
 */
public class RSAPSSSignatureRawCodec
    implements ISignatureCodec
{
  // implicit 0-arguments constructor

  public int getFormatID()
  {
    return RAW_FORMAT;
  }

  /**
   * Returns the encoded form of the designated RSA-PSS signature object
   * according to the <i>Raw</i> format supported by this library.
   * <p>
   * The <i>Raw</i> format for an RSA-PSS signature, in this implementation, is
   * a byte sequence consisting of the following:
   * <ol>
   * <li>4-byte magic consisting of the value of the literal
   * {@link Registry#MAGIC_RAW_RSA_PSS_SIGNATURE},
   * <li>
   * <li>1-byte version consisting of the constant: 0x01,</li>
   * <li>4-byte count of following bytes representing the RSA-PSS signature
   * bytes in internet order,</li>
   * <li>the RSA-PSS signature bytes in internet order.</li>
   * </ol>
   *
   * @param signature the signature to encode, consisting of the output of the
   *          <code>sign()</code> method of a {@link RSAPSSSignature} instance
   *          --a byte array.
   * @return the <i>Raw</i> format encoding of the designated signature.
   * @exception IllegalArgumentException if the designated signature is not an
   *              RSA-PSS one.
   */
  public byte[] encodeSignature(Object signature)
  {
    byte[] buffer;
    try
      {
        buffer = (byte[]) signature;
      }
    catch (Exception x)
      {
        throw new IllegalArgumentException("signature");
      }
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    // magic
    baos.write(Registry.MAGIC_RAW_RSA_PSS_SIGNATURE[0]);
    baos.write(Registry.MAGIC_RAW_RSA_PSS_SIGNATURE[1]);
    baos.write(Registry.MAGIC_RAW_RSA_PSS_SIGNATURE[2]);
    baos.write(Registry.MAGIC_RAW_RSA_PSS_SIGNATURE[3]);
    // version
    baos.write(0x01);
    // signature bytes
    int length = buffer.length;
    baos.write( length >>> 24);
    baos.write((length >>> 16) & 0xFF);
    baos.write((length >>> 8) & 0xFF);
    baos.write(length & 0xFF);
    baos.write(buffer, 0, length);
    return baos.toByteArray();
  }

  public Object decodeSignature(byte[] k)
  {
    // magic
    if (k[0] != Registry.MAGIC_RAW_RSA_PSS_SIGNATURE[0]
        || k[1] != Registry.MAGIC_RAW_RSA_PSS_SIGNATURE[1]
        || k[2] != Registry.MAGIC_RAW_RSA_PSS_SIGNATURE[2]
        || k[3] != Registry.MAGIC_RAW_RSA_PSS_SIGNATURE[3])
      throw new IllegalArgumentException("magic");
    // version
    if (k[4] != 0x01)
      throw new IllegalArgumentException("version");
    int i = 5;
    int l;
    // signature bytes
    l =  k[i++]         << 24
      | (k[i++] & 0xFF) << 16
      | (k[i++] & 0xFF) << 8
      | (k[i++] & 0xFF);
    byte[] result = new byte[l];
    System.arraycopy(k, i, result, 0, l);
    return result;
  }
}
