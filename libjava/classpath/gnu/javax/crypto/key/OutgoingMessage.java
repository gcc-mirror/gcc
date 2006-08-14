/* OutgoingMessage.java -- 
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


package gnu.javax.crypto.key;

import gnu.java.security.Registry;
import gnu.java.security.key.dss.DSSKey;
import gnu.java.security.key.rsa.GnuRSAKey;
import gnu.java.security.util.FormatUtil;
import gnu.javax.crypto.key.dh.GnuDHKey;
import gnu.javax.crypto.key.srp6.SRPKey;

import java.io.ByteArrayOutputStream;
import java.io.UnsupportedEncodingException;
import java.security.Key;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.math.BigInteger;

/**
 * An implementation of outgoing messages for use with key agreement protocols.
 */
public class OutgoingMessage
{
  /** The internal output stream. */
  private ByteArrayOutputStream out;

  public OutgoingMessage()
  {
    super();

    out = new ByteArrayOutputStream();
  }

  /**
   * Returns the encoded form of the current message including the 4-byte length
   * header.
   * 
   * @throws KeyAgreementException if an encoding size constraint is violated.
   */
  public byte[] toByteArray() throws KeyAgreementException
  {
    byte[] buffer = wrap();
    int length = buffer.length;
    byte[] result = new byte[length + 4];
    result[0] = (byte)(length >>> 24);
    result[1] = (byte)(length >>> 16);
    result[2] = (byte)(length >>> 8);
    result[3] = (byte) length;
    System.arraycopy(buffer, 0, result, 4, length);
    return result;
  }

  /**
   * Returns the encoded form of the current message excluding the 4-byte length
   * header.
   * 
   * @throws KeyAgreementException if an encoding size constraint is violated.
   */
  public byte[] wrap() throws KeyAgreementException
  {
    int length = out.size();
    if (length > Registry.SASL_BUFFER_MAX_LIMIT || length < 0)
      throw new KeyAgreementException("message content is too long");
    return out.toByteArray();
  }

  /**
   * Encodes a public key into the message.
   * <p>
   * When a public key is encoded into an outgoing message, the byte array of
   * the encoded key --according to its encoding/decoding format specified when
   * the key was first instantiated-- are put in the message (a) preceeded by
   * one byte representing both the type of key (upper 4-bit) and the identifier
   * of the format used (lower 4-bit), and (b) preceeed by a 4-byte entity
   * representing the total length, excluding these 4 bytes, of the bytes
   * representing the encoded key and the one-byte representing the key-type and
   * format; i.e.
   * <pre>
   * key --&gt; 4-byte-length || 1-byte-type-and-format || encoded-key-bytes
   * </pre>
   * 
   * @param k the public key to encode.
   * @throws KeyAgreementException if an encoding size constraint is violated.
   */
  public void writePublicKey(PublicKey k) throws KeyAgreementException
  {
    writeKey(k);
  }

  /**
   * Encodes a private key into the message.
   * <p>
   * When a private key is encoded into an outgoing message, the byte array of
   * the encoded key --according to its encoding/decoding format specified when
   * the key was first instantiated-- are put in the message (a) preceeded by
   * one byte representing both the type of key (upper 4-bit) and the identifier
   * of the format used (lower 4-bit), and (b) preceeed by a 4-byte entity
   * representing the total length, excluding these 4 bytes, of the bytes
   * representing the encoded key and the one-byte representing the key-type and
   * format; i.e.
   * <pre>
   * key --&gt; 4-byte-length || 1-byte-type-and-format || encoded-key-bytes
   * </pre>
   * 
   * @param k the private key to encode.
   * @throws KeyAgreementException if an encoding size constraint is violated.
   */
  public void writePrivateKey(PrivateKey k) throws KeyAgreementException
  {
    writeKey(k);
  }

  /**
   * Encodes an MPI into the message.
   * 
   * @param val the MPI to encode.
   * @throws KeyAgreementException if an encoding size constraint is violated.
   */
  public void writeMPI(BigInteger val) throws KeyAgreementException
  {
    byte[] b = val.toByteArray();
    int length = b.length;
    if (length > Registry.SASL_TWO_BYTE_MAX_LIMIT)
      throw new KeyAgreementException("MPI is too long");
    byte[] lengthBytes = { (byte)(length >>> 8), (byte) length };
    out.write(lengthBytes, 0, 2);
    out.write(b, 0, b.length);
  }

  /**
   * Encodes a string into the message.
   * 
   * @param s the string to encode.
   * @throws KeyAgreementException if the UTF8 encoding is not supported on this
   *           platform, or if an encoding size constraint is violated.
   */
  public void writeString(String s) throws KeyAgreementException
  {
    byte[] b = null;
    try
      {
        b = s.getBytes("UTF8");
      }
    catch (UnsupportedEncodingException x)
      {
        throw new KeyAgreementException("unxupported UTF8 encoding", x);
      }
    int length = b.length;
    if (length > Registry.SASL_TWO_BYTE_MAX_LIMIT)
      throw new KeyAgreementException("text too long");
    byte[] lengthBytes = { (byte)(length >>> 8), (byte) length };
    out.write(lengthBytes, 0, 2);
    out.write(b, 0, b.length);
  }

  /**
   * @param k the key to encode.
   * @throws KeyAgreementException if an encoding size constraint is violated.
   */
  private void writeKey(Key k) throws KeyAgreementException
  {
    byte[] b = k.getEncoded();
    int keyType = getKeyType(k);
    int formatID = FormatUtil.getFormatID(k.getFormat());
    int length = b.length + 1;
    if (length > Registry.SASL_FOUR_BYTE_MAX_LIMIT)
      throw new KeyAgreementException("Encoded key is too long");
    byte[] lengthBytes = {
        (byte)(length >>> 24),
        (byte)(length >>> 16),
        (byte)(length >>> 8),
        (byte) length };
    out.write(lengthBytes, 0, 4);
    out.write(((keyType & 0x0F) << 4) | (formatID & 0x0F));
    out.write(b, 0, b.length);
  }

  /**
   * @param k the key to find an identifier for.
   * @return an integer from <code>0</code> to <code>3</code> identifying
   *         the type of key.
   * @throws KeyAgreementException if the designated key is of unknown or
   *           unsupported type.
   */
  private int getKeyType(Key k) throws KeyAgreementException
  {
    if (k instanceof DSSKey)
      return 0;
    if (k instanceof GnuRSAKey)
      return 1;
    if (k instanceof GnuDHKey)
      return 2;
    if (k instanceof SRPKey)
      return 3;
    throw new KeyAgreementException("Unknown or unsupported key type: "
                                    + k.getClass().getName());
  }
}
