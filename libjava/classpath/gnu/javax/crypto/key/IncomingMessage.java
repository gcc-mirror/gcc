/* IncomingMessage.java --
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
import gnu.java.security.key.IKeyPairCodec;
import gnu.java.security.key.dss.DSSKeyPairPKCS8Codec;
import gnu.java.security.key.dss.DSSKeyPairRawCodec;
import gnu.java.security.key.dss.DSSKeyPairX509Codec;
import gnu.java.security.key.rsa.RSAKeyPairPKCS8Codec;
import gnu.java.security.key.rsa.RSAKeyPairRawCodec;
import gnu.java.security.key.rsa.RSAKeyPairX509Codec;
import gnu.javax.crypto.key.dh.DHKeyPairPKCS8Codec;
import gnu.javax.crypto.key.dh.DHKeyPairRawCodec;
import gnu.javax.crypto.key.dh.DHKeyPairX509Codec;
import gnu.javax.crypto.key.srp6.SRPKeyPairRawCodec;

import java.io.ByteArrayInputStream;
import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.security.PrivateKey;
import java.security.PublicKey;

/**
 * An implementation of an incoming message for use with key agreement
 * protocols.
 */
public class IncomingMessage
{
  /** The internal buffer stream containing the message's contents. */
  protected ByteArrayInputStream in;
  /** The length of the message contents, according to its 4-byte header. */
  protected int length;

  /**
   * Constructs an incoming message given the message's encoded form, including
   * its header bytes.
   *
   * @param b the encoded form, including the header bytes, of an incoming
   *          message.
   * @throws KeyAgreementException if the buffer is malformed.
   */
  public IncomingMessage(byte[] b) throws KeyAgreementException
  {
    this();

    if (b.length < 4)
      throw new KeyAgreementException("message header too short");
    length =  b[0]         << 24
           | (b[1] & 0xFF) << 16
           | (b[2] & 0xFF) << 8
           | (b[3] & 0xFF);
    if (length > Registry.SASL_BUFFER_MAX_LIMIT || length < 0)
      throw new KeyAgreementException("message size limit exceeded");
    in = new ByteArrayInputStream(b, 4, length);
  }

  /** Trivial private constructor for use by the class method. */
  private IncomingMessage()
  {
    super();
  }

  /**
   * Returns an instance of a message given its encoded contents, excluding the
   * message's header bytes.
   * <p>
   * Calls the method with the same name and three arguments as:
   * <code>getInstance(raw, 0, raw.length)</code>.
   *
   * @param raw the encoded form, excluding the header bytes.
   * @return a new instance of <code>IncomingMessage</code>.
   */
  public static IncomingMessage getInstance(byte[] raw)
  {
    return getInstance(raw, 0, raw.length);
  }

  /**
   * Returns an instance of a message given its encoded contents, excluding the
   * message's header bytes.
   *
   * @param raw the encoded form, excluding the header bytes.
   * @param offset offset where to start using raw bytes from.
   * @param len number of bytes to use.
   * @return a new instance of <code>IncomingMessage</code>.
   */
  public static IncomingMessage getInstance(byte[] raw, int offset, int len)
  {
    IncomingMessage result = new IncomingMessage();
    result.in = new ByteArrayInputStream(raw, offset, len);
    return result;
  }

  /**
   * Converts two octets into the number that they represent.
   *
   * @param b the two octets.
   * @return the length.
   */
  public static int twoBytesToLength(byte[] b) throws KeyAgreementException
  {
    int result = (b[0] & 0xFF) << 8 | (b[1] & 0xFF);
    if (result > Registry.SASL_TWO_BYTE_MAX_LIMIT)
      throw new KeyAgreementException("encoded MPI size limit exceeded");
    return result;
  }

  /**
   * Converts four octets into the number that they represent.
   *
   * @param b the four octets.
   * @return the length.
   */
  public static int fourBytesToLength(byte[] b) throws KeyAgreementException
  {
    int result =  b[0]         << 24
               | (b[1] & 0xFF) << 16
               | (b[2] & 0xFF) << 8
               | (b[3] & 0xFF);
    if (result > Registry.SASL_FOUR_BYTE_MAX_LIMIT || result < 0)
      throw new KeyAgreementException("encoded entity size limit exceeded");
    return result;
  }

  public boolean hasMoreElements()
  {
    return (in.available() > 0);
  }

  /**
   * Decodes a public key from the message.
   * <p>
   * See {@link OutgoingMessage#writePublicKey(java.security.PublicKey)} for
   * more details on the internal format.
   *
   * @throws KeyAgreementException if an encoding size constraint is violated or
   *           a mismatch was detected in the encoding.
   */
  public PublicKey readPublicKey() throws KeyAgreementException
  {
    if (in.available() < 5)
      throw new KeyAgreementException("not enough bytes for a public key in message");
    byte[] elementLengthBytes = new byte[4];
    in.read(elementLengthBytes, 0, 4);
    int elementLength = fourBytesToLength(elementLengthBytes);
    if (in.available() < elementLength)
      throw new KeyAgreementException("illegal public key encoding");
    int keyTypeAndFormatID = in.read() & 0xFF;
    elementLength--;
    byte[] kb = new byte[elementLength];
    in.read(kb, 0, elementLength);
    // instantiate the right codec and decode
    IKeyPairCodec kpc = getKeyPairCodec(keyTypeAndFormatID);
    return kpc.decodePublicKey(kb);
  }

  /**
   * Decodes a private key from the message.
   * <p>
   * See {@link OutgoingMessage#writePrivateKey(java.security.PrivateKey)} for
   * more details.
   *
   * @throws KeyAgreementException if an encoding size constraint is violated or
   *           a mismatch was detected in the encoding.
   */
  public PrivateKey readPrivateKey() throws KeyAgreementException
  {
    if (in.available() < 5)
      throw new KeyAgreementException("not enough bytes for a private key in message");
    byte[] elementLengthBytes = new byte[4];
    in.read(elementLengthBytes, 0, 4);
    int elementLength = fourBytesToLength(elementLengthBytes);
    if (in.available() < elementLength)
      throw new KeyAgreementException("illegal private key encoding");
    int keyTypeAndFormatID = in.read() & 0xFF;
    elementLength--;
    byte[] kb = new byte[elementLength];
    in.read(kb, 0, elementLength);
    // instantiate the right codec and decode
    IKeyPairCodec kpc = getKeyPairCodec(keyTypeAndFormatID);
    return kpc.decodePrivateKey(kb);
  }

  /**
   * Decodes an MPI from the current message's contents.
   *
   * @return a native representation of an MPI.
   * @throws KeyAgreementException if an encoding exception occurs during the
   *           operation.
   */
  public BigInteger readMPI() throws KeyAgreementException
  {
    if (in.available() < 2)
      throw new KeyAgreementException("not enough bytes for an MPI in message");
    byte[] elementLengthBytes = new byte[2];
    in.read(elementLengthBytes, 0, 2);
    int elementLength = twoBytesToLength(elementLengthBytes);
    if (in.available() < elementLength)
      throw new KeyAgreementException("illegal MPI encoding");
    byte[] element = new byte[elementLength];
    in.read(element, 0, element.length);
    return new BigInteger(1, element);
  }

  public String readString() throws KeyAgreementException
  {
    if (in.available() < 2)
      throw new KeyAgreementException("not enough bytes for a text in message");
    byte[] elementLengthBytes = new byte[2];
    in.read(elementLengthBytes, 0, 2);
    int elementLength = twoBytesToLength(elementLengthBytes);
    if (in.available() < elementLength)
      throw new KeyAgreementException("illegal text encoding");
    byte[] element = new byte[elementLength];
    in.read(element, 0, element.length);
    String result = null;
    try
      {
        result = new String(element, "UTF8");
      }
    catch (UnsupportedEncodingException x)
      {
        throw new KeyAgreementException("unxupported UTF8 encoding", x);
      }
    return result;
  }

  private IKeyPairCodec getKeyPairCodec(int keyTypeAndFormatID)
      throws KeyAgreementException
  {
    int keyType = (keyTypeAndFormatID >>> 4) & 0x0F;
    int formatID = keyTypeAndFormatID & 0x0F;
    switch (formatID)
      {
      case Registry.RAW_ENCODING_ID:
        switch (keyType)
          {
          case 0:
            return new DSSKeyPairRawCodec();
          case 1:
            return new RSAKeyPairRawCodec();
          case 2:
            return new DHKeyPairRawCodec();
          case 3:
            return new SRPKeyPairRawCodec();
          default:
            throw new KeyAgreementException("Unknown key-type for Raw format: "
                                            + keyType);
          }
      case Registry.X509_ENCODING_ID:
        switch (keyType)
          {
          case 0:
            return new DSSKeyPairX509Codec();
          case 1:
            return new RSAKeyPairX509Codec();
          case 2:
            return new DHKeyPairX509Codec();
          default:
            throw new KeyAgreementException("Unknown key-type for X.509 format: "
                                            + keyType);
          }
      case Registry.PKCS8_ENCODING_ID:
        switch (keyType)
          {
          case 0:
            return new DSSKeyPairPKCS8Codec();
          case 1:
            return new RSAKeyPairPKCS8Codec();
          case 2:
            return new DHKeyPairPKCS8Codec();
          default:
            throw new KeyAgreementException("Unknown key-type for PKCS#8 format: "
                                            + keyType);
          }
      default:
        throw new KeyAgreementException("Unknown format identifier: "
                                        + formatID);
      }
  }
}
