/* EMSA_PKCS1_V1_5.java --
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


package gnu.java.security.sig.rsa;

import gnu.java.security.Registry;
import gnu.java.security.hash.HashFactory;
import gnu.java.security.hash.IMessageDigest;

import java.io.ByteArrayOutputStream;

/**
 * An implementation of the EMSA-PKCS1-V1.5 encoding scheme.
 * <p>
 * EMSA-PKCS1-V1.5 is parameterised by the choice of hash function Hash and
 * hLen which denotes the length in octets of the hash function output.
 * <p>
 * References:
 * <ol>
 *    <li><a href="http://www.ietf.org/rfc/rfc3447.txt">Public-Key Cryptography
 *    Standards (PKCS) #1:</a><br>
 *    RSA Cryptography Specifications Version 2.1.<br>
 *    Jakob Jonsson and Burt Kaliski.</li>
 * </ol>
 */
public class EMSA_PKCS1_V1_5
    implements Cloneable
{
  /* Notes.
   1. For the six hash functions mentioned in Appendix B.1, the DER encoding
   T of the DigestInfo value is equal to the following:

   MD2:     (0x)30 20 30 0c 06 08 2a 86 48 86 f7 0d 02 02 05 00 04 10 || H
   MD5:     (0x)30 20 30 0c 06 08 2a 86 48 86 f7 0d 02 05 05 00 04 10 || H
   SHA-1:   (0x)30 21 30 09 06 05 2b 0e 03 02 1a 05 00 04 14 || H
   SHA-256: (0x)30 31 30 0d 06 09 60 86 48 01 65 03 04 02 01 05 00 04 20 || H
   SHA-384: (0x)30 41 30 0d 06 09 60 86 48 01 65 03 04 02 02 05 00 04 30 || H
   SHA-512: (0x)30 51 30 0d 06 09 60 86 48 01 65 03 04 02 03 05 00 04 40 || H
   */
  private static final byte[] MD2_PREFIX = {
      (byte) 0x30, (byte) 0x20, (byte) 0x30, (byte) 0x0c, (byte) 0x06,
      (byte) 0x08, (byte) 0x2a, (byte) 0x86, (byte) 0x48, (byte) 0x86,
      (byte) 0xf7, (byte) 0x0d, (byte) 0x02, (byte) 0x02, (byte) 0x05,
      (byte) 0x00, (byte) 0x04, (byte) 0x10
  };

  private static final byte[] MD5_PREFIX = {
      (byte) 0x30, (byte) 0x20, (byte) 0x30, (byte) 0x0c, (byte) 0x06,
      (byte) 0x08, (byte) 0x2a, (byte) 0x86, (byte) 0x48, (byte) 0x86,
      (byte) 0xf7, (byte) 0x0d, (byte) 0x02, (byte) 0x05, (byte) 0x05,
      (byte) 0x00, (byte) 0x04, (byte) 0x10
  };

  private static final byte[] SHA160_PREFIX = {
      (byte) 0x30, (byte) 0x21, (byte) 0x30, (byte) 0x09, (byte) 0x06,
      (byte) 0x05, (byte) 0x2b, (byte) 0x0e, (byte) 0x03, (byte) 0x02,
      (byte) 0x1a, (byte) 0x05, (byte) 0x00, (byte) 0x04, (byte) 0x14
  };

  private static final byte[] SHA256_PREFIX = {
      (byte) 0x30, (byte) 0x31, (byte) 0x30, (byte) 0x0d, (byte) 0x06,
      (byte) 0x09, (byte) 0x60, (byte) 0x86, (byte) 0x48, (byte) 0x01,
      (byte) 0x65, (byte) 0x03, (byte) 0x04, (byte) 0x02, (byte) 0x01,
      (byte) 0x05, (byte) 0x00, (byte) 0x04, (byte) 0x20
  };

  private static final byte[] SHA384_PREFIX = {
      (byte) 0x30, (byte) 0x41, (byte) 0x30, (byte) 0x0d, (byte) 0x06,
      (byte) 0x09, (byte) 0x60, (byte) 0x86, (byte) 0x48, (byte) 0x01,
      (byte) 0x65, (byte) 0x03, (byte) 0x04, (byte) 0x02, (byte) 0x02,
      (byte) 0x05, (byte) 0x00, (byte) 0x04, (byte) 0x30
  };

  private static final byte[] SHA512_PREFIX = {
      (byte) 0x30, (byte) 0x51, (byte) 0x30, (byte) 0x0d, (byte) 0x06,
      (byte) 0x09, (byte) 0x60, (byte) 0x86, (byte) 0x48, (byte) 0x01,
      (byte) 0x65, (byte) 0x03, (byte) 0x04, (byte) 0x02, (byte) 0x03,
      (byte) 0x05, (byte) 0x00, (byte) 0x04, (byte) 0x40
  };

  /** The underlying hash function to use with this instance. */
  private IMessageDigest hash;

  /** The output size of the hash function in octets. */
  private int hLen; // TODO: field not used!!!  investigate

  /** The DER part of DigestInfo not containing the hash value itself. */
  private byte[] prefix;

  /**
   * Trivial private constructor to enforce use through Factory method.
   *
   * @param hash the message digest instance to use with this scheme instance.
   */
  private EMSA_PKCS1_V1_5(final IMessageDigest hash)
  {
    super();

    this.hash = hash;
    hLen = hash.hashSize();
    final String name = hash.name();
    if (name.equals(Registry.MD2_HASH))
      prefix = MD2_PREFIX;
    else if (name.equals(Registry.MD5_HASH))
      prefix = MD5_PREFIX;
    else if (name.equals(Registry.SHA160_HASH))
      prefix = SHA160_PREFIX;
    else if (name.equals(Registry.SHA256_HASH))
      prefix = SHA256_PREFIX;
    else if (name.equals(Registry.SHA384_HASH))
      prefix = SHA384_PREFIX;
    else if (name.equals(Registry.SHA512_HASH))
      prefix = SHA512_PREFIX;
    else
      throw new UnsupportedOperationException(); // should not happen
  }

  /**
   * Returns an instance of this object given a designated name of a hash
   * function.
   *
   * @param mdName the canonical name of a hash function.
   * @return an instance of this object configured for use with the designated
   * options.
   * @throws UnsupportedOperationException if the hash function is not
   * implemented or does not have an ID listed in RFC-3447.
   */
  public static final EMSA_PKCS1_V1_5 getInstance(final String mdName)
  {
    final IMessageDigest hash = HashFactory.getInstance(mdName);
    final String name = hash.name();
    if (! (name.equals(Registry.MD2_HASH)
          || name.equals(Registry.MD5_HASH)
          || name.equals(Registry.SHA160_HASH)
          || name.equals(Registry.SHA256_HASH)
          || name.equals(Registry.SHA384_HASH)
          || name.equals(Registry.SHA512_HASH)))
      throw new UnsupportedOperationException("hash with no OID: " + name);

    return new EMSA_PKCS1_V1_5(hash);
  }

  public Object clone()
  {
    return getInstance(hash.name());
  }

  /**
   * Frames the hash of a message, along with an ID of the hash function in
   * a DER sequence according to the specifications of EMSA-PKCS1-V1.5 as
   * described in RFC-3447 (see class documentation).
   *
   * @param mHash the byte sequence resulting from applying the message digest
   * algorithm Hash to the message <i>M</i>.
   * @param emLen intended length in octets of the encoded message, at least
   * <code>tLen + 11</code>, where <code>tLen</code> is the octet length of the
   * DER encoding <code>T</code> of a certain value computed during the
   * encoding operation.
   * @return encoded message, an octet string of length <code>emLen</code>.
   * @throws IllegalArgumentException if the message is too long, or if the
   * intended encoded message length is too short.
   */
  public byte[] encode(final byte[] mHash, final int emLen)
  {
    // 1. Apply the hash function to the message M to produce a hash value
    //    H: H = Hash(M).
    //    If the hash function outputs "message too long," output "message
    //    too long" and stop.
    // 2. Encode the algorithm ID for the hash function and the hash value
    //    into an ASN.1 value of type DigestInfo (see Appendix A.2.4) with
    //    the Distinguished Encoding Rules (DER), where the type DigestInfo
    //    has the syntax
    //       DigestInfo ::= SEQUENCE {
    //          digestAlgorithm AlgorithmIdentifier,
    //          digest OCTET STRING
    //       }
    //   The first field identifies the hash function and the second contains
    //   the hash value.  Let T be the DER encoding of the DigestInfo value
    //   (see the notes below) and let tLen be the length in octets of T.
    final ByteArrayOutputStream baos = new ByteArrayOutputStream();
    baos.write(prefix, 0, prefix.length);
    baos.write(mHash, 0, mHash.length);
    final byte[] T = baos.toByteArray();
    final int tLen = T.length;
    // 3. If emLen < tLen + 11, output "intended encoded message length too
    //    short" and stop.
    if (emLen < tLen + 11)
      throw new IllegalArgumentException("emLen too short");
    // 4. Generate an octet string PS consisting of emLen - tLen - 3 octets
    //    with hexadecimal value 0xff.  The length of PS will be at least 8
    //    octets.
    final byte[] PS = new byte[emLen - tLen - 3];
    for (int i = 0; i < PS.length; i++)
      PS[i] = (byte) 0xFF;
    // 5. Concatenate PS, the DER encoding T, and other padding to form the
    //    encoded message EM as: EM = 0x00 || 0x01 || PS || 0x00 || T.
    baos.reset();
    baos.write(0x00);
    baos.write(0x01);
    baos.write(PS, 0, PS.length);
    baos.write(0x00);
    baos.write(T, 0, tLen);
    final byte[] result = baos.toByteArray();
    baos.reset();
    // 6. Output EM.
    return result;
  }
}
