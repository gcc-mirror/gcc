/* RSAPKCS1V1_5Signature.java --
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
import gnu.java.security.hash.HashFactory;
import gnu.java.security.hash.IMessageDigest;
import gnu.java.security.sig.BaseSignature;

import java.math.BigInteger;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.interfaces.RSAPrivateKey;
import java.security.interfaces.RSAPublicKey;
import java.util.Arrays;

/**
 * The RSA-PKCS1-V1.5 signature scheme is a digital signature scheme with
 * appendix (SSA) combining the RSA algorithm with the EMSA-PKCS1-v1_5 encoding
 * method.
 * <p>
 * References:
 * <ol>
 * <li><a
 * href="http://www.cosic.esat.kuleuven.ac.be/nessie/workshop/submissions/rsa-pss.zip">
 * RSA-PSS Signature Scheme with Appendix, part B.</a><br>
 * Primitive specification and supporting documentation.<br>
 * Jakob Jonsson and Burt Kaliski.</li>
 * <li><a href="http://www.ietf.org/rfc/rfc3447.txt">Public-Key Cryptography
 * Standards (PKCS) #1:</a><br>
 * RSA Cryptography Specifications Version 2.1.<br>
 * Jakob Jonsson and Burt Kaliski.</li>
 * </ol>
 */
public class RSAPKCS1V1_5Signature
    extends BaseSignature
{
  /** The underlying EMSA-PKCS1-v1.5 instance for this object. */
  private EMSA_PKCS1_V1_5 pkcs1;

  /**
   * Default 0-arguments constructor. Uses SHA-1 as the default hash.
   */
  public RSAPKCS1V1_5Signature()
  {
    this(Registry.SHA160_HASH);
  }

  /**
   * Constructs an instance of this object using the designated message digest
   * algorithm as its underlying hash function.
   *
   * @param mdName the canonical name of the underlying hash function.
   */
  public RSAPKCS1V1_5Signature(final String mdName)
  {
    this(HashFactory.getInstance(mdName));
  }

  public RSAPKCS1V1_5Signature(IMessageDigest md)
  {
    super(Registry.RSA_PKCS1_V1_5_SIG, md);

    pkcs1 = EMSA_PKCS1_V1_5.getInstance(md.name());
  }

  /** Private constructor for cloning purposes. */
  private RSAPKCS1V1_5Signature(final RSAPKCS1V1_5Signature that)
  {
    this(that.md.name());

    this.publicKey = that.publicKey;
    this.privateKey = that.privateKey;
    this.md = (IMessageDigest) that.md.clone();
    this.pkcs1 = (EMSA_PKCS1_V1_5) that.pkcs1.clone();
  }

  public Object clone()
  {
    return new RSAPKCS1V1_5Signature(this);
  }

  protected void setupForVerification(final PublicKey k)
      throws IllegalArgumentException
  {
    if (! (k instanceof RSAPublicKey))
      throw new IllegalArgumentException();

    publicKey = k;
  }

  protected void setupForSigning(final PrivateKey k)
      throws IllegalArgumentException
  {
    if (! (k instanceof RSAPrivateKey))
      throw new IllegalArgumentException();

    privateKey = k;
  }

  protected Object generateSignature() throws IllegalStateException
  {
    // 1. EMSA-PKCS1-v1_5 encoding: Apply the EMSA-PKCS1-v1_5 encoding
    // operation (Section 9.2) to the message M to produce an encoded
    // message EM of length k octets:
    //
    // EM = EMSA-PKCS1-V1_5-ENCODE (M, k).
    //
    // If the encoding operation outputs "message too long," output
    // "message too long" and stop. If the encoding operation outputs
    // "intended encoded message length too short," output "RSA modulus
    // too short" and stop.
    final int modBits = ((RSAPrivateKey) privateKey).getModulus().bitLength();
    final int k = (modBits + 7) / 8;
    final byte[] EM = pkcs1.encode(md.digest(), k);
    // 2. RSA signature:
    // a. Convert the encoded message EM to an integer message epresentative
    // m (see Section 4.2): m = OS2IP (EM).
    final BigInteger m = new BigInteger(1, EM);
    // b. Apply the RSASP1 signature primitive (Section 5.2.1) to the RSA
    // private key K and the message representative m to produce an
    // integer signature representative s: s = RSASP1 (K, m).
    final BigInteger s = RSA.sign(privateKey, m);
    // c. Convert the signature representative s to a signature S of length
    // k octets (see Section 4.1): S = I2OSP (s, k).
    // 3. Output the signature S.
    return RSA.I2OSP(s, k);
  }

  protected boolean verifySignature(final Object sig)
      throws IllegalStateException
  {
    if (publicKey == null)
      throw new IllegalStateException();
    final byte[] S = (byte[]) sig;
    // 1. Length checking: If the length of the signature S is not k octets,
    // output "invalid signature" and stop.
    final int modBits = ((RSAPublicKey) publicKey).getModulus().bitLength();
    final int k = (modBits + 7) / 8;
    if (S.length != k)
      return false;
    // 2. RSA verification:
    // a. Convert the signature S to an integer signature representative
    // s (see Section 4.2): s = OS2IP (S).
    final BigInteger s = new BigInteger(1, S);
    // b. Apply the RSAVP1 verification primitive (Section 5.2.2) to the
    // RSA public key (n, e) and the signature representative s to
    // produce an integer message representative m:
    // m = RSAVP1 ((n, e), s).
    // If RSAVP1 outputs "signature representative out of range,"
    // output "invalid signature" and stop.
    final BigInteger m;
    try
      {
        m = RSA.verify(publicKey, s);
      }
    catch (IllegalArgumentException x)
      {
        return false;
      }
    // c. Convert the message representative m to an encoded message EM
    // of length k octets (see Section 4.1): EM = I2OSP (m, k).
    // If I2OSP outputs "integer too large," output "invalid signature"
    // and stop.
    final byte[] EM;
    try
      {
        EM = RSA.I2OSP(m, k);
      }
    catch (IllegalArgumentException x)
      {
        return false;
      }
    // 3. EMSA-PKCS1-v1_5 encoding: Apply the EMSA-PKCS1-v1_5 encoding
    // operation (Section 9.2) to the message M to produce a second
    // encoded message EM' of length k octets:
    // EM' = EMSA-PKCS1-V1_5-ENCODE (M, k).
    // If the encoding operation outputs "message too long," output
    // "message too long" and stop. If the encoding operation outputs
    // "intended encoded message length too short," output "RSA modulus
    // too short" and stop.
    final byte[] EMp = pkcs1.encode(md.digest(), k);
    // 4. Compare the encoded message EM and the second encoded message EM'.
    // If they are the same, output "valid signature"; otherwise, output
    // "invalid signature."
    return Arrays.equals(EM, EMp);
  }
}
