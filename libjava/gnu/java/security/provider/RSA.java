/* RSA.java -- RSA PKCS#1 signatures.
   Copyright (C) 2004  Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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
exception statement from your version. */


package gnu.java.security.provider;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

import java.math.BigInteger;

import java.security.InvalidKeyException;
import java.security.MessageDigest;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.SecureRandom;
import java.security.SignatureException;
import java.security.SignatureSpi;
import java.security.interfaces.RSAPrivateKey;
import java.security.interfaces.RSAPublicKey;

import java.util.ArrayList;

import gnu.java.security.OID;
import gnu.java.security.der.DER;
import gnu.java.security.der.DERReader;
import gnu.java.security.der.DERValue;
import gnu.java.security.der.DERWriter;

public abstract class RSA extends SignatureSpi implements Cloneable
{

  // Constants and fields.
  // -------------------------------------------------------------------------

  /**
   * digestAlgorithm OBJECT IDENTIFIER ::=
   *   { iso(1) member-body(2) US(840) rsadsi(113549) digestAlgorithm(2) }
   */
  protected static final OID DIGEST_ALGORITHM = new OID("1.2.840.113549.2");

  protected final OID digestAlgorithm;
  protected final MessageDigest md;
  protected RSAPrivateKey signerKey;
  protected RSAPublicKey verifierKey;

  // Constructor.
  // -------------------------------------------------------------------------

  protected RSA(MessageDigest md, OID digestAlgorithm)
  {
    super();
    this.md = md;
    this.digestAlgorithm = digestAlgorithm;
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public Object clone() throws CloneNotSupportedException
  {
    return super.clone();
  }

  protected Object engineGetParameter(String param)
  {
    throw new UnsupportedOperationException("deprecated");
  }

  protected void engineSetParameter(String param, Object value)
  {
    throw new UnsupportedOperationException("deprecated");
  }

  protected void engineInitSign(PrivateKey privateKey)
    throws InvalidKeyException
  {
    if (!(privateKey instanceof RSAPrivateKey))
      throw new InvalidKeyException();
    verifierKey = null;
    signerKey = (RSAPrivateKey) privateKey;
  }

  protected void engineInitSign(PrivateKey privateKey, SecureRandom random)
    throws InvalidKeyException
  {
    // This class does not need random bytes.
    engineInitSign(privateKey);
  }

  protected void engineInitVerify(PublicKey publicKey)
    throws InvalidKeyException
  {
    if (!(publicKey instanceof RSAPublicKey))
      throw new InvalidKeyException();
    signerKey = null;
    verifierKey = (RSAPublicKey) publicKey;
  }

  protected void engineUpdate(byte b) throws SignatureException
  {
    if (signerKey == null && verifierKey == null)
      throw new SignatureException("not initialized");
    md.update(b);
  }

  protected void engineUpdate(byte[] buf, int off, int len)
    throws SignatureException
  {
    if (signerKey == null && verifierKey == null)
      throw new SignatureException("not initialized");
    md.update(buf, off, len);
  }

  protected byte[] engineSign() throws SignatureException
  {
    if (signerKey == null)
      throw new SignatureException("not initialized for signing");
    //
    // The signature will be the RSA encrypted BER representation of
    // the following:
    //
    //   DigestInfo ::= SEQUENCE {
    //     digestAlgorithm  DigestAlgorithmIdentifier,
    //     digest           Digest }
    //
    //   DigestAlgorithmIdentifier ::= AlgorithmIdentifier
    //
    //   Digest ::= OCTET STRING
    //
    ArrayList digestAlg = new ArrayList(2);
    digestAlg.add(new DERValue(DER.OBJECT_IDENTIFIER, digestAlgorithm));
    digestAlg.add(new DERValue(DER.NULL, null));
    ArrayList digestInfo = new ArrayList(2);
    digestInfo.add(new DERValue(DER.SEQUENCE, digestAlg));
    digestInfo.add(new DERValue(DER.OCTET_STRING, md.digest()));
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    try
      {
        DERWriter.write(out, new DERValue(DER.SEQUENCE, digestInfo));
      }
    catch (IOException ioe)
      {
        throw new SignatureException(ioe.toString());
      }
    byte[] buf = out.toByteArray();
    md.reset();

    // k = octect length of the modulus.
    int k = signerKey.getModulus().bitLength();
    k = (k >>> 3) + ((k & 7) == 0 ? 0 : 1);
    if (buf.length < k - 3)
      {
        throw new SignatureException("RSA modulus too small");
      }
    byte[] d = new byte[k];

    // Padding type 1:
    //     00 | 01 | FF | ... | FF | 00 | D
    d[1] = 0x01;
    for (int i = 2; i < k - buf.length - 1; i++)
      d[i] = (byte) 0xFF;
    System.arraycopy(buf, 0, d, k - buf.length, buf.length);

    BigInteger eb = new BigInteger(d);

    byte[] ed = eb.modPow(signerKey.getPrivateExponent(),
                          signerKey.getModulus()).toByteArray();

    // Ensure output is k octets long.
    if (ed.length < k)
      {
        byte[] b = new byte[k];
        System.arraycopy(eb, 0, b, k - ed.length, ed.length);
        ed = b;
      }
    else if (ed.length > k)
      {
        if (ed.length != k + 1)
          {
            throw new SignatureException("modPow result is larger than the modulus");
          }
        // Maybe an extra 00 octect.
        byte[] b = new byte[k];
        System.arraycopy(ed, 1, b, 0, k);
        ed = b;
      }

    return ed;
  }

  protected int engineSign(byte[] out, int off, int len)
    throws SignatureException
  {
    if (out == null || off < 0 || len < 0 || off+len > out.length)
      throw new SignatureException("illegal output argument");
    byte[] result = engineSign();
    if (result.length > len)
      throw new SignatureException("not enough space for signature");
    System.arraycopy(result, 0, out, off, result.length);
    return result.length;
  }

  protected boolean engineVerify(byte[] sig) throws SignatureException
  {
    if (verifierKey == null)
      throw new SignatureException("not initialized for verifying");
    if (sig == null)
      throw new SignatureException("no signature specified");
    int k = verifierKey.getModulus().bitLength();
    k = (k >>> 3) + ((k & 7) == 0 ? 0 : 1);
    if (sig.length != k)
      throw new SignatureException("signature is the wrong size (expecting "
                                   + k + " bytes, got " + sig.length + ")");
    BigInteger ed = new BigInteger(1, sig);
    byte[] eb = ed.modPow(verifierKey.getPublicExponent(),
                          verifierKey.getModulus()).toByteArray();

    int i = 0;
    if (eb[0] == 0x00)
      {
        for (i = 1; i < eb.length && eb[i] == 0x00; i++);
        if (i == 1)
          throw new SignatureException("wrong RSA padding");
        i--;
      }
    else if (eb[0] == 0x01)
      {
        for (i = 1; i < eb.length && eb[i] != 0x00; i++)
          if (eb[i] != (byte) 0xFF)
            throw new IllegalArgumentException("wrong RSA padding");
      }
    else
      throw new SignatureException("wrong RSA padding type");

    byte[] d = new byte[eb.length-i-1];
    System.arraycopy(eb, i+1, d, 0, eb.length-i-1);

    DERReader der = new DERReader(d);
    try
      {
        DERValue val = der.read();
        if (val.getTag() != DER.SEQUENCE)
          throw new SignatureException("failed to parse DigestInfo");
        val = der.read();
        if (val.getTag() != DER.SEQUENCE)
          throw new SignatureException("failed to parse DigestAlgorithmIdentifier");
        boolean sequenceIsBer = val.getLength() == 0;
        val = der.read();
        if (val.getTag() != DER.OBJECT_IDENTIFIER)
          throw new SignatureException("failed to parse object identifier");
        if (!val.getValue().equals(digestAlgorithm))
          throw new SignatureException("digest algorithms do not match");
        val = der.read();
        // We should never see parameters here, since they are never used.
        if (val.getTag() != DER.NULL)
          throw new SignatureException("cannot handle digest parameters");
        if (sequenceIsBer)
          der.skip(1); // end-of-sequence byte.
        val = der.read();
        if (val.getTag() != DER.OCTET_STRING)
          throw new SignatureException("failed to parse Digest");
        return MessageDigest.isEqual(md.digest(), (byte[]) val.getValue());
      }
    catch (IOException ioe)
      {
        throw new SignatureException(ioe.toString());
      }
  }

  protected boolean engineVerify(byte[] sig, int off, int len)
    throws SignatureException
  {
    if (sig == null || off < 0 || len < 0 || off+len > sig.length)
      throw new SignatureException("illegal parameter");
    byte[] buf = new byte[len];
    System.arraycopy(sig, off, buf, 0, len);
    return engineVerify(buf);
  }
}
