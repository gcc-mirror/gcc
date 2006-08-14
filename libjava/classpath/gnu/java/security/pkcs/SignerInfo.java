/* SignerInfo.java -- a SignerInfo object, from PKCS #7
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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

package gnu.java.security.pkcs;

import gnu.java.security.Configuration;
import gnu.java.security.OID;
import gnu.java.security.ber.BER;
import gnu.java.security.ber.BEREncodingException;
import gnu.java.security.ber.BERReader;
import gnu.java.security.ber.BERValue;
import gnu.java.security.der.DER;
import gnu.java.security.der.DERValue;
import gnu.java.security.der.DERWriter;
import gnu.java.security.util.Util;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.logging.Logger;

import javax.security.auth.x500.X500Principal;

public class SignerInfo
{
  private static final Logger log = Logger.getLogger(SignerInfo.class.getName());

  private final BigInteger version;
  private final BigInteger serialNumber;
  private final X500Principal issuer;
  private final OID digestAlgorithmId;
  private final byte[] digestAlgorithmParams;
  private final byte[] authenticatedAttributes;
  private final OID digestEncryptionAlgorithmId;
  private final byte[] digestEncryptionAlgorithmParams;
  private final byte[] encryptedDigest;
  private final byte[] unauthenticatedAttributes;

  /**
   * Parse a SignerInfo object.
   * <p>
   * A SignerInfo is a structure with the following ASN.1 syntax:
   * <pre>
   * SignerInfo ::= SEQUENCE {
   *   version                       Version, -- always 1 for PKCS7 v1.5
   *   issuerAndSerialNumber         IssuerAndSerialNumber, -- an INTEGER
   *   digestAlgorithm               DigestAlgorithmIdentifier,
   *   authenticatedAttributes   [0] IMPLICIT Attributes OPTIONAL,
   *   digestEncryptionAlgorithm     DigestEncryptionAlgorithmIdentifier,
   *   encryptedDigest               EncryptedDigest,
   *   unauthenticatedAttributes [1] IMPLICIT Attributes OPTIONAL }
   *
   * IssuerAndSerialNumber ::= SEQUENCE {
   *   issuer       Name,
   *   serialNumber CertificateSerialNumber
   * }
   * 
   * DigestAlgorithmIdentifier ::= AlgorithmIdentifier
   * 
   * DigestEncryptionAlgorithmIdentifier ::= AlgorithmIdentifier
   *
   * EncryptedDigest ::= OCTET STRING
   * </pre>
   */
  public SignerInfo(BERReader ber) throws IOException
  {
    DERValue val = ber.read();
    if (Configuration.DEBUG)
      log.fine("SignerInfo: " + val);
    if (!val.isConstructed())
      throw new BEREncodingException("malformed SignerInfo");

    val = ber.read();
    if (val.getTag() != BER.INTEGER)
      throw new BEREncodingException("malformed Version");

    version = (BigInteger) val.getValue();
    log.fine("  Version: " + version);

    val = ber.read();
    if (!val.isConstructed())
      throw new BEREncodingException("malformed IssuerAndSerialNumber");
    if (Configuration.DEBUG)
      log.fine("  IssuerAndSerialNumber: " + val);

    val = ber.read();
    if (!val.isConstructed())
      throw new BEREncodingException("malformed Issuer");

    issuer = new X500Principal(val.getEncoded());
    ber.skip(val.getLength());
    if (Configuration.DEBUG)
      log.fine("    Issuer: " + issuer);

    val = ber.read();
    if (val.getTag() != BER.INTEGER)
      throw new BEREncodingException("malformed SerialNumber");

    serialNumber = (BigInteger) val.getValue();
    if (Configuration.DEBUG)
      log.fine("    SerialNumber: " + serialNumber);

    val = ber.read();
    if (!val.isConstructed())
      throw new BEREncodingException("malformed DigestAlgorithmIdentifier");
    if (Configuration.DEBUG)
      log.fine("  DigestAlgorithmIdentifier: " + val);

    int count = 0;
    DERValue val2 = ber.read();
    if (val2.getTag() != BER.OBJECT_IDENTIFIER)
      throw new BEREncodingException("malformed AlgorithmIdentifier");

    digestAlgorithmId = (OID) val2.getValue();
    if (Configuration.DEBUG)
      log.fine("    digestAlgorithm OID: " + digestAlgorithmId);

    if (BERValue.isIndefinite(val))
      {
        val2 = ber.read();
        if (val2 != BER.END_OF_SEQUENCE)
          {
            digestAlgorithmParams = val2.getEncoded();
            val2 = ber.read();
            if (val2 != BER.END_OF_SEQUENCE)
              throw new BEREncodingException("expecting BER end-of-sequence");
          }
        else
          digestAlgorithmParams = null;
      }
    else if (val2.getEncodedLength() < val.getLength())
      {
        val2 = ber.read();
        digestAlgorithmParams = val2.getEncoded();
        if (val2.isConstructed())
          ber.skip(val2.getLength());
      }
    else
      digestAlgorithmParams = null;

    if (Configuration.DEBUG)
      {
        log.fine("    digestAlgorithm params: ");
        log.fine(Util.dumpString(digestAlgorithmParams,
                                 "    digestAlgorithm params: "));
      }
    val = ber.read();
    if (val.getTag() == 0)
      {
        authenticatedAttributes = val.getEncoded();
        val = ber.read();
        if (val.isConstructed())
          ber.skip(val.getLength());

        val = ber.read();
      }
    else
      authenticatedAttributes = null;

    if (Configuration.DEBUG)
      {
        log.fine("  AuthenticatedAttributes: ");
        log.fine(Util.dumpString(authenticatedAttributes,
                                 "  AuthenticatedAttributes: "));
      }
    if (!val.isConstructed())
      throw new BEREncodingException("malformed DigestEncryptionAlgorithmIdentifier");
    if (Configuration.DEBUG)
      log.fine("  DigestEncryptionAlgorithmIdentifier: " + val);
    count = 0;
    val2 = ber.read();
    if (val2.getTag() != BER.OBJECT_IDENTIFIER)
      throw new BEREncodingException("malformed AlgorithmIdentifier");

    digestEncryptionAlgorithmId = (OID) val2.getValue();
    if (Configuration.DEBUG)
      log.fine("    digestEncryptionAlgorithm OID: " + digestEncryptionAlgorithmId);

    if (BERValue.isIndefinite(val))
      {
        val2 = ber.read();
        if (val2 != BER.END_OF_SEQUENCE)
          {
            digestEncryptionAlgorithmParams = val2.getEncoded();
            val2 = ber.read();
            if (val2 != BER.END_OF_SEQUENCE)
              throw new BEREncodingException("expecting BER end-of-sequence");
          }
        else
          digestEncryptionAlgorithmParams = null;
      }
    else if (val2.getEncodedLength() < val.getLength())
      {
        val2 = ber.read();
        digestEncryptionAlgorithmParams = val2.getEncoded();
        if (val2.isConstructed())
          ber.skip(val2.getLength());
      }
    else
      digestEncryptionAlgorithmParams = null;

    if (Configuration.DEBUG)
      {
        log.fine("    digestEncryptionAlgorithm params: ");
        log.fine(Util.dumpString(digestEncryptionAlgorithmParams,
                                 "    digestEncryptionAlgorithm params: "));
      }
    val = ber.read();
    if (val.getTag() != BER.OCTET_STRING)
      throw new BEREncodingException("malformed EncryptedDigest");

    encryptedDigest = (byte[]) val.getValue();
    if (Configuration.DEBUG)
      {
        log.fine("  EncryptedDigest: ");
        log.fine(Util.dumpString(encryptedDigest, "  EncryptedDigest: "));
      }
    if (ber.peek() == 1)
      unauthenticatedAttributes = ber.read().getEncoded();
    else
      unauthenticatedAttributes = null;

    if (Configuration.DEBUG)
      {
        log.fine("  UnauthenticatedAttributes: ");
        log.fine(Util.dumpString(unauthenticatedAttributes,
                                 "  UnauthenticatedAttributes: "));
      }
    if (ber.peek() == 0)
      ber.read();
  }

  /**
   * Constructs a new instance of <code>SignerInfo</code> given a designated
   * set of fields.
   * 
   * @param issuer the X.500 Principal name of the signer referenced by this
   *          instance.
   * @param serialNumber the serial number of the certificate being used. Both
   *          this and the previous arguments are gleaned from the signer's
   *          certificate.
   * @param digestAlgorithmOID the OID of the digest algorithm. When
   *          constructing the DigestAlgorithmIdentifier with this OID, the
   *          parameters part will be NULL.
   * @param authenticatedAttributes the encoding of the set of authenticated
   *          attributes to use.
   * @param digestEncryptionAlgorithmOID the OID of the digest encryption
   *          algorithm. When constructing the
   *          DigestEncryptionAlgorithmIdentifier with this OID, the parameters
   *          part will be NULL.
   * @param encryptedDigest the encrypted hash generated with this signer's
   *          private key.
   * @param unauthenticatedAttributes the encoding of the set of
   *          unauthencticated attributes.
   */
  public SignerInfo(X500Principal issuer, BigInteger serialNumber,
                    OID digestAlgorithmOID, byte[] authenticatedAttributes,
                    OID digestEncryptionAlgorithmOID,
                    byte[] encryptedDigest, byte[] unauthenticatedAttributes)
  {
    super();

    this.version = BigInteger.ONE;
    this.issuer = issuer;
    this.serialNumber = serialNumber;
    this.digestAlgorithmId = digestAlgorithmOID;
    this.digestAlgorithmParams = null;
    this.authenticatedAttributes = authenticatedAttributes;
    this.digestEncryptionAlgorithmId = digestEncryptionAlgorithmOID;
    this.digestEncryptionAlgorithmParams = null;
    this.encryptedDigest = encryptedDigest;
    this.unauthenticatedAttributes = unauthenticatedAttributes;
  }

  public BigInteger getVersion()
  {
    return version;
  }

  public BigInteger getSerialNumber()
  {
    return serialNumber;
  }

  public X500Principal getIssuer()
  {
    return issuer;
  }

  public OID getDigestAlgorithmId()
  {
    return digestAlgorithmId;
  }

  public byte[] getDigestAlgorithmParams()
  {
    return (digestAlgorithmParams != null
            ? (byte[]) digestAlgorithmParams.clone()
            : null);
  }

  public byte[] getAuthenticatedAttributes()
  {
    return (authenticatedAttributes != null
            ? (byte[]) authenticatedAttributes.clone()
            : null);
  }

  public OID getDigestEncryptionAlgorithmId()
  {
    return digestEncryptionAlgorithmId;
  }

  public byte[] getDigestEncryptionAlgorithmParams()
  {
    return (digestEncryptionAlgorithmParams != null
            ? (byte[]) digestEncryptionAlgorithmParams.clone()
            : null);
  }

  public byte[] getEncryptedDigest()
  {
    return (encryptedDigest != null ? (byte[]) encryptedDigest.clone() : null);
  }

  public byte[] getUnauthenticatedAttributes()
  {
    return (unauthenticatedAttributes != null
            ? (byte[]) unauthenticatedAttributes.clone()
            : null);
  }

  /**
   * Writes to the designated output stream the DER encoding of the current
   * contents of this instance.
   * 
   * @param out the destination output stream.
   * @throws IOException if an I/O related exception occurs during the process.
   */
  public void encode(OutputStream out) throws IOException
  {
    DERValue derVersion = new DERValue(DER.INTEGER, version);

    ByteArrayOutputStream baos = new ByteArrayOutputStream(4096);
    baos.write(issuer.getEncoded());
    DERValue derSerialNumber = new DERValue(DER.INTEGER, serialNumber);
    DERWriter.write(baos, derSerialNumber);
    baos.flush();
    byte[] b = baos.toByteArray();
    DERValue derIssuerAndSerialNumber =
        new DERValue(DER.CONSTRUCTED | DER.SEQUENCE, b.length, b, null);

    DERValue derDigestAlgorithmOID = new DERValue(DER.OBJECT_IDENTIFIER,
                                                  digestAlgorithmId);
    ArrayList digestAlgorithmIdentifier = new ArrayList(1);
    digestAlgorithmIdentifier.add(derDigestAlgorithmOID);
    DERValue derDigestAlgorithmIdentifier =
        new DERValue(DER.CONSTRUCTED | DER.SEQUENCE, digestAlgorithmIdentifier);

    DERValue derAuthenticatedAttributes;
    if (authenticatedAttributes == null)
      derAuthenticatedAttributes = new DERValue(DER.NULL, null);
    else
      derAuthenticatedAttributes = new DERValue(DER.CONSTRUCTED | DER.SET,
                                                authenticatedAttributes);

    DERValue derDigestEncryptionAlgorithmOID =
        new DERValue(DER.OBJECT_IDENTIFIER, digestEncryptionAlgorithmId);
    ArrayList digestEncryptionAlgorithmIdentifier = new ArrayList(1);
    digestEncryptionAlgorithmIdentifier.add(derDigestEncryptionAlgorithmOID);
    DERValue derDigestEncryptionAlgorithmIdentifier =
        new DERValue(DER.CONSTRUCTED | DER.SEQUENCE, digestEncryptionAlgorithmIdentifier);

    DERValue derEncryptedDigest = new DERValue(DER.OCTET_STRING, encryptedDigest);

    DERValue derUnauthenticatedAttributes;
    if (unauthenticatedAttributes == null)
      derUnauthenticatedAttributes = new DERValue(DER.NULL, null);
    else
      derUnauthenticatedAttributes = new DERValue(DER.CONSTRUCTED | DER.SET,
                                                  unauthenticatedAttributes);

    ArrayList signerInfo = new ArrayList(5);
    signerInfo.add(derVersion);
    signerInfo.add(derIssuerAndSerialNumber);
    signerInfo.add(derDigestAlgorithmIdentifier);
    signerInfo.add(derDigestEncryptionAlgorithmIdentifier);
    signerInfo.add(derEncryptedDigest);
    DERValue derSignerInfo = new DERValue(DER.CONSTRUCTED | DER.SEQUENCE,
                                          signerInfo);
    DERWriter.write(out, derSignerInfo);
  }
}
