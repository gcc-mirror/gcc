/* PKCS7SignedData.java -- reader for PKCS#7 signedData objects
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

import gnu.java.security.OID;
import gnu.java.security.ber.BER;
import gnu.java.security.ber.BEREncodingException;
import gnu.java.security.ber.BERReader;
import gnu.java.security.ber.BERValue;
import gnu.java.security.der.DERValue;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

import java.math.BigInteger;

import java.security.cert.CRL;
import java.security.cert.CRLException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

/**
 * The SignedData object in PKCS #7. This is a read-only implementation of
 * this format, and is used to provide signed Jar file support.
 *
 * @author Casey Marshall (csm@gnu.org)
 */
public class PKCS7SignedData
{

  public static final OID PKCS7_DATA = new OID("1.2.840.113549.1.7.1");
  public static final OID PKCS7_SIGNED_DATA = new OID("1.2.840.113549.1.7.2");

  private BigInteger version;
  private Set digestAlgorithms;
  private OID contentType;
  private byte[] content;
  private Certificate[] certificates;
  private CRL[] crls;
  private Set signerInfos;

  private static final boolean DEBUG = false;
  private static void debug(String msg)
  {
    System.err.print("PKCS7SignedData >> ");
    System.err.println(msg);
  }

  public PKCS7SignedData(InputStream in)
    throws CRLException, CertificateException, IOException
  {
    this(new BERReader(in));
  }

  /**
   * Parse an encoded PKCS#7 SignedData object. The ASN.1 format of this
   * object is:
   *
   * <pre>
   * SignedData ::= SEQUENCE {
   *   version Version,
   *   digestAlgorithms DigestAlgorithmIdentifiers,
   *   contentInfo ContentInfo,
   *   certificates
   *     [0] IMPLICIT ExtendedCertificatesAndCertificates OPTIONAL,
   *   crls
   *     [1] IMPLICIT CertificateRevocationLists OPTIONAL,
   *   signerInfos SignerInfos }
   *
   * Version ::= INTEGER
   *
   * DigestAlgorithmIdentifiers ::= SET OF DigestAlgorithmIdentifier
   *
   * DigestAlgorithmIdentifier ::= AlgorithmIdentifier
   *
   * ContentInfo ::= SEQUENCE {
   *   contentType ContentType,
   *   content [0] EXPLICIT ANY DEFINED BY contentType OPTIONAL }
   *
   * ContentType ::= OBJECT IDENTIFIER
   *
   * ExtendedCertificatesAndCertificates ::=
   *   SET OF ExtendedCertificatesAndCertificate
   *
   * ExtendedCertificatesAndCertificate ::= CHOICE {
   *   certificate Certificate, -- from X.509
   *   extendedCertificate [0] IMPLICIT ExtendedCertificate }
   *
   * CertificateRevocationLists ::= SET OF CertificateRevocationList
   *   -- from X.509
   *
   * SignerInfos ::= SET OF SignerInfo
   *
   * SignerInfo ::= SEQUENCE {
   *   version Version,
   *   issuerAndSerialNumber IssuerAndSerialNumber,
   *   digestAlgorithm DigestAlgorithmIdentifier,
   *   authenticatedAttributes
   *     [0] IMPLICIT Attributes OPTIONAL,
   *   digestEncryptionAlgorithm DigestEncryptionAlgorithmIdentifier,
   *   encryptedDigest EncryptedDigest,
   *   unauthenticatedAttributes
   *     [1] IMPLICIT Attributes OPTIONAL }
   *
   * EncryptedDigest ::= OCTET STRING
   * </pre>
   *
   * <p>(Readers who are confused as to why it takes 40 levels of indirection
   * to specify "data with a signature", rest assured that the present author
   * is as confused as you are).</p>
   */
  public PKCS7SignedData(BERReader ber)
    throws CRLException, CertificateException, IOException
  {
    CertificateFactory x509 = CertificateFactory.getInstance("X509");
    DERValue val = ber.read();
    if (!val.isConstructed())
      throw new BEREncodingException("malformed ContentInfo");

    val = ber.read();
    if (val.getTag() != BER.OBJECT_IDENTIFIER)
      throw new BEREncodingException("malformed ContentType");

    if (!PKCS7_SIGNED_DATA.equals(val.getValue()))
      throw new BEREncodingException("content is not SignedData");

    val = ber.read();
    if (val.getTag() != 0)
      throw new BEREncodingException("malformed Content");

    val = ber.read();
    if (!val.isConstructed())
      throw new BEREncodingException("malformed SignedData");

    if (DEBUG)
      debug("SignedData: " + val);

    val = ber.read();
    if (val.getTag() != BER.INTEGER)
      throw new BEREncodingException("expecting Version");
    version = (BigInteger) val.getValue();

    if (DEBUG)
      debug("  Version: " + version);

    digestAlgorithms = new HashSet();
    val = ber.read();
    if (!val.isConstructed())
      throw new BEREncodingException("malformed DigestAlgorithmIdentifiers");
    if (DEBUG)
      debug("  DigestAlgorithmIdentifiers: " + val);
    int count = 0;
    DERValue val2 = ber.read();
    while (val2 != BER.END_OF_SEQUENCE &&
           (val.getLength() > 0 && val.getLength() > count))
      {
        if (!val2.isConstructed())
          throw new BEREncodingException("malformed AlgorithmIdentifier");
        if (DEBUG)
          debug("    AlgorithmIdentifier: " + val2);
        count += val2.getEncodedLength();
        val2 = ber.read();
        if (val2.getTag() != BER.OBJECT_IDENTIFIER)
          throw new BEREncodingException("malformed AlgorithmIdentifier");
        if (DEBUG)
          debug("      ID: " + val2.getValue());
        List algId = new ArrayList(2);
        algId.add(val2.getValue());
        val2 = ber.read();
        if (val2 != BER.END_OF_SEQUENCE)
          {
            count += val2.getEncodedLength();
            if (val2.getTag() == BER.NULL)
              algId.add(null);
            else
              algId.add(val2.getEncoded());
            if (DEBUG)
              debug("      params: " + new BigInteger(1, val2.getEncoded()).toString(16));
            if (val2.isConstructed())
              ber.skip(val2.getLength());
            if (BERValue.isIndefinite(val))
              val2 = ber.read();
          }
        else
          algId.add(null);
        digestAlgorithms.add(algId);
      }

    val = ber.read();
    if (!val.isConstructed())
      throw new BEREncodingException("malformed ContentInfo");
    if (DEBUG)
      debug("  ContentInfo: " + val);
    val2 = ber.read();
    if (val2.getTag() != BER.OBJECT_IDENTIFIER)
      throw new BEREncodingException("malformed ContentType");
    contentType = (OID) val2.getValue();
    if (DEBUG)
      debug("    ContentType: " + contentType);
    if (BERValue.isIndefinite(val)
        || (val.getLength() > 0 && val.getLength() > val2.getEncodedLength()))
      {
        val2 = ber.read();
        if (val2 != BER.END_OF_SEQUENCE)
          {
            content = val2.getEncoded();
            if (BERValue.isIndefinite(val))
              val2 = ber.read();
            if (DEBUG)
              debug("    Content: " + new BigInteger(1, content).toString(16));
          }
      }

    val = ber.read();
    if (val.getTag() == 0)
      {
        if (!val.isConstructed())
          throw new BEREncodingException("malformed ExtendedCertificatesAndCertificates");
        if (DEBUG)
          debug("  ExtendedCertificatesAndCertificates: " + val);
        count = 0;
        val2 = ber.read();
        List certs = new LinkedList();
        while (val2 != BER.END_OF_SEQUENCE &&
               (val.getLength() > 0 && val.getLength() > count))
          {
            Certificate cert =
              x509.generateCertificate(new ByteArrayInputStream(val2.getEncoded()));
            if (DEBUG)
              debug("    Certificate: " + cert);
            certs.add(cert);
            count += val2.getEncodedLength();
            ber.skip(val2.getLength());
            if (BERValue.isIndefinite(val) || val.getLength() > count)
              val2 = ber.read();
          }
        certificates = (Certificate[]) certs.toArray(new Certificate[certs.size()]);
        val = ber.read();
      }

    if (val.getTag() == 1)
      {
        if (!val.isConstructed())
          throw new BEREncodingException("malformed CertificateRevocationLists");
        if (DEBUG)
          debug("  CertificateRevocationLists: " + val);
        count = 0;
        val2 = ber.read();
        List crls = new LinkedList();
        while (val2 != BER.END_OF_SEQUENCE &&
               (val.getLength() > 0 && val.getLength() > count))
          {
            CRL crl = x509.generateCRL(new ByteArrayInputStream(val2.getEncoded()));
            if (DEBUG)
              debug ("    CRL: " + crl);
            crls.add(crl);
            count += val2.getEncodedLength();
            ber.skip(val2.getLength());
            if (BERValue.isIndefinite(val) || val.getLength() > count)
              val2 = ber.read();
          }
        this.crls = (CRL[]) crls.toArray(new CRL[crls.size()]);
        val = ber.read();
      }

    signerInfos = new HashSet();
    if (!val.isConstructed())
      throw new BEREncodingException("malformed SignerInfos");

    if (DEBUG)
      debug("  SignerInfos: " + val);

    // FIXME read this more carefully.
    // Since we are just reading a file (probably) we just read until we
    // reach the end.
    while (true)
      {
        int i = ber.peek();
        if (i == 0 || i == -1)
          break;
        signerInfos.add(new SignerInfo(ber));
      }
  }

  public BigInteger getVersion()
  {
    return version;
  }

  public Certificate[] getCertificates()
  {
    return (certificates != null ? (Certificate[]) certificates.clone()
            : null);
  }

  public OID getContentType()
  {
    return contentType;
  }

  public byte[] getContent()
  {
    return (content != null ? (byte[]) content.clone() : null);
  }

  public Set getDigestAlgorithms()
  {
    // FIXME copy contents too, they are mutable!!!
    return Collections.unmodifiableSet(digestAlgorithms);
  }

  public Set getSignerInfos()
  {
    Set copy = new HashSet();
    for (Iterator it = signerInfos.iterator(); it.hasNext(); )
      copy.add(it.next());
    return Collections.unmodifiableSet(copy);
  }
}
