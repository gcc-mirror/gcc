/* X509Certificate.java -- X.509 certificate.
   Copyright (C) 2003 Free Software Foundation, Inc.

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


package gnu.java.security.x509;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.ObjectStreamException;
import java.io.Serializable;

import java.math.BigInteger;

import java.net.InetAddress;

import java.security.AlgorithmParameters;
import java.security.InvalidKeyException;
import java.security.KeyFactory;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.Principal;
import java.security.PublicKey;
import java.security.Signature;
import java.security.SignatureException;

import java.security.cert.CertificateEncodingException;
import java.security.cert.CertificateException;
import java.security.cert.CertificateExpiredException;
import java.security.cert.CertificateNotYetValidException;
import java.security.cert.CertificateParsingException;

import java.security.spec.DSAParameterSpec;
import java.security.spec.DSAPublicKeySpec;
import java.security.spec.RSAPublicKeySpec;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import javax.security.auth.x500.X500Principal;

import gnu.java.io.ASN1ParsingException;
import gnu.java.security.OID;
import gnu.java.security.der.BitString;
import gnu.java.security.der.DER;
import gnu.java.security.der.DERReader;
import gnu.java.security.der.DERValue;
import gnu.java.security.der.DERWriter;

/**
 * An implementation of X.509 certificates.
 *
 * @author Casey Marshall (rsdio@metastatic.org)
 */
public class X509Certificate extends java.security.cert.X509Certificate
  implements Serializable
{

  // Constants and fields.
  // ------------------------------------------------------------------------

  private static final OID ID_DSA = new OID("1.2.840.10040.4.1");
  private static final OID ID_DSA_WITH_SHA1 = new OID("1.2.840.10040.4.3");
  private static final OID ID_RSA = new OID("1.2.840.113549.1.1.1");
  private static final OID ID_RSA_WITH_MD2 = new OID("1.2.840.113549.1.1.2");
  private static final OID ID_RSA_WITH_MD5 = new OID("1.2.840.113549.1.1.4");
  private static final OID ID_RSA_WITH_SHA1 = new OID("1.2.840.113549.1.1.5");

  private static final OID ID_EXTENSION = new OID("2.5.29");
  private static final OID ID_KEY_USAGE = ID_EXTENSION.getChild(15);
  private static final OID ID_BASIC_CONSTRAINTS = ID_EXTENSION.getChild(19);
  private static final OID ID_EXT_KEY_USAGE = ID_EXTENSION.getChild(37);

  private static final int OTHER_NAME     = 0;
  private static final int RFC882_NAME    = 1;
  private static final int DNS_NAME       = 2;
  private static final int X400_ADDRESS   = 3;
  private static final int DIRECTORY_NAME = 4;
  private static final int EDI_PARTY_NAME = 5;
  private static final int URI            = 6;
  private static final int IP_ADDRESS     = 7;
  private static final int REGISTERED_ID  = 8;

  // This object SHOULD be serialized with an instance of
  // java.security.cert.Certificate.CertificateRep, thus all fields are
  // transient.

  // The encoded certificate.
  private transient byte[] encoded;

  // TBSCertificate part.
  private transient byte[] tbsCertBytes;
  private transient int version;
  private transient BigInteger serialNo;
  private transient OID algId;
  private transient byte[] algVal;
  private transient X500Principal issuer;
  private transient Date notBefore;
  private transient Date notAfter;
  private transient X500Principal subject;
  private transient PublicKey subjectKey;
  private transient BitString issuerUniqueId;
  private transient BitString subjectUniqueId;
  private transient HashMap extensions;
  private transient HashSet critOids;
  private transient HashSet nonCritOids;
  
  private transient BitString keyUsage;
  private transient int basicConstraints = -1;

  // Signature.
  private transient OID sigAlgId;
  private transient byte[] sigAlgVal;
  private transient byte[] signature;

  // Constructors.
  // ------------------------------------------------------------------------

  /**
   * Create a new X.509 certificate from the encoded data. The input
   * data are expected to be the ASN.1 DER encoding of the certificate.
   *
   * @param encoded The encoded certificate data.
   * @throws IOException If the certificate cannot be read, possibly
   * from a formatting error.
   * @throws CertificateException If the data read is not an X.509
   * certificate.
   */
  public X509Certificate(InputStream encoded)
    throws CertificateException, IOException
  {
    super();
    extensions = new HashMap();
    critOids = new HashSet();
    nonCritOids = new HashSet();
    try
      {
        parse(encoded);
      }
    catch (IOException ioe)
      {
        throw ioe;
      }
    catch (Exception e)
      {
        throw new CertificateException(e.toString());
      }
  }

  // X509Certificate methods.
  // ------------------------------------------------------------------------

  public void checkValidity()
    throws CertificateExpiredException, CertificateNotYetValidException
  {
    checkValidity(new Date());
  }

  public void checkValidity(Date date)
    throws CertificateExpiredException, CertificateNotYetValidException
  {
    if (date.compareTo(notBefore) < 0)
      throw new CertificateNotYetValidException();
    if (date.compareTo(notAfter) > 0)
      throw new CertificateExpiredException();
  }

  public int getVersion()
  {
    return version;
  }

  public BigInteger getSerialNumber()
  {
    return serialNo;
  }

  public Principal getIssuerDN()
  {
    return getIssuerX500Principal();
  }

  public X500Principal getIssuerX500Principal()
  {
    return issuer;
  }

  public Principal getSubjectDN()
  {
    return getSubjectX500Principal();
  }

  public X500Principal getSubjectX500Principal()
  {
    return subject;
  }

  public Date getNotBefore()
  {
    return (Date) notBefore.clone();
  }

  public Date getNotAfter()
  {
    return (Date) notAfter.clone();
  }

  public byte[] getTBSCertificate() throws CertificateEncodingException
  {
    return (byte[]) tbsCertBytes.clone();
  }

  public byte[] getSignature()
  {
    return (byte[]) signature.clone();
  }

  public String getSigAlgName()
  {
    if (sigAlgId.equals(ID_DSA_WITH_SHA1))
      return "SHA1withDSA";
    if (sigAlgId.equals(ID_RSA_WITH_MD2 ))
      return "MD2withRSA";
    if (sigAlgId.equals(ID_RSA_WITH_MD5 ))
      return "MD5withRSA";
    if (sigAlgId.equals(ID_RSA_WITH_SHA1 ))
      return "SHA1withRSA";
    return "unknown";
    // return sigAlgId.getShortName();
  }

  public String getSigAlgOID()
  {
    return sigAlgId.toString();
  }

  public byte[] getSigAlgParams()
  {
    return (byte[]) sigAlgVal.clone();
  }

  public boolean[] getIssuerUniqueID()
  {
    if (issuerUniqueId != null)
      return issuerUniqueId.toBooleanArray();
    return null;
  }

  public boolean[] getSubjectUniqueID()
  {
    if (subjectUniqueId != null)
      return subjectUniqueId.toBooleanArray();
    return null;
  }

  public boolean[] getKeyUsage()
  {
    if (keyUsage != null)
      return keyUsage.toBooleanArray();
    return null;
  }

  public List getExtendedKeyUsage() throws CertificateParsingException
  {
    byte[] ext = (byte[]) extensions.get("2.5.29.37");
    if (ext == null)
      return null;
    LinkedList usages = new LinkedList();
    try
      {
        DERReader der = new DERReader(new ByteArrayInputStream(ext));
        DERValue seq = der.read();
        if (!seq.isConstructed())
          throw new CertificateParsingException();
        int len = 0;
        while (len < seq.getLength())
          {
            DERValue oid = der.read();
            if (!(oid.getValue() instanceof OID))
              throw new CertificateParsingException();
            usages.add(oid.getValue().toString());
            len += DERWriter.definiteEncodingSize(oid.getLength())
                 + oid.getLength() + 1;
          }
      }
    catch (IOException ioe)
      {
        throw new CertificateParsingException();
      }
    return usages;
  }

  public int getBasicConstraints()
  {
    return basicConstraints;
  }

  public Collection getSubjectAlternativeNames()
    throws CertificateParsingException
  {
    byte[] ext = getExtensionValue("2.5.29.17");
    if (ext == null)
      return null;
    return getAltNames(ext);
  }

  public Collection getIssuerAlternativeNames()
    throws CertificateParsingException
  {
    byte[] ext = getExtensionValue("2.5.29.18");
    if (ext == null)
      return null;
    return getAltNames(ext);
  }

// X509Extension methods.
  // ------------------------------------------------------------------------

  public boolean hasUnsupportedCriticalExtension()
  {
    for (Iterator it = critOids.iterator(); it.hasNext(); )
      {
        String oid = (String) it.next();
        if (!oid.equals("2.5.29.15") && !oid.equals("2.5.29.17") &&
            !oid.equals("2.5.29.18") && !oid.equals("2.5.29.19") &&
            !oid.equals("2.5.29.37"))
          return true;
      }
    return false;
  }

  public Set getCriticalExtensionOIDs()
  {
    return Collections.unmodifiableSet(critOids);
  }

  public Set getNonCriticalExtensionOIDs()
  {
    return Collections.unmodifiableSet(nonCritOids);
  }

  public byte[] getExtensionValue(String oid)
  {
    byte[] ext = (byte[]) extensions.get(oid);
    if (ext != null)
      return (byte[]) ext.clone();
    return null;
  }

  // Certificate methods.
  // ------------------------------------------------------------------------

  public byte[] getEncoded() throws CertificateEncodingException
  {
    return (byte[]) encoded.clone();
  }

  public void verify(PublicKey key)
    throws CertificateException, NoSuchAlgorithmException, 
           InvalidKeyException, NoSuchProviderException, SignatureException
  {
    Signature sig = Signature.getInstance(sigAlgId.toString());
    doVerify(sig, key);
  }

  public void verify(PublicKey key, String provider)
    throws CertificateException, NoSuchAlgorithmException,
           InvalidKeyException, NoSuchProviderException, SignatureException
  {
    Signature sig = Signature.getInstance(sigAlgId.toString(), provider);
    doVerify(sig, key);
  }

  public String toString()
  {
    // XXX say more than this.
    return gnu.java.security.x509.X509Certificate.class.getName();
  }

  public PublicKey getPublicKey()
  {
    return subjectKey;
  }

  protected Object writeReplace() throws ObjectStreamException
  {
    return super.writeReplace();
  }
  
  // Own methods.
  // ------------------------------------------------------------------------

  /**
   * Verify this certificate's signature.
   */
  private void doVerify(Signature sig, PublicKey key)
    throws CertificateException, InvalidKeyException, SignatureException
  {
    sig.initVerify(key);
    sig.update(tbsCertBytes);
    if (!sig.verify(signature))
      throw new CertificateException("signature not validated");
  }

  /**
   * Read a GeneralNames structure.
   */
  private List getAltNames(byte[] encoded)
    throws CertificateParsingException
  {
    LinkedList names = new LinkedList();
    try
      {
        ByteArrayInputStream in = new ByteArrayInputStream(encoded);
        DERReader der = new DERReader(in);
        DERValue seq = der.read();
        if (!seq.isConstructed())
          throw new CertificateParsingException();
        int len = 0;
        while (len < seq.getLength())
          {
            DERValue name = der.read();
            ArrayList pair = new ArrayList(2);
            Object nameVal = null;
            switch (name.getTag())
              {
                case RFC882_NAME:
                case DNS_NAME:
                case URI:
                  nameVal = new String((byte[]) name.getValue());
                  break;
                case IP_ADDRESS:
                  nameVal = InetAddress.getByAddress(
                    (byte[]) name.getValue()).getHostAddress();
                  break;
                case REGISTERED_ID:
                  nameVal = new OID((byte[]) name.getValue());
                  break;
                case OTHER_NAME:
                case X400_ADDRESS:
                case DIRECTORY_NAME:
                case EDI_PARTY_NAME:
                  nameVal = name.getEncoded();
                  break;
                default:
                  throw new CertificateParsingException();
              }
            pair.add(new Integer(name.getTag()));
            pair.add(nameVal);
            names.add(pair);
            if (name.isConstructed())
              in.skip(name.getLength());
            len += name.getEncodedLength();
          }
      }
    catch (IOException ioe)
      {
        throw new CertificateParsingException(ioe.toString());
      }
    return Collections.unmodifiableList(names);
  }

  /**
   * Parse a DER stream into an X.509 certificate.
   *
   * @param encoded The encoded bytes.
   */
  private void parse(InputStream encoded) throws Exception
  {
    DERReader der = new DERReader(encoded);

    // Certificate ::= SEQUENCE {
    DERValue cert = der.read();
    this.encoded = cert.getEncoded();
    if (!cert.isConstructed())
      throw new ASN1ParsingException("malformed Certificate");

    // TBSCertificate ::= SEQUENCE {
    DERValue tbsCert = der.read();
    if (tbsCert.getValue() != DER.CONSTRUCTED_VALUE)
      throw new ASN1ParsingException("malformed TBSCertificate");
    tbsCertBytes = tbsCert.getEncoded();

    DERValue val = der.read();
    if (val.getTagClass() == DER.CONTEXT && val.getTag() == 0)
      {
        // Version ::= INTEGER [0] { v1(0), v2(1), v3(2) }
        version = ((BigInteger) der.read().getValue()).intValue() + 1;
        val = der.read();
      }
    else
      {
        version = 1;
      }
    // SerialNumber ::= INTEGER
    serialNo = (BigInteger) val.getValue();

    // AlgorithmIdentifier ::= SEQUENCE {
    val = der.read();
    if (!val.isConstructed())
      throw new ASN1ParsingException("malformed AlgorithmIdentifier");
    int certAlgLen = val.getLength();
    val = der.read();
    algId = (OID) val.getValue();
    if (certAlgLen > val.getEncodedLength())
      {
        val = der.read();
        if (val == null)
          algVal = null;
        else
          algVal = val.getEncoded();
        if (val.isConstructed())
          encoded.skip(val.getLength());
      }

    issuer = new X500Principal(encoded);

    if (!der.read().isConstructed())
      throw new ASN1ParsingException("malformed Validity");
    notBefore = (Date) der.read().getValue();
    notAfter  = (Date) der.read().getValue();

    subject = new X500Principal(encoded);

    if (!der.read().isConstructed())
      throw new ASN1ParsingException("malformed SubjectPublicKeyInfo");
   
    val = der.read();
    if (!val.isConstructed())
      throw new ASN1ParsingException("malformed AlgorithmIdentifier");
    int keyAlgLen = val.getLength();
    val = der.read();
    OID keyID = (OID) val.getValue();
    byte[] keyParams = null;
    if (keyAlgLen > val.getEncodedLength())
      {
        val = der.read();
        keyParams = val.getEncoded();
        if (algVal == null)
          algVal = keyParams;
        if (val.isConstructed())
          encoded.skip(val.getLength());
      }
    val = der.read();
    byte[] keyVal = ((BitString) val.getValue()).toByteArray();

    if (keyID.equals(ID_DSA))
      {
        AlgorithmParameters params = AlgorithmParameters.getInstance("DSA");
        params.init(keyParams, "ASN.1");
        KeyFactory keyFac = KeyFactory.getInstance("DSA");
        DSAParameterSpec spec = (DSAParameterSpec)
          params.getParameterSpec(DSAParameterSpec.class);
        subjectKey = keyFac.generatePublic(new DSAPublicKeySpec(
          (BigInteger) new DERReader(keyVal).read().getValue(),
          spec.getP(), spec.getQ(), spec.getG()));
      }
    else if (keyID.equals(ID_RSA))
      {
        KeyFactory keyFac = KeyFactory.getInstance("RSA");
        DERReader rsaKey = new DERReader(keyVal);
        if (!rsaKey.read().isConstructed())
          throw new ASN1ParsingException("malformed RSAPublicKey");
        subjectKey = keyFac.generatePublic(new RSAPublicKeySpec(
          (BigInteger) rsaKey.read().getValue(),
          (BigInteger) rsaKey.read().getValue()));
      }
    else
      throw new ASN1ParsingException("unknown key algorithm " + keyID);

    if (version > 1)
      val = der.read();
    if (version >= 2 && val.getTagClass() != DER.UNIVERSAL && val.getTag() == 1)
      {
        byte[] b = (byte[]) val.getValue();
        issuerUniqueId = new BitString(b, 1, b.length-1, b[0] & 0xFF);
        val = der.read();
      }
    if (version >= 2 && val.getTagClass() != DER.UNIVERSAL && val.getTag() == 2)
      {
        byte[] b = (byte[]) val.getValue();
        subjectUniqueId = new BitString(b, 1, b.length-1, b[0] & 0xFF);
        val = der.read();
      }
    if (version >= 3 && val.getTagClass() != DER.UNIVERSAL && val.getTag() == 3)
      {
        val = der.read();
        int len = 0;
        while (len < val.getLength())
          {
            DERValue ext = der.read();
            OID extId = (OID) der.read().getValue();
            DERValue val2 = der.read();
            Boolean crit = Boolean.valueOf(false);
            if (val2.getValue() instanceof Boolean)
              {
                crit = (Boolean) val2.getValue();
                val2 = der.read();
              }
            byte[] extVal = (byte[]) val2.getValue();
            extensions.put(extId.toString(), extVal);
            if (crit.booleanValue())
              critOids.add(extId.toString());
            else
              nonCritOids.add(extId.toString());
            if (extId.equals(ID_KEY_USAGE))
              {
                keyUsage = (BitString) DERReader.read(extVal).getValue();
              }
            else if (extId.equals(ID_BASIC_CONSTRAINTS))
              {
                DERReader bc = new DERReader(extVal);
                DERValue constraints = bc.read();
                if (!constraints.isConstructed())
                  throw new ASN1ParsingException("malformed BasicConstraints");
                if (constraints.getLength() > 0)
                  {
                    boolean ca = false;
                    int constr = -1;
                    val2 = bc.read();
                    if (val2.getValue() instanceof Boolean)
                      {
                        ca = ((Boolean) val2.getValue()).booleanValue();
                        if (constraints.getLength() > val2.getEncodedLength())
                          val2 = bc.read();
                      }
                    if (val2.getValue() instanceof BigInteger)
                      constr = ((BigInteger) val2.getValue()).intValue();
                    basicConstraints = constr;
                  }
              }
            len += ext.getEncodedLength();
          }
      }

    val = der.read();
    if (!val.isConstructed())
      throw new ASN1ParsingException("malformed AlgorithmIdentifier");
    int sigAlgLen = val.getLength();
    val = der.read();
    sigAlgId = (OID) val.getValue();
    if (sigAlgLen > val.getEncodedLength())
      {
        val = der.read();
        if (val.getValue() == null)
          sigAlgVal = keyParams;
        else
          sigAlgVal = (byte[]) val.getEncoded();
        if (val.isConstructed())
          encoded.skip(val.getLength());
      }
    signature = ((BitString) der.read().getValue()).toByteArray();
  }
}
