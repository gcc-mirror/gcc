/* X509CRL.java -- X.509 certificate revocation list.
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

import java.io.InputStream;
import java.io.IOException;

import java.math.BigInteger;

import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.HashMap;
import java.util.Set;

import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.PublicKey;
import java.security.Principal;
import java.security.Signature;
import java.security.SignatureException;
import java.security.cert.Certificate;
import java.security.cert.CRLException;
import java.security.cert.X509CRLEntry;

import javax.security.auth.x500.X500Principal;

import gnu.java.io.ASN1ParsingException;
import gnu.java.security.OID;
import gnu.java.security.der.BitString;
import gnu.java.security.der.DER;
import gnu.java.security.der.DERReader;
import gnu.java.security.der.DERValue;
import gnu.java.security.der.DERWriter;

/**
 * X.509 certificate revocation lists.
 *
 * @author Casey Marshall (rsdio@metastatic.org)
 */
public class X509CRL extends java.security.cert.X509CRL
{

  // Constants and fields.
  // ------------------------------------------------------------------------

  private static final OID ID_DSA = new OID("1.2.840.10040.4.1");
  private static final OID ID_DSA_WITH_SHA1 = new OID("1.2.840.10040.4.3");
  private static final OID ID_RSA = new OID("1.2.840.113549.1.1.1");
  private static final OID ID_RSA_WITH_MD2 = new OID("1.2.840.113549.1.1.2");
  private static final OID ID_RSA_WITH_MD5 = new OID("1.2.840.113549.1.1.4");
  private static final OID ID_RSA_WITH_SHA1 = new OID("1.2.840.113549.1.1.5");

  private byte[] encoded;

  private byte[] tbsCRLBytes;
  private int version;
  private OID algId;
  private byte[] algParams;
  private Date thisUpdate;
  private Date nextUpdate;
  private X500Principal issuerDN;
  private HashMap revokedCerts;
  private HashMap extensions;
  private HashSet critOids;
  private HashSet nonCritOids;
  
  private OID sigAlg;
  private byte[] sigAlgParams;
  private byte[] rawSig;
  private byte[] signature;

  // Constructors.
  // ------------------------------------------------------------------------

  /**
   * Create a new X.509 CRL.
   *
   * @param encoded The DER encoded CRL.
   * @throws CRLException If the input bytes are incorrect.
   * @throws IOException  If the input bytes cannot be read.
   */
  public X509CRL(InputStream encoded) throws CRLException, IOException
  {
    super();
    revokedCerts = new HashMap();
    extensions = new HashMap();
    critOids = new HashSet();
    nonCritOids = new HashSet();
    try
      {
        parse(encoded);
      }
    catch (IOException ioe)
      {
        ioe.printStackTrace();
        throw ioe;
      }
    catch (Exception x)
      {
        x.printStackTrace();
        throw new CRLException(x.toString());
      }
  }

  // X509CRL methods.
  // ------------------------------------------------------------------------

  public boolean equals(Object o)
  {
    return ((X509CRL) o).revokedCerts.equals(revokedCerts);
  }

  public int hashCode()
  {
    return revokedCerts.hashCode();
  }

  public byte[] getEncoded() throws CRLException
  {
    return (byte[]) encoded.clone();
  }

  public void verify(PublicKey key)
    throws CRLException, NoSuchAlgorithmException, InvalidKeyException,
           NoSuchProviderException, SignatureException
  {
    Signature sig = Signature.getInstance(sigAlg.toString());
    doVerify(sig, key);
  }

  public void verify(PublicKey key, String provider)
    throws CRLException, NoSuchAlgorithmException, InvalidKeyException,
           NoSuchProviderException, SignatureException
  {
    Signature sig = Signature.getInstance(sigAlg.toString(), provider);
    doVerify(sig, key);
  }

  public int getVersion()
  {
    return version;
  }

  public Principal getIssuerDN()
  {
    return issuerDN;
  }

  public X500Principal getIssuerX500Principal()
  {
    return issuerDN;
  }

  public Date getThisUpdate()
  {
    return (Date) thisUpdate.clone();
  }

  public Date getNextUpdate()
  {
    if (nextUpdate != null)
      return (Date) nextUpdate.clone();
    return null;
  }

  public X509CRLEntry getRevokedCertificate(BigInteger serialNo)
  {
    return (X509CRLEntry) revokedCerts.get(serialNo);
  }

  public Set getRevokedCertificates()
  {
    return Collections.unmodifiableSet(new HashSet(revokedCerts.values()));
  }

  public byte[] getTBSCertList() throws CRLException
  {
    return (byte[]) tbsCRLBytes.clone();
  }

  public byte[] getSignature()
  {
    return (byte[]) rawSig.clone();
  }

  public String getSigAlgName()
  {
    if (sigAlg.equals(ID_DSA_WITH_SHA1))
      return "SHA1withDSA";
    if (sigAlg.equals(ID_RSA_WITH_MD2))
      return "MD2withRSA";
    if (sigAlg.equals(ID_RSA_WITH_MD5))
      return "MD5withRSA";
    if (sigAlg.equals(ID_RSA_WITH_SHA1))
      return "SHA1withRSA";
    return "unknown";
  }

  public String getSigAlgOID()
  {
    return sigAlg.toString();
  }

  public byte[] getSigAlgParams()
  {
    if (sigAlgParams != null)
      return (byte[]) sigAlgParams.clone();
    return null;
  }

  // X509Extension methods.
  // ------------------------------------------------------------------------

  public boolean hasUnsupportedCriticalExtension()
  {
    return false; // XXX
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

  // CRL methods.
  // ------------------------------------------------------------------------

  public String toString()
  {
    return gnu.java.security.x509.X509CRL.class.getName();
  }

  public boolean isRevoked(Certificate cert)
  {
    if (!(cert instanceof java.security.cert.X509Certificate))
      throw new IllegalArgumentException("not a X.509 certificate");
    BigInteger certSerial =
      ((java.security.cert.X509Certificate) cert).getSerialNumber();
    X509CRLEntry ent = (X509CRLEntry) revokedCerts.get(certSerial);
    if (ent == null)
      return false;
    return ent.getRevocationDate().compareTo(new Date()) < 0;
  }

  // Own methods.
  // ------------------------------------------------------------------------

  private void doVerify(Signature sig, PublicKey key)
    throws CRLException, InvalidKeyException, SignatureException
  {
    sig.initVerify(key);
    sig.update(tbsCRLBytes);
    if (!sig.verify(signature))
      throw new CRLException("signature not verified");
  }

  private void parse(InputStream in) throws Exception
  {
    DERReader der = new DERReader(in);
    DERValue val = der.read();
    if (!val.isConstructed())
      throw new ASN1ParsingException("malformed CertificateList");
    encoded = val.getEncoded();

    val = der.read();
    if (!val.isConstructed())
      throw new ASN1ParsingException("malformed TBSCertList");
    tbsCRLBytes = val.getEncoded();

    val = der.read();
    if (val.getValue() instanceof BigInteger)
      {
        version = ((BigInteger) val.getValue()).intValue() + 1;
        val = der.read();
      }
    else
      version = 1;

    if (!val.isConstructed())
      throw new ASN1ParsingException("malformed AlgorithmIdentifier");
    DERValue algIdVal = der.read();
    algId = (OID) algIdVal.getValue();
    if (val.getLength() > algIdVal.getEncodedLength())
      {
        val = der.read();
        algParams = val.getEncoded();
        if (val.isConstructed())
          in.skip(val.getLength());
      }

    issuerDN = new X500Principal(in);

    thisUpdate = (Date) der.read().getValue();

    val = der.read();
    if (val.getValue() instanceof Date)
      {
        nextUpdate = (Date) val.getValue();
        val = der.read();
      }
    if (val.getTag() != 0)
      {
        int len = 0;
        while (len < val.getLength())
          {
            X509CRLEntry entry =
               new gnu.java.security.x509.X509CRLEntry(version, in);
            revokedCerts.put(entry.getSerialNumber(), entry);
            len += entry.getEncoded().length;
          }
      }
    if (version >= 2 && val.getTagClass() != DER.UNIVERSAL && val.getTag() == 0)
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
            len += ext.getEncodedLength();
          }
      }

    val = der.read();
    if (!val.isConstructed())
      throw new ASN1ParsingException("malformed AlgorithmIdentifier");
    DERValue sigAlgVal = der.read();
    sigAlg = (OID) sigAlgVal.getValue();
    if (val.getLength() > sigAlgVal.getEncodedLength())
      {
        val = der.read();
        sigAlgParams = (byte[]) val.getEncoded();
        if (val.isConstructed())
          in.skip(val.getLength());
      }
    val = der.read();
    rawSig = val.getEncoded();
    signature = ((BitString) val.getValue()).toByteArray();
  }
}
