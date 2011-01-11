/* X509CertSelector.java -- selects X.509 certificates by criteria.
   Copyright (C) 2004, 2005, 2006 Free Software Foundation, Inc.

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


package java.security.cert;

import gnu.classpath.SystemProperties;
import gnu.java.lang.CPStringBuilder;
import gnu.java.security.OID;
import gnu.java.security.x509.GnuPKIExtension;
import gnu.java.security.x509.ext.CertificatePolicies;
import gnu.java.security.x509.ext.Extension;
import gnu.java.security.x509.ext.GeneralName;
import gnu.java.security.x509.ext.GeneralSubtree;
import gnu.java.security.x509.ext.NameConstraints;
import gnu.java.security.x509.ext.GeneralName.Kind;

import java.io.IOException;
import java.math.BigInteger;
import java.net.InetAddress;
import java.security.KeyFactory;
import java.security.PublicKey;
import java.security.spec.X509EncodedKeySpec;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import javax.security.auth.x500.X500Principal;

/**
 * A concrete implementation of {@link CertSelector} for X.509 certificates,
 * which allows a number of criteria to be set when accepting certificates,
 * from validity dates, to issuer and subject distinguished names, to some
 * of the various X.509 extensions.
 *
 * <p>Use of this class requires extensive knowledge of the Internet
 * Engineering Task Force's Public Key Infrastructure (X.509). The primary
 * document describing this standard is <a
 * href="http://www.ietf.org/rfc/rfc3280.txt">RFC 3280: Internet X.509
 * Public Key Infrastructure Certificate and Certificate Revocation List
 * (CRL) Profile</a>.
 *
 * <p>Note that this class is not thread-safe. If multiple threads will
 * use or modify this class then they need to synchronize on the object.
 *
 * @author Casey Marshall (csm@gnu.org)
 * @since 1.4
 */
public class X509CertSelector implements CertSelector, Cloneable
{

  // Constants and fields.
  // -------------------------------------------------------------------------

  private static final String AUTH_KEY_ID = "2.5.29.35";
  private static final String SUBJECT_KEY_ID = "2.5.29.14";
  private static final String NAME_CONSTRAINTS_ID = "2.5.29.30";

  private static boolean checkOid(int[] oid)
  {
    return (oid != null && oid.length > 2 &&
            (oid[0] >= 0 && oid[0] <= 2) && (oid[1] >= 0 && oid[1] <= 39));
  }

  private static GeneralName makeName(int id, String name) throws IOException
  {
    byte[] nameBytes = null;
    GeneralName.Kind kind = GeneralName.Kind.forTag(id);
    switch (Kind.forTag(id))
    {
      case dNSName:
      case rfc822Name:
      case uniformResourceIdentifier:
        nameBytes = name.getBytes("ASCII");
        break;

      case iPAddress:
        InetAddress addr = InetAddress.getByName(name);
        nameBytes = addr.getAddress();
        break;

      case registeredId:
        OID oid = new OID(name);
        nameBytes = oid.getDER();
        break;

      case directoryName:
        X500Principal xname = new X500Principal(name);
        nameBytes = xname.getEncoded();
        break;

      case ediPartyName:
      case x400Address:
      case otherName:
        throw new IOException("cannot decode string representation of "
                              + kind);
    }
    return new GeneralName(kind, nameBytes);
  }

  private int basicConstraints;
  private X509Certificate cert;
  private BigInteger serialNo;
  private X500Principal issuer;
  private X500Principal subject;
  private byte[] subjectKeyId;
  private byte[] authKeyId;
  private boolean[] keyUsage;
  private Date certValid;
  private OID sigId;
  private PublicKey subjectKey;
  private X509EncodedKeySpec subjectKeySpec;
  private Set<String> keyPurposeSet;
  private List<GeneralName> altNames;
  private boolean matchAllNames;
  private byte[] nameConstraints;
  private Set<OID> policy;
  private List<GeneralName> pathToNames;

  /**
   * Creates a new X.509 certificate selector. The new selector will be
   * empty, and will accept any certificate (provided that it is an
   * {@link X509Certificate}).
   */
  public X509CertSelector()
  {
    basicConstraints = -1;
  }

  /**
   * Add a name to match in the NameConstraints extension. The argument is
   * the DER-encoded bytes of a GeneralName structure.
   *
   * See the method {@link #addSubjectAlternativeName(int, byte[])} for the
   * format of the GeneralName structure.
   *
   * @param id The name identifier. Must be between 0 and 8.
   * @param name The DER-encoded bytes of the name to match.
   * @throws IOException If the name DER is malformed.
   */
  public void addPathToName(int id, byte[] name) throws IOException
  {
    GeneralName generalName = new GeneralName(GeneralName.Kind.forTag(id), name);
    if (pathToNames == null)
      pathToNames = new LinkedList<GeneralName>();
    pathToNames.add(generalName);
  }

  /**
   * Add a name to match in the NameConstraints extension. This method will
   * only recognize certain types of name that have convenient string
   * encodings. For robustness, you should use the {@link
   *  #addPathToName(int, byte[])} method whenever possible.
   *
   * @param id The name identifier. Must be between 0 and 8.
   * @param name The name.
   * @throws IOException If the name cannot be decoded.
   */
  public void addPathToName(int id, String name) throws IOException
  {
    GeneralName generalName = makeName(id, name);
    if (pathToNames == null)
      pathToNames = new LinkedList<GeneralName>();
    pathToNames.add(generalName);
  }

  /**
   * Add a name, as DER-encoded bytes, to the subject alternative names
   * criterion.
   *
   * The name is a GeneralName structure, which has the ASN.1 format:
   *
   * <pre>
  GeneralName ::= CHOICE {
    otherName                       [0]     OtherName,
    rfc822Name                      [1]     IA5String,
    dNSName                         [2]     IA5String,
    x400Address                     [3]     ORAddress,
    directoryName                   [4]     Name,
    ediPartyName                    [5]     EDIPartyName,
    uniformResourceIdentifier       [6]     IA5String,
    iPAddress                       [7]     OCTET STRING,
    registeredID                    [8]     OBJECT IDENTIFIER }
</pre>
   *
   * @param id The type of name this is.
   * @param name The DER-encoded name.
   * @throws IOException If the name is not a valid DER sequence.
   */
  public void addSubjectAlternativeName(int id, byte[] name)
    throws IOException
  {
    GeneralName generalName = new GeneralName(GeneralName.Kind.forTag(id), name);
    if (altNames == null)
      altNames = new LinkedList<GeneralName>();
    altNames.add(generalName);
  }

  /**
   * Add a name to the subject alternative names criterion. This method will
   * only recognize certain types of name that have convenient string
   * encodings. For robustness, you should use the {@link
   *  #addSubjectAlternativeName(int, byte[])} method whenever possible.
   *
   * This method can only decode certain name kinds of names as strings.
   *
   * @param id The type of name this is. Must be in the range [0,8].
   * @param name The name.
   * @throws IOException If the id is out of range, or if the name
   *   is null.
   */
  public void addSubjectAlternativeName(int id, String name)
    throws IOException
  {
    GeneralName generalName = makeName(id, name);
    if (altNames == null)
      altNames = new LinkedList<GeneralName>();
    altNames.add(generalName);
  }

  public Object clone()
  {
    try
      {
        return super.clone();
      }
    catch (CloneNotSupportedException shouldNotHappen)
      {
        throw new Error(shouldNotHappen);
      }
  }

  /**
   * Returns the authority key identifier criterion, or <code>null</code> if
   * this value was not set. Note that the byte array is cloned to prevent
   * modification.
   *
   * @return The authority key identifier.
   */
  public byte[] getAuthorityKeyIdentifier()
  {
    if (authKeyId != null)
      return (byte[]) authKeyId.clone();
    else
      return null;
  }

  /**
   * Returns the basic constraints criterion, or -1 if this value is not set.
   *
   * @return The basic constraints.
   */
  public int getBasicConstraints()
  {
    return basicConstraints;
  }

  /**
   * Returns the certificate criterion, or <code>null</code> if this value
   * was not set.
   *
   * @return The certificate.
   */
  public X509Certificate getCertificate()
  {
    return cert;
  }

  /**
   * Returns the date at which certificates must be valid, or <code>null</code>
   * if this criterion was not set.
   *
   * @return The target certificate valitity date.
   */
  public Date getCertificateValid()
  {
    if (certValid != null)
      return (Date) certValid.clone();
    else
      return null;
  }

  /**
   * Returns the set of extended key purpose IDs, as an unmodifiable set
   * of OID strings. Returns <code>null</code> if this criterion is not
   * set.
   *
   * @return The set of key purpose OIDs (strings).
   */
  public Set<String> getExtendedKeyUsage()
  {
    if (keyPurposeSet != null)
      return Collections.unmodifiableSet(keyPurposeSet);
    else
      return null;
  }

  /**
   * Returns the issuer criterion as a sequence of DER bytes, or
   * <code>null</code> if this value was not set.
   *
   * @return The issuer.
   */
  public byte[] getIssuerAsBytes() throws IOException
  {
    if (issuer != null)
      return issuer.getEncoded();
    else
      return null;
  }

  /**
   * Returns the issuer criterion as a string, or <code>null</code> if this
   * value was not set.
   *
   * @return The issuer.
   */
  public String getIssuerAsString()
  {
    if (issuer != null)
      return issuer.getName();
    else
      return null;
  }

  /**
   * Returns the public key usage criterion, or <code>null</code> if this
   * value is not set. Note that the array is cloned to prevent modification.
   *
   * @return The public key usage.
   */
  public boolean[] getKeyUsage()
  {
    if (keyUsage != null)
      return (boolean[]) keyUsage.clone();
    else
      return null;
  }

  /**
   * Returns whether or not all specified alternative names must match.
   * If false, a certificate is considered a match if <em>one</em> of the
   * specified alternative names matches.
   *
   * @return true if all names must match.
   */
  public boolean getMatchAllSubjectAltNames()
  {
    return matchAllNames;
  }

  /**
   * Returns the name constraints criterion, or <code>null</code> if this
   * value is not set. Note that the byte array is cloned to prevent
   * modification.
   *
   * @return The name constraints.
   */
  public byte[] getNameConstraints()
  {
    if (nameConstraints != null)
      return (byte[]) nameConstraints.clone();
    else
      return null;
  }

  public Collection<List<?>> getPathToNames()
  {
    if (pathToNames != null)
      {
        List<List<?>> names = new ArrayList<List<?>>(pathToNames.size());
        for (GeneralName name : pathToNames)
          {
            List<Object> n = new ArrayList<Object>(2);
            n.add(name.kind().tag());
            n.add(name.name());
            names.add(n);
          }

        return names;
      }
    return null;
  }

  /**
   * Returns the certificate policy extension that will be matched by this
   * selector, or null if the certificate policy will not be matched.
   *
   * @return The policy to be matched, or null.
   */
  public Set<String> getPolicy()
  {
    Set<OID> p = this.policy;
    if (p != null)
      {
        Set<String> strings = new HashSet<String>(p.size());
        for (OID o : p)
          {
            strings.add(o.toString());
          }
        return strings;
      }
    return null;
  }

  /**
   * This method, and its related X.509 certificate extension &mdash; the
   * private key usage period &mdash; is not supported under the Internet
   * PKI for X.509 certificates (PKIX), described in RFC 3280. As such, this
   * method is not supported either.
   *
   * <p>Do not use this method. It is not deprecated, as it is not deprecated
   * in the Java standard, but it is basically a no-operation and simply
   * returns <code>null</code>.
   *
   * @return Null.
   */
  public Date getPrivateKeyValid()
  {
    return null;
  }

  /**
   * Returns the serial number criterion, or <code>null</code> if this
   * value was not set.
   *
   * @return The serial number.
   */
  public BigInteger getSerialNumber()
  {
    return serialNo;
  }

  /**
   * Get the subject alternative names criterion. The collection returned
   * is a collection of pairs: the first element is an {@link Integer}
   * containing the name type, and the second is a byte array containing
   * the DER-encoded name bytes.
   *
   * @return The subject alternative names criterion. Returns null if this
   *  criterion is not set.
   */
  public Collection<List<?>> getSubjectAlternativeNames()
  {
    if (altNames != null)
      {
        List<List<?>> names = new ArrayList<List<?>>(altNames.size());
        for (GeneralName name : altNames)
          {
            List<Object> n = new ArrayList<Object>(2);
            n.add(name.kind().tag());
            n.add(name.name());
            names.add(n);
          }
        return names;
      }
    return null;
  }

  /**
   * Returns the subject criterion as a sequence of DER bytes, or
   * <code>null</code> if this value is not set.
   *
   * @return The subject.
   */
  public byte[] getSubjectAsBytes() throws IOException
  {
    if (subject != null)
      return subject.getEncoded();
    else
      return null;
  }

  /**
   * Returns the subject criterion as a string, of <code>null</code> if
   * this value was not set.
   *
   * @return The subject.
   */
  public String getSubjectAsString()
  {
    if (subject != null)
      return subject.getName();
    else
      return null;
  }

  /**
   * Returns the subject key identifier criterion, or <code>null</code> if
   * this value was not set. Note that the byte array is cloned to prevent
   * modification.
   *
   * @return The subject key identifier.
   */
  public byte[] getSubjectKeyIdentifier()
  {
    if (subjectKeyId != null)
      return (byte[]) subjectKeyId.clone();
    else
      return null;
  }

  /**
   * Returns the subject public key criterion, or <code>null</code> if this
   * value is not set.
   *
   * @return The subject public key.
   */
  public PublicKey getSubjectPublicKey()
  {
    return subjectKey;
  }

  /**
   * Returns the public key algorithm ID that matching certificates must have,
   * or <code>null</code> if this criterion was not set.
   *
   * @return The public key algorithm ID.
   */
  public String getSubjectPublicKeyAlgID()
  {
    return String.valueOf(sigId);
  }

  /**
   * Match a certificate. This method will check the given certificate
   * against all the enabled criteria of this selector, and will return
   * <code>true</code> if the given certificate matches.
   *
   * @param certificate The certificate to check.
   * @return true if the certificate matches all criteria.
   */
  public boolean match(Certificate certificate)
  {
    if (!(certificate instanceof X509Certificate))
      return false;
    X509Certificate cert = (X509Certificate) certificate;
    if (this.cert != null)
      {
        try
          {
            byte[] e1 = this.cert.getEncoded();
            byte[] e2 = cert.getEncoded();
            if (!Arrays.equals(e1, e2))
              return false;
          }
        catch (CertificateEncodingException cee)
          {
            return false;
          }
      }
    if (serialNo != null)
      {
        if (!serialNo.equals(cert.getSerialNumber()))
          return false;
      }
    if (certValid != null)
      {
        try
          {
            cert.checkValidity(certValid);
          }
        catch (CertificateException ce)
          {
            return false;
          }
      }
    if (issuer != null)
      {
        if (!issuer.equals(cert.getIssuerX500Principal()))
          return false;
      }
    if (subject != null)
      {
        if (!subject.equals(cert.getSubjectX500Principal()))
          return false;
      }
    if (sigId != null)
      {
        if (!sigId.toString().equals(cert.getSigAlgOID()))
          return false;
      }
    if (subjectKeyId != null)
      {
        byte[] b = cert.getExtensionValue(SUBJECT_KEY_ID);
        if (!Arrays.equals(b, subjectKeyId))
          return false;
      }
    if (authKeyId != null)
      {
        byte[] b = cert.getExtensionValue(AUTH_KEY_ID);
        if (!Arrays.equals(b, authKeyId))
          return false;
      }
    if (keyUsage != null)
      {
        boolean[] b = cert.getKeyUsage();
        if (!Arrays.equals(b, keyUsage))
          return false;
      }
    if (basicConstraints >= 0)
      {
        if (cert.getBasicConstraints() != basicConstraints)
          return false;
      }
    if (keyPurposeSet != null)
      {
        List kp = null;
        try
          {
            kp = cert.getExtendedKeyUsage();
          }
        catch (CertificateParsingException cpe)
          {
            return false;
          }
        if (kp == null)
          return false;
        for (Iterator it = keyPurposeSet.iterator(); it.hasNext(); )
          {
            if (!kp.contains(it.next()))
              return false;
          }
      }
    if (altNames != null)
      {
        Collection<List<?>> an = null;
        try
          {
            an = cert.getSubjectAlternativeNames();
          }
        catch (CertificateParsingException cpe)
          {
            return false;
          }
        if (an == null)
          return false;
        int match = 0;
        for (GeneralName name : altNames)
          {
            for (List<?> list : an)
              {
                try
                  {
                    Integer id = (Integer) list.get(0);
                    Object val = list.get(1);
                    GeneralName n = null;
                    if (val instanceof String)
                      n = makeName(id, (String) val);
                    else if (val instanceof byte[])
                      {
                        n = new GeneralName(GeneralName.Kind.forTag(id),
                                            (byte[]) val);
                      }
                    else
                      continue;
                    if (name.equals(n))
                      match++;
                  }
                catch (Exception e)
                  {
                    continue;
                  }
              }
            if (match == 0 || (matchAllNames && match < altNames.size()))
              return false;
          }
      }
    if (nameConstraints != null)
      {
        byte[] nc = cert.getExtensionValue(NAME_CONSTRAINTS_ID);
        if (!Arrays.equals(nameConstraints, nc))
          return false;
      }

    if (policy != null)
      {
        CertificatePolicies policies = null;
        if (cert instanceof GnuPKIExtension)
          {
            policies = (CertificatePolicies)
              ((GnuPKIExtension) cert).getExtension(CertificatePolicies.ID).getValue();
          }
        else
          {
            byte[] policiesDer =
              cert.getExtensionValue(CertificatePolicies.ID.toString());
            try
              {
                policies = new CertificatePolicies(policiesDer);
              }
            catch (IOException ioe)
              {
                // ignored
              }
          }

        if (policies == null)
          return false;
        if (!policies.getPolicies().containsAll(policy))
          return false;
      }

    if (pathToNames != null)
      {
        NameConstraints nc = null;
        if (cert instanceof GnuPKIExtension)
          {
            Extension e =
              ((GnuPKIExtension) cert).getExtension(NameConstraints.ID);
            if (e != null)
              nc = (NameConstraints) e.getValue();
          }
        else
          {
            byte[] b = cert.getExtensionValue(NameConstraints.ID.toString());
            if (b != null)
              {
                try
                  {
                    nc = new NameConstraints(b);
                  }
                catch (IOException ioe)
                  {
                  }
              }
          }

        if (nc == null)
          return false;

        int match = 0;
        for (GeneralName name : pathToNames)
          {
            for (GeneralSubtree subtree : nc.permittedSubtrees())
              {
                if (name.equals(subtree.base()))
                  match++;
              }
          }
        if (match == 0 || (matchAllNames && match < pathToNames.size()))
          return false;
      }

    return true;
  }

  /**
   * Sets the authority key identifier criterion, or <code>null</code> to clear
   * this criterion. Note that the byte array is cloned to prevent modification.
   *
   * @param authKeyId The authority key identifier.
   */
  public void setAuthorityKeyIdentifier(byte[] authKeyId)
  {
    this.authKeyId = authKeyId != null ? (byte[]) authKeyId.clone() : null;
  }

  /**
   * Sets the basic constraints criterion. Specify -1 to clear this parameter.
   *
   * @param basicConstraints The new basic constraints value.
   */
  public void setBasicConstraints(int basicConstraints)
  {
    if (basicConstraints < -1)
      basicConstraints = -1;
    this.basicConstraints = basicConstraints;
  }

  /**
   * Sets the certificate criterion. If set, only certificates that are
   * equal to the certificate passed here will be accepted.
   *
   * @param cert The certificate.
   */
  public void setCertificate(X509Certificate cert)
  {
    this.cert = cert;
  }

  /**
   * Sets the date at which certificates must be valid. Specify
   * <code>null</code> to clear this criterion.
   *
   * @param certValid The certificate validity date.
   */
  public void setCertificateValid(Date certValid)
  {
    this.certValid = certValid != null ? (Date) certValid.clone() : null;
  }

  /**
   * Sets the extended key usage criterion, as a set of OID strings. Specify
   * <code>null</code> to clear this value.
   *
   * @param keyPurposeSet The set of key purpose OIDs.
   * @throws IOException If any element of the set is not a valid OID string.
   */
  public void setExtendedKeyUsage(Set<String> keyPurposeSet) throws IOException
  {
    if (keyPurposeSet == null)
      {
        this.keyPurposeSet = null;
        return;
      }
    Set<String> s = new HashSet<String>();
    for (Iterator it = keyPurposeSet.iterator(); it.hasNext(); )
      {
        Object o = it.next();
        if (!(o instanceof String))
          throw new IOException("not a string: " + o);
        try
          {
            OID oid = new OID((String) o);
            int[] comp = oid.getIDs();
            if (!checkOid(comp))
              throw new IOException("malformed OID: " + o);
          }
        catch (IllegalArgumentException iae)
          {
            IOException ioe = new IOException("malformed OID: " + o);
            ioe.initCause(iae);
            throw ioe;
          }
      }
    this.keyPurposeSet = s;
  }

  /**
   * Sets the issuer, specified as the DER encoding of the issuer's
   * distinguished name. Only certificates issued by this issuer will
   * be accepted.
   *
   * @param name The DER encoding of the issuer's distinguished name.
   * @throws IOException If the given name is incorrectly formatted.
   */
  public void setIssuer(byte[] name) throws IOException
  {
    if (name != null)
      {
        try
          {
            issuer = new X500Principal(name);
          }
        catch (IllegalArgumentException iae)
          {
            throw new IOException(iae.getMessage());
          }
      }
    else
      issuer = null;
  }

  /**
   * Sets the issuer, specified as a string representation of the issuer's
   * distinguished name. Only certificates issued by this issuer will
   * be accepted.
   *
   * @param name The string representation of the issuer's distinguished name.
   * @throws IOException If the given name is incorrectly formatted.
   */
  public void setIssuer(String name) throws IOException
  {
    if (name != null)
      {
        try
          {
            issuer = new X500Principal(name);
          }
        catch (IllegalArgumentException iae)
          {
            throw new IOException(iae.getMessage());
          }
      }
    else
      issuer = null;
  }

  /**
   * Sets the public key usage criterion. Specify <code>null</code> to clear
   * this value.
   *
   * @param keyUsage The public key usage.
   */
  public void setKeyUsage(boolean[] keyUsage)
  {
    this.keyUsage = keyUsage != null ? (boolean[]) keyUsage.clone() : null;
  }

  /**
   * Sets whether or not all subject alternative names must be matched.
   * If false, then a certificate will be considered a match if one
   * alternative name matches.
   *
   * @param matchAllNames Whether or not all alternative names must be
   *        matched.
   */
  public void setMatchAllSubjectAltNames(boolean matchAllNames)
  {
    this.matchAllNames = matchAllNames;
  }

  /**
   * Sets the name constraints criterion; specify <code>null</code> to
   * clear this criterion. Note that if non-null, the argument will be
   * cloned to prevent modification.
   *
   * @param nameConstraints The new name constraints.
   * @throws IOException If the argument is not a valid DER-encoded
   *         name constraints.
   */
  public void setNameConstraints(byte[] nameConstraints)
    throws IOException
  {
    // Check if the input is well-formed...
    new NameConstraints(nameConstraints);

    // But we just compare raw byte arrays.
    this.nameConstraints = nameConstraints != null
      ? (byte[]) nameConstraints.clone() : null;
  }

  /**
   * Sets the pathToNames criterion. The argument is a collection of
   * pairs, the first element of which is an {@link Integer} giving
   * the ID of the name, and the second element is either a {@link String}
   * or a byte array.
   *
   * See {@link #addPathToName(int, byte[])} and {@link #addPathToName(int, String)}
   * for how these arguments are handled.
   *
   * @param names The names.
   * @throws IOException If any argument is malformed.
   */
  public void setPathToNames(Collection<List<?>> names) throws IOException
  {
    if (names == null || names.size() == 0)
      {
        pathToNames = null;
      }
    else
      {
        pathToNames = new ArrayList<GeneralName>(names.size());
        for (List<?> name : names)
          {
            Integer id = (Integer) name.get(0);
            Object name2 = name.get(1);
            if (name2 instanceof String)
              addPathToName(id, (String) name2);
            else if (name2 instanceof byte[])
              addPathToName(id, (byte[]) name2);
            else
              throw new IOException("invalid name type: "
                                    + name2.getClass().getName());
          }
      }
  }

  /**
   * Sets the certificate policy to match, or null if this criterion should
   * not be checked. Each element if the set must be a dotted-decimal form
   * of certificate policy object identifier.
   *
   * @param policy The policy to match.
   * @throws IOException If some element of the policy is not a valid
   *  policy extenison OID.
   */
  public void setPolicy(Set<String> policy) throws IOException
  {
    if (policy != null)
      {
        HashSet<OID> p = new HashSet<OID>(policy.size());
        for (String s : policy)
          {
            try
              {
                OID oid = new OID(s);
                int[] i = oid.getIDs();
                if (!checkOid(i))
                  throw new IOException("invalid OID");
                p.add(oid);
              }
            catch (IOException ioe)
              {
                throw ioe;
              }
            catch (Exception x)
              {
                IOException ioe = new IOException("invalid OID");
                ioe.initCause(x);
                throw ioe;
              }
          }
        this.policy = p;
      }
    else
      this.policy = null;
  }

  /**
   * This method, and its related X.509 certificate extension &mdash; the
   * private key usage period &mdash; is not supported under the Internet
   * PKI for X.509 certificates (PKIX), described in RFC 3280. As such, this
   * method is not supported either.
   *
   * <p>Do not use this method. It is not deprecated, as it is not deprecated
   * in the Java standard, but it is basically a no-operation.
   *
   * @param UNUSED Is silently ignored.
   */
  public void setPrivateKeyValid(Date UNUSED)
  {
  }

  /**
   * Sets the serial number of the desired certificate. Only certificates that
   * contain this serial number are accepted.
   *
   * @param serialNo The serial number.
   */
  public void setSerialNumber(BigInteger serialNo)
  {
    this.serialNo = serialNo;
  }

  /**
   * Sets the subject, specified as the DER encoding of the subject's
   * distinguished name. Only certificates with the given subject will
   * be accepted.
   *
   * @param name The DER encoding of the subject's distinguished name.
   * @throws IOException If the given name is incorrectly formatted.
   */
  public void setSubject(byte[] name) throws IOException
  {
    if (name != null)
      {
        try
          {
            subject = new X500Principal(name);
          }
        catch (IllegalArgumentException iae)
          {
            throw new IOException(iae.getMessage());
          }
      }
    else
      subject = null;
  }

  /**
   * Sets the subject, specified as a string representation of the
   * subject's distinguished name. Only certificates with the given
   * subject will be accepted.
   *
   * @param name The string representation of the subject's distinguished name.
   * @throws IOException If the given name is incorrectly formatted.
   */
  public void setSubject(String name) throws IOException
  {
    if (name != null)
      {
        try
          {
            subject = new X500Principal(name);
          }
        catch (IllegalArgumentException iae)
          {
            throw new IOException(iae.getMessage());
          }
      }
    else
      subject = null;
  }

  /**
   * Sets the subject alternative names critertion. Each element of the
   * argument must be a {@link java.util.List} that contains exactly two
   * elements: the first an {@link Integer}, representing the type of
   * name, and the second either a {@link String} or a byte array,
   * representing the name itself.
   *
   * @param altNames The alternative names.
   * @throws IOException If any element of the argument is invalid.
   */
  public void setSubjectAlternativeNames(Collection<List<?>> altNames)
    throws IOException
  {
    if (altNames == null || altNames.isEmpty())
      {
        this.altNames = null;
        return;
      }
    List<GeneralName> l = new ArrayList<GeneralName>(altNames.size());
    for (List<?> list : altNames)
      {
        Integer id = (Integer) list.get(0);
        Object value = list.get(1);
        GeneralName name = null;
        if (value instanceof String)
          name = makeName(id, (String) value);
        else if (value instanceof byte[])
          name = new GeneralName(GeneralName.Kind.forTag(id), (byte[]) value);
        else
          throw new IOException("invalid name type: " + value.getClass().getName());
        l.add(name);
      }
    this.altNames = l;
  }

  /**
   * Sets the subject key identifier criterion, or <code>null</code> to clear
   * this criterion. Note that the byte array is cloned to prevent modification.
   *
   * @param subjectKeyId The subject key identifier.
   */
  public void setSubjectKeyIdentifier(byte[] subjectKeyId)
  {
    this.subjectKeyId = subjectKeyId != null ? (byte[]) subjectKeyId.clone() :
      null;
  }

  /**
   * Sets the subject public key criterion as a DER-encoded key. Specify
   * <code>null</code> to clear this value.
   *
   * @param key The DER-encoded key bytes.
   * @throws IOException If the argument is not a valid DER-encoded key.
   */
  public void setSubjectPublicKey(byte[] key) throws IOException
  {
    if (key == null)
      {
        subjectKey = null;
        subjectKeySpec = null;
        return;
      }
    try
      {
        subjectKeySpec = new X509EncodedKeySpec(key);
        KeyFactory enc = KeyFactory.getInstance("X.509");
        subjectKey = enc.generatePublic(subjectKeySpec);
      }
    catch (Exception x)
      {
        subjectKey = null;
        subjectKeySpec = null;
        IOException ioe = new IOException(x.getMessage());
        ioe.initCause(x);
        throw ioe;
      }
  }

  /**
   * Sets the subject public key criterion as an opaque representation.
   * Specify <code>null</code> to clear this criterion.
   *
   * @param key The public key.
   */
  public void setSubjectPublicKey(PublicKey key)
  {
    this.subjectKey = key;
    if (key == null)
      {
        subjectKeySpec = null;
        return;
      }
    try
      {
        KeyFactory enc = KeyFactory.getInstance("X.509");
        subjectKeySpec = (X509EncodedKeySpec)
          enc.getKeySpec(key, X509EncodedKeySpec.class);
      }
    catch (Exception x)
      {
        subjectKey = null;
        subjectKeySpec = null;
      }
  }

  /**
   * Sets the public key algorithm ID that matching certificates must have.
   * Specify <code>null</code> to clear this criterion.
   *
   * @param sigId The public key ID.
   * @throws IOException If the specified ID is not a valid object identifier.
   */
  public void setSubjectPublicKeyAlgID(String sigId) throws IOException
  {
    if (sigId != null)
      {
        try
          {
            OID oid = new OID(sigId);
            int[] comp = oid.getIDs();
            if (!checkOid(comp))
              throw new IOException("malformed OID: " + sigId);
            this.sigId = oid;
          }
        catch (IllegalArgumentException iae)
          {
            IOException ioe = new IOException("malformed OID: " + sigId);
            ioe.initCause(iae);
            throw ioe;
          }
      }
    else
      this.sigId = null;
  }

  public String toString()
  {
    CPStringBuilder str = new CPStringBuilder(X509CertSelector.class.getName());
    String nl = SystemProperties.getProperty("line.separator");
    String eol = ";" + nl;
    str.append(" {").append(nl);
    if (cert != null)
      str.append("  certificate = ").append(cert).append(eol);
    if (basicConstraints >= 0)
      str.append("  basic constraints = ").append(basicConstraints).append(eol);
    if (serialNo != null)
      str.append("  serial number = ").append(serialNo).append(eol);
    if (certValid != null)
      str.append("  valid date = ").append(certValid).append(eol);
    if (issuer != null)
      str.append("  issuer = ").append(issuer).append(eol);
    if (subject != null)
      str.append("  subject = ").append(subject).append(eol);
    if (sigId != null)
      str.append("  signature OID = ").append(sigId).append(eol);
    if (subjectKey != null)
      str.append("  subject public key = ").append(subjectKey).append(eol);
    if (subjectKeyId != null)
      {
        str.append("  subject key ID = ");
        for (int i = 0; i < subjectKeyId.length; i++)
          {
            str.append(Character.forDigit((subjectKeyId[i] & 0xF0) >>> 8, 16));
            str.append(Character.forDigit((subjectKeyId[i] & 0x0F), 16));
            if (i < subjectKeyId.length - 1)
              str.append(':');
          }
        str.append(eol);
      }
    if (authKeyId != null)
      {
        str.append("  authority key ID = ");
        for (int i = 0; i < authKeyId.length; i++)
          {
            str.append(Character.forDigit((authKeyId[i] & 0xF0) >>> 8, 16));
            str.append(Character.forDigit((authKeyId[i] & 0x0F), 16));
            if (i < authKeyId.length - 1)
              str.append(':');
          }
        str.append(eol);
      }
    if (keyUsage != null)
      {
        str.append("  key usage = ");
        for (int i = 0; i < keyUsage.length; i++)
          str.append(keyUsage[i] ? '1' : '0');
        str.append(eol);
      }
    if (keyPurposeSet != null)
      str.append("  key purpose = ").append(keyPurposeSet).append(eol);
    if (altNames != null)
      str.append("  alternative names = ").append(altNames).append(eol);
    if (nameConstraints != null)
      str.append("  name constraints = <blob of data>").append(eol);
    if (policy != null)
      str.append("  policy = ").append(policy).append(eol);
    if (pathToNames != null)
      str.append("  pathToNames = ").append(pathToNames).append(eol);
    str.append("}").append(nl);
    return str.toString();
  }
}
