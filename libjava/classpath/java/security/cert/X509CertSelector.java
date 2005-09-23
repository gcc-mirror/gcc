/* X509CertSelector.java -- selects X.509 certificates by criteria.
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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
import gnu.java.security.OID;

import java.io.IOException;
import java.math.BigInteger;
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
 */
public class X509CertSelector implements CertSelector, Cloneable
{

  // Constants and fields.
  // -------------------------------------------------------------------------

  private static final String AUTH_KEY_ID = "2.5.29.35";
  private static final String SUBJECT_KEY_ID = "2.5.29.14";
  private static final String NAME_CONSTRAINTS_ID = "2.5.29.30";

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
  private Set keyPurposeSet;
  private List altNames;
  private boolean matchAllNames;
  private byte[] nameConstraints;
  private Set policy;

  // Constructors.
  // ------------------------------------------------------------------------

  /**
   * Creates a new X.509 certificate selector. The new selector will be
   * empty, and will accept any certificate (provided that it is an
   * {@link X509Certificate}).
   */
  public X509CertSelector()
  {
    basicConstraints = -1;
  }

  // Instance methods.
  // ------------------------------------------------------------------------

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
   * Sets the authority key identifier criterion, or <code>null</code> to clear
   * this criterion. Note that the byte array is cloned to prevent modification.
   *
   * @param subjectKeyId The subject key identifier.
   */
  public void setAuthorityKeyIdentifier(byte[] authKeyId)
  {
    this.authKeyId = authKeyId != null ? (byte[]) authKeyId.clone() : null;
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
   * Returns the set of extended key purpose IDs, as an unmodifiable set
   * of OID strings. Returns <code>null</code> if this criterion is not
   * set.
   *
   * @return The set of key purpose OIDs (strings).
   */
  public Set getExtendedKeyUsage()
  {
    if (keyPurposeSet != null)
      return Collections.unmodifiableSet(keyPurposeSet);
    else
      return null;
  }

  /**
   * Sets the extended key usage criterion, as a set of OID strings. Specify
   * <code>null</code> to clear this value.
   *
   * @param keyPurposeSet The set of key purpose OIDs.
   * @throws IOException If any element of the set is not a valid OID string.
   */
  public void setExtendedKeyUsage(Set keyPurposeSet) throws IOException
  {
    if (keyPurposeSet == null)
      {
        this.keyPurposeSet = null;
        return;
      }
    Set s = new HashSet();
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
   * Sets the subject alternative names critertion. Each element of the
   * argument must be a {@link java.util.List} that contains exactly two
   * elements: the first an {@link Integer}, representing the type of
   * name, and the second either a {@link String} or a byte array,
   * representing the name itself.
   *
   * @param altNames The alternative names.
   * @throws IOException If any element of the argument is invalid.
   */
  public void setSubjectAlternativeNames(Collection altNames)
    throws IOException
  {
    if (altNames == null)
      {
        this.altNames = null;
        return;
      }
    List l = new ArrayList(altNames.size());
    for (Iterator it = altNames.iterator(); it.hasNext(); )
      {
        Object o = it.next();
        if (!(o instanceof List) || ((List) o).size() != 2 ||
            !(((List) o).get(0) instanceof Integer) ||
            !(((List) o).get(1) instanceof String) ||
            !(((List) o).get(1) instanceof byte[]))
          throw new IOException("illegal alternative name: " + o);
        Integer i = (Integer) ((List) o).get(0);
        if (i.intValue() < 0 || i.intValue() > 8)
          throw new IOException("illegal alternative name: " + o +
                                ", bad id: " + i);
        l.add(new ArrayList((List) o));
      }
    this.altNames = l;
  }

  /**
   * Add a name to the subject alternative names criterion.
   *
   * @param id The type of name this is. Must be in the range [0,8].
   * @param name The name.
   * @throws IOException If the id is out of range, or if the name
   *   is null.
   */
  public void addSubjectAlternativeName(int id, String name)
    throws IOException
  {
    if (id < 0 || id > 8 || name == null)
      throw new IOException("illegal alternative name");
    if (altNames == null)
      altNames = new LinkedList();
    ArrayList l = new ArrayList(2);
    l.add(new Integer(id));
    l.add(name);
    altNames.add(l);
  }

  /**
   * Add a name, as DER-encoded bytes, to the subject alternative names
   * criterion.
   *
   * @param id The type of name this is.
   */
  public void addSubjectAlternativeName(int id, byte[] name)
    throws IOException
  {
    if (id < 0 || id > 8 || name == null)
      throw new IOException("illegal alternative name");
    if (altNames == null)
      altNames = new LinkedList();
    ArrayList l = new ArrayList(2);
    l.add(new Integer(id));
    l.add(name);
    altNames.add(l);
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
    // FIXME check if the argument is valid.
    this.nameConstraints = nameConstraints != null
      ? (byte[]) nameConstraints.clone() : null;
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

  // The last two criteria not yet implemented are certificate policies
  // and path-to-names. Both of these are somewhat advanced extensions
  // (you could probably count the applications that actually use them
  //  on one hand), and they both have no support in the X509Certificate
  // class.
  //
  // Not having support in X509Certificate is not always a problem; for
  // example, we can compare DER-encoded values as byte arrays for some
  // extensions. We can't, however, compare them if they are specified
  // in a set (as policies are). We need to parse the actual value in the
  // certificate, and check it against the specified set.

  // FIXME
//   public void setPolicy(Set policy) throws IOException
//   {
//     if (policy != null)
//       {
//         for (Iterator it = policy.iterator(); it.hasNext(); )
//           try
//             {
//               OID oid = new OID((String) it.next());
//               int[] i = oid.getIDs();
//               if (!checkOid(i))
//                 throw new IOException("invalid OID");
//             }
//           catch (Exception x)
//             {
//               throw new IOException("invalid OID");
//             }
//       }
//     this.policy = policy != null ? new HashSet(policy) : null;
//   }

  // FIXME
//   public void setPathToNames(Collection names) throws IOException
//   {
//     if (names == null)
//       {
//         this.names = null;
//         return;
//       }
//     for (Iterator it = names.iterator(); it.hasNext(); )
//       {
//         try
//           {
//             List l = (List) it.next();
//             if (l.get(1) instanceof String)
//               addPathToName(((Integer)l.get(0)).intValue(), (String)l.get(1));
//             else
//               addPathToName(((Integer)l.get(0)).intValue(), (byte[])l.get(1));
//           }
//         catch (Exception x)
//           {
//            this.names = null;
//             throw new IOException("invalid names");
//           }
//       }
//   }

  // FIXME
//   public void addPathToName(int id, String name) throws IOException
//   {
//   }

  // FIXME
//   public void addPathToName(int id, byte[] name) throws IOException
//   {
//   }

  // FIXME
//   public Collection getSubjectAlternativeNames()
//   {
//     return null;
//   }

  // FIXME
//   public Set getPolicy()
//   {
//     return null;
//   }

  // FIXME
//   public Collection getPathToNames()
//   {
//     return null;
//   }

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
        Collection an = null;
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
        for (Iterator it = altNames.iterator(); it.hasNext(); )
          {
            List l = (List) it.next();
            Integer id = (Integer) l.get(0);
            String s = null;
            byte[] b = null;
            if (l.get(1) instanceof String)
              s = (String) l.get(1);
            else if (l.get(1) instanceof byte[])
              b = (byte[]) l.get(1);
            else
              return false;
            for (Iterator it2 = an.iterator(); it2.hasNext(); )
              {
                Object o = it2.next();
                if (!(o instanceof List))
                  continue;
                List l2 = (List) o;
                if (l2.size() != 2)
                  continue;
                if (!id.equals(l2.get(0)))
                  continue;
                if (s != null && (l2.get(1) instanceof String) &&
                    s.equals(l2.get(1)))
                  match++;
                else if (b != null && (l2.get(1) instanceof byte[]) &&
                         Arrays.equals(b, (byte[]) l2.get(1)))
                  match++;
              }
            if (match == 0 || (matchAllNames && match != altNames.size()))
              return false;
          }
      }
    if (nameConstraints != null)
      {
        byte[] nc = cert.getExtensionValue(NAME_CONSTRAINTS_ID);
        if (!Arrays.equals(nameConstraints, nc))
          return false;
      }

    // FIXME check policies.
    // FIXME check path-to-names.

    return true;
  }

  public String toString()
  {
    StringBuffer str = new StringBuffer(X509CertSelector.class.getName());
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
    str.append("}").append(nl);
    return str.toString();
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

  // Own methods.
  // -------------------------------------------------------------------------

  private static boolean checkOid(int[] oid)
  {
    return (oid != null && oid.length > 2 &&
            (oid[0] >= 0 && oid[0] <= 2) && (oid[1] >= 0 && oid[1] <= 39));
  }
}
