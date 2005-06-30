/* PKIXParameters.java -- parameters for the PKIX cert path algorithm
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

import java.security.InvalidAlgorithmParameterException;
import java.security.KeyStore;
import java.security.KeyStoreException;

import java.util.Collections;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

/**
 * Parameters for verifying certificate paths using the PKIX
 * (Public-Key Infrastructure (X.509)) algorithm.
 *
 * @see CertPathBulider
 */
public class PKIXParameters implements CertPathParameters
{

  // Fields.
  // ------------------------------------------------------------------------

  /** The trusted certificates. */
  private final Set trustAnchors;

  /** The set of initial policy identifiers. */
  private final Set initPolicies;

  /** The list of certificate stores. */
  private final List certStores;

  /** The list of path checkers. */
  private final List pathCheckers;

  /** The revocation enabled flag. */
  private boolean revocationEnabled;

  /** The explicit policy required flag. */
  private boolean exPolicyRequired;

  /** The policy mapping inhibited flag. */
  private boolean policyMappingInhibited;

  /** The any policy inhibited flag. */
  private boolean anyPolicyInhibited;

  /** The policy qualifiers rejected flag. */
  private boolean policyQualRejected;

  /** The target validation date. */
  private Date date;

  /** The signature algorithm provider. */
  private String sigProvider;

  /** The target constraints. */
  private CertSelector targetConstraints;

  // Constructors.
  // ------------------------------------------------------------------------

  /**
   * Create a new PKIXParameters object, populating the trusted
   * certificates set with all certificates found in the given key
   * store. All certificates found in the key store are assumed to be
   * trusted by this constructor.
   *
   * @param keystore The key store.
   * @throws KeyStoreException If the certificates cannot be retrieved
   *         from the key store.
   * @throws InvalidAlgorithmParameterException If there are no
   *         certificates in the key store.
   * @throws NullPointerException If <i>keystore</i> is null.
   */
  public PKIXParameters(KeyStore keystore)
    throws KeyStoreException, InvalidAlgorithmParameterException
  {
    this();
    for (Enumeration e = keystore.aliases(); e.hasMoreElements(); )
      {
        String alias = (String) e.nextElement();
        if (!keystore.isCertificateEntry(alias))
          continue;
        Certificate cert = keystore.getCertificate(alias);
        if (cert instanceof X509Certificate)
          trustAnchors.add(new TrustAnchor((X509Certificate) cert, null));
      }
    if (trustAnchors.isEmpty())
      throw new InvalidAlgorithmParameterException("no certs in the key store");
  }

  /**
   * Create a new PKIXParameters object, populating the trusted
   * certificates set with the elements of the given set, each of which
   * must be a {@link TrustAnchor}.
   *
   * @param trustAnchors The set of trust anchors.
   * @throws InvalidAlgorithmParameterException If there are no
   *         certificates in the set.
   * @throws NullPointerException If <i>trustAnchors</i> is null.
   * @throws ClassCastException If every element in <i>trustAnchors</i>
   *         is not a {@link TrustAnchor}.
   */
  public PKIXParameters(Set trustAnchors)
    throws InvalidAlgorithmParameterException
  {
    this();
    setTrustAnchors(trustAnchors);
  }

  /**
   * Default constructor.
   */
  private PKIXParameters()
  {
    trustAnchors = new HashSet();
    initPolicies = new HashSet();
    certStores = new LinkedList();
    pathCheckers = new LinkedList();
    revocationEnabled = true;
    exPolicyRequired = false;
    policyMappingInhibited = false;
    anyPolicyInhibited = false;
    policyQualRejected = true;
  }

  /**
   * Copying constructor for cloning.
   *
   * @param that The instance being cloned.
   */
  private PKIXParameters(PKIXParameters that)
  {
    this();
    this.trustAnchors.addAll(that.trustAnchors);
    this.initPolicies.addAll(that.initPolicies);
    this.certStores.addAll(that.certStores);
    this.pathCheckers.addAll(that.pathCheckers);
    this.revocationEnabled = that.revocationEnabled;
    this.exPolicyRequired = that.exPolicyRequired;
    this.policyMappingInhibited = that.policyMappingInhibited;
    this.anyPolicyInhibited = that.anyPolicyInhibited;
    this.policyQualRejected = that.policyQualRejected;
    this.date = that.date;
    this.sigProvider = that.sigProvider;
    this.targetConstraints = that.targetConstraints != null
      ? (CertSelector) that.targetConstraints.clone() : null;
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  /**
   * Returns an immutable set of trust anchors. The set returned will
   * never be null and will never be empty.
   *
   * @return A (never null, never empty) immutable set of trust anchors.
   */
  public Set getTrustAnchors()
  {
    return Collections.unmodifiableSet(trustAnchors);
  }

  /**
   * Sets the trust anchors of this class, replacing the current trust
   * anchors with those in the given set. The supplied set is copied to
   * prevent modification.
   *
   * @param trustAnchors The new set of trust anchors.
   * @throws InvalidAlgorithmParameterException If there are no
   *         certificates in the set.
   * @throws NullPointerException If <i>trustAnchors</i> is null.
   * @throws ClassCastException If every element in <i>trustAnchors</i>
   *         is not a {@link TrustAnchor}.
   */
  public void setTrustAnchors(Set trustAnchors)
    throws InvalidAlgorithmParameterException
  {
    if (trustAnchors.isEmpty())
      throw new InvalidAlgorithmParameterException("no trust anchors");
    this.trustAnchors.clear();
    for (Iterator i = trustAnchors.iterator(); i.hasNext(); )
      {
        this.trustAnchors.add((TrustAnchor) i.next());
      }
  }

  /**
   * Returns the set of initial policy identifiers (as OID strings). If
   * any policy is accepted, this method returns the empty set.
   *
   * @return An immutable set of initial policy OID strings, or the
   *         empty set if any policy is acceptable.
   */
  public Set getInitialPolicies()
  {
    return Collections.unmodifiableSet(initPolicies);
  }

  /**
   * Sets the initial policy identifiers (as OID strings). If the
   * argument is null or the empty set, then any policy identifier will
   * be accepted.
   *
   * @param initPolicies The new set of policy strings, or null.
   * @throws ClassCastException If any element in <i>initPolicies</i> is
   *         not a string.
   */
  public void setInitialPolicies(Set initPolicies)
  {
    this.initPolicies.clear();
    if (initPolicies == null)
      return;
    for (Iterator i = initPolicies.iterator(); i.hasNext(); )
      {
        this.initPolicies.add((String) i.next());
      }
  }

  /**
   * Add a {@link CertStore} to the list of cert stores.
   *
   * @param store The CertStore to add.
   */
  public void addCertStore(CertStore store)
  {
    if (store != null)
      certStores.add(store);
  }

  /**
   * Returns an immutable list of cert stores. This method never returns
   * null.
   *
   * @return The list of cert stores.
   */
  public List getCertStores()
  {
    return Collections.unmodifiableList(certStores);
  }

  /**
   * Set the cert stores. If the argument is null the list of cert
   * stores will be empty.
   *
   * @param certStores The cert stores.
   */
  public void setCertStores(List certStores)
  {
    this.certStores.clear();
    if (certStores == null)
      return;
    for (Iterator i = certStores.iterator(); i.hasNext(); )
      {
        this.certStores.add((CertStore) i.next());
      }
  }

  /**
   * Returns the value of the <i>revocation enabled</i> flag. The default
   * value for this flag is <code>true</code>.
   *
   * @return The <i>revocation enabled</i> flag.
   */
  public boolean isRevocationEnabled()
  {
    return revocationEnabled;
  }

  /**
   * Sets the value of the <i>revocation enabled</i> flag.
   *
   * @param value The new value.
   */
  public void setRevocationEnabled(boolean value)
  {
    revocationEnabled = value;
  }

  /**
   * Returns the value of the <i>explicit policy required</i> flag. The
   * default value of this flag is <code>false</code>.
   *
   * @return The <i>explicit policy required</i> flag.
   */
  public boolean isExplicitPolicyRequired()
  {
    return exPolicyRequired;
  }

  /**
   * Sets the value of the <i>explicit policy required</i> flag.
   *
   * @param value The new value.
   */
  public void setExplicitPolicyRequired(boolean value)
  {
    exPolicyRequired = value;
  }

  /**
   * Returns the value of the <i>policy mapping inhibited</i> flag. The
   * default value of this flag is <code>false</code>.
   *
   * @return The <i>policy mapping inhibited</i> flag.
   */
  public boolean isPolicyMappingInhibited()
  {
    return policyMappingInhibited;
  }

  /**
   * Sets the value of the <i>policy mapping inhibited</i> flag.
   *
   * @param value The new value.
   */
  public void setPolicyMappingInhibited(boolean value)
  {
    policyMappingInhibited = value;
  }

  /**
   * Returns the value of the <i>any policy inhibited</i> flag. The
   * default value of this flag is <code>false</code>.
   *
   * @return The <i>any policy inhibited</i> flag.
   */
  public boolean isAnyPolicyInhibited()
  {
    return anyPolicyInhibited;
  }

  /**
   * Sets the value of the <i>any policy inhibited</i> flag.
   *
   * @param value The new value.
   */
  public void setAnyPolicyInhibited(boolean value)
  {
    anyPolicyInhibited = value;
  }

  /**
   * Returns the value of the <i>policy qualifiers enabled</i> flag. The
   * default value of this flag is <code>true</code>.
   *
   * @return The <i>policy qualifiers enabled</i> flag.
   */
  public boolean getPolicyQualifiersRejected()
  {
    return policyQualRejected;
  }

  /**
   * Sets the value of the <i>policy qualifiers enabled</i> flag.
   *
   * @param value The new value.
   */
  public void setPolicyQualifiersRejected(boolean value)
  {
    policyQualRejected = value;
  }

  /**
   * Returns the date for which the certificate path should be
   * validated, or null if the current time should be used. The date
   * object is copied to prevent subsequent modification.
   *
   * @return The date, or null if not set.
   */
  public Date getDate()
  {
    return date != null ? (Date) date.clone() : null;
  }

  /**
   * Sets the date for which the certificate path should be validated,
   * or null if the current time should be used.
   *
   * @param date The new date, or null.
   */
  public void setDate(Date date)
  {
    if (date != null)
      this.date = (Date) date.clone();
    else
      this.date = null;
  }

  /**
   * Add a certificate path checker.
   *
   * @param checker The certificate path checker to add.
   */
  public void addCertPathChecker(PKIXCertPathChecker checker)
  {
    if (checker != null)
      pathCheckers.add(checker);
  }

  /**
   * Returns an immutable list of all certificate path checkers.
   *
   * @return An immutable list of all certificate path checkers.
   */
  public List getCertPathCheckers()
  {
    return Collections.unmodifiableList(pathCheckers);
  }

  /**
   * Sets the certificate path checkers. If the argument is null, the
   * list of checkers will merely be cleared.
   *
   * @param pathCheckers The new list of certificate path checkers.
   * @throws ClassCastException If any element of <i>pathCheckers</i> is
   *         not a {@link PKIXCertPathChecker}.
   */
  public void setCertPathCheckers(List pathCheckers)
  {
    this.pathCheckers.clear();
    if (pathCheckers == null)
      return;
    for (Iterator i = pathCheckers.iterator(); i.hasNext(); )
      {
        this.pathCheckers.add((PKIXCertPathChecker) i.next());
      }
  }

  /**
   * Returns the signature algorithm provider, or null if not set.
   *
   * @return The signature algorithm provider, or null if not set.
   */
  public String getSigProvider()
  {
    return sigProvider;
  }

  /**
   * Sets the signature algorithm provider, or null if there is no
   * preferred provider.
   *
   * @param sigProvider The signature provider name.
   */
  public void setSigProvider(String sigProvider)
  {
    this.sigProvider = sigProvider;
  }

  /**
   * Returns the constraints placed on the target certificate, or null
   * if there are none. The target constraints are copied to prevent
   * subsequent modification.
   *
   * @return The target constraints, or null.
   */
  public CertSelector getTargetCertConstraints()
  {
    return targetConstraints != null
      ? (CertSelector) targetConstraints.clone() : null;
  }

  /**
   * Sets the constraints placed on the target certificate.
   *
   * @param targetConstraints The target constraints.
   */
  public void setTargetCertConstraints(CertSelector targetConstraints)
  {
    this.targetConstraints = targetConstraints != null
      ? (CertSelector) targetConstraints.clone() : null;
  }

  /**
   * Returns a copy of these parameters.
   *
   * @return The copy.
   */
  public Object clone()
  {
    return new PKIXParameters(this);
  }

  /**
   * Returns a printable representation of these parameters.
   *
   * @return A printable representation of these parameters.
   */
  public String toString() {
    return "[ Trust Anchors: " + trustAnchors + "; Initial Policy OIDs="
      + (initPolicies != null ? initPolicies.toString() : "any")
      + "; Validity Date=" + date + "; Signature Provider="
      + sigProvider + "; Default Revocation Enabled=" + revocationEnabled
      + "; Explicit Policy Required=" + exPolicyRequired
      + "; Policy Mapping Inhibited=" + policyMappingInhibited
      + "; Any Policy Inhibited=" + anyPolicyInhibited
      + "; Policy Qualifiers Rejected=" + policyQualRejected
      + "; Target Cert Contstraints=" + targetConstraints
      + "; Certification Path Checkers=" + pathCheckers
      + "; CertStores=" + certStores + " ]";
  }
}
