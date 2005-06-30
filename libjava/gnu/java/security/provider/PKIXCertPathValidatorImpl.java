/* PKIXCertPathValidatorImpl.java -- PKIX certificate path validator.
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


package gnu.java.security.provider;

import gnu.java.security.OID;
import gnu.java.security.x509.GnuPKIExtension;
import gnu.java.security.x509.PolicyNodeImpl;
import gnu.java.security.x509.X509CRLSelectorImpl;
import gnu.java.security.x509.X509CertSelectorImpl;
import gnu.java.security.x509.ext.BasicConstraints;
import gnu.java.security.x509.ext.CertificatePolicies;
import gnu.java.security.x509.ext.Extension;
import gnu.java.security.x509.ext.KeyUsage;
import gnu.java.security.x509.ext.PolicyConstraint;

import java.io.IOException;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.PublicKey;
import java.security.cert.CRL;
import java.security.cert.CertPath;
import java.security.cert.CertPathParameters;
import java.security.cert.CertPathValidatorException;
import java.security.cert.CertPathValidatorResult;
import java.security.cert.CertPathValidatorSpi;
import java.security.cert.CertStore;
import java.security.cert.CertStoreException;
import java.security.cert.CertificateException;
import java.security.cert.PKIXCertPathChecker;
import java.security.cert.PKIXCertPathValidatorResult;
import java.security.cert.PKIXParameters;
import java.security.cert.TrustAnchor;
import java.security.cert.X509CRL;
import java.security.cert.X509Certificate;
import java.security.interfaces.DSAParams;
import java.security.interfaces.DSAPublicKey;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

/**
 * An implementation of the Public Key Infrastructure's X.509
 * certificate path validation algorithm.
 *
 * <p>See <a href="http://www.ietf.org/rfc/rfc3280.txt">RFC 3280:
 * Internet X.509 Public Key Infrastructure Certificate and
 * Certificate Revocation List (CRL) Profile</a>.
 *
 * @author Casey Marshall (rsdio@metastatic.org)
 */
public class PKIXCertPathValidatorImpl extends CertPathValidatorSpi
{

  // Constants.
  // -------------------------------------------------------------------------

  private static final boolean DEBUG = false;
  private static void debug (String msg)
  {
    System.err.print (">> PKIXCertPathValidatorImpl: ");
    System.err.println (msg);
  }

  public static final String ANY_POLICY = "2.5.29.32.0";

  // Constructor.
  // -------------------------------------------------------------------------

  public PKIXCertPathValidatorImpl()
  {
    super();
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public CertPathValidatorResult engineValidate(CertPath path,
                                                CertPathParameters params)
    throws CertPathValidatorException, InvalidAlgorithmParameterException
  {
    if (!(params instanceof PKIXParameters))
      throw new InvalidAlgorithmParameterException("not a PKIXParameters object");

    // First check if the certificate path is valid.
    //
    // This means that:
    //
    //   (a)  for all x in {1, ..., n-1}, the subject of certificate x is
    //        the issuer of certificate x+1;
    //
    //   (b)  for all x in {1, ..., n}, the certificate was valid at the
    //        time in question.
    //
    // Because this is the X.509 algorithm, we also check if all
    // cerificates are of type X509Certificate.

    PolicyNodeImpl rootNode = new PolicyNodeImpl();
    Set initPolicies = ((PKIXParameters) params).getInitialPolicies();
    rootNode.setValidPolicy(ANY_POLICY);
    rootNode.setCritical(false);
    rootNode.setDepth(0);
    if (initPolicies != null)
      rootNode.addAllExpectedPolicies(initPolicies);
    else
      rootNode.addExpectedPolicy(ANY_POLICY);
    List checks = ((PKIXParameters) params).getCertPathCheckers();
    List l = path.getCertificates();
    if (l == null || l.size() == 0)
      throw new CertPathValidatorException();
    X509Certificate[] p = null;
    try
      {
        p = (X509Certificate[]) l.toArray(new X509Certificate[l.size()]);
      }
    catch (ClassCastException cce)
      {
        throw new CertPathValidatorException("invalid certificate path");
      }

    String sigProvider = ((PKIXParameters) params).getSigProvider();
    PublicKey prevKey = null;
    Date now = ((PKIXParameters) params).getDate();
    if (now == null)
      now = new Date();
    LinkedList policyConstraints = new LinkedList();
    for (int i = p.length - 1; i >= 0; i--)
      {
        try
          {
            p[i].checkValidity(now);
          }
        catch (CertificateException ce)
          {
            throw new CertPathValidatorException(ce.toString());
          }
        Set uce = getCritExts(p[i]);
        for (Iterator check = checks.iterator(); check.hasNext(); )
          {
            try
              {
                ((PKIXCertPathChecker) check.next()).check(p[i], uce);
              }
            catch (Exception x)
              {
              }
          }

        PolicyConstraint constr = null;
        if (p[i] instanceof GnuPKIExtension)
          {
            Extension pcx =
              ((GnuPKIExtension) p[i]).getExtension (PolicyConstraint.ID);
            if (pcx != null)
              constr = (PolicyConstraint) pcx.getValue();
          }
        else
          {
            byte[] pcx = p[i].getExtensionValue (PolicyConstraint.ID.toString());
            if (pcx != null)
              {
                try
                  {
                    constr = new PolicyConstraint (pcx);
                  }
                catch (Exception x)
                  {
                  }
              }
          }
        if (constr != null && constr.getRequireExplicitPolicy() >= 0)
          {
            policyConstraints.add (new int[]
              { p.length-i, constr.getRequireExplicitPolicy() });
          }

        updatePolicyTree(p[i], rootNode, p.length-i, (PKIXParameters) params,
                         checkExplicitPolicy (p.length-i, policyConstraints));

        // The rest of the tests involve this cert's relationship with the
        // next in the path. If this cert is the end entity, we can stop.
        if (i == 0)
          break;

        basicSanity(p, i);
        PublicKey pubKey = null;
        try
          {
            pubKey = p[i].getPublicKey();
            if (pubKey instanceof DSAPublicKey)
              {
                DSAParams dsa = ((DSAPublicKey) pubKey).getParams();
                // If the DSA public key is missing its parameters, use those
                // from the previous cert's key.
                if (dsa == null || dsa.getP() == null || dsa.getG() == null
                      || dsa.getQ() == null)
                  {
                    if (prevKey == null)
                      throw new InvalidKeyException("DSA keys not chainable");
                    if (!(prevKey instanceof DSAPublicKey))
                      throw new InvalidKeyException("DSA keys not chainable");
                    dsa = ((DSAPublicKey) prevKey).getParams();
                    pubKey = new GnuDSAPublicKey(((DSAPublicKey) pubKey).getY(),
                      dsa.getP(), dsa.getQ(), dsa.getG());
                  }
              }
            if (sigProvider == null)
              p[i-1].verify(pubKey);
            else
              p[i-1].verify(pubKey, sigProvider);
            prevKey = pubKey;
          }
        catch (Exception e)
          {
            throw new CertPathValidatorException(e.toString());
          }
        if (!p[i].getSubjectDN().equals(p[i-1].getIssuerDN()))
          throw new CertPathValidatorException("issuer DN mismatch");
        boolean[] issuerUid = p[i-1].getIssuerUniqueID();
        boolean[] subjectUid = p[i].getSubjectUniqueID();
        if (issuerUid != null && subjectUid != null)
          if (!Arrays.equals(issuerUid, subjectUid))
            throw new CertPathValidatorException("UID mismatch");

        // Check the certificate against the revocation lists.
        if (((PKIXParameters) params).isRevocationEnabled())
          {
            X509CRLSelectorImpl selector = new X509CRLSelectorImpl();
            try
              {
                selector.addIssuerName(p[i].getSubjectDN());
              }
            catch (IOException ioe)
              {
                throw new CertPathValidatorException("error selecting CRLs");
              }
            List certStores = ((PKIXParameters) params).getCertStores();
            List crls = new LinkedList();
            for (Iterator it = certStores.iterator(); it.hasNext(); )
              {
                CertStore cs = (CertStore) it.next();
                try
                  {
                    Collection c = cs.getCRLs(selector);
                    crls.addAll(c);
                  }
                catch (CertStoreException cse)
                  {
                  }
              }
            if (crls.isEmpty())
              throw new CertPathValidatorException("no CRLs for issuer");
            boolean certOk = false;
            for (Iterator it = crls.iterator(); it.hasNext(); )
              {
                CRL crl = (CRL) it.next();
                if (!(crl instanceof X509CRL))
                  continue;
                X509CRL xcrl = (X509CRL) crl;
                if (!checkCRL(xcrl, p, now, p[i], pubKey, certStores))
                  continue;
                if (xcrl.isRevoked(p[i-1]))
                  throw new CertPathValidatorException("certificate is revoked");
                else
                  certOk = true;
              }
            if (!certOk)
              throw new CertPathValidatorException("certificate's validity could not be determined");
          }
      }
    rootNode.setReadOnly();

    // Now ensure that the first certificate in the chain was issued
    // by a trust anchor.
    Exception cause = null;
    Set anchors = ((PKIXParameters) params).getTrustAnchors();
    for (Iterator i = anchors.iterator(); i.hasNext(); )
      {
        TrustAnchor anchor = (TrustAnchor) i.next();
        X509Certificate anchorCert = null;
        PublicKey anchorKey = null;
        if (anchor.getTrustedCert() != null)
          {
            anchorCert = anchor.getTrustedCert();
            anchorKey = anchorCert.getPublicKey();
          }
        else
          anchorKey = anchor.getCAPublicKey();
        if (anchorKey == null)
          continue;
        try
          {
            if (anchorCert == null)
              anchorCert.checkValidity(now);
            p[p.length-1].verify(anchorKey);
            if (anchorCert != null && anchorCert.getBasicConstraints() >= 0
                && anchorCert.getBasicConstraints() < p.length)
              continue;

            if (((PKIXParameters) params).isRevocationEnabled())
              {
                X509CRLSelectorImpl selector = new X509CRLSelectorImpl();
                if (anchorCert != null)
                  try
                    {
                      selector.addIssuerName(anchorCert.getSubjectDN());
                    }
                  catch (IOException ioe)
                    {
                    }
                else
                  selector.addIssuerName(anchor.getCAName());
                List certStores = ((PKIXParameters) params).getCertStores();
                List crls = new LinkedList();
                for (Iterator it = certStores.iterator(); it.hasNext(); )
                  {
                    CertStore cs = (CertStore) it.next();
                    try
                      {
                        Collection c = cs.getCRLs(selector);
                        crls.addAll(c);
                      }
                    catch (CertStoreException cse)
                      {
                      }
                  }
                if (crls.isEmpty())
                  continue;
                for (Iterator it = crls.iterator(); it.hasNext(); )
                  {
                    CRL crl = (CRL) it.next();
                    if (!(crl instanceof X509CRL))
                      continue;
                    X509CRL xcrl = (X509CRL) crl;
                    try
                      {
                        xcrl.verify(anchorKey);
                      }
                    catch (Exception x)
                      {
                        continue;
                      }
                    Date nextUpdate = xcrl.getNextUpdate();
                    if (nextUpdate != null && nextUpdate.compareTo(now) < 0)
                      continue;
                    if (xcrl.isRevoked(p[p.length-1]))
                      throw new CertPathValidatorException("certificate is revoked");
                  }
              }

            // The chain is valid; return the result.
            return new PKIXCertPathValidatorResult(anchor, rootNode,
                                                   p[0].getPublicKey());
          }
        catch (Exception ignored)
          {
            cause = ignored;
            continue;
          }
      }

    // The path is not valid.
    CertPathValidatorException cpve =
      new CertPathValidatorException("path validation failed");
    if (cause != null)
      cpve.initCause (cause);
    throw cpve;
  }

  // Own methods.
  // -------------------------------------------------------------------------

  /**
   * Check if a given CRL is acceptable for checking the revocation status
   * of certificates in the path being checked.
   *
   * <p>The CRL is accepted iff:</p>
   *
   * <ol>
   * <li>The <i>nextUpdate</i> field (if present) is in the future.</li>
   * <li>The CRL does not contain any unsupported critical extensions.</li>
   * <li>The CRL is signed by one of the certificates in the path, or,</li>
   * <li>The CRL is signed by the given public key and was issued by the
   * public key's subject, or,</li>
   * <li>The CRL is signed by a certificate in the given cert stores, and
   * that cert is signed by one of the certificates in the path.</li>
   * </ol>
   *
   * @param crl The CRL being checked.
   * @param path The path this CRL is being checked against.
   * @param now The value to use as 'now'.
   * @param pubKeySubject The subject of the public key.
   * @param pubKey The public key to check.
   * @return True if the CRL is acceptable.
   */
  private static boolean checkCRL(X509CRL crl, X509Certificate[] path, Date now,
                                  X509Certificate pubKeyCert, PublicKey pubKey,
                                  List certStores)
  {
    Date nextUpdate = crl.getNextUpdate();
    if (nextUpdate != null && nextUpdate.compareTo(now) < 0)
      return false;
    if (crl.hasUnsupportedCriticalExtension())
      return false;
    for (int i = 0; i < path.length; i++)
      {
        if (!path[i].getSubjectDN().equals(crl.getIssuerDN()))
          continue;
        boolean[] keyUsage = path[i].getKeyUsage();
        if (keyUsage != null)
          {
            if (!keyUsage[KeyUsage.CRL_SIGN])
              continue;
          }
        try
          {
            crl.verify(path[i].getPublicKey());
            return true;
          }
        catch (Exception x)
          {
          }
      }
    if (crl.getIssuerDN().equals(pubKeyCert.getSubjectDN()))
      {
        try
          {
            boolean[] keyUsage = pubKeyCert.getKeyUsage();
            if (keyUsage != null)
              {
                if (!keyUsage[KeyUsage.CRL_SIGN])
                  throw new Exception();
              }
            crl.verify(pubKey);
            return true;
          }
        catch (Exception x)
          {
          }
      }
    try
      {
        X509CertSelectorImpl select = new X509CertSelectorImpl();
        select.addSubjectName(crl.getIssuerDN());
        List certs = new LinkedList();
        for (Iterator it = certStores.iterator(); it.hasNext(); )
          {
            CertStore cs = (CertStore) it.next();
            try
              {
                certs.addAll(cs.getCertificates(select));
              }
            catch (CertStoreException cse)
              {
              }
          }
        for (Iterator it = certs.iterator(); it.hasNext(); )
          {
            X509Certificate c = (X509Certificate) it.next();
            for (int i = 0; i < path.length; i++)
              {
                if (!c.getIssuerDN().equals(path[i].getSubjectDN()))
                  continue;
                boolean[] keyUsage = c.getKeyUsage();
                if (keyUsage != null)
                  {
                    if (!keyUsage[KeyUsage.CRL_SIGN])
                      continue;
                  }
                try
                  {
                    c.verify(path[i].getPublicKey());
                    crl.verify(c.getPublicKey());
                    return true;
                  }
                catch (Exception x)
                  {
                  }
              }
            if (c.getIssuerDN().equals(pubKeyCert.getSubjectDN()))
              {
                c.verify(pubKey);
                crl.verify(c.getPublicKey());
              }
          }
      }
    catch (Exception x)
      {
      }
    return false;
  }

  private static Set getCritExts(X509Certificate cert)
  {
    HashSet s = new HashSet();
    if (cert instanceof GnuPKIExtension)
      {
        Collection exts = ((GnuPKIExtension) cert).getExtensions();
        for (Iterator it = exts.iterator(); it.hasNext(); )
          {
            Extension ext = (Extension) it.next();
            if (ext.isCritical() && !ext.isSupported())
              s.add(ext.getOid().toString());
          }
      }
    else
      s.addAll(cert.getCriticalExtensionOIDs());
    return s;
  }

  /**
   * Perform a basic sanity check on the CA certificate at <code>index</code>.
   */
  private static void basicSanity(X509Certificate[] path, int index)
    throws CertPathValidatorException
  {
    X509Certificate cert = path[index];
    int pathLen = 0;
    for (int i = index - 1; i > 0; i--)
      {
        if (!path[i].getIssuerDN().equals(path[i].getSubjectDN()))
          pathLen++;
      }
    Extension e = null;
    if (cert instanceof GnuPKIExtension)
      {
        e = ((GnuPKIExtension) cert).getExtension(BasicConstraints.ID);
      }
    else
      {
        try
          {
            e = new Extension(cert.getExtensionValue(BasicConstraints.ID.toString()));
          }
        catch (Exception x)
          {
          }
      }
    if (e == null)
      throw new CertPathValidatorException("no basicConstraints");
    BasicConstraints bc = (BasicConstraints) e.getValue();
    if (!bc.isCA())
      throw new CertPathValidatorException("certificate cannot be used to verify signatures");
    if (bc.getPathLengthConstraint() >= 0 && bc.getPathLengthConstraint() < pathLen)
      throw new CertPathValidatorException("path is too long");

    boolean[] keyUsage = cert.getKeyUsage();
    if (keyUsage != null)
      {
        if (!keyUsage[KeyUsage.KEY_CERT_SIGN])
          throw new CertPathValidatorException("certificate cannot be used to sign certificates");
      }
  }

  private static void updatePolicyTree(X509Certificate cert, PolicyNodeImpl root,
                                       int depth, PKIXParameters params,
                                       boolean explicitPolicy)
    throws CertPathValidatorException
  {
    if (DEBUG) debug("updatePolicyTree depth == " + depth);
    Set nodes = new HashSet();
    LinkedList stack = new LinkedList();
    Iterator current = null;
    stack.addLast(Collections.singleton(root).iterator());
    do
      {
        current = (Iterator) stack.removeLast();
        while (current.hasNext())
          {
            PolicyNodeImpl p = (PolicyNodeImpl) current.next();
            if (DEBUG) debug("visiting node == " + p);
            if (p.getDepth() == depth - 1)
              {
                if (DEBUG) debug("added node");
                nodes.add(p);
              }
            else
              {
                if (DEBUG) debug("skipped node");
                stack.addLast(current);
                current = p.getChildren();
              }
          }
      }
    while (!stack.isEmpty());

    Extension e = null;
    CertificatePolicies policies = null;
    List qualifierInfos = null;
    if (cert instanceof GnuPKIExtension)
      {
        e = ((GnuPKIExtension) cert).getExtension(CertificatePolicies.ID);
        if (e != null)
          policies = (CertificatePolicies) e.getValue();
      }

    List cp = null;
    if (policies != null)
      cp = policies.getPolicies();
    else
      cp = Collections.EMPTY_LIST;
    boolean match = false;
    if (DEBUG) debug("nodes are == " + nodes);
    if (DEBUG) debug("cert policies are == " + cp);
    for (Iterator it = nodes.iterator(); it.hasNext(); )
      {
        PolicyNodeImpl parent = (PolicyNodeImpl) it.next();
        if (DEBUG) debug("adding policies to " + parent);
        for (Iterator it2 = cp.iterator(); it2.hasNext(); )
          {
            OID policy = (OID) it2.next();
            if (DEBUG) debug("trying to add policy == " + policy);
            if (policy.toString().equals(ANY_POLICY) &&
                params.isAnyPolicyInhibited())
              continue;
            PolicyNodeImpl child = new PolicyNodeImpl();
            child.setValidPolicy(policy.toString());
            child.addExpectedPolicy(policy.toString());
            if (parent.getExpectedPolicies().contains(policy.toString()))
              {
                parent.addChild(child);
                match = true;
              }
            else if (parent.getExpectedPolicies().contains(ANY_POLICY))
              {
                parent.addChild(child);
                match = true;
              }
            else if (ANY_POLICY.equals (policy.toString()))
              {
                parent.addChild (child);
                match = true;
              }
            if (match && policies != null)
              {
                List qualifiers = policies.getPolicyQualifierInfos (policy);
                if (qualifiers != null)
                  child.addAllPolicyQualifiers (qualifiers);
              }
          }
      }
    if (!match && (params.isExplicitPolicyRequired() || explicitPolicy))
      throw new CertPathValidatorException("policy tree building failed");
  }

  private boolean checkExplicitPolicy (int depth, List explicitPolicies)
  {
    if (DEBUG) debug ("checkExplicitPolicy depth=" + depth);
    for (Iterator it = explicitPolicies.iterator(); it.hasNext(); )
      {
        int[] i = (int[]) it.next();
        int caDepth = i[0];
        int limit = i[1];
        if (DEBUG) debug ("  caDepth=" + caDepth + " limit=" + limit);
        if (depth - caDepth >= limit)
          return true;
      }
    return false;
  }
}
