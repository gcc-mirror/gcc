/* Identity.java --- Identity Class
   Copyright (C) 1999, 2003, Free Software Foundation, Inc.

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

package java.security;

import java.io.Serializable;
import java.util.Vector;

/**
 * <p>This class represents identities: real-world objects such as people,
 * companies or organizations whose identities can be authenticated using their
 * public keys. Identities may also be more abstract (or concrete) constructs,
 * such as daemon threads or smart cards.</p>
 *
 * <p>All Identity objects have a <i>name</i> and a <i>public key</i>. Names
 * are immutable. <i>Identities</i> may also be <b>scoped</b>. That is, if an
 * <i>Identity</i> is specified to have a particular <i>scope</i>, then the
 * <i>name</i> and <i>public key</i> of the <i>Identity</i> are unique within
 * that <i>scope</i>.</p>
 *
 * <p>An <i>Identity</i> also has a <i>set of certificates</i> (all certifying
 * its own <i>public key</i>). The <i>Principal</i> names specified in these
 * certificates need not be the same, only the key.</p>
 *
 * <p>An <i>Identity</i> can be subclassed, to include postal and email
 * addresses, telephone numbers, images of faces and logos, and so on.</p>
 *
 * @author Mark Benvenuto
 * @see IdentityScope
 * @see Signer
 * @see Principal
 * @deprecated This class is no longer used. Its functionality has been replaced
 * by <code>java.security.KeyStore</code>, the <code>java.security.cert</code>
 * package, and <code>java.security.Principal</code>.
 */
public abstract class Identity implements Principal, Serializable
{
  private static final long serialVersionUID = 3609922007826600659L;

  private String name;
  private IdentityScope scope;
  private PublicKey publicKey;
  private String info;
  private Vector certificates;

  /** Constructor for serialization only. */
  protected Identity()
  {
  }

  /**
   * Constructs an identity with the specified name and scope.
   *
   * @param name the identity name.
   * @param scope the scope of the identity.
   * @throws KeyManagementException if there is already an identity with the
   * same name in the scope.
   */
  public Identity(String name, IdentityScope scope)
    throws KeyManagementException
  {
    this.name = name;
    this.scope = scope;
  }

  /**
   * Constructs an identity with the specified name and no scope.
   *
   * @param name the identity name.
   */
  public Identity(String name)
  {
    this.name = name;
    this.scope = null;
  }

  /**
   * Returns this identity's name.
   *
   * @return the name of this identity.
   */
  public final String getName()
  {
    return name;
  }

  /**
   * Returns this identity's scope.
   *
   * @return the scope of this identity.
   */
  public final IdentityScope getScope()
  {
    return scope;
  }

  /**
   * Returns this identity's public key.
   *
   * @return the public key for this identity.
   * @see #setPublicKey(java.security.PublicKey)
   */
  public PublicKey getPublicKey()
  {
    return publicKey;
  }

  /**
   * <p>Sets this identity's public key. The old key and all of this identity's
   * certificates are removed by this operation.</p>
   *
   * <p>First, if there is a security manager, its <code>checkSecurityAccess()
   * </code> method is called with <code>"setIdentityPublicKey"</code> as its
   * argument to see if it's ok to set the public key.</p>
   *
   * @param key the public key for this identity.
   * @throws KeyManagementException if another identity in the identity's scope
   * has the same public key, or if another exception occurs.
   * @throws SecurityException if a security manager exists and its
   * <code>checkSecurityAccess()<code> method doesn't allow setting the public
   * key.
   * @see #getPublicKey()
   * @see SecurityManager#checkSecurityAccess(String)
   */
  public void setPublicKey(PublicKey key) throws KeyManagementException
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("setIdentityPublicKey");

    this.publicKey = key;
  }

  /**
   * <p>Specifies a general information string for this identity.</p>
   *
   * <p>First, if there is a security manager, its <code>checkSecurityAccess()
   * </code> method is called with <code>"setIdentityInfo"</code> as its
   * argument to see if it's ok to specify the information string.</p>
   *
   * @param info the information string.
   * @throws SecurityException if a security manager exists and its
   * <code>checkSecurityAccess()</code> method doesn't allow setting the
   * information string.
   * @see #getInfo()
   * @see SecurityManager#checkSecurityAccess(String)
   */
  public void setInfo(String info)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("setIdentityInfo");

    this.info = info;
  }

  /**
   * Returns general information previously specified for this identity.
   *
   * @return general information about this identity.
   * @see #setInfo(String)
   */
  public String getInfo()
  {
    return info;
  }

  /**
   * <p>Adds a certificate for this identity. If the identity has a public key,
   * the public key in the certificate must be the same, and if the identity
   * does not have a public key, the identity's public key is set to be that
   * specified in the certificate.</p>
   *
   * <p>First, if there is a security manager, its <code>checkSecurityAccess()
   * </code> method is called with <code>"addIdentityCertificate"</code> as its
   * argument to see if it's ok to add a certificate.</p>
   *
   * @param certificate the certificate to be added.
   * @throws KeyManagementException if the certificate is not valid, if the
   * public key in the certificate being added conflicts with this identity's
   * public key, or if another exception occurs.
   * @throws SecurityException if a security manager exists and its
   * <code>checkSecurityAccess()</code> method doesn't allow adding a
   * certificate.
   * @see SecurityManager#checkSecurityAccess(String)
   */
  public void addCertificate(Certificate certificate)
    throws KeyManagementException
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("addIdentityCertificate");

    // Check public key of this certificate against the first one in the vector
    if (certificates.size() > 0)
      {
	if (((Certificate) certificates.firstElement()).getPublicKey() != publicKey)
	  throw new KeyManagementException("Public key does not match");
      }
    certificates.addElement(certificate);
  }

  /**
   * <p>Removes a certificate from this identity.</p>
   *
   * <p>First, if there is a security manager, its <code>checkSecurityAccess()
   * </code> method is called with <code>"removeIdentityCertificate"</code> as
   * its argument to see if it's ok to remove a certificate.</p>
   *
   * @param certificate the certificate to be removed.
   * @throws KeyManagementException if the certificate is missing, or if
   * another exception occurs.
   * @throws SecurityException if a security manager exists and its
   * <code>checkSecurityAccess()</code> method doesn't allow removing a
   * certificate.
   * @see SecurityManager#checkSecurityAccess(String)
   */
  public void removeCertificate(Certificate certificate)
    throws KeyManagementException
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("removeIdentityCertificate");

    if (certificates.contains(certificate) == false)
      throw new KeyManagementException("Certificate not found");

    certificates.removeElement(certificate);
  }

  /**
   * Returns a copy of all the certificates for this identity.
   *
   * @return a copy of all the certificates for this identity.
   */
  public Certificate[] certificates()
  {
    Certificate certs[] = new Certificate[certificates.size()];
    int max = certificates.size();
    for (int i = 0; i < max; i++)
      certs[i] = (Certificate) certificates.elementAt(i);

    return certs;
  }

  /**
   * Tests for equality between the specified object and this identity. This
   * first tests to see if the entities actually refer to the same object, in
   * which case it returns <code>true</code>. Next, it checks to see if the
   * entities have the same <i>name</i> and the same <i>scope</i>. If they do,
   * the method returns <code>true</code>. Otherwise, it calls
   * <code>identityEquals()</code>, which subclasses should override.
   *
   * @param identity the object to test for equality with this identity.
   * @return <code>true</code> if the objects are considered equal, <code>false
   * </code>otherwise.
   * @see #identityEquals(Identity)
   */
  public final boolean equals(Object identity)
  {
    if (identity instanceof Identity)
      {
	if (identity == this)
	  return true;

	if ((((Identity) identity).getName() == this.name) &&
	    (((Identity) identity).getScope() == this.scope))
	  return true;

	return identityEquals((Identity) identity);
      }
    return false;
  }

  /**
   * Tests for equality between the specified <code>identity</code> and this
   * <i>identity</i>. This method should be overriden by subclasses to test for
   * equality. The default behavior is to return <code>true</code> if the names
   * and public keys are equal.
   *
   * @param identity the identity to test for equality with this identity.
   * @return <code>true</code> if the identities are considered equal,
   * <code>false</code> otherwise.
   * @see #equals(Object)
   */
  protected boolean identityEquals(Identity identity)
  {
    return ((identity.getName() == this.name) &&
	    (identity.getPublicKey() == this.publicKey));
  }

  /**
   * <p>Returns a short string describing this identity, telling its name and
   * its scope (if any).</p>
   *
   * <p>First, if there is a security manager, its <code>checkSecurityAccess()
   * </code> method is called with <code>"printIdentity"</code> as its argument
   * to see if it's ok to return the string.</p>
   *
   * @return information about this identity, such as its name and the name of
   * its scope (if any).
   * @throws SecurityException if a security manager exists and its
   * <code>checkSecurityAccess()</code> method doesn't allow returning a string
   * describing this identity.
   * @see SecurityManager#checkSecurityAccess(String)
   */
  public String toString()
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("printIdentity");

    /* TODO: Insert proper format here */
    return (name + ":@" + scope + " Public Key: " + publicKey);
  }

  /**
   * <p>Returns a string representation of this identity, with optionally more
   * details than that provided by the <code>toString()</code> method without
   * any arguments.</p>
   *
   * <p>First, if there is a security manager, its <code>checkSecurityAccess()
   * </code> method is called with <code>"printIdentity"</code> as its argument
   * to see if it's ok to return the string.</p>
   *
   * @param detailed whether or not to provide detailed information.
   * @return information about this identity. If detailed is <code>true</code>,
   * then this method returns more information than that provided by the
   * <code>toString()</code> method without any arguments.
   * @throws SecurityException if a security manager exists and its
   * <code>checkSecurityAccess()</code> method doesn't allow returning a string
   * describing this identity.
   * @see #toString()
   * @see SecurityManager#checkSecurityAccess(String)
   */
  public String toString(boolean detailed)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("printIdentity");

    if (detailed)
      {
	/* TODO: Insert proper detailed format here */
	return (name + ":@" + scope + " Public Key: " + publicKey);
      }
    else
      {
	/* TODO: Insert proper format here */
	return (name + ":@" + scope + " Public Key: " + publicKey);
      }
  }

  /**
   * Returns a hashcode for this identity.
   *
   * @return a hashcode for this identity.
   */
  public int hashCode()
  {
    int ret = name.hashCode();
    if (publicKey != null)
      ret |= publicKey.hashCode();
    if (scope != null)
      ret |= scope.hashCode();
    if (info != null)
      ret |= info.hashCode();
    if (certificates != null)
      ret |= certificates.hashCode();

    return ret;
  }
}
