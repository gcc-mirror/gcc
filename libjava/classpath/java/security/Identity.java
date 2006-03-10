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

package java.security;

import java.io.Serializable;
import java.util.Vector;

/**
 * The <code>Identity</code> class is used to represent people and companies
 * that can be authenticated using public key encryption. The identities can
 * also be abstract objects such as smart cards.
 * 
 * <p><code>Identity</code> objects store a name and public key for each
 * identity. The names cannot be changed and the identities can be scoped. Each
 * identity (name and public key) within a scope are unique to that scope.</p>
 * 
 * <p>Each identity has a set of ceritificates which all specify the same
 * public key, but not necessarily the same name.</p>
 * 
 * <p>The <code>Identity</code> class can be subclassed to allow additional
 * information to be attached to it.</p>
 *
 * @author Mark Benvenuto
 * @see IdentityScope
 * @see Signer
 * @see Principal
 * @deprecated Replaced by <code>java.security.KeyStore</code>, the
 * <code>java.security.cert</code> package, and
 * <code>java.security.Principal</code>.
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
   * Constructs a new instance of <code>Identity</code> with the specified
   * name and scope.
   * 
   * @param name
   *          the name to use.
   * @param scope
   *          the scope to use.
   * @throws KeyManagementException
   *           if the identity is already present.
   */
  public Identity(String name, IdentityScope scope)
    throws KeyManagementException
  {
    this.name = name;
    this.scope = scope;
  }

  /**
   * Constructs a new instance of <code>Identity</code> with the specified
   * name and no scope.
   * 
   * @param name
   *          the name to use.
   */
  public Identity(String name)
  {
    this.name = name;
    this.scope = null;
  }

  /** @return the name of this identity. */
  public final String getName()
  {
    return name;
  }

  /** @return the scope of this identity. */
  public final IdentityScope getScope()
  {
    return scope;
  }

  /**
   * @return the public key of this identity.
   * @see #setPublicKey(java.security.PublicKey)
   */
  public PublicKey getPublicKey()
  {
    return publicKey;
  }

  /**
   * Sets the public key for this identity. The old key and all certificates
   * are removed.
   * 
   * @param key
   *          the public key to use.
   * @throws KeyManagementException
   *           if this public key is used by another identity in the current
   *           scope.
   * @throws SecurityException
   *           if a {@link SecurityManager} is installed which disallows this
   *           operation.
   */
  public void setPublicKey(PublicKey key) throws KeyManagementException
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("setIdentityPublicKey");

    this.publicKey = key;
  }

  /**
   * Sets the general information string.
   * 
   * @param info
   *          the general information string.
   * @throws SecurityException
   *           if a {@link SecurityManager} is installed which disallows this
   *           operation.
   */
  public void setInfo(String info)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("setIdentityInfo");

    this.info = info;
  }

  /**
   * @return the general information string of this identity.
   * @see #setInfo(String)
   */
  public String getInfo()
  {
    return info;
  }

  /**
   * Adds a certificate to the list of ceritificates for this identity. The
   * public key in this certificate must match the existing public key if it
   * exists.
   * 
   * @param certificate
   *          the certificate to add.
   * @throws KeyManagementException
   *           if the certificate is invalid, or the public key conflicts.
   * @throws SecurityException
   *           if a {@link SecurityManager} is installed which disallows this
   *           operation.
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
   * Removes a certificate from the list of ceritificates for this identity.
   * 
   * @param certificate
   *          the certificate to remove.
   * @throws KeyManagementException
   *           if the certificate is invalid.
   * @throws SecurityException
   *           if a {@link SecurityManager} is installed which disallows this
   *           operation.
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

  /** @return an array of {@link Certificate}s for this identity. */
  public Certificate[] certificates()
  {
    Certificate[] certs = new Certificate[certificates.size()];
    int max = certificates.size();
    for (int i = 0; i < max; i++)
      certs[i] = (Certificate) certificates.elementAt(i);

    return certs;
  }

  /**
   * Checks for equality between this Identity and a specified object. It first
   * checks if they are the same object, then if the name and scope match and
   * returns <code>true</code> if successful. If these tests fail, the
   * {@link #identityEquals(Identity)} method is called.
   * 
   * @return <code>true</code> if they are equal, <code>false</code>
   *         otherwise.
   */
  public final boolean equals(Object identity)
  {
    if (identity instanceof Identity)
      {
	if (identity == this)
	  return true;

	if ((((Identity) identity).getName().equals(this.name)) &&
	    (((Identity) identity).getScope().equals(this.scope)))
	  return true;

	return identityEquals((Identity) identity);
      }
    return false;
  }

  /**
   * Checks for equality between this Identity and a specified object. A
   * subclass should override this method. The default behavior is to return
   * <code>true</code> if the public key and names match.
   * 
   * @return <code>true</code> if they are equal, <code>false</code>
   *         otherwise.
   */
  protected boolean identityEquals(Identity identity)
  {
    return ((identity.getName().equals(this.name)) &&
	    (identity.getPublicKey().equals(this.publicKey)));
  }

  /**
   * Returns a string representation of this Identity.
   * 
   * @return a string representation of this Identity.
   * @throws SecurityException
   *           if a {@link SecurityManager} is installed which disallows this
   *           operation.
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
   * Returns a detailed string representation of this Identity.
   * 
   * @param detailed
   *          indicates whether or detailed information is desired.
   * @return a string representation of this Identity.
   * @throws SecurityException
   *           if a {@link SecurityManager} is installed which disallows this
   *           operation.
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

  /** @return a hashcode of this identity. */
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
