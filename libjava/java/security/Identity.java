/* Identity.java --- Identity Class
   Copyright (C) 1999 Free Software Foundation, Inc.

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
   The Identity class is used to repsent people and companies that 
   can be authenticated using public key encryption. The identities
   can also be abstract objects such as smart cards.

   Identity object store a name and public key for each identity.
   The names cannot be changed and the identities can be scoped.
   Each identity (name and public key) within a scope is unique 
   to that scope.

   Each identity has a set of ceritificates which all specify the 
   same public key but not necessarily the same name.

   The Identity class can be subclassed to allow additional 
   information to be attached to it.

   @since JDK 1.1

   @deprecated Use java.security.KeyStore, the java.security.cert 
   package, and java.security.Principal. 

   @author Mark Benvenuto       
 */
public abstract class Identity implements Principal, Serializable
{
  private String name;
  private IdentityScope scope;
  private PublicKey publicKey;
  private String info;
  private Vector certificates;

  /**
     Creates a new instance of Identity from Serialized Data
   */
  protected Identity()
  {
  }

  /**
     Creates a new instance of Identity with the specified name 
     and IdentityScope.

     @param name the name to use
     @param scope the scope to use

     @throws KeyManagementException if the identity is already 
     present
   */
  public Identity(String name, IdentityScope scope)
    throws KeyManagementException
  {
    this.name = name;
    this.scope = scope;
  }

  /**
     Creates a new instance of Identity with the specified name 
     and no scope.

     @param name the name to use
   */
  public Identity(String name)
  {
    this.name = name;
    this.scope = null;
  }

  /**
     Gets the name for this Identity.

     @return the name
   */
  public final String getName()
  {
    return name;
  }

  /**
     Gets the scope for this Identity.

     @return the scope
   */
  public final IdentityScope getScope()
  {
    return scope;
  }

  /**
     Gets the public key for this identity.

     @return the public key
   */
  public PublicKey getPublicKey()
  {
    return publicKey;
  }

  /**
     Sets the public key for this identity.
     The old key and all certificates are removed.

     This class checks the security manager with the call 
     checkSecurityAccess with "setIdentityPublicKey".

     @param key the public key to use

     @throws KeyManagementException if this public key is used by 
     another identity in the current scope.
     @throws SecurityException - if the security manager denies 
     access to "setIdentityPublicKey"
   */
  public void setPublicKey(PublicKey key) throws KeyManagementException
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("setIdentityPublicKey");

    this.publicKey = key;
  }

  /**
     Sets the general information string.

     This class checks the security manager with the call 
     checkSecurityAccess with "setIdentityInfo".

     @param info the general information string.

     @throws SecurityException - if the security manager denies 
     access to "setIdentityInfo"
   */
  public void setInfo(String info)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("setIdentityInfo");

    this.info = info;
  }

  /**
     Gets the general information string.

     @return the string
   */
  public String getInfo()
  {
    return info;
  }

  /**
     Adds a certificate to the list of ceritificates for this 
     identity. The public key in this certificate must match the 
     existing public key if it exists.

     This class checks the security manager with the call 
     checkSecurityAccess with "addIdentityCertificate".

     @param certificate the certificate to add

     @throws KeyManagementException if the certificate is invalid
     or the public key conflicts
     @throws SecurityException - if the security manager denies 
     access to "addIdentityCertificate"
   */
  public void addCertificate(java.security.Certificate certificate)
    throws KeyManagementException
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("addIdentityCertificate");

    //Check public key of this certificate against the first one 
    //in the vector
    if (certificates.size() > 0)
      {
	if (((Certificate) certificates.firstElement()).getPublicKey() !=
	    publicKey)
	  throw new KeyManagementException("Public key does not match");
      }
    certificates.addElement(certificate);
  }

  /**
     Removes a certificate from the list of ceritificates for this 
     identity. 

     This class checks the security manager with the call 
     checkSecurityAccess with "removeIdentityCertificate".

     @param certificate the certificate to add

     @throws KeyManagementException if the certificate is invalid
     @throws SecurityException - if the security manager denies 
     access to "removeIdentityCertificate"
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
     Returns an array of certificates for this identity.

     @returns array of certificates
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
     Checks for equality between this Identity and the specified 
     object. If first checks if they are the same object, then 
     if the name and scope matches and returns true if successful.
     If these tests fail, identityEquals is called.

     @return true if they are equal, false otherwise
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
     Checks for equality between this Identity and the specified 
     object. A subclass should override this method. The default 
     behavior is to return true if the public key and names match.

     @return true if they are equal, false otherwise
   */
  protected boolean identityEquals(Identity identity)
  {
    return ((identity.getName() == this.name) &&
	    (identity.getPublicKey() == this.publicKey));
  }

  /**
     Returns a string representing this Identity.

     This class checks the security manager with the call 
     checkSecurityAccess with "printIdentity".

     @returns a string representing this Identity.

     @throws SecurityException - if the security manager denies 
     access to "printIdentity"
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
     Returns a detailed string representing this Identity.

     This class checks the security manager with the call 
     checkSecurityAccess with "printIdentity".

     @param detailed indicates whether or not to provide detailed 
     information

     @returns a string representing this Identity.

     @throws SecurityException - if the security manager denies 
     access to "printIdentity"
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
     Gets the hashcode for this Identity.

     @returns the hashcode
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
