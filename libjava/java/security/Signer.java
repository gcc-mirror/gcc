/* Signer.java --- Signer Class
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

/**
 * <p>This class is used to represent an {@link Identity} that can also
 * digitally sign data.</p>
 *
 * <p>The management of a signer's private keys is an important and sensitive
 * issue that should be handled by subclasses as appropriate to their intended
 * use.</p>
 *
 * @author Mark Benvenuto <ivymccough@worldnet.att.net>
 * @deprecated This class is no longer used. Its functionality has been replaced
 * by <code>java.security.KeyStore</code>, the <code>java.security.cert</code>
 * package, and <code>java.security.Principal</code>.
 */
public abstract class Signer extends Identity
{
  private static final long serialVersionUID = -1763464102261361480L;
  private PrivateKey privateKey = null;

  /**
   * Creates a <code>Signer</code>. This constructor should only be used for
   * serialization.
   */
  protected Signer()
  {
  }

  /**
   * Creates a <code>Signer</code> with the specified identity name.
   *
   * @param name the identity name.
   */
  public Signer(String name)
  {
    super(name);
  }

  /**
   * Creates a <code>Signer</code> with the specified identity name and scope.
   *
   * @param name the identity name.
   * @param scope the scope of the identity.
   * @throws KeyManagementException if there is already an identity with the
   * same name in the scope.
   */
  public Signer(String name, IdentityScope scope) throws KeyManagementException
  {
    super(name, scope);
  }

  /**
   * <p>Returns this signer's private key.</p>
   *
   * <p>First, if there is a security manager, its <code>checkSecurityAccess()
   * </code> method is called with <code>"getSignerPrivateKey"</code> as its
   * argument to see if it's ok to return the private key.</p>
   *
   * @return this signer's private key, or <code>null</code> if the private key
   * has not yet been set.
   * @throws SecurityException if a security manager exists and its
   * <code>checkSecurityAccess()</code> method doesn't allow returning the
   * private key.
   * @see SecurityManager#checkSecurityAccess(String)
   */
  public PrivateKey getPrivateKey()
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("getSignerPrivateKey");

    return privateKey;
  }

  /**
   * <p>Sets the key pair (public key and private key) for this signer.</p>
   *
   * <p>First, if there is a security manager, its <code>checkSecurityAccess()
   * </code> method is called with <code>"setSignerKeyPair"</code> as its
   * argument to see if it's ok to set the key pair.</p>
   *
   * @param pair an initialized key pair.
   * @throws InvalidParameterException if the key pair is not properly
   * initialized.
   * @throws KeyException if the key pair cannot be set for any other reason.
   * @throws SecurityException if a security manager exists and its
   * <code>checkSecurityAccess()</code> method doesn't allow setting the key
   * pair.
   * @see SecurityManager#checkSecurityAccess(String)
   */
  public final void setKeyPair(KeyPair pair)
    throws InvalidParameterException, KeyException
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("setSignerKeyPair");

    try
      {
        if (pair.getPublic() != null)
          setPublicKey(pair.getPublic());
        else
          throw new InvalidParameterException();

      }
    catch (KeyManagementException kme)
      {
        throw new KeyException();
      }

    if (pair.getPrivate() != null)
        privateKey = pair.getPrivate();
    else
      throw new InvalidParameterException();
  }

  /**
   * Returns a string of information about the signer.
   *
   * @return a string of information about the signer.
   * @see SecurityManager#checkSecurityAccess(String)
   */
  public String toString()
  {
    return (getName() + ": " + privateKey);
  }
}
