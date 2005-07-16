/* IdentityScope.java --- IdentityScope Class
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

import java.util.Enumeration;

/**
 * <p>This class represents a scope for identities. It is an Identity itself,
 * and therefore has a name and can have a scope. It can also optionally have a
 * public key and associated certificates.</p>
 *
 * <p>An <code>IdentityScope</code> can contain {@link Identity} objects of all
 * kinds, including {@link Signer}s. All types of <code>Identity</code> objects
 * can be retrieved, added, and removed using the same methods. Note that it is
 * possible, and in fact expected, that different types of identity scopes will
 * apply different policies for their various operations on the various types of
 * Identities.</p>
 *
 * <p>There is a one-to-one mapping between keys and identities, and there can
 * only be one copy of one key per scope. For example, suppose Acme Software,
 * Inc is a software publisher known to a user. Suppose it is an <i>Identity</i>,
 * that is, it has a public key, and a set of associated certificates. It is
 * named in the scope using the name "Acme Software". No other named <i>Identity
 * </i> in the scope has the same public key. Of course, none has the same name
 * as well.</p>
 *
 * @author Mark Benvenuto
 * @see Identity
 * @see Signer
 * @see Principal
 * @see Key
 * @deprecated This class is no longer used. Its functionality has been replaced
 * by <code>java.security.KeyStore</code>, the <code>java.security.cert</code>
 * package, and <code>java.security.Principal</code>.
 */
public abstract class IdentityScope extends Identity
{
  private static final long serialVersionUID = -2337346281189773310L;
  private static IdentityScope systemScope;

  /**
   * This constructor is used for serialization only and should not be used by
   * subclasses.
   */
  protected IdentityScope()
  {
    super();
  }

  /**
   * Constructs a new identity scope with the specified name.
   *
   * @param name the scope name.
   */
  public IdentityScope(String name)
  {
    super(name);
  }

  /**
   * Constructs a new identity scope with the specified name and scope.
   *
   * @param name the scope name.
   * @param scope the scope for the new identity scope.
   * @throws KeyManagementException if there is already an identity with the
   * same name in the scope.
   */
  public IdentityScope(String name, IdentityScope scope)
    throws KeyManagementException
  {
    super(name, scope);
  }

  /**
   * Returns the system's identity scope.
   *
   * @return the system's identity scope.
   * @see #setSystemScope(IdentityScope)
   */
  public static IdentityScope getSystemScope()
  {
    if (systemScope == null)
      {
	//Load it
	//systemScope;
      }
    return systemScope;
  }

  /**
   * Sets the system's identity scope.
   *
   * <p>First, if there is a security manager, its <code>checkSecurityAccess()
   * </code> method is called with <code>"setSystemScope"</code> as its argument
   * to see if it's ok to set the identity scope.</p>
   *
   * @param scope the scope to set.
   * @throws SecurityException if a security manager exists and its
   * <code>checkSecurityAccess()</code> method doesn't allow setting the
   * identity scope.
   * @see #getSystemScope()
   * @see SecurityManager#checkSecurityAccess(String)
   */
  protected static void setSystemScope(IdentityScope scope)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("setSystemScope");

    systemScope = scope;
  }

  /**
   * Returns the number of identities within this identity scope.
   *
   * @return the number of identities within this identity scope.
   */
  public abstract int size();

  /**
   * Returns the identity in this scope with the specified name (if any).
   *
   * @param name the name of the identity to be retrieved.
   * @return the identity named name, or <code>null</code> if there are no
   * identities named name in this scope.
   */
  public abstract Identity getIdentity(String name);

  /**
   * Retrieves the identity whose name is the same as that of the specified
   * principal. (Note: <code>Identity</code> implements <code>Principal</code>.)
   *
   * @param principal the principal corresponding to the identity to be
   * retrieved.
   * @return the identity whose name is the same as that of the principal, or
   * <code>null</code> if there are no identities of the same name in this scope.
   */
  public Identity getIdentity(Principal principal)
  {
    return getIdentity(principal.getName());
  }

  /**
   * Retrieves the identity with the specified public key.
   *
   * @param key the public key for the identity to be returned.
   * @return the identity with the given key, or <code>null</code> if there are
   * no identities in this scope with that key.
   */
  public abstract Identity getIdentity(PublicKey key);

  /**
   * Adds an identity to this identity scope.
   *
   * @param identity the identity to be added.
   * @throws KeyManagementException if the identity is not valid, a name
   * conflict occurs, another identity has the same public key as the identity
   * being added, or another exception occurs.
   */
  public abstract void addIdentity(Identity identity)
    throws KeyManagementException;

  /**
   * Removes an identity from this identity scope.
   *
   * @param identity the identity to be removed.
   * @throws KeyManagementException if the identity is missing, or another
   * exception occurs.
   */
  public abstract void removeIdentity(Identity identity)
    throws KeyManagementException;

  /**
   * Returns an enumeration of all identities in this identity scope.
   *
   * @return an enumeration of all identities in this identity scope.
   */
  public abstract Enumeration identities();

  /**
   * Returns a string representation of this identity scope, including its name,
   * its scope name, and the number of identities in this identity scope.
   *
   * @return a string representation of this identity scope.
   * @see SecurityManager#checkSecurityAccess(String)
   */
  public String toString()
  {
    return (super.getName() + " " + super.getScope().getName() + " " + size());
  }
}
