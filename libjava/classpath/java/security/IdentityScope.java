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
 * <code>IdentityScope</code> represents a scope of an identity.
 * <code>IdentityScope</code> is also an {@link Identity} and can have a name
 * and scope along with the other qualitites identities possess.
 * 
 * <p>An <code>IdentityScope</code> contains other {@link Identity} objects.
 * All {@link Identity} objects are manipulated in the scope the same way. The
 * scope is supposed to apply different scope to different type of
 * Identities.</p>
 * 
 * <p>No identity within the same scope can have the same public key.</p>
 * 
 * @author Mark Benvenuto
 * @see Identity
 * @see Signer
 * @see Principal
 * @see Key
 * @deprecated Use java.security.KeyStore, the java.security.cert package, and
 *             java.security.Principal.
 */
public abstract class IdentityScope extends Identity
{
  private static final long serialVersionUID = -2337346281189773310L;
  private static IdentityScope systemScope;

  /** Constructor for serialization purposes. */
  protected IdentityScope()
  {
    super();
  }

  /**
   * Constructs a new instance of <code>IdentityScope</code> with the
   * specified name and no scope.
   * 
   * @param name
   *          the name to use.
   */
  public IdentityScope(String name)
  {
    super(name);
  }

  /**
   * Constructs a new instance of <code>IdentityScope</code> with the
   * specified name and {@link IdentityScope}.
   * 
   * @param name
   *          the name to use.
   * @param scope
   *          the scope to use.
   * @throws KeyManagementException
   *           if the identity scope is already present.
   */
  public IdentityScope(String name, IdentityScope scope)
    throws KeyManagementException
  {
    super(name, scope);
  }

  /**
   * Returns the system's Scope.
   * 
   * @return the system's Scope.
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
   * Sets the scope of the system.
   * 
   * @param scope
   *          the new system scope.
   * @throws SecurityException
   *           if a {@link SecurityManager} is installed which disallows this
   *           operation.
   */
  protected static void setSystemScope(IdentityScope scope)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("setSystemScope");

    systemScope = scope;
  }

  /**
   * Returns the number of entries within this <code>IdentityScope</code>.
   * 
   * @return the number of entries within this <code>IdentityScope</code>.
   */
  public abstract int size();

  /**
   * Returns the specified {@link Identity}, by name, within this scope.
   * 
   * @param name
   *          name of {@link Identity} to get.
   * @return an {@link Identity} representing the name or <code>null</code> if
   *         it cannot be found.
   */
  public abstract Identity getIdentity(String name);

  /**
   * Returns the specified {@link Identity}, by {@link Principal}, within this
   * scope.
   * 
   * @param principal
   *          the {@link Principal} to use.
   * @return an identity representing the {@link Principal} or <code>null</code>
   *         if it cannot be found.
   */
  public Identity getIdentity(Principal principal)
  {
    return getIdentity(principal.getName());
  }

  /**
   * Returns the specified {@link Identity}, by public key, within this scope.
   * 
   * @param key
   *          the {@link PublicKey} to use.
   * @return an identity representing the public key or <code>null</code> if
   *         it cannot be found.
   */
  public abstract Identity getIdentity(PublicKey key);

  /**
   * Adds an identity to his scope.
   * 
   * @param identity
   *          the {@link Identity} to add.
   * @throws KeyManagementException
   *           if it is an invalid identity, an identity with the same key
   *           exists, or if another error occurs.
   */
  public abstract void addIdentity(Identity identity)
    throws KeyManagementException;

  /**
   * Removes an identity in this scope.
   * 
   * @param identity
   *          the {@link Identity} to remove.
   * @throws KeyManagementException
   *           if it is a missing identity, or if another error occurs.
   */
  public abstract void removeIdentity(Identity identity)
    throws KeyManagementException;

  /**
   * Returns an {@link Enumeration} of identities in this scope.
   * 
   * @return an {@link Enumeration} of the identities in this scope.
   */
  public abstract Enumeration<Identity> identities();

  /**
   * Returns a string representing this instance. It includes the name, the
   * scope name, and number of identities.
   * 
   * @return a string representation of this instance.
   */
  public String toString()
  {
    return (super.getName() + " " + super.getScope().getName() + " " + size());
  }
}
