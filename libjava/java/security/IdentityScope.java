/* IdentityScope.java --- IdentityScope Class
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
import java.util.Enumeration;

/**
   IdentityScope represents a scope of an identity. IdentityScope 
   is also an Identity and can have a name and scope along with 
   the other qualitites identities posses.

   An IdentityScope contains other Identity objects. All Identity 
   objects are manipulated in the scope the same way. The scope 
   is suppose to apply different scope to different type of 
   Identities.

   No identity within the same scope can have the same public key.

   @since JDK 1.1

   @deprecated Use java.security.KeyStore, the java.security.cert 
   package, and java.security.Principal. 

   @author Mark Benvenuto       
 */
public abstract class IdentityScope extends Identity
{
  private static IdentityScope systemScope = null;

  /**
     Creates a new instance of IdentityScope from Serialized Data
   */
  protected IdentityScope()
  {
    super();
  }

  /**
     Creates a new instance of IdentityScope with the specified name 
     and no scope.

     @param name the name to use
   */
  public IdentityScope(String name)
  {
    super(name);
  }

  /**
     Creates a new instance of IdentityScope with the specified name 
     and IdentityScope.

     @param name the name to use
     @param scope the scope to use

     @throws KeyManagementException if the identity scope is already 
     present
   */
  public IdentityScope(String name, IdentityScope scope)
    throws KeyManagementException
  {
    super(name, scope);
  }

  /**
     Gets the system's Scope.
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
     Sets the scope of the system.

     This class checks the security manager with the call 
     checkSecurityAccess with "setSystemScope".

     @param scope the new sustem scope

     @throws SecurityException - if the security manager denies 
     access to "setSystemScope"
   */
  protected static void setSystemScope(IdentityScope scope)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkSecurityAccess("setSystemScope");

    systemScope = scope;
  }

  /**
     Gets the number of entries within this IdentityScope.

     @returns the number of entries
   */
  public abstract int size();

  /**
     Gets the specified Identity within this scope
     by specified name.

     @param name name of Identity to get

     @returns an identity representing the name or null if it 
     cannot be found
   */
  public abstract Identity getIdentity(String name);

  /**
     Gets the specified Identity within this scope
     by the specified Principal.

     @param principal The Principal of the Identity to get

     @returns an identity representing the principal or null if it 
     cannot be found
   */
  public Identity getIdentity(Principal principal)
  {
    return getIdentity(principal.getName());
  }

  /**
     Gets the specified Identity within this scope
     by the specified public key.

     @param key the PublicKey of the Identity to get

     @returns an identity representing the public key or null if it 
     cannot be found
   */
  public abstract Identity getIdentity(PublicKey key);

  /**
     Adds an identity to his scope.

     @param identity the identity to add

     @throws KeyManagementException if it is an invalid identity,
     an identity with the same key exists, or another error
     occurs.
   */
  public abstract void addIdentity(Identity identity)
    throws KeyManagementException;

  /**
     Removes an identity to his scope.

     @param identity the identity to remove

     @throws KeyManagementException if it is a missing identity, 
     or another error occurs.
   */
  public abstract void removeIdentity(Identity identity)
    throws KeyManagementException;

  /**
     Returns an Enumeration of identities.

     @returns an enumeration of the identities.
   */
  public abstract Enumeration identities();

  /**
     Returns a string representing this IdentityScope.
     It includes the name, the scope name, and number of identities.

     @returns a string representing this IdentityScope.
   */
  public String toString()
  {
    return (super.getName() + " " + super.getScope().getName()
	    + " " + size());
  }
}
