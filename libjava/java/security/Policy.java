/* Policy.java --- Policy Manager Class
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

/**
   Policy is an abstract class for managing the system security
   policy for the Java application environment. It specifies
   which permissions are available for code from various
   sources. The security policy is represented through a 
   subclass of Policy.

   Only one Policy is in effect at any time. ProtectionDomain
   initializes itself with information from this class on the 
   set of permssions to grant.

   The location for the actual Policy could be anywhere in any
   form because it depends on the Policy implementation. The
   default system is in a flat ASCII file or it could be in a 
   database.

   The current installed Policy can be accessed with getPolicy
   and changed with setPolicy if the code has the correct
   permissions.

   The refresh method causes the Policy class to refresh/reload
   its configuration. The method used to refresh depends on the 
   Policy implementation.

   When a protection domain initializes its permissions it uses
   code like:
   <code>
   policy = Policy.getPolicy();
   permissionCollection perms = policy.getPermissions(MyCodeSource)     
   </code>
   The protection domain passes the Policy handler a CodeSource
   object which contains the codebase URL and public key. The 
   Policy implementation then returns the proper set of 
   permissions for the CodeSource.

   The default Policy implementation can be changed by setting
   the "policy.provider" security provider in java.security
   to the correct Policy implementation class.

   @author Mark Benvenuto

   @since JDK 1.2
 */
public abstract class Policy
{
  // FIXME: The class name of the Policy provider should really be sourced 
  // from the "java.security" configuration file. For now, just hard-code 
  // a stub implementation.
  static private Policy currentPolicy = null;
  static 
  {
    String pp = System.getProperty ("policy.provider");
    if (pp != null)
      try
	{
	  currentPolicy = (Policy)Class.forName(pp).newInstance();
	} 
      catch (Exception _) 
	{
	  currentPolicy = null;
	}
    if (currentPolicy == null)
      currentPolicy = new gnu.java.security.provider.DefaultPolicy();
  }
  
  /**
     Constructs a new Policy class.
   */
  public Policy()
  {
  }

  /**
     Gets the currently installed Policy handler. The value should
     not be cached as it can be changed by setPolicy. This 
     function first calls <code>SecurityManager.checkPermission</code>
     with <code>SecurityPermission("getPolicy")</code> to check
     if the caller has Permission to get the current Policy.

     @return the current Policy

     @throws SecurityException if the security manager exists
     the caller does not have permission to 
     <code>getPolicy</code>.
   */
  public static Policy getPolicy()
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPermission(new SecurityPermission("getPolicy"));

    return currentPolicy;
  }

  /**
     Sets the currently installed Policy handler. This 
     function first calls <code>SecurityManager.checkPermission</code>
     with <code>SecurityPermission("setPolicy")</code> to check
     if the caller has Permission to get the current Policy.

     @param policy the new Policy to use

     @throws SecurityException if the security manager exists
     the caller does not have permission to 
     <code>getPolicy</code>.
   */
  public static void setPolicy(Policy policy)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPermission(new SecurityPermission("setPolicy"));

    currentPolicy = policy;
  }


  /**
     Evalutes the global policy and returns a set of Permissions 
     allowed for the specified CodeSource.

     @param codesource The CodeSource to get Permission for

     @return a set of permissions for codesource specified by 
     the current policy

     @throws SecurityException if the current thread does not
     have permission to call <code>getPermissions</code>
   */
  public abstract PermissionCollection getPermissions(CodeSource codesource);

  /**
     Refreshes and/or reloads the current Policy. The actual
     behavior of this method depends on the implementation. 
   */
  public abstract void refresh();
}
