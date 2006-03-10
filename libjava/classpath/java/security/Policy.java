/* Policy.java --- Policy Manager Class
   Copyright (C) 1999, 2003, 2004 Free Software Foundation, Inc.

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

import java.util.Collections;
import java.util.Enumeration;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * <code>Policy</code> is an abstract class for managing the system security
 * policy for the Java application environment. It specifies which permissions
 * are available for code from various sources. The security policy is
 * represented through a subclass of <code>Policy</code>.
 * 
 * <p>Only one <code>Policy</code> is in effect at any time. A
 * {@link ProtectionDomain} initializes itself with information from this class
 * on the set of permssions to grant.</p>
 * 
 * <p>The location for the actual <code>Policy</code> could be anywhere in any
 * form because it depends on the Policy implementation. The default system is
 * in a flat ASCII file or it could be in a database.</p>
 * 
 * <p>The current installed <code>Policy</code> can be accessed with
 * {@link #getPolicy()} and changed with {@link #setPolicy(Policy)} if the code
 * has the correct permissions.</p>
 * 
 * <p>The {@link #refresh()} method causes the <code>Policy</code> instance to
 * refresh/reload its configuration. The method used to refresh depends on the
 * <code>Policy</code> implementation.</p>
 * 
 * <p>When a protection domain initializes its permissions, it uses code like
 * the following:</p>
 * 
 * <code>
 * policy = Policy.getPolicy();
 * PermissionCollection perms = policy.getPermissions(myCodeSource);
 * </code>
 * 
 * <p>The protection domain passes the <code>Policy</code> handler a
 * {@link CodeSource} instance which contains the codebase URL and a public key.
 * The <code>Policy</code> implementation then returns the proper set of
 * permissions for that {@link CodeSource}.</p>
 * 
 * <p>The default <code>Policy</code> implementation can be changed by setting
 * the "policy.provider" security provider in the "java.security" file to the
 * correct <code>Policy</code> implementation class.</p>
 *
 * @author Mark Benvenuto
 * @see CodeSource
 * @see PermissionCollection
 * @see SecureClassLoader
 * @since 1.2
 */
public abstract class Policy
{
  private static Policy currentPolicy;

  /** Map of ProtectionDomains to PermissionCollections for this instance. */
  private Map pd2pc = null;

  /** Constructs a new <code>Policy</code> object. */
  public Policy()
  {
  }

  /**
   * Returns the currently installed <code>Policy</code> handler. The value
   * should not be cached as it can be changed any time by
   * {@link #setPolicy(Policy)}.
   * 
   * @return the current <code>Policy</code>.
   * @throws SecurityException
   *           if a {@link SecurityManager} is installed which disallows this
   *           operation.
   */
  public static Policy getPolicy()
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPermission(new SecurityPermission("getPolicy"));

    return getCurrentPolicy();
  }

  /**
   * Sets the <code>Policy</code> handler to a new value.
   * 
   * @param policy
   *          the new <code>Policy</code> to use.
   * @throws SecurityException
   *           if a {@link SecurityManager} is installed which disallows this
   *           operation.
   */
  public static void setPolicy(Policy policy)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPermission(new SecurityPermission("setPolicy"));

    setup(policy);
    currentPolicy = policy;
  }

  private static void setup(final Policy policy)
  {
    if (policy.pd2pc == null)
      policy.pd2pc = Collections.synchronizedMap(new LinkedHashMap());

    ProtectionDomain pd = policy.getClass().getProtectionDomain();
    if (pd.getCodeSource() != null)
      {
        PermissionCollection pc = null;
        if (currentPolicy != null)
          pc = currentPolicy.getPermissions(pd);

        if (pc == null) // assume it has all
          {
            pc = new Permissions();
            pc.add(new AllPermission());
          }

        policy.pd2pc.put(pd, pc); // add the mapping pd -> pc
      }
  }

  /**
   * Ensures/forces loading of the configured policy provider, while bypassing
   * the {@link SecurityManager} checks for <code>"getPolicy"</code> security
   * permission.  Needed by {@link ProtectionDomain}.
   */
  static Policy getCurrentPolicy()
  {
    // FIXME: The class name of the Policy provider should really be sourced
    // from the "java.security" configuration file. For now, just hard-code
    // a stub implementation.
    if (currentPolicy == null)
      {
        String pp = System.getProperty ("policy.provider");
        if (pp != null)
          try
            {
              currentPolicy = (Policy) Class.forName(pp).newInstance();
            }
	  catch (Exception e)
	    {
	      // Ignored.
	    }

        if (currentPolicy == null)
          currentPolicy = new gnu.java.security.provider.DefaultPolicy();
      }
    return currentPolicy;
  }

  /**
   * Tests if <code>currentPolicy</code> is not <code>null</code>,
   * thus allowing clients to not force loading of any policy
   * provider; needed by {@link ProtectionDomain}.
   */
  static boolean isLoaded()
  {
    return currentPolicy != null;
  }

  /**
   * Returns the set of Permissions allowed for a given {@link CodeSource}.
   * 
   * @param codesource
   *          the {@link CodeSource} for which, the caller needs to find the
   *          set of granted permissions.
   * @return a set of permissions for {@link CodeSource} specified by the
   *         current <code>Policy</code>.
   * @throws SecurityException
   *           if a {@link SecurityManager} is installed which disallows this
   *           operation.
   */
  public abstract PermissionCollection getPermissions(CodeSource codesource);

  /**
   * Returns the set of Permissions allowed for a given {@link ProtectionDomain}.
   * 
   * @param domain
   *          the {@link ProtectionDomain} for which, the caller needs to find
   *          the set of granted permissions.
   * @return a set of permissions for {@link ProtectionDomain} specified by the
   *         current <code>Policy.</code>.
   * @since 1.4
   * @see ProtectionDomain
   * @see SecureClassLoader
   */
  public PermissionCollection getPermissions(ProtectionDomain domain)
  {
    if (domain == null)
      return new Permissions();

    if (pd2pc == null)
      setup(this);

    PermissionCollection result = (PermissionCollection) pd2pc.get(domain);
    if (result != null)
      {
        Permissions realResult = new Permissions();
        for (Enumeration e = result.elements(); e.hasMoreElements(); )
          realResult.add((Permission) e.nextElement());

        return realResult;
      }

    result = getPermissions(domain.getCodeSource());
    if (result == null)
      result = new Permissions();

    PermissionCollection pc = domain.getPermissions();
    if (pc != null)
      for (Enumeration e = pc.elements(); e.hasMoreElements(); )
        result.add((Permission) e.nextElement());

    return result;
  }

  /**
   * Checks if the designated {@link Permission} is granted to a designated
   * {@link ProtectionDomain}.
   * 
   * @param domain
   *          the {@link ProtectionDomain} to test.
   * @param permission
   *          the {@link Permission} to check.
   * @return <code>true</code> if <code>permission</code> is implied by a
   *         permission granted to this {@link ProtectionDomain}. Returns
   *         <code>false</code> otherwise.
   * @since 1.4
   * @see ProtectionDomain
   */
  public boolean implies(ProtectionDomain domain, Permission permission)
  {
    if (pd2pc == null)
      setup(this);

    PermissionCollection pc = (PermissionCollection) pd2pc.get(domain);
    if (pc != null)
      return pc.implies(permission);

    boolean result = false;
    pc = getPermissions(domain);
    if (pc != null)
      {
        result = pc.implies(permission);
        pd2pc.put(domain, pc);
      }

    return result;
  }

  /**
   * Causes this <code>Policy</code> instance to refresh / reload its
   * configuration. The method used to refresh depends on the concrete
   * implementation.
   */
  public abstract void refresh();
}
