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

import java.util.Collections;
import java.util.Enumeration;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * <p>This is an abstract class for representing the system security policy for
 * a Java application environment (specifying which permissions are available
 * for code from various sources). That is, the security policy is represented
 * by a <code>Policy</code> subclass providing an implementation of the abstract
 * methods in this <code>Policy</code> class.</p>
 *
 * <p>There is only one <code>Policy</code> object in effect at any given time.
 * </p>
 *
 * <p>The source location for the policy information utilized by the
 * <code>Policy</code> object is up to the <code>Policy</code> implementation.
 * The policy configuration may be stored, for example, as a flat ASCII file, as
 * a serialized binary file of the <code>Policy</code> class, or as a database.
 * </p>
 *
 * <p>The currently-installed <code>Policy</code> object can be obtained by
 * calling the <code>getPolicy()</code> method, and it can be changed by a call
 * to the <code>setPolicy()</code> method (by code with permission to reset the
 * <code>Policy</code>).</p>
 *
 * <p>The <code>refresh()</code> method causes the policy object to refresh /
 * reload its current configuration.</p>
 *
 * <p>This is implementation-dependent. For example, if the policy object stores
 * its policy in configuration files, calling <code>refresh()</code> will cause
 * it to re-read the configuration policy files. The refreshed policy may not
 * have an effect on classes in a particular {@link ProtectionDomain}. This is
 * dependent on the <code>Policy</code> provider's implementation of the
 * <code>implies()</code> method and the {@link PermissionCollection} caching
 * strategy.</p>
 *
 * <p>The default <code>Policy</code> implementation can be changed by setting
 * the value of the <code>"policy.provider"</code> security property (in the
 * Java security properties file) to the fully qualified name of the desired
 * <code>Policy</code> implementation class. The Java security properties file
 * is located in the file named <code>&lt;JAVA_HOME>/lib/security/java.security
 * </code>, where <code>&lt;JAVA_HOME></code> refers to the directory where the
 * SDK was installed.</p>
 *
 * <p><b>IMPLEMENTATION NOTE:</b> This implementation attempts to read the
 * System property named <code>policy.provider</code> to find the concrete
 * implementation of the <code>Policy</code>. If/when this fails, it falls back
 * to a default implementation, which <b>allows everything</b>.
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
   * Returns the installed <code>Policy</code> object. This value should not be
   * cached, as it may be changed by a call to <code>setPolicy()</code>. This
   * method first calls {@link SecurityManager#checkPermission(Permission)} with
   * a <code>SecurityPermission("getPolicy")</code> permission to ensure it's ok
   * to get the <code>Policy</code> object.
   *
   * @return the installed <code>Policy</code>.
   * @throws SecurityException if a security manager exists and its
   * <code>checkPermission()</code> method doesn't allow getting the
   * <code>Policy</code> object.
   * @see SecurityManager#checkPermission(Permission)
   * @see #setPolicy(Policy)
   */
  public static Policy getPolicy()
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPermission(new SecurityPermission("getPolicy"));

    return getCurrentPolicy();
  }

  /**
   * Sets the system-wide <code>Policy</code> object. This method first calls
   * {@link SecurityManager#checkPermission(Permission)} with a
   * <code>SecurityPermission("setPolicy")</code> permission to ensure it's ok
   * to set the <code>Policy</code>.
   *
   * @param policy the new system <code>Policy</code> object.
   * @throws SecurityException if a security manager exists and its
   * <code>checkPermission()</code> method doesn't allow setting the
   * <code>Policy</code>.
   * @see SecurityManager#checkPermission(Permission)
   * @see #getPolicy()
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
   * Evaluates the global policy and returns a {@link PermissionCollection}
   * object specifying the set of permissions allowed for code from the
   * specified code source.
   *
   * @param codesource the {@link CodeSource} associated with the caller. This
   * encapsulates the original location of the code (where the code came from)
   * and the public key(s) of its signer.
   * @return the set of permissions allowed for code from codesource according
   * to the policy. The returned set of permissions must be a new mutable
   * instance and it must support heterogeneous {@link Permission} types.
   */
  public abstract PermissionCollection getPermissions(CodeSource codesource);

  /**
   * Evaluates the global policy and returns a {@link PermissionCollection}
   * object specifying the set of permissions allowed given the characteristics
   * of the protection domain.
   *
   * @param domain the {@link ProtectionDomain} associated with the caller.
   * @return the set of permissions allowed for the domain according to the
   * policy. The returned set of permissions must be a new mutable instance and
   * it must support heterogeneous {@link Permission} types.
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
   * Evaluates the global policy for the permissions granted to the {@link
   * ProtectionDomain} and tests whether the <code>permission</code> is granted.
   *
   * @param domain the {@link ProtectionDomain} to test.
   * @param permission the {@link Permission} object to be tested for
   * implication.
   * @return <code>true</code> if <code>permission</code> is a proper subset of
   * a permission granted to this {@link ProtectionDomain}.
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
   * Refreshes/reloads the policy configuration. The behavior of this method
   * depends on the implementation. For example, calling refresh on a file-based
   * policy will cause the file to be re-read.
   */
  public abstract void refresh();
}
