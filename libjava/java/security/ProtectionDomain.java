/* ProtectionDomain.java -- A security domain
   Copyright (C) 1998, 2003, 2004  Free Software Foundation, Inc.

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
 * <p>This <code>ProtectionDomain</code> class encapsulates the characteristics
 * of a domain, which encloses a set of classes whose instances are granted a
 * set of permissions when being executed on behalf of a given set of
 * <i>Principals</i>.
 *
 * <p>A static set of permissions can be bound to a <code>ProtectionDomain</code>
 * when it is constructed; such permissions are granted to the domain regardless
 * of the {@link Policy} in force. However, to support dynamic security
 * policies, a <code>ProtectionDomain</code> can also be constructed such that
 * it is dynamically mapped to a set of permissions by the current {@link
 * Policy} whenever a permission is checked.</p>
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @version 0.0
 */
public class ProtectionDomain
{
  /** This is the <code>CodeSource</code> for this protection domain. */
  private CodeSource code_source;

  /** This is the set of permissions granted to this domain. */
  private PermissionCollection perms;

  /** The {@link ClassLoader} associated with this domain. */
  private ClassLoader classloader;

  /** The array of Principals associated with this domain.. */
  private Principal[] principals;

  /** Post 1.4 the policy may be refreshed! use false for pre 1.4. */
  private boolean staticBinding;

  /**
   * Creates a new <code>ProtectionDomain</code> with the given {@link
   * CodeSource} and {@link Permissions}. If the permissions object is not
   * <code>null</code>, then <code>setReadOnly()</code> will be called on the
   * passed in {@link Permissions} object. The only permissions granted to this
   * domain are the ones specified; the current {@link Policy} will not be
   * consulted.
   *
   * @param codesource the codesource associated with this domain.
   * @param permissions the permissions granted to this domain
   */
  public ProtectionDomain(CodeSource codesource, PermissionCollection permissions)
  {
    this(codesource, permissions, null, null, true);
  }

  /**
   * <p>Creates a new ProtectionDomain qualified by the given CodeSource,
   * Permissions, ClassLoader and array of Principals. If the permissions
   * object is not null, then <code>setReadOnly()</code> will be called on the
   * passed in Permissions object. The permissions granted to this domain are
   * dynamic; they include both the static permissions passed to this
   * constructor, and any permissions granted to this domain by the current
   * Policy at the time a permission is checked.</p>
   *
   * <p>This constructor is typically used by {@link ClassLoader}s and {@link
   * DomainCombiner}s which delegate to <code>Policy</code> to actively
   * associate the permissions granted to this domain. This constructor affords
   * the Policy provider the opportunity to augment the supplied
   * PermissionCollection to reflect policy changes.</p>
   *
   * @param codesource the CodeSource associated with this domain.
   * @param permissions the permissions granted to this domain.
   * @param classloader the ClassLoader associated with this domain.
   * @param principals the array of Principals associated with this domain.
   * @since 1.4
   * @see Policy#refresh()
   * @see Policy#getPermissions(ProtectionDomain)
  */
  public ProtectionDomain(CodeSource codesource,
                          PermissionCollection permissions,
                          ClassLoader classloader, Principal[] principals)
  {
    this(codesource, permissions, classloader, principals, false);
  }

  private ProtectionDomain(CodeSource codesource,
                           PermissionCollection permissions,
                           ClassLoader classloader, Principal[] principals,
                           boolean staticBinding)
  {
    super();

    code_source = codesource;
    if (permissions != null)
      {
        perms = permissions;
        perms.setReadOnly();
      }

    this.classloader = classloader;
    this.principals =
        (principals != null ? (Principal[]) principals.clone() : new Principal[0]);
    this.staticBinding = staticBinding;
  }

  /**
   * Returns the {@link CodeSource} of this domain.
   *
   * @return the {@link CodeSource} of this domain which may be <code>null</code>.
   * @since 1.2
   */
  public final CodeSource getCodeSource()
  {
    return code_source;
  }

  /**
   * Returns the {@link ClassLoader} of this domain.
   *
   * @return the {@link ClassLoader} of this domain which may be
   * <code>null</code>.
   * @since 1.4
   */
  public final ClassLoader getClassLoader()
  {
    return this.classloader;
  }

  /**
   * Returns an array of principals for this domain.
   *
   * @return returns a non-null array of principals for this domain. Changes to
   * this array will have no impact on the <code>ProtectionDomain</code>.
   * @since 1.4
   */
  public final Principal[] getPrincipals()
  {
    return (Principal[]) principals.clone();
  }

  /**
   * Returns the static permissions granted to this domain.
   *
   * @return the static set of permissions for this domain which may be
   * <code>null</code>.
   * @see Policy#refresh()
   * @see Policy#getPermissions(ProtectionDomain)
   */
  public final PermissionCollection getPermissions()
  {
    return perms;
  }

  /**
   * <p>Check and see if this <code>ProtectionDomain</code> implies the
   * permissions expressed in the <code>Permission</code> object.</p>
   *
   * <p>The set of permissions evaluated is a function of whether the
   * <code>ProtectionDomain</code> was constructed with a static set of
   * permissions or it was bound to a dynamically mapped set of permissions.</p>
   *
   * <p>If the <code>ProtectionDomain</code> was constructed to a statically
   * bound {@link PermissionCollection} then the permission will only be checked
   * against the {@link PermissionCollection} supplied at construction.</p>
   *
   * <p>However, if the <code>ProtectionDomain</code> was constructed with the
   * constructor variant which supports dynamically binding permissions, then
   * the permission will be checked against the combination of the
   * {@link PermissionCollection} supplied at construction and the current
   * {@link Policy} binding.
   *
   * @param permission the {@link Permission} object to check.
   * @return <code>true</code> if <code>permission</code> is implicit to this
   * <code>ProtectionDomain</code>.
   */
  public boolean implies(Permission permission)
  {
    if (staticBinding)
      return (perms == null ? false : perms.implies(permission));
    // Else dynamically bound.  Do we have it?
    // NOTE: this will force loading of Policy.currentPolicy
    return Policy.getCurrentPolicy().implies(this, permission);
  }

  /**
   * Convert a <code>ProtectionDomain</code> to a String.
   *
   * @return a string representation of the object.
   */
  public String toString()
  {
    String linesep = System.getProperty("line.separator");
    StringBuffer sb = new StringBuffer("ProtectionDomain (").append(linesep);

    if (code_source == null)
      sb.append("CodeSource:null");
    else
      sb.append(code_source);

    sb.append(linesep);
    if (classloader == null)
      sb.append("ClassLoader:null");
    else
      sb.append(classloader);

    sb.append(linesep);
    sb.append("Principals:");
    if (principals != null && principals.length > 0)
      {
        sb.append("[");
        Principal pal;
        for (int i = 0; i < principals.length; i++)
          {
            pal = principals[i];
            sb.append("'").append(pal.getName())
                .append("' of type ").append(pal.getClass().getName());
            if (i < principals.length-1)
              sb.append(", ");
          }
        sb.append("]");
      }
    else
      sb.append("none");

    sb.append(linesep);
    if (!staticBinding) // include all but dont force loading Policy.currentPolicy
      if (Policy.isLoaded())
        sb.append(Policy.getCurrentPolicy().getPermissions(this));
      else // fallback on this one's permissions
        sb.append(perms);
    else
      sb.append(perms);

    return sb.append(linesep).append(")").append(linesep).toString();
  }
}
