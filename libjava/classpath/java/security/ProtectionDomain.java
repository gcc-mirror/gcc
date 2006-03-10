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

import gnu.classpath.SystemProperties;

/**
 * This class represents a group of classes, along with their granted
 * permissions. The classes are identified by a {@link CodeSource}. Thus, any
 * class loaded from the specified {@link CodeSource} is treated as part of
 * this domain. The set of permissions is represented by an instance of
 * {@link PermissionCollection}.
 * 
 * <p>Every class in the system will belong to one and only one
 * <code>ProtectionDomain</code>.</p>
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
   * Initializes a new instance of <code>ProtectionDomain</code> representing
   * the specified {@link CodeSource} and set of permissions. No permissions
   * can be added later to the {@link PermissionCollection} and this contructor
   * will call the <code>setReadOnly</code> method on the specified set of
   * permissions.
   * 
   * @param codesource
   *          The {@link CodeSource} for this domain.
   * @param permissions
   *          The set of permissions for this domain.
   * @see PermissionCollection#setReadOnly()
   */
  public ProtectionDomain(CodeSource codesource, PermissionCollection permissions)
  {
    this(codesource, permissions, null, null, true);
  }

  /**
   * This method initializes a new instance of <code>ProtectionDomain</code>
   * given its {@link CodeSource}, granted permissions, associated
   * {@link ClassLoader} and {@link Principal}s.
   * 
   * <p>Similar to the previous constructor, if the designated set of
   * permissions is not <code>null</code>, the <code>setReadOnly</code> method
   * is called on that set.</p>
   * 
   * @param codesource
   *          The {@link CodeSource} for this domain.
   * @param permissions
   *          The permission set for this domain.
   * @param classloader
   *          the ClassLoader associated with this domain.
   * @param principals
   *          the array of {@link Principal}s associated with this domain.
   * @since 1.4
   * @see PermissionCollection#setReadOnly()
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
   * @return the {@link CodeSource} of this domain.
   * @since 1.2
   */
  public final CodeSource getCodeSource()
  {
    return code_source;
  }

  /**
   * Returns the {@link ClassLoader} of this domain.
   * 
   * @return the {@link ClassLoader} of this domain.
   * @since 1.4
   */
  public final ClassLoader getClassLoader()
  {
    return this.classloader;
  }

  /**
   * Returns a clone of the {@link Principal}s of this domain.
   * 
   * @return a clone of the {@link Principal}s of this domain.
   * @since 1.4
   */
  public final Principal[] getPrincipals()
  {
    return (Principal[]) principals.clone();
  }

  /**
   * Returns the {@link PermissionCollection} of this domain.
   * 
   * @return The {@link PermissionCollection} of this domain.
   */
  public final PermissionCollection getPermissions()
  {
    return perms;
  }

  /**
   * Tests whether or not the specified {@link Permission} is implied by the
   * set of permissions granted to this domain.
   * 
   * @param permission
   *          the {@link Permission} to test.
   * @return <code>true</code> if the specified {@link Permission} is implied
   *         for this domain, <code>false</code> otherwise.
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
   * Returns a string representation of this object. It will include the
   * {@link CodeSource} and set of permissions associated with this domain.
   * 
   * @return A string representation of this object.
   */
  public String toString()
  {
    String linesep = SystemProperties.getProperty("line.separator");
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
