/* AuthPermission.java -- permissions related to authentication.
   Copyright (C) 2004 Free Software Foundation, Inc.

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


package javax.security.auth;

import java.security.BasicPermission;

/**
 * <p>A permission controlling access to authentication service. The
 * <i>actions</i> field of auth permission objects is ignored; the whole
 * of the permission is defined by the <i>target</i>.</p>
 *
 * <p>The authentication permission targets recognized are:</p>
 *
 * <dl>
 * <dt><code>doAs</code></dt>
 *
 * <dd><p>Allows access to the {@link
 * Subject#doAs(javax.security.auth.Subject  java.security.PrivilegedAction)}
 * methods.</p></dd>
 *
 * <dt><code>doAsPrivileged</code></dt>
 *
 * <dd><p>Allows access to the {@link
 * Subject#doAsPrivileged(javax.security.auth.Subject,
 * java.security.PrivilegedAction, java.security.AccessControlContext)}
 * methods.</p></dd>
 *
 * <dt><code>getSubject</code></dt>
 *
 * <dd><p>Allows access to the {@link Subject} associated with a
 * thread.</p></dd>
 *
 * <dt><code>getSubjectFromDomainCombiner</code></dt>
 *
 * <dd><p>Allows access to the {@link Subject} associated with a
 * {@link SubjectDomainCombiner}.</p></dd>
 *
 * <dt><code>setReadOnly</code></dt>
 *
 * <dd><p>Allows a {@link Subject} to be marked as read-only.</p></dd>
 *
 * <dt><code>modifyPrincipals</code></dt>
 *
 * <dd><p>Allows the set of principals of a subject to be modified.</p></dd>
 *
 * <dt><code>modifyPublicCredentials</code></dt>
 *
 * <dd><p>Allows the set of public credentials of a subject to be
 * modified.</p></dd>
 *
 * <dt><code>modifyPrivateCredentials</code></dt>
 *
 * <dd><p>Allows the set of private credentials of a subject to be
 * modified.</p></dd>
 *
 * <dt><code>refreshCredential</code></dt>
 *
 * <dd><p>Allows a {@link Refreshable} credential to be refreshed.</p></dd>
 *
 * <dt><code>destroyCredential</code></dt>
 *
 * <dd><p>Allows a {@link Destroyable} credential to be destroyed.</p></dd>
 *
 * <dt><code>createLoginContext.<i>name</i></code></dt>
 *
 * <dd><p>Allows a {@link javax.security.auth.login.LoginContext} for the
 * given <i>name</i>. <i>name</i> can also be a wildcard (<code>'*'</code>),
 * which allows the creation of a context with any name.</p></dd>
 *
 * <dt><code>getLoginConfiguration</code></dt>
 *
 * <dd><p>Allows the system-wide login {@link
 * javax.security.auth.login.Configuration} to be retrieved.</p></dd>
 *
 * <dt><code>setLoginConfiguration</code></dt>
 *
 * <dd><p>Allows the system-wide login {@link
 * javax.security.auth.login.Configuration} to be set.</p></dd>
 *
 * <dt><code>refreshLoginConfiguration</code></dt>
 *
 * <dd><p>Allows the system-wide login {@link
 * javax.security.auth.login.Configuration} to be refreshed.</p></dd>
 * </dl>
 */
public final class AuthPermission extends BasicPermission
{

  /**
   * Creates a new authentication permission for the given target name.
   *
   * @param name The target name.
   */
  public AuthPermission (String name)
  {
    super (name);
  }

  /**
   * Creates a new authentication permission for the given target name.
   * The actions list is not used by this class.
   *
   * @param name The target name.
   * @param actions The action list.
   */
  public AuthPermission (String name, String actions)
  {
    super (name, actions);
  }
}
