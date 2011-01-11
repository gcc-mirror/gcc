/* MBeanTrustPermission.java -- Represents a trusted bean source.
   Copyright (C) 2006 Free Software Foundation, Inc.

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

package javax.management;

import java.security.BasicPermission;

/**
 * Represents the permission held by a trusted source of
 * management beans.  For a bean to be added to a management
 * server, the source of that bean must hold this permission.
 * It has a target, but no actions. Valid values for the target
 * are <code>"register"</code> and <code>"*"</code>, the latter
 * representing both the existing <code>"register"</code> target
 * and any future targets.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class MBeanTrustPermission
  extends BasicPermission
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = -2952178077029018140L;

  /**
   * Constructs a {@link MBeanTrustPermission} with the given target.
   * The target must be either <code>"register"</code> or <code>"*"</code>.
   * The actions of the permission default to <code>null</code>,
   * so this is equivalent to calling
   * <code>MBeanTrustPermission(target, null)</code>.
   *
   * @param target the target of this permission.
   * @throws NullPointerException if <code>target</code> is null.
   * @throws IllegalArgumentException if the target is other than
   *                                  <code>"register"</code> or <code>"*"</code>.
   * @see #MBeanTrustPermission(String, String)
   */
  public MBeanTrustPermission(String target)
  {
    this(target, null);
  }

  /**
   * Constructs a {@link MBeanTrustPermission} with the given target
   * and actions.  The target must be either <code>"register"</code>
   * or <code>"*"</code>.  The actions must be either <code>null</code>
   * or the empty string, <code>""</code>.
   *
   * @param target the target of this permission.
   * @param actions the actions for this permission.
   * @throws NullPointerException if <code>target</code> is null.
   * @throws IllegalArgumentException if the target is other than
   *                                  <code>"register"</code> or <code>"*"</code>
   *                                  or actions is other than
   *                                  <code>null</code> or <code>""</code>.
   */
  public MBeanTrustPermission(String target, String actions)
  {
    super(target, actions);
    if ((!(target.equals("register"))) &&
        (!(target.equals("*"))))
      throw new IllegalArgumentException("The target must be 'register' or '*'");
    if (actions != null && !(actions.length() == 0))
      throw new IllegalArgumentException("The actions must be null or ''");
  }

}
