/* ManagementPermission.java - Permissions for system management.
   Copyright (C) 2006 Free Software Foundation

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

package java.lang.management;

import java.security.BasicPermission;

/**
 * <p>
 * Represents the permission to view or modify the data
 * which forms part of the system management interfaces.
 * Calls to methods of the system management beans,
 * provided by the {@link ManagementFactory}, may perform
 * checks against the current {@link java.lang.SecurityManager}
 * (if any) before allowing the operation to proceed.
 * Instances of this object are supplied to the
 * {@link java.lang.SecurityManager} in order to perform
 * these checks.  It is not normal for instances of this
 * class to be created outside the use of the
 * {@link java.lang.SecurityManager}.
 * </p>
 * <p>
 * This object can represent two types of management
 * permission:
 * </p>
 * <ul>
 * <li><strong>monitor</strong> &mdash; this allows access
 * to information such as the arguments supplied to the
 * virtual machine, the currently loaded classes and the
 * stack traces of running threads.  Malicious code may
 * use this to obtain information about the system and
 * exploit any vulnerabilities found.</li>
 * <li><strong>control</strong> &mdash; this allows the
 * information stored by the management beans to be altered.
 * For example, additional debugging information (such
 * as class loading traces) may be turned on or memory
 * usage limits changed.  Malicious code could use
 * this to alter the behaviour of the system.</li>
 * </ul>
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public final class ManagementPermission
  extends BasicPermission
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = 1897496590799378737L;

  /**
   * Constructs a new <code>ManagementPermission</code>
   * for one of the two permission targets, "monitor"
   * and "control".
   *
   * @param name the name of the permission this instance
   *             should represent; either "monitor" or
   *             "control".
   * @throws IllegalArgumentException if the name is not
   *                                  either "monitor"
   *                                  or "control".
   */
  public ManagementPermission(String name)
  {
    super(name);
    if (!(name.equals("monitor") || name.equals("control")))
      throw new IllegalArgumentException("Invalid permission.");
  }

  /**
   * Constructs a new <code>ManagementPermission</code>
   * for one of the two permission targets, "monitor"
   * and "control".  Actions are not supported, so
   * this value should be either <code>null</code>
   * or the empty string.
   *
   * @param name the name of the permission this instance
   *             should represent; either "monitor" or
   *             "control".
   * @param actions either <code>null</code> or the
   *                empty string.
   * @throws IllegalArgumentException if the name is not
   *                                  either "monitor"
   *                                  or "control", or
   *                                  a value for actions
   *                                  is specified.
   */
  public ManagementPermission(String name, String actions)
  {
    this(name);
    if (!(actions == null || actions.equals("")))
      throw new IllegalArgumentException("Invalid actions.");
  }

}
