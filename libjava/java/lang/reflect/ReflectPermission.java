/* ReflectPermission.java - named permission for reflaction
   Copyright (C) 2000, 2001 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 */

package java.lang.reflect;

import java.security.BasicPermission;

/**
 * This class implements permissions for reflection.  This is a named
 * permission, and the only defined name is suppressAccessChecks, which
 * allows suppression of normal Java objects when using reflection.
 *
 * <table>
 *  <tr>
 *   <th>Permission Target Name</th>
 *   <th>What Permission Allows</th>
 *   <th>Risk of Allowing Permission</th>
 *  </tr>
 *  <tr>
 *   <td><code>suppressAccessChecks</code></td>
 *   <td>Ability to access fields, invoke methods, and construct objects
 *       via reflection, including non-public members in contexts where
 *       such access is not legal at compile-time.</td>
 *   <td>This is dangerous. It exposes possibly confidential information,
 *       and malicious code could interfere with the internals of the Virtual
 *       Machine by corrupting private data.</td>
 *  </tr>
 * </table>
 *
 * @author Tom Tromey <tromey@redhat.com>
 * @author Eric Blake <ebb9@email.byu.edu>
 * @since 1.2
 * @status updated to 1.4
 */
public final class ReflectPermission
  extends BasicPermission
{
  /**
   * Compatible with JDK 1.2.
   */
  private static final long serialVersionUID = 7412737110241507485L;

  /**
   * Construct a ReflectPermission with the given name.
   *
   * @param name The permission name
   */
  public ReflectPermission(String name)
  {
    super(name);
  }

  /**
   * Construct a ReflectPermission with the given name.
   *
   * @param name The permission name
   * @param actions The actions; this is ignored and should be null
   */
  public ReflectPermission(String name, String actions)
  {
    super(name, actions);
  }
}
