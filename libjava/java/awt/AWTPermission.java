/* AWTPermission.java -- AWT related permissions
   Copyright (C) 2000, 2002  Free Software Foundation

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


package java.awt;

import java.security.BasicPermission;

/**
 * This class implements permissions for AWT.  This is a named
 * permission.  No actions are defined.
 *
 * <p>The following table provides a list of all the possible AWTPermission
 * permission names with a description of what that permission allows.<br>
 * <table border=1>
 * <tr><th>Permission Name</th><th>Permission Allows</th><th>Risks</th</tr>
 * <tr>
 *   <td><code>accessClipboard</code></td>
 *   <td>posting and reading the AWT clipboard</td>
 *   <td>the clipboard may contain sensitive data</td></tr>
 * <tr>
 *   <td><code>accessEventQueue</code></td>
 *   <td>access to the AWT event queue</td>
 *   <td>malicious code could remove real events and replace them with bogus
 *       ones, including simulating the user granting permission</td></tr>
 * <tr>
 *   <td><code>listenToAllAWTEvents</code></td>
 *   <td>listen to system-wide AWT events</td>
 *   <td>malicious code can read passwords entered in an AWT event, and in
 *       combination with accessEventQueue, could fake system events</td></tr>
 * <tr>
 *   <td><code>showWindowWithoutWarningBanner</code></td>
 *   <td>display a window without a banner notification of insecurity</td>
 *   <td>malicious code could install a Trojan horse applet that looks like
 *       a normal window, and thus steal data like passwords</td></tr>
 * <tr>
 *   <td><code>readDisplayPixels</code></td>
 *   <td>read back pixels from the display screen</td>
 *   <td>malicious code could snoop on the user's actions</td></tr>
 * <tr>
 *   <td><code>createRobot</code></td>
 *   <td>create an instance of java.awt.Robot</td>
 *   <td>these objects can generate events as though they were the user; so
 *       malicious code could control the system</td></tr>
 * <tr>
 *   <td><code>fullScreenExclusive</code></td>
 *   <td>enter full-screen exclusive mode</td>
 *   <td>malicious code could masquerade as a trusted program</td></tr>
 * </table>
 *
 * @author Tom Tromey <tromey@redhat.com>
 * @since 1.2
 * @status updated to 1.4
 */
public final class AWTPermission extends BasicPermission
{
  /**
   * Compatible with JDK 1.2+.
   */
  private static final long serialVersionUID = 8890392402588814465L;

  /**
   * Construct a AWTPermission with the given name.
   *
   * @param name the permission name
   * @throws NullPointerException if name is null
   * @throws IllegalArgumentException if name is invalid
   */
  public AWTPermission(String name)
  {
    super(name);
  }

  /**
   * Create a new permission with the specified name. The actions argument
   * is ignored, as AWT permissions have no actions.
   *
   * @param name the permission name
   * @param actions ignored
   * @throws NullPointerException if name is null
   * @throws IllegalArgumentException if name is invalid
   */
  public AWTPermission(String name, String actions)
  {
    super(name);
  }
} // class AWTPermission
