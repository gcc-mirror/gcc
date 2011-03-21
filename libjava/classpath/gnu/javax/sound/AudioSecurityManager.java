/* AudioSecurityManager.java -- Manages Security requests for Sound classes.

 Copyright (C) 2007 Free Software Foundation, Inc.

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

package gnu.javax.sound;

import javax.sound.sampled.AudioPermission;

/**
 * This class handles security requests for classes in the Sound API.
 *
 * A class that needs to check against a particular permission type may use this
 * class to query the <code>SecurityManager</code>.
 *
 * For example, to check for a read permission, a class can simply pass the
 * <code>Permission.READ</code> constant to
 * {@link #checkPermissions(gnu.javax.sound.AudioSecurityManager.Permission))},
 * like the following code demonstrates:
 *
 * <pre>
 * AudioSecurityManager.checkPermissions(Permission.PLAY);
 * </pre>
 *
 * If there is need to query for all the defined permissions type, the constant
 * <code>Permission.ALL</code> can be used. In alternative, the
 * {@link #checkPermissions()} is presented as a shorthand.
 *
 * @author Mario Torre <neugens@limasoftware.net>
 */
public class AudioSecurityManager
{
  /**
   * Defines a common set of permission allowed by the specification.
   */
  public static enum Permission
  {
    PLAY, RECORD, ALL
  }

  /**
   * Shorthand to <code>checkPermissions(Permission.ALL)</code>.
   */
  public static final void checkPermissions()
  {
    checkPermissions(Permission.ALL);
  }

  /**
   * Query the <code>SecurityManager</code> agains the given
   * <code>Permission</code>.
   *
   * @param permission
   */
  public static final void checkPermissions(Permission permission)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      {
        String perm = null;
        switch (permission)
          {
          case PLAY:
            perm = "play";
            break;

          case RECORD:
            perm = "record";
            break;

          case ALL: default:
            perm = "*";
            break;
          }

        sm.checkPermission(new AudioPermission(perm));
      }
  }
}
