// AWTPermission.java - AWT permissions

/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/**
 * @author Tom Tromey <tromey@redhat.com>
 * @date December 2, 2000
 */

package java.awt;

import java.security.BasicPermission;

/**
 * This class implements permissions for AWT.  This is a named
 * permission.  No actions are defined.
 */
public final class AWTPermission extends BasicPermission
{
  /**
   * Construct a AWTPermission with the given name.
   * @param name The permission name
   */
  public AWTPermission (String name)
  {
    super (name);
  }

  /**
   * Construct a AWTPermission with the given name.
   * @param name The permission name
   * @param actions The actions; this is ignored and should be null.
   */
  public AWTPermission (String name, String actions)
  {
    super (name, actions);
  }
}
