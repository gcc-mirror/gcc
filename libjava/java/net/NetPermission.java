/* NetPermission.java -- A class for basic miscellaneous network permission
   Copyright (C) 1998, 2000 Free Software Foundation, Inc.

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

package java.net;

import java.security.BasicPermission;

/**
 * This class is used to model miscellaneous network permissions.  It is
 * a subclass of BasicPermission.  This means that it models a "boolean"
 * permission.  One that you either have or do not have.  Thus there is
 * no permitted action list associated with this object. 
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public final class NetPermission extends BasicPermission
  implements java.io.Serializable
{
  /**
   * Initializes a new instance of <code>NetPermission</code> with the
   * specified name.
   *
   * @param name The name of this permission.
   */
  public NetPermission(String name)
  {
    super(name);
  }

  /**
   * Initializes a new instance of <code>NetPermission</code> with the 
   * specified name and value.  Note that the value field is irrelevant and is 
   * ignored.  This constructor should never need to be used.
   *
   * @param name The name of this permission
   * @param perms The permitted actions of this permission (ignored)
   */
  public NetPermission(String name, String perms)
  {
    super(name);
  }
}
