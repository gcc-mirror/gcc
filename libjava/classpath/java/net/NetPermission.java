/* NetPermission.java -- A class for basic miscellaneous network permission
   Copyright (C) 1998, 2000, 2003 Free Software Foundation, Inc.

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

package java.net;

import java.security.BasicPermission;


/**
 * This class is used to model miscellaneous network permissions.  It is
 * a subclass of <code>BasicPermission</code>.  This means that it models a
 * "boolean" permission.  One that you either have or do not have.  Thus
 * there is no permitted action list associated with this object.
 *
 * The following permission names are defined for this class:
 *
 * <ul>
 * <li>setDefaultAuthenticator - Grants the ability to install a facility
 * to collect username and password information when requested by a
 * web site or proxy server.</li>
 * <li>requestPasswordAuthentication - Grants the ability to ask the
 * authentication facility for the user's password.</li>
 * <li>specifyStreamHandler - Grants the permission to specify the
 * stream handler class used when loading from a URL.</li>
 * </ul>
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public final class NetPermission extends BasicPermission
{
  static final long serialVersionUID = -8343910153355041693L;

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
   * specified name and perms.  Note that the perms field is irrelevant and is
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
