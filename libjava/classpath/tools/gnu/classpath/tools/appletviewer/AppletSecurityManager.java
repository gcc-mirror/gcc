/* AppletSecurityManager.java -- an applet security manager
   Copyright (C) 2004, 2006  Free Software Foundation, Inc.

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

package gnu.classpath.tools.appletviewer;

import java.io.FilePermission;
import java.net.SocketPermission;
import java.security.Permission;
import java.security.SecurityPermission;
import java.util.PropertyPermission;

class AppletSecurityManager extends SecurityManager
{
  private boolean plugin;

  AppletSecurityManager(boolean plugin)
  {
    this.plugin = plugin;
  }

  public void checkPermission(Permission permission)
  {
    if (permission == null)
      throw new NullPointerException();

    // FIXME: we need to restrict this.
    //
    // libgcj asks for "java.io.FilePermission <<ALL FILES>> execute"
    // to be able to execute "addr2line" to get proper stack traces.
    if (permission instanceof FilePermission)
      return;

    // FIXME: we need to restrict this.
    if (permission instanceof SecurityPermission)
      return;

    // FIXME: is this really needed ?
    if (permission instanceof PropertyPermission)
      return;

    // Needed to allow to access AWT event queue.
    if (permission.getName().equals("accessEventQueue"))
      return;

    // Needed to create a class loader for each codebase.
    if (permission.getName().equals("createClassLoader"))
      return;

    // FIXME: we need to allow access to codebase here.

    if (permission instanceof SocketPermission      // for net access
        || permission instanceof RuntimePermission) // for checkWrite(FileDescriptor)
      return;

    if (! plugin && permission.getName().equals("exitVM"))
      return;

    // Reject all other permissions.
    throw new SecurityException();
  }
}
