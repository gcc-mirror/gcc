/* RuntimePermission.java
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


package java.lang;

import java.security.*;

/**
 * A <code>RuntimePermission</code> contains a permission name, but no
 * actions list.  This means you either have the permission or you don't.
 *
 * Permission names have the follow the hierarchial property naming
 * convention.  In addition, an asterisk may appear at the end of a 
 * name if following a period or by itself.
 *
 * <table border=1>
 * <tr><th>Valid names</th><th>Invalid names</th></tr>
 * <tr><td>"accessClassInPackage.*","*"</td>
 * <td>"**", "*x", "*.a"</td></tr>
 * </table>
 * <br> 
 * 
 * The following table provides a list of all the possible RuntimePermission
 * permission names with a description of what that permission allows.
 * <br>
 * <table border=1>
 * <tr><th>Permission Name</th><th>Permission Allows</th></tr>
 * <tr>
 *   <td><code>createClassLoader</code></td>
 *   <td>creation of a class loader</td></tr>
 * <tr>
 *   <td><code>getClassLoader</code></td>
 *   <td>retrieval of the class loader for the calling class</td></tr>
 * <tr>
 *   <td><code>setContextClassLoader</code></td>
 *   <td>allows the setting of the context class loader used by a 
 *       thread including system threads</td></tr>
 * <tr>
 *   <td><code>setSecurityManager</code></td>
 *   <td>allows the application to replace the security manager with
 *       another, possibly less restrictive one.</td></tr>
 * <tr>
 *   <td><code>createSecurityManager</code></td>
 *   <td>allows the application to create a new security manager</td></tr>
 * <tr>
 *   <td><code>exitVM</code></td>
 *   <td>allows the application to halt the virtual machine</td></tr>
 * <tr>
 *   <td><code>setFactory</code></td>
 *   <td>allows the application to set the socket factory for socket, 
 *       server socket, stream handler, or RMI socket factory.</td></tr>
 * <tr>
 *   <td><code>setIO</code></td>
 *   <td>allows the application to set System.out, System.in, and 
 *       System.err</td></tr>
 * <tr>
 *   <td><code>modifyThread</code></td>
 *   <td>allows the application to modify any thread in the virtual machine
 *       using any of the methods <code>stop</code>, <code>resume</code>,
 *       <code>suspend</code>, <code>setPriority</code>, and 
 *       <code>setName</code> of classs <code>Thread</code></td></tr>
 * <tr>
 *   <td><code>stopThread</code></td>
 *   <td>allows the application to <code>stop</code> any thread it has
 *       access to in the system</td></tr>
 * <tr>
 *   <td><code>modifyThreadGroup</td>
 *   <td>allows the application to modify thread groups using any of the
 *       methods <code>destroy</code>, <code>resume</code>, 
 *       <code>setDaemon</code>, <code>setMaxPriority</code>, 
 *       <code>stop</code>, and <code>suspend</code> of the class
 *       <code>ThreadGroup</code></td></tr>
 * <tr>
 *   <td><code>getProtectionDomain</code></td>
 *   <td></td></tr>
 * <tr>
 *   <td><code>readFileDescriptor</code></td>
 *   <td></td></tr>
 * <tr>
 *   <td><code>writeFileDescriptor</code</td>
 *   <td></td></tr>
 * <tr>
 *   <td><code>loadLibrary.{library name}</code></td>
 *   <td></td></tr>
 * <tr>
 *   <td><code>accessClassInPackage.{package name}</code></td>
 *   <td></td></tr>
 * <tr>
 *   <td><code>defineClassInPackage.{package name}</code></td>
 *   <td></td></tr>
 * <tr>
 *   <td><code>accessDeclaredMembers</code></td>
 *   <td></td></tr>
 * <tr>
 *   <td><code>queuePrintJob</code></td>
 *   <td></td></tr>
 * </table>
 * 
 * @since JDK 1.2
 * 
 * @author Brian Jones
 */
public final class RuntimePermission extends java.security.BasicPermission
{
  /**
   * 
   * @param permissionName the name of the granted permission
   * 
   * @throws IllegalArgumentException thrown if the name contains an invalid 
   * wildcard character
   */
  public RuntimePermission(String permissionName)
    {
      this(permissionName, null);
    }

  /**
   *
   * @param permissionName the name of the granted permission
   * @param actions this should always be null
   * 
   * @throws IllegalArgumentException throw if the name contains an invalid
   * wildcard character
   */
  public RuntimePermission(String permissionName, String actions)
    {
      super(permissionName, actions);
    }
}
