/* RuntimePermission.java -- permission for a secure runtime action
   Copyright (C) 1998, 2000, 2002, 2005  Free Software Foundation, Inc.

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


package java.lang;

import java.security.BasicPermission;

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
 * permission names with a description of what that permission allows.<br>
 * <table border=1>
 * <tr><th>Permission Name</th><th>Permission Allows</th><th>Risks</th</tr>
 * <tr>
 *   <td><code>createClassLoader</code></td>
 *   <td>creation of a class loader</td>
 *   <td>a class loader can load rogue classes which bypass all security
 *       permissions</td></tr>
 * <tr>
 *   <td><code>getClassLoader</code></td>
 *   <td>retrieval of the class loader for the calling class</td>
 *   <td>rogue code could load classes not otherwise available</td></tr>
 * <tr>
 *   <td><code>setContextClassLoader</code></td>
 *   <td>allows the setting of the context class loader used by a thread</td>
 *   <td>rogue code could change the context class loader needed by system
 *       threads</td></tr>
 * <tr>
 *   <td><code>setSecurityManager</code></td>
 *   <td>allows the application to replace the security manager</td>
 *   <td>the new manager may be less restrictive, so that rogue code can
 *       bypass existing security checks</td></tr>
 * <tr>
 *   <td><code>createSecurityManager</code></td>
 *   <td>allows the application to create a new security manager</td>
 *   <td>rogue code can use the new security manager to discover information
 *       about the execution stack</td></tr>
 * <tr>
 *   <td><code>exitVM</code></td>
 *   <td>allows the application to halt the virtual machine</td>
 *   <td>rogue code can mount a denial-of-service attack by killing the
 *       virtual machine</td></tr>
 * <tr>
 *   <td><code>shutdownHooks</code></td>
 *   <td>allows registration and modification of shutdown hooks</td>
 *   <td>rogue code can add a hook that interferes with clean
 *       virtual machine shutdown</td></tr>
 * <tr>
 *   <td><code>setFactory</code></td>
 *   <td>allows the application to set the socket factory for socket,
 *       server socket, stream handler, or RMI socket factory.</td>
 *   <td>rogue code can create a rogue network object which mangles or
 *       intercepts data</td></tr>
 * <tr>
 *   <td><code>setIO</code></td>
 *   <td>allows the application to set System.out, System.in, and
 *       System.err</td>
 *   <td>rogue code could sniff user input and intercept or mangle
 *       output</td></tr>
 * <tr>
 *   <td><code>modifyThread</code></td>
 *   <td>allows the application to modify any thread in the virtual machine
 *       using any of the methods <code>stop</code>, <code>resume</code>,
 *       <code>suspend</code>, <code>setPriority</code>, and
 *       <code>setName</code> of classs <code>Thread</code></td>
 *   <td>rogue code could adversely modify system or user threads</td></tr>
 * <tr>
 *   <td><code>stopThread</code></td>
 *   <td>allows the application to <code>stop</code> any thread it has
 *       access to in the system</td>
 *   <td>rogue code can stop arbitrary threads</td></tr>
 * <tr>
 *   <td><code>modifyThreadGroup</code></td>
 *   <td>allows the application to modify thread groups using any of the
 *       methods <code>destroy</code>, <code>resume</code>,
 *       <code>setDaemon</code>, <code>setMaxPriority</code>,
 *       <code>stop</code>, and <code>suspend</code> of the class
 *       <code>ThreadGroup</code></td>
 *   <td>rogue code can mount a denial-of-service attack by changing run
 *       priorities</td></tr>
 * <tr>
 *   <td><code>getProtectionDomain</code></td>
 *   <td>retrieve a class's ProtectionDomain</td>
 *   <td>rogue code can gain information about the security policy, to
 *       prepare a better attack</td></tr>
 * <tr>
 *   <td><code>readFileDescriptor</code></td>
 *   <td>read a file descriptor</td>
 *   <td>rogue code can read sensitive information</td></tr>
 * <tr>
 *   <td><code>writeFileDescriptor</code></td>
 *   <td>write a file descriptor</td>
 *   <td>rogue code can write files, including viruses, and can modify the
 *       virtual machine binary; if not just fill up the disk</td></tr>
 * <tr>
 *   <td><code>loadLibrary.</code><em>library name</em></td>
 *   <td>dynamic linking of the named library</td>
 *   <td>native code can bypass many security checks of pure Java</td></tr>
 * <tr>
 *   <td><code>accessClassInPackage.</code><em>package name</em></td>
 *   <td>access to a package via a ClassLoader</td>
 *   <td>rogue code can access classes not normally available</td></tr>
 * <tr>
 *   <td><code>defineClassInPackage.</code><em>package name</em></td>
 *   <td>define a class inside a given package</td>
 *   <td>rogue code can install rogue classes, including in trusted packages
 *       like java.security or java.lang</td></tr>
 * <tr>
 *   <td><code>accessDeclaredMembers</code></td>
 *   <td>access declared class members via reflection</td>
 *   <td>rogue code can discover information, invoke methods, or modify fields
 *       that are not otherwise available</td></tr>
 * <tr>
 *   <td><code>queuePrintJob</code></td>
 *   <td>initiate a print job</td>
 *   <td>rogue code could make a hard copy of sensitive information, or
 *       simply waste paper</td></tr>
 * </table>
 *
 * @author Brian Jones
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see BasicPermission
 * @see Permission
 * @see SecurityManager
 * @since 1.2
 * @status updated to 1.4
 */
public final class RuntimePermission extends BasicPermission
{
  /**
   * Compatible with JDK 1.2+.
   */
  private static final long serialVersionUID = 7399184964622342223L;

  /**
   * Create a new permission with the specified name.
   *
   * @param permissionName the name of the granted permission
   * @throws NullPointerException if name is null
   * @throws IllegalArgumentException thrown if name is empty or invalid
   */
  public RuntimePermission(String permissionName)
    {
      super(permissionName);
    }

  /**
   * Create a new permission with the specified name. The actions argument
   * is ignored, as runtime permissions have no actions.
   *
   * @param permissionName the name of the granted permission
   * @param actions ignored
   * @throws NullPointerException if name is null
   * @throws IllegalArgumentException thrown if name is empty or invalid
   */
  public RuntimePermission(String permissionName, String actions)
  {
    super(permissionName);
  }
}
