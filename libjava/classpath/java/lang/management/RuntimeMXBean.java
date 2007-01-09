/* RuntimeMXBean.java - Interface for a runtime bean
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

import java.util.List;
import java.util.Map;

/**
 * Provides access to information about the underlying virtual
 * machine.  An instance of this bean is obtained by calling
 * {@link ManagementFactory#getRuntimeMXBean()}.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public interface RuntimeMXBean
{

  /**
   * <p>
   * Returns the boot classpath used by the virtual machine.  This
   * value follows the standard path syntax used by the underlying
   * operating system (e.g. directories separated by ':' on UNIX
   * or ';' on Windows). 
   * </p>
   * <p>
   * Supplying this value is optional.  Users should check the
   * return value of {@link isBootClassPathSupported()} prior to
   * calling this method.
   * </p>
   *
   * @return the boot classpath of the virtual machine, if supported.
   * @throws UnsupportedOperationException in cases where this
   *                                       functionality is not
   *                                       supported by the VM.
   * @throws SecurityException if a security manager exists and
   *                           denies ManagementPermission("monitor").
   * @see #isBootClassPathSupported()
   * @see java.lang.management.ManagementPermission
   */
  String getBootClassPath();

  /**
   * Returns the classpath used by the system classloader.  This
   * is equivalent to obtaining the <code>java.class.path</code>
   * property via {@link System#getProperty(String)}.  This value
   * follows the standard path syntax used by the underlying operating
   * system (e.g. directories separated by ':' on UNIX or ';' on
   * Windows). 
   *
   * @return the classpath used by the system class loader.
   * @throws SecurityException if a security manager exists which
   *                           prevents access to the classpath
   *                           property.
   * @see java.lang.System#getProperty(String)
   * @see java.lang.SecurityManager#checkPropertyAccess(String)
   */
  String getClassPath();

  /**
   * Returns a list of the arguments given to the virtual machine,
   * excluding those that apply to the <code>main()</code> method
   * of the class file being executed.  These may not just be those
   * specified at the command line, but may also include arguments
   * from environment variables, configuration files, etc.  All
   * command line arguments may not reach the virtual machine, so
   * these are not included in this list.
   *
   * @return a list of arguments passed to the virtual machine.
   * @throws SecurityException if a security manager exists and
   *                           denies ManagementPermission("monitor").
   * @see java.lang.management.ManagementPermission
   */
  List<String> getInputArguments();

  /**
   * Returns the library path.  This is equivalent to obtaining the
   * <code>java.library.path</code> property via
   * {@link System#getProperty(String)}.  This value follows the
   * standard path syntax used by the underlying operating
   * system (e.g. directories separated by ':' on UNIX or ';' on
   * Windows). 
   *
   * @return the library path.
   * @throws SecurityException if a security manager exists which
   *                           prevents access to the library path
   *                           property.
   * @see java.lang.System#getProperty(String)
   * @see java.lang.SecurityManager#checkPropertyAccess(String)
   */
  String getLibraryPath();

  /**
   * Returns the version of the management specification
   * implemented by the virtual machine.
   *
   * @return the version of the management specification
   *         implemented.
   */
  String getManagementSpecVersion();

  /**
   * Returns the name of this virtual machine.  The content
   * of this property is left up to the developer of the
   * virtual machine.  It may include a number of system
   * attributes and may differ between instances of the
   * same virtual machine (for example, it might include
   * the process identifier or the host name of the machine
   * on which it is running).  The intention is that this
   * name refers to the precise entity that the other data 
   * supplied by this bean refers to, rather than the VM
   * in general.
   *
   * @return the name of this virtual machine.
   */
  String getName();

  /**
   * Returns the specification name of the virtual machine.
   * This is equivalent to obtaining the
   * <code>java.vm.specification.name</code> property via
   * {@link System#getProperty(String)}.  
   *
   * @return the specification name of the VM.
   * @throws SecurityException if a security manager exists which
   *                           prevents access to the VM
   *                           specification name property.
   * @see java.lang.System#getProperty(String)
   * @see java.lang.SecurityManager#checkPropertyAccess(String)
   */
  String getSpecName();

  /**
   * Returns the specification vendor of the virtual machine.
   * This is equivalent to obtaining the
   * <code>java.vm.specification.vendor</code> property via
   * {@link System#getProperty(String)}.  
   *
   * @return the specification vendor of the VM.
   * @throws SecurityException if a security manager exists which
   *                           prevents access to the VM
   *                           specification vendor property.
   * @see java.lang.System#getProperty(String)
   * @see java.lang.SecurityManager#checkPropertyAccess(String)
   */
  String getSpecVendor();

  /**
   * Returns the specification version of the virtual machine.
   * This is equivalent to obtaining the
   * <code>java.vm.specification.version</code> property via
   * {@link System#getProperty(String)}.  
   *
   * @return the specification version of the VM.
   * @throws SecurityException if a security manager exists which
   *                           prevents access to the VM
   *                           specification version property.
   * @see java.lang.System#getProperty(String)
   * @see java.lang.SecurityManager#checkPropertyAccess(String)
   */
  String getSpecVersion();

  /**
   * Returns the approximate start time of the virtual machine
   * in milliseconds.
   * 
   * @return the start time of the virtual machine.
   */
  long getStartTime();

  /**
   * Returns a map containing the keys and values of the system
   * properties.  This gives largely the same result as calling
   * {@link System#getProperties()}, but the resulting map
   * is filtered so as to only provide keys and values that
   * are <code>String</code>s.
   *
   * @return the map of system properties.
   */
  Map<String,String> getSystemProperties();

  /**
   * Returns the uptime of the virtual machine in milliseconds.
   * 
   * @return the uptime of the virtual machine.
   */
  long getUptime();

  /**
   * Returns the implementation name of the virtual machine.
   * This is equivalent to obtaining the
   * <code>java.vm.name</code> property via
   * {@link System#getProperty(String)}.  
   *
   * @return the implementation name of the VM.
   * @throws SecurityException if a security manager exists which
   *                           prevents access to the VM name
   *                           property.
   * @see java.lang.System#getProperty(String)
   * @see java.lang.SecurityManager#checkPropertyAccess(String)
   */
  String getVmName();

  /**
   * Returns the implementation vendor of the virtual machine.
   * This is equivalent to obtaining the
   * <code>java.vm.vendor</code> property via
   * {@link System#getProperty(String)}.  
   *
   * @return the implementation vendor of the VM.
   * @throws SecurityException if a security manager exists which
   *                           prevents access to the VM vendor
   *                           property.
   * @see java.lang.System#getProperty(String)
   * @see java.lang.SecurityManager#checkPropertyAccess(String)
   */
  String getVmVendor();

  /**
   * Returns the implementation version of the virtual machine.
   * This is equivalent to obtaining the
   * <code>java.vm.version</code> property via
   * {@link System#getProperty(String)}.  
   *
   * @return the implementation version of the VM.
   * @throws SecurityException if a security manager exists which
   *                           prevents access to the VM version
   *                           property.
   * @see java.lang.System#getProperty(String)
   * @see java.lang.SecurityManager#checkPropertyAccess(String)
   */
  String getVmVersion();

  /**
   * Returns true if the virtual machine supports the boot classpath
   * mechanism.
   *
   * @return true if the boot classpath property is supported by the
   *         virtual machine.
   */
  boolean isBootClassPathSupported();

}
