/* OperatingSystemMXBean.java - Interface for an operating system bean
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

/**
 * Provides access to information about the underlying operating
 * system.  An instance of this bean is obtained by calling
 * {@link ManagementFactory#getOperatingSystemMXBean()}.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public interface OperatingSystemMXBean
{

  /**
   * Returns the name of the underlying system architecture.  This
   * is equivalent to obtaining the <code>os.arch</code> property
   * via {@link System#getProperty(String)}.
   *
   * @return the name of the underlying system architecture on which
   *         the VM is running.
   * @throws SecurityException if a security manager exists which
   *                           prevents access to the name property.
   * @see java.lang.System#getProperty(String)
   * @see java.lang.SecurityManager#checkPropertyAccess(String)
   */
  String getArch();

  /**
   * Returns the number of processors currently available to the
   * virtual machine.  This number is subject to change during
   * execution of the virtual machine, and will always be >= 1.
   * The call is equivalent to {@link Runtime#availableProcessors()}.
   *
   * @return the number of processors available to the VM.
   */
  int getAvailableProcessors();

  /**
   * Returns the name of the underlying operating system.  This
   * is equivalent to obtaining the <code>os.name</code> property
   * via {@link System#getProperty(String)}.
   *
   * @return the name of the operating system on which the VM
   *         is running.
   * @throws SecurityException if a security manager exists which
   *                           prevents access to the name property.
   * @see java.lang.System#getProperty(String)
   * @see java.lang.SecurityManager#checkPropertyAccess(String)
   */
  String getName();

  /**
   * Returns the system load average for the last minute, or -1
   * if this is unavailable.  The availability and calculation
   * of the load average is system-dependent, but is usually
   * a damped time-dependent average obtained by monitoring the
   * number of queued and running processes.  It is expected
   * that this method will be called frequently to monitor the
   * average over time, so it may not be implemented on systems
   * where such a call is expensive.
   *
   * @return the system load average for the last minute, or -1
   *         if this is unavailable.
   * @since 1.6
   */
  double getSystemLoadAverage();

  /**
   * Returns the version of the underlying operating system.  This
   * is equivalent to obtaining the <code>os.version</code> property
   * via {@link System#getProperty(String)}.
   *
   * @return the version of the operating system on which the VM
   *         is running.
   * @throws SecurityException if a security manager exists which
   *                           prevents access to the name property.
   * @see java.lang.System#getProperty(String)
   * @see java.lang.SecurityManager#checkPropertyAccess(String)
   */
  String getVersion();

}
