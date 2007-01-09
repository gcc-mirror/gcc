/* VMRuntimeMXBeanImpl.java - VM implementation of an runtime bean
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

package gnu.java.lang.management;

import gnu.classpath.SystemProperties;

import java.net.InetAddress;
import java.net.UnknownHostException;

/**
 * Provides access to information about the virtual machine.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
final class VMRuntimeMXBeanImpl
{

  /**
   * Returns the command-line arguments supplied
   * to the virtual machine, excluding those supplied
   * to <code>main()</code>.
   *
   * @return the command-line arguments.
   */
  static native String[] getInputArguments();

  /**
   * Returns a developer-chosen name for the virtual
   * machine, which may differ over different running
   * instances of the same virtual machine binary.
   * For example, this may include the particular
   * process identifier used by this instance or
   * the host name of the machine on which it is
   * running.  The intention is that this name refers
   * to the precise entity that the other data supplied
   * by the bean refers to, rather than the VM in general.
   *
   * @return the custom name of the VM.
   */
  static String getName()
  {
    String hostName;
    try
      {
	hostName = InetAddress.getLocalHost().getHostName();
      }
    catch (UnknownHostException e)
      {
	hostName = "Unknown host";
      }
    return SystemProperties.getProperty("java.vm.name") + " " +
      SystemProperties.getProperty("java.vm.version") + " [" +
      getPID() + "@" + hostName + "]";
  }

  /**
   * The time in milliseconds at which the virtual
   * machine was started.  This method is only executed
   * once (for efficency), as the value is not expected
   * to change.
   *
   * @return the VM start time.
   */
  static native long getStartTime();

  /**
   * The process identifier of the runtime.
   *
   * @return the PID of the runtime.
   */
  private static native long getPID();

}
