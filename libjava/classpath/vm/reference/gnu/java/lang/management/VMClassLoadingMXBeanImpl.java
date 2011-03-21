/* VMClassLoadingMXBeanImpl.java - VM impl. of a class loading bean
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

/**
 * Provides access to information about the class loading
 * behaviour of the current invocation of the virtual
 * machine.  Instances of this bean are obtained by calling
 * {@link ManagementFactory#getClassLoadingMXBean()}.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
final class VMClassLoadingMXBeanImpl
{

  /**
   * Returns the number of classes currently loaded by
   * the virtual machine.
   *
   * @return the number of loaded classes.
   */
  static native int getLoadedClassCount();

  /**
   * Returns the number of classes that have been unloaded
   * by the virtual machine since it was started.
   *
   * @return the number of unloaded classes.
   */
  static native long getUnloadedClassCount();

  /**
   * Returns true if the virtual machine will emit additional
   * information when classes are loaded and unloaded.  The
   * format of the output is left up to the virtual machine.
   *
   * @return true if verbose class loading output is on.
   */
  static native boolean isVerbose();

  /**
   * Turns on or off the emission of additional information
   * when classes are loaded and unloaded.  The format of the
   * output is left up to the virtual machine.  This method
   * may be called by multiple threads concurrently, but there
   * is only one global setting of verbosity that is affected.
   *
   * @param verbose the new setting for verbose class loading
   *                output.
   */
  static native void setVerbose(boolean verbose);

}
