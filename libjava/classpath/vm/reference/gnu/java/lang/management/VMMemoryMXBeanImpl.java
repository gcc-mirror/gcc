/* VMMemoryMXBeanImpl.java - VM impl. of a memory bean
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

import java.lang.management.ManagementFactory;
import java.lang.management.MemoryPoolMXBean;
import java.lang.management.MemoryType;
import java.lang.management.MemoryUsage;

import java.util.Iterator;
import java.util.List;

/**
 * Provides access to information about the memory
 * management of the current invocation of the virtual
 * machine.  Instances of this bean are obtained by calling
 * {@link ManagementFactory#getMemoryMXBean()}.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
final class VMMemoryMXBeanImpl
{

  /**
   * Returns an instance of {@link java.lang.management.MemoryUsage}
   * with appropriate initial, used, committed and maximum values
   * for the heap.  By default, this uses the methods of
   * {@link java.lang.Runtime} to provide some of the values.
   *
   * @return an {@link java.lang.management.MemoryUsage} instance
   *         for the heap.
   */
  static MemoryUsage getHeapMemoryUsage()
  {
    return getUsage(MemoryType.HEAP);
  }

  /**
   * Returns an instance of {@link java.lang.management.MemoryUsage}
   * with appropriate initial, used, committed and maximum values
   * for non-heap memory.
   *
   * @return an {@link java.lang.management.MemoryUsage} instance
   *         for non-heap memory.
   */
  static MemoryUsage getNonHeapMemoryUsage()
  {
    return getUsage(MemoryType.NON_HEAP);
  }

  /**
   * Returns the number of objects ready to be garbage collected.
   *
   * @return the number of finalizable objects.
   */
  static native int getObjectPendingFinalizationCount();

  /**
   * Returns true if the virtual machine will emit additional
   * information when memory is allocated and deallocated.  The
   * format of the output is left up to the virtual machine.
   *
   * @return true if verbose memory usage output is on.
   */
  static native boolean isVerbose();

  /**
   * Turns on or off the emission of additional information
   * when memory is allocated and deallocated.  The format of the
   * output is left up to the virtual machine.  This method
   * may be called by multiple threads concurrently, but there
   * is only one global setting of verbosity that is affected.
   *
   * @param verbose the new setting for verbose memory usage
   *                output.
   */
  static native void setVerbose(boolean verbose);

  /**
   * Totals the memory usage from all the pools that match
   * the given type.
   *
   * @param type the type of memory pools to accumulate
   *             (heap or non-heap).
   * @return the memory usage overall.
   */
  private static MemoryUsage getUsage(MemoryType type) {
    long init = 0, committed = 0, used = 0, max = 0;
    Iterator pools =
      ManagementFactory.getMemoryPoolMXBeans().iterator();
    while (pools.hasNext())
      {
        MemoryPoolMXBean pool = (MemoryPoolMXBean) pools.next();
        if (pool.getType() == type)
          {
            MemoryUsage usage = pool.getUsage();
            if (init != -1)
              {
                long poolInit = usage.getInit();
                if (poolInit == -1)
                  init = -1;
                else
                  init += poolInit;
              }
            committed += usage.getCommitted();
            used += usage.getUsed();
            if (max != -1)
              {
                long poolMax = usage.getMax();
                if (poolMax == -1)
                  max = -1;
                else
                  max += poolMax;
              }
          }
      }
    return new MemoryUsage(init, used, committed, max);
  }

}
