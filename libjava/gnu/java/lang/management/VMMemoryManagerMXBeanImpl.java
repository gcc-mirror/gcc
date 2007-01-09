/* VMMemoryManagerMXBeanImpl.java - VM interface for a memory manager bean
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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * Provides access to information about the memory managers
 * of the virtual machine.  An instance of this bean for each
 * memory manager is obtained by calling
 * {@link ManagementFactory#getMemoryManagerMXBeans()}.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
final class VMMemoryManagerMXBeanImpl
{

  /** 
   * Returns an array containing the names of the memory pools
   * this memory manager manages.
   * 
   * @param name the name of the memory manager.
   * @return an array containing the name of each memory pool
   *         this manager is responsible for.
   */
  static String[] getMemoryPoolNames(String name)
  {
    List managedPools = new ArrayList();
    Iterator beans = ManagementFactory.getMemoryPoolMXBeans().iterator();
    while (beans.hasNext())
      {
	MemoryPoolMXBean bean = (MemoryPoolMXBean) beans.next();
	String[] managers = bean.getMemoryManagerNames();
	for (int a = 0; a < managers.length; ++a)
	  if (managers[a].equals(name))
	    {
	      managedPools.add(bean.getName());
	      break;
	    }
      }
    return (String[]) managedPools.toArray(new String[managedPools.size()]);
  }

  /**
   * Returns true if this memory manager is still valid.  A memory
   * manager becomes invalid when it is removed by the virtual machine
   * and no longer used.
   *
   * @param name the name of the memory manager.
   * @return true if this memory manager is valid.
   */
  static native boolean isValid(String name);

}
