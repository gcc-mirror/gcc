/* MemoryManagerMXBeanImpl.java - Implementation of a memory manager bean
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

import java.lang.management.MemoryManagerMXBean;

import javax.management.NotCompliantMBeanException;

/**
 * Provides access to information about one of the memory
 * managers used by the current invocation of the
 * virtual machine.  An instance of this bean for each memory
 * manager is obtained by calling
 * {@link ManagementFactory#getMemoryPoolMXBeans()}.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class MemoryManagerMXBeanImpl
  extends BeanImpl
  implements MemoryManagerMXBean
{

  /**
   * The name of the memory manager.
   */
  protected String name;

  /**
   * Constructs a new <code>MemoryManagerMXBeanImpl</code>.
   *
   * @param name the name of the manager this bean represents.
   * @throws NotCompliantMBeanException if this class doesn't implement
   *                                    the interface or a method appears
   *                                    in the interface that doesn't comply
   *                                    with the naming conventions.
   */
  public MemoryManagerMXBeanImpl(String name)
    throws NotCompliantMBeanException
  {
    this(name, MemoryManagerMXBean.class);
  }

  /**
   * Constructs a new <code>MemoryManagerMXBeanImpl</code>
   * implementing the specified bean interface.
   *
   * @param name the name of the manager this bean represents.
   * @param iface the bean interface being implemented.
   * @throws NotCompliantMBeanException if this class doesn't implement
   *                                    the interface or a method appears
   *                                    in the interface that doesn't comply
   *                                    with the naming conventions.
   */
  protected MemoryManagerMXBeanImpl(String name, Class iface)
    throws NotCompliantMBeanException
  {
    super(iface);
    this.name = name;
  }

  public String[] getMemoryPoolNames()
  {
    return VMMemoryManagerMXBeanImpl.getMemoryPoolNames(name);
  }

  public String getName()
  {
    return name;
  }

  public boolean isValid()
  {
    return VMMemoryManagerMXBeanImpl.isValid(name);
  }

}
