/* ClassLoadingMXBeanImpl.java - Implementation of a class loading bean
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

import java.lang.management.ClassLoadingMXBean;

import javax.management.NotCompliantMBeanException;

/**
 * Provides access to information about the class loading
 * behaviour of the current invocation of the virtual
 * machine.  Instances of this bean are obtained by calling
 * {@link ManagementFactory#getClassLoadingMXBean()}.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public final class ClassLoadingMXBeanImpl
  extends BeanImpl
  implements ClassLoadingMXBean
{

  /**
   * Constructs a new <code>ClassLoadingMXBeanImpl</code>.
   *
   * @throws NotCompliantMBeanException if this class doesn't implement
   *                                    the interface or a method appears
   *                                    in the interface that doesn't comply
   *                                    with the naming conventions.
   */
  public ClassLoadingMXBeanImpl()
    throws NotCompliantMBeanException
  {
    super(ClassLoadingMXBean.class);
  }

  public int getLoadedClassCount()
  {
    return VMClassLoadingMXBeanImpl.getLoadedClassCount();
  }

  public long getTotalLoadedClassCount()
  {
    return getLoadedClassCount() + getUnloadedClassCount();
  }

  public long getUnloadedClassCount()
  {
    return VMClassLoadingMXBeanImpl.getUnloadedClassCount();
  }

  public boolean isVerbose()
  {
    return VMClassLoadingMXBeanImpl.isVerbose();
  }

  public void setVerbose(boolean verbose)
  {
    checkControlPermissions();
    VMClassLoadingMXBeanImpl.setVerbose(verbose);
  }

}
