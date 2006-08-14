/* ManagementFactory.java - Factory for obtaining system beans.
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

import gnu.classpath.SystemProperties;

import gnu.java.lang.management.ClassLoadingMXBeanImpl;
import gnu.java.lang.management.CompilationMXBeanImpl;
import gnu.java.lang.management.GarbageCollectorMXBeanImpl;
import gnu.java.lang.management.OperatingSystemMXBeanImpl;
import gnu.java.lang.management.MemoryMXBeanImpl;
import gnu.java.lang.management.MemoryManagerMXBeanImpl;
import gnu.java.lang.management.MemoryPoolMXBeanImpl;
import gnu.java.lang.management.RuntimeMXBeanImpl;
import gnu.java.lang.management.ThreadMXBeanImpl;

import java.util.ArrayList;
import java.util.List;

import javax.management.NotCompliantMBeanException;

/**
 * <p>
 * Provides access to the system's management beans via a series
 * of static methods.  
 * </p>
 * <p>
 * An instance of a system management bean can be obtained by
 * using one of the following methods:
 * </p>
 * <ol>
 * <li>Calling the appropriate static method of this factory.
 * </li>
 * </ol>
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class ManagementFactory
{

  /**
   * The operating system management bean.
   */
  private static OperatingSystemMXBean osBean;

  /**
   * The runtime management bean.
   */
  private static RuntimeMXBean runtimeBean;

  /**
   * The class loading management bean.
   */
  private static ClassLoadingMXBean classLoadingBean;

  /**
   * The thread bean.
   */
  private static ThreadMXBean threadBean;

  /**
   * The memory bean.
   */
  private static MemoryMXBean memoryBean;

  /**
   * The compilation bean (may remain null).
   */
  private static CompilationMXBean compilationBean;

  /**
   * Private constructor to prevent instance creation.
   */
  private ManagementFactory() {}

  /**
   * Returns the operating system management bean for the
   * operating system on which the virtual machine is running.
   *
   * @return an instance of {@link OperatingSystemMXBean} for
   *         the underlying operating system.
   */
  public static OperatingSystemMXBean getOperatingSystemMXBean()
  {
    if (osBean == null)
      try 
	{
	  osBean = new OperatingSystemMXBeanImpl();
	}
      catch (NotCompliantMBeanException e)
	{
	  throw new InternalError("The GNU implementation of the " +
				  "operating system bean is not a " +
				  "compliant management bean.");
	}
    return osBean;
  }

  /**
   * Returns the runtime management bean for the
   * running virtual machine.
   *
   * @return an instance of {@link RuntimeMXBean} for
   *         this virtual machine.
   */
  public static RuntimeMXBean getRuntimeMXBean()
  {
    if (runtimeBean == null)
      try
	{
	  runtimeBean = new RuntimeMXBeanImpl();
	}
      catch (NotCompliantMBeanException e)
	{
	  throw new InternalError("The GNU implementation of the " +
				  "runtime bean is not a compliant " +
				  "management bean.");
	}
    return runtimeBean;
  }

  /**
   * Returns the class loading management bean for the
   * running virtual machine.
   *
   * @return an instance of {@link ClassLoadingMXBean} for
   *         this virtual machine.
   */
  public static ClassLoadingMXBean getClassLoadingMXBean()
  {
    if (classLoadingBean == null)
      try
	{
	  classLoadingBean = new ClassLoadingMXBeanImpl();
	}
      catch (NotCompliantMBeanException e)
	{
	  throw new InternalError("The GNU implementation of the " +
				  "class loading bean is not a " +
				  "compliant management bean.");
	}
    return classLoadingBean;
  }

  /**
   * Returns the thread management bean for the running
   * virtual machine.
   *
   * @return an instance of {@link ThreadMXBean} for
   *         this virtual machine.
   */
  public static ThreadMXBean getThreadMXBean()
  {
    if (threadBean == null)
      try
	{
	  threadBean = new ThreadMXBeanImpl();
	}
      catch (NotCompliantMBeanException e)
	{
	  throw new InternalError("The GNU implementation of the " +
				  "thread bean is not a compliant " +
				  "management bean.");
	}
    return threadBean;
  }

  /**
   * Returns the memory management bean for the running
   * virtual machine.
   *
   * @return an instance of {@link MemoryMXBean} for
   *         this virtual machine.
   */
  public static MemoryMXBean getMemoryMXBean()
  {
    if (memoryBean == null)
      try
	{
	  memoryBean = new MemoryMXBeanImpl();
	}
      catch (NotCompliantMBeanException e)
	{
	  throw new InternalError("The GNU implementation of the " +
				  "memory bean is not a compliant " +
				  "management bean.");
	}
    return memoryBean;
  }

  /**
   * Returns the compilation bean for the running
   * virtual machine, if supported.  Otherwise,
   * it returns <code>null</code>.
   *
   * @return an instance of {@link CompilationMXBean} for
   *         this virtual machine, or <code>null</code>
   *         if the virtual machine doesn't include
   *         a Just-In-Time (JIT) compiler.
   */
  public static CompilationMXBean getCompilationMXBean()
  {
    if (compilationBean == null &&
	SystemProperties.getProperty("gnu.java.compiler.name") != null)
      try
	{
	  compilationBean = new CompilationMXBeanImpl();
	}
      catch (NotCompliantMBeanException e)
	{
	  throw new InternalError("The GNU implementation of the " +
				  "compilation bean is not a compliant " +
				  "management bean.");
	}
    return compilationBean;
  }

  /**
   * Returns the memory pool beans for the running
   * virtual machine.  These may change during the course
   * of execution.
   *
   * @return a list of memory pool beans, one for each pool.
   */
  public static List getMemoryPoolMXBeans()
  {
    List poolBeans = new ArrayList();
    String[] names = VMManagementFactory.getMemoryPoolNames();
    for (int a = 0; a < names.length; ++a)
      try
	{
	  poolBeans.add(new MemoryPoolMXBeanImpl(names[a]));
	}
      catch (NotCompliantMBeanException e)
	{
	  throw new InternalError("The GNU implementation of the " +
				  "memory pool bean, " + a + ", is " +
				  "not a compliant management bean.");
	}
    return poolBeans;
  }

  /**
   * Returns the memory manager beans for the running
   * virtual machine.  These may change during the course
   * of execution.
   *
   * @return a list of memory manager beans, one for each manager.
   */
  public static List getMemoryManagerMXBeans()
  {
    List managerBeans = new ArrayList();
    String[] names = VMManagementFactory.getMemoryManagerNames();
    for (int a = 0; a < names.length; ++a)
      try
	{
	  managerBeans.add(new MemoryManagerMXBeanImpl(names[a]));
	}
      catch (NotCompliantMBeanException e)
	{
	  throw new InternalError("The GNU implementation of the " +
				  "memory manager bean, " + a + ", is " +
				  "not a compliant management bean.");
	}
    managerBeans.addAll(getGarbageCollectorMXBeans());
    return managerBeans;
  }

  /**
   * Returns the garbage collector beans for the running
   * virtual machine.  These may change during the course
   * of execution.
   *
   * @return a list of garbage collector beans, one for each pool.
   */
  public static List getGarbageCollectorMXBeans()
  {
    List gcBeans = new ArrayList();
    String[] names = VMManagementFactory.getGarbageCollectorNames();
    for (int a = 0; a < names.length; ++a)
      try
	{
	  gcBeans.add(new GarbageCollectorMXBeanImpl(names[a]));
	}
      catch (NotCompliantMBeanException e)
	{
	  throw new InternalError("The GNU implementation of the " +
				  "garbage collector bean, " + a + 
				  ", is not a compliant management " +
				  "bean.");
	}
    return gcBeans;
  }

}
