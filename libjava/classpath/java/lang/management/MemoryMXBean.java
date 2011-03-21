/* MemoryMXBean.java - Interface for a memory bean
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
 * <p>
 * Provides access to information about the memory used
 * by the virtual machine.  An instance of this bean is
 * obtained by calling
 * {@link ManagementFactory#getMemoryMXBean()}.
 * </p>
 * <p>
 * The Java virtual machine uses two types of memory:
 * heap memory and non-heap memory.  The heap is the
 * storage location for class and array instances, and is
 * thus the main source of memory associated with running
 * Java programs.  The heap is created when the virtual
 * machine is started, and is periodically scanned by the
 * garbage collector(s), in order to reclaim memory which
 * is no longer used (e.g. because an object reference has
 * gone out of scope).
 * </p>
 * <p>
 * Non-heap memory is used by the virtual machine in order to
 * perform its duties.  Thus, it mainly acts as the storage
 * location for structures created as a result of parsing Java
 * bytecode, such as the constant pool and constructor/method
 * declarations.  When a Just-In-Time (JIT) compiler is in
 * operation, this will use non-heap memory to store compiled
 * bytecode.
 * </p>
 * <p>
 * Both types of memory may be non-contiguous.  During the
 * lifetime of the virtual machine, the size of both may
 * either change (either expanding or contracting) or stay
 * the same.
 * </p>
 * <h2>Notifications</h2>
 * <p>
 * Implementations of this interface also conform to the
 * {@link javax.management.NotificationEmitter} interface,
 * and supply two notifications reflecting memory usage.
 * These notifications occur when a usage threshold is
 * exceeded; for more details of these, see the documentation
 * of {@link MemoryPoolMXBean}.  If threshold monitoring
 * is supported, then a notification will be emitted each time
 * the threshold is crossed.  Another notification will not
 * be emitted unless the usage level has dropped below the
 * threshold again in the meantime.
 * </p>
 * <p>
 * The emitted notifications are instances of
 * {@link javax.management.Notification}, with a type of
 * either
 * {@link java.lang.management.MemoryNotificationInfo#MEMORY_THRESHOLD_EXCEEDED}
 * or
 * {@link java.lang.management.MemoryNotificationInfo#MEMORY_COLLECTION_THRESHOLD_EXCEEDED}
 * (depending on whether the notification refers to the general
 * usage threshold or the garbage collection threshold) and an instance
 * of {@link java.lang.management.MemoryNotificationInfo} contained
 * in the user data section.  This is wrapped inside an instance
 * of {@link javax.management.openmbean.CompositeData}, as explained
 * in the documentation for
 * {@link java.lang.management.MemoryNotificationInfo}.
 * </p>
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public interface MemoryMXBean
{

  /**
   * Instigates a garbage collection cycle.  The virtual
   * machine's garbage collector should make the best
   * attempt it can at reclaiming unused memory.  This
   * is equivalent to invoking {@link java.lang.System.gc()}.
   *
   * @see java.lang.System#gc()
   */
  void gc();

  /**
   * Returns a {@link MemoryUsage} object representing the
   * current state of the heap.  This incorporates various
   * statistics on both the initial and current memory
   * allocations used by the heap.
   *
   * @return a {@link MemoryUsage} object for the heap.
   */
  MemoryUsage getHeapMemoryUsage();

  /**
   * Returns a {@link MemoryUsage} object representing the
   * current state of non-heap memory.  This incorporates
   * various statistics on both the initial and current
   * memory allocations used by non-heap memory..
   *
   * @return a {@link MemoryUsage} object for non-heap
   *         memory.
   */
  MemoryUsage getNonHeapMemoryUsage();

  /**
   * Returns the number of objects which are waiting to
   * be garbage collected (finalized).  An object is
   * finalized when the garbage collector determines that
   * there are no more references to that object are in
   * use.
   *
   * @return the number of objects awaiting finalization.
   */
  int getObjectPendingFinalizationCount();

  /**
   * Returns true if the virtual machine will emit additional
   * information when memory is allocated and deallocated.  The
   * format of the output is left up to the virtual machine.
   *
   * @return true if verbose memory output is on.
   */
  boolean isVerbose();

  /**
   * Turns on or off the emission of additional information
   * when memory is allocated and deallocated.  The format of the
   * output is left up to the virtual machine.  This method
   * may be called by multiple threads concurrently, but there
   * is only one global setting of verbosity that is affected.
   *
   * @param verbose the new setting for verbose memory output.
   * @throws SecurityException if a security manager exists and
   *                           denies ManagementPermission("control").
   */
  void setVerbose(boolean verbose);

}
