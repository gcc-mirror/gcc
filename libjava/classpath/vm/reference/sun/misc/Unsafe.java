/* Unsafe.java - Unsafe operations needed for concurrency
   Copyright (C) 2006, 2010 Free Software Foundation, Inc.

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

package sun.misc;

import java.lang.reflect.Field;

/**
 * This class should provide access to low-level operations and its
 * use should be limited to trusted code.  Fields can be accessed using
 * memory addresses, with undefined behaviour occurring if invalid memory
 * addresses are given.
 *
 * @author Tom Tromey (tromey@redhat.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 */
public final class Unsafe
{
  // Singleton class.
  private static final Unsafe unsafe = new Unsafe();

  /**
   * Private default constructor to prevent creation of an arbitrary
   * number of instances.
   */
  private Unsafe()
  {
  }

  /**
   * Retrieve the singleton instance of <code>Unsafe</code>.  The calling
   * method should guard this instance from untrusted code, as it provides
   * access to low-level operations such as direct memory access.
   *
   * @throws SecurityException if a security manager exists and prevents
   *                           access to the system properties.
   */
  public static Unsafe getUnsafe()
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPropertiesAccess();
    return unsafe;
  }

  /**
   * Returns the memory address offset of the given static field.
   * The offset is merely used as a means to access a particular field
   * in the other methods of this class.  The value is unique to the given
   * field and the same value should be returned on each subsequent call.
   *
   * @param field the field whose offset should be returned.
   * @return the offset of the given field.
   */
  public native long objectFieldOffset(Field field);

  /**
   * Compares the value of the integer field at the specified offset
   * in the supplied object with the given expected value, and updates
   * it if they match.  The operation of this method should be atomic,
   * thus providing an uninterruptible way of updating an integer field.
   *
   * @param obj the object containing the field to modify.
   * @param offset the offset of the integer field within <code>obj</code>.
   * @param expect the expected value of the field.
   * @param update the new value of the field if it equals <code>expect</code>.
   * @return true if the field was changed.
   */
  public native boolean compareAndSwapInt(Object obj, long offset,
                                          int expect, int update);

  /**
   * Compares the value of the long field at the specified offset
   * in the supplied object with the given expected value, and updates
   * it if they match.  The operation of this method should be atomic,
   * thus providing an uninterruptible way of updating a long field.
   *
   * @param obj the object containing the field to modify.
   * @param offset the offset of the long field within <code>obj</code>.
   * @param expect the expected value of the field.
   * @param update the new value of the field if it equals <code>expect</code>.
   * @return true if the field was changed.
   */
  public native boolean compareAndSwapLong(Object obj, long offset,
                                           long expect, long update);

  /**
   * Compares the value of the object field at the specified offset
   * in the supplied object with the given expected value, and updates
   * it if they match.  The operation of this method should be atomic,
   * thus providing an uninterruptible way of updating an object field.
   *
   * @param obj the object containing the field to modify.
   * @param offset the offset of the object field within <code>obj</code>.
   * @param expect the expected value of the field.
   * @param update the new value of the field if it equals <code>expect</code>.
   * @return true if the field was changed.
   */
  public native boolean compareAndSwapObject(Object obj, long offset,
                                             Object expect, Object update);

  /**
   * Sets the value of the integer field at the specified offset in the
   * supplied object to the given value.  This is an ordered or lazy
   * version of <code>putIntVolatile(Object,long,int)</code>, which
   * doesn't guarantee the immediate visibility of the change to other
   * threads.  It is only really useful where the integer field is
   * <code>volatile</code>, and is thus expected to change unexpectedly.
   *
   * @param obj the object containing the field to modify.
   * @param offset the offset of the integer field within <code>obj</code>.
   * @param value the new value of the field.
   * @see #putIntVolatile(Object,long,int)
   */
  public native void putOrderedInt(Object obj, long offset, int value);

  /**
   * Sets the value of the long field at the specified offset in the
   * supplied object to the given value.  This is an ordered or lazy
   * version of <code>putLongVolatile(Object,long,long)</code>, which
   * doesn't guarantee the immediate visibility of the change to other
   * threads.  It is only really useful where the long field is
   * <code>volatile</code>, and is thus expected to change unexpectedly.
   *
   * @param obj the object containing the field to modify.
   * @param offset the offset of the long field within <code>obj</code>.
   * @param value the new value of the field.
   * @see #putLongVolatile(Object,long,long)
   */
  public native void putOrderedLong(Object obj, long offset, long value);

  /**
   * Sets the value of the object field at the specified offset in the
   * supplied object to the given value.  This is an ordered or lazy
   * version of <code>putObjectVolatile(Object,long,Object)</code>, which
   * doesn't guarantee the immediate visibility of the change to other
   * threads.  It is only really useful where the object field is
   * <code>volatile</code>, and is thus expected to change unexpectedly.
   *
   * @param obj the object containing the field to modify.
   * @param offset the offset of the object field within <code>obj</code>.
   * @param value the new value of the field.
   */
  public native void putOrderedObject(Object obj, long offset, Object value);

  /**
   * Sets the value of the integer field at the specified offset in the
   * supplied object to the given value, with volatile store semantics.
   *
   * @param obj the object containing the field to modify.
   * @param offset the offset of the integer field within <code>obj</code>.
   * @param value the new value of the field.
   */
  public native void putIntVolatile(Object obj, long offset, int value);

  /**
   * Retrieves the value of the integer field at the specified offset in the
   * supplied object with volatile load semantics.
   *
   * @param obj the object containing the field to read.
   * @param offset the offset of the integer field within <code>obj</code>.
   */
  public native int getIntVolatile(Object obj, long offset);

  /**
   * Sets the value of the long field at the specified offset in the
   * supplied object to the given value, with volatile store semantics.
   *
   * @param obj the object containing the field to modify.
   * @param offset the offset of the long field within <code>obj</code>.
   * @param value the new value of the field.
   * @see #putLong(Object,long,long)
   */
  public native void putLongVolatile(Object obj, long offset, long value);

  /**
   * Sets the value of the long field at the specified offset in the
   * supplied object to the given value.
   *
   * @param obj the object containing the field to modify.
   * @param offset the offset of the long field within <code>obj</code>.
   * @param value the new value of the field.
   * @see #putLongVolatile(Object,long,long)
   */
  public native void putLong(Object obj, long offset, long value);

  /**
   * Retrieves the value of the long field at the specified offset in the
   * supplied object with volatile load semantics.
   *
   * @param obj the object containing the field to read.
   * @param offset the offset of the long field within <code>obj</code>.
   * @see #getLong(Object,long)
   */
  public native long getLongVolatile(Object obj, long offset);

  /**
   * Retrieves the value of the long field at the specified offset in the
   * supplied object.
   *
   * @param obj the object containing the field to read.
   * @param offset the offset of the long field within <code>obj</code>.
   * @see #getLongVolatile(Object,long)
   */
  public native long getLong(Object obj, long offset);

  /**
   * Sets the value of the object field at the specified offset in the
   * supplied object to the given value, with volatile store semantics.
   *
   * @param obj the object containing the field to modify.
   * @param offset the offset of the object field within <code>obj</code>.
   * @param value the new value of the field.
   * @see #putObject(Object,long,Object)
   */
  public native void putObjectVolatile(Object obj, long offset, Object value);

  /**
   * Sets the value of the object field at the specified offset in the
   * supplied object to the given value.
   *
   * @param obj the object containing the field to modify.
   * @param offset the offset of the object field within <code>obj</code>.
   * @param value the new value of the field.
   * @see #putObjectVolatile(Object,long,Object)
   */
  public native void putObject(Object obj, long offset, Object value);

  /**
   * Retrieves the value of the object field at the specified offset in the
   * supplied object with volatile load semantics.
   *
   * @param obj the object containing the field to read.
   * @param offset the offset of the object field within <code>obj</code>.
   */
  public native Object getObjectVolatile(Object obj, long offset);

  /**
   * Returns the offset of the first element for a given array class.
   * To access elements of the array class, this value may be used along
   * with that returned by
   * <a href="#arrayIndexScale"><code>arrayIndexScale</code></a>,
   * if non-zero.
   *
   * @param arrayClass the class for which the first element's address should
   *                   be obtained.
   * @return the offset of the first element of the array class.
   * @see arrayIndexScale(Class)
   */
  public native int arrayBaseOffset(Class arrayClass);

  /**
   * Returns the scale factor used for addressing elements of the supplied
   * array class.  Where a suitable scale factor can not be returned (e.g.
   * for primitive types), zero should be returned.  The returned value
   * can be used with
   * <a href="#arrayBaseOffset"><code>arrayBaseOffset</code></a>
   * to access elements of the class.
   *
   * @param arrayClass the class whose scale factor should be returned.
   * @return the scale factor, or zero if not supported for this array class.
   */
  public native int arrayIndexScale(Class arrayClass);

  /**
   * Releases the block on a thread created by
   * <a href="#park"><code>park</code></a>.  This method can also be used
   * to terminate a blockage caused by a prior call to <code>park</code>.
   * This operation is unsafe, as the thread must be guaranteed to be
   * live.  This is true of Java, but not native code.
   *
   * @param thread the thread to unblock.
   */
  public native void unpark(Object thread);

  /**
   * Blocks the thread until a matching
   * <a href="#unpark"><code>unpark</code></a> occurs, the thread is
   * interrupted or the optional timeout expires.  If an <code>unpark</code>
   * call has already occurred, this also counts.  A timeout value of zero
   * is defined as no timeout.  When <code>isAbsolute</code> is
   * <code>true</code>, the timeout is in milliseconds relative to the
   * epoch.  Otherwise, the value is the number of nanoseconds which must
   * occur before timeout.  This call may also return spuriously (i.e.
   * for no apparent reason).
   *
   * @param isAbsolute true if the timeout is specified in milliseconds from
   *                   the epoch.
   * @param time either the number of nanoseconds to wait, or a time in
   *             milliseconds from the epoch to wait for.
   */
  public native void park(boolean isAbsolute, long time);

}
