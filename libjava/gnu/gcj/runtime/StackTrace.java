/* gnu.gcj.runtime.StackTrace -- VM support methods for walking the
   stack.
   Copyright (C) 1998, 1999, 2002 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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

package gnu.gcj.runtime;

import gnu.gcj.RawData;
import java.util.TreeMap;
import java.util.IdentityHashMap;
import java.util.SortedMap;
import gnu.gcj.runtime.NameFinder;
import java.util.NoSuchElementException;

/**
 * VM dependent state and support methods for walking the stack.
 * <p>
 * This is the version used by libgcj (http://gcc.gnu.org/java/).
 *
 * @author Mark Wielaard (mark@klomp.org)
 * @author Andrew Haley (aph@redhat.com)
 */
public final class StackTrace
{
  /**
   * Fill in the stack trace with the top n frames on current
   * execution stack.  Can return null if the VM does not support
   * capturing the VM execution state.
   *
   * @see Throwable#fillInStackTrace()
   */
  public StackTrace(int n)
  {
    fillInStackTrace(n, 1);
  }

  /**
   * Fill in the stack trace with state of the entire execution stack,
   * starting from frame <code>offset</code>.  Can return null if the
   * VM does not support capturing the VM execution state.
   *
   * This can be very expensive.  If you only want part of the stack,
   * see <code>Throwable.fillInStackTrace(int)</code>
   *
   * @see Throwable#fillInStackTrace()
   */
  public StackTrace()
  {
    int n = 64;
    
    do
      {
	n *= 4;
	fillInStackTrace(n, 1);
      }
    while (len >= n);
  }

  /**
   * Return the class containing the execution point represented by
   * the Nth frame down the stack.  The zeroth frame represents the
   * top of the stack, which is the method that called classAt().
   *
   * If the Nth frame down the stack was not create by a method
   * invocation, return null.
   *
   * It is not necessary to call <code>fillInStackTrace()</code> with
   * a size greater than N before calling this method; if the current
   * stack trace is insufficiently large, it will be expanded as
   * required.  This requires some caution if
   * <code>fillInStackTrace()</code> is called from a different
   * invocation to the one that calls <code>classAt()</code>.
   * classAt() will not call <code>fillInStackTrace()</code> unless N
   * is greater than the current length.
   *
   */
  public native Class classAt(int n);

  /**
   * Return the name of the method containing the execution point
   * represented by the Nth frame down the stack.  The zeroth frame
   * represents the top of the stack, which is the method that called
   * classAt().
   *
   * If the Nth frame down the stack was not create by a method
   * invocation, return null.
   *
   * It is not necessary to call <code>fillInStackTrace()</code> with
   * a size greater than N before calling this method; if the current
   * stack trace is insufficiently large, it will be expanded as
   * required.  This requires some caution if
   * <code>fillInStackTrace()</code> is called from a different
   * invocation to the one that calls <code>classAt()</code>.
   * classAt() will not call <code>fillInStackTrace()</code> unless N
   * is greater than the current length.
   *
   */
  public native String methodAt(int n);

  /**
   * Return the length of this stack trace.
   *
   */
  public int length ()
  {
    return len;
  }

  public static native Class getClass(RawData ip);

  private static native void update();
  private static MethodRef methodAtAddress(RawData addr)
  {
    update();
    synchronized (map)
      {
	return (MethodRef) map.get (addr);
      }
  }

  gnu.gcj.RawData stackTraceAddrs()
  {
    return addrs;
  }
  
  private native void fillInStackTrace(int n, int offset);
  protected native void finalize();

  private static native MethodRef getCompiledMethodRef(RawData addr);
  private static IdentityHashMap map = new IdentityHashMap();

  private gnu.gcj.RawData addrs;
  private int len;
}
