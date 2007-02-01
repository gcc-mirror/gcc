/* java.lang.Object - The universal superclass in Java
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2004, 2007
   Free Software Foundation, Inc.

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


package java.lang;

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * plus gcj compiler sources (to determine object layout)
 * Status:  Complete to version 1.1
 */

/**
 * Object is the ultimate superclass of every class
 * (excepting interfaces).  When you define a class that
 * does not extend any other class, it implicitly extends
 * java.lang.Object.  Also, an anonymous class based on
 * an interface will extend Object.
 *
 * <p>It provides general-purpose methods that every single
 * Object, regardless of race, sex or creed, implements.
 * All of the public methods may be invoked on arrays or
 * interfaces.  The protected methods <code>clone</code>
 * and <code>finalize</code> are not accessible on arrays
 * or interfaces, but all array types have a public version
 * of <code>clone</code> which is accessible.
 *
 * @author John Keiser
 * @author Eric Blake (ebb9@email.byu.edu)
 * @author Tom Tromey (tromey@cygnus.com)
 */
public class Object
{
  /**
   * Called on an object by the Virtual Machine at most once,
   * at some point after the Object is determined unreachable
   * but before it is destroyed. You would think that this
   * means it eventually is called on every Object, but this is
   * not necessarily the case.  If execution terminates
   * abnormally, garbage collection does not always happen.
   * Thus you cannot rely on this method to always work.
   * For finer control over garbage collection, use references
   * from the {@link java.lang.ref} package.
   *
   * <p>Virtual Machines are free to not call this method if
   * they can determine that it does nothing important; for
   * example, if your class extends Object and overrides
   * finalize to do simply <code>super.finalize()</code>.
   *
   * <p>finalize() will be called by a {@link Thread} that has no
   * locks on any Objects, and may be called concurrently.
   * There are no guarantees on the order in which multiple
   * objects are finalized.  This means that finalize() is
   * usually unsuited for performing actions that must be
   * thread-safe, and that your implementation must be
   * use defensive programming if it is to always work.
   *
   * <p>If an Exception is thrown from finalize() during garbage
   * collection, it will be patently ignored and the Object will
   * still be destroyed.
   *
   * <p>It is allowed, although not typical, for user code to call
   * finalize() directly.  User invocation does not affect whether
   * automatic invocation will occur.  It is also permitted,
   * although not recommended, for a finalize() method to "revive"
   * an object by making it reachable from normal code again.
   *
   * <p>Unlike constructors, finalize() does not get called
   * for an object's superclass unless the implementation
   * specifically calls <code>super.finalize()</code>.
   *
   * <p>The default implementation does nothing.
   *
   * @throws Throwable permits a subclass to throw anything in an
   *         overridden version; but the default throws nothing
   * @see System#gc()
   * @see System#runFinalizersOnExit(boolean)
   * @see java.lang.ref
   */
  // This must come first.  See _JvObjectPrefix in Object.h.
  protected void finalize () throws Throwable
  {
  }

  /**
   * Returns the runtime {@link Class} of this Object.
   *
   * <p>The class object can also be obtained without a runtime
   * instance by using the class literal, as in:
   * <code>Foo.class</code>.  Notice that the class literal
   * also works on primitive types, making it useful for
   * reflection purposes.
   *
   * @return the class of this Object
   */
  public final native Class<? extends Object> getClass();

  /**
   * Get a value that represents this Object, as uniquely as
   * possible within the confines of an int.
   *
   * <p>There are some requirements on this method which
   * subclasses must follow:<br>
   *
   * <ul>
   * <li>Semantic equality implies identical hashcodes.  In other
   *     words, if <code>a.equals(b)</code> is true, then
   *     <code>a.hashCode() == b.hashCode()</code> must be as well.
   *     However, the reverse is not necessarily true, and two
   *     objects may have the same hashcode without being equal.</li>
   * <li>It must be consistent.  Whichever value o.hashCode()
   *     returns on the first invocation must be the value
   *     returned on all later invocations as long as the object
   *     exists.  Notice, however, that the result of hashCode may
   *     change between separate executions of a Virtual Machine,
   *     because it is not invoked on the same object.</li>
   * </ul>
   *
   * <p>Notice that since <code>hashCode</code> is used in
   * {@link java.util.Hashtable} and other hashing classes,
   * a poor implementation will degrade the performance of hashing
   * (so don't blindly implement it as returning a constant!). Also,
   * if calculating the hash is time-consuming, a class may consider
   * caching the results.
   *
   * <p>The default implementation returns
   * <code>System.identityHashCode(this)</code>
   *
   * @return the hash code for this Object
   * @see #equals(Object)
   * @see System#identityHashCode(Object)
   */
  public native int hashCode();

  /**
   * Wakes up one of the {@link Thread}s that has called
   * <code>wait</code> on this Object.  Only the owner
   * of a lock on this Object may call this method.  This lock
   * is obtained by a <code>synchronized</code> method or statement.
   *
   * <p>The Thread to wake up is chosen arbitrarily.  The
   * awakened thread is not guaranteed to be the next thread
   * to actually obtain the lock on this object.
   *
   * <p>This thread still holds a lock on the object, so it is
   * typical to release the lock by exiting the synchronized
   * code, calling wait(), or calling {@link Thread#sleep()}, so
   * that the newly awakened thread can actually resume.  The
   * awakened thread will most likely be awakened with an
   * {@link InterruptedException}, but that is not guaranteed.
   *
   * @throws IllegalMonitorStateException if this Thread
   *         does not own the lock on the Object
   * @see #notifyAll()
   * @see #wait()
   * @see #wait(long)
   * @see #wait(long, int)
   * @see Thread
   */
  public final native void notify();
  
  /**
   * Wakes up all of the {@link Thread}s that have called
   * <code>wait</code> on this Object.  Only the owner
   * of a lock on this Object may call this method.  This lock
   * is obtained by a <code>synchronized</code> method or statement.
   *
   * <p>There are no guarantees as to which thread will next
   * obtain the lock on the object.
   *
   * <p>This thread still holds a lock on the object, so it is
   * typical to release the lock by exiting the synchronized
   * code, calling wait(), or calling {@link Thread#sleep()}, so
   * that one of the newly awakened threads can actually resume.
   * The resuming thread will most likely be awakened with an
   * {@link InterruptedException}, but that is not guaranteed.
   *
   * @throws IllegalMonitorStateException if this Thread
   *         does not own the lock on the Object
   * @see #notify()
   * @see #wait()
   * @see #wait(long)
   * @see #wait(long, int)
   * @see Thread
   */
  public final native void notifyAll();

  /**
   * Waits a specified amount of time (or indefinitely if
   * the time specified is 0) for someone to call notify()
   * or notifyAll() on this Object, waking up this Thread.
   *
   * <p>The Thread that calls wait must have a lock on this Object,
   * obtained by a <code>synchronized</code> method or statement.
   * After calling wait, the thread loses the lock on this
   * object until the method completes (abruptly or normally),
   * at which time it regains the lock.  All locks held on
   * other objects remain in force, even though the thread is
   * inactive. Therefore, caution must be used to avoid deadlock.
   *
   * <p>Usually, this call will complete normally if the time
   * expires, or abruptly with {@link InterruptedException}
   * if another thread called notify, but neither result
   * is guaranteed.
   *
   * <p>The waiting period is nowhere near as precise as
   * nanoseconds; considering that even wait(int) is inaccurate,
   * how much can you expect?  But on supporting
   * implementations, this offers somewhat more granularity
   * than milliseconds.
   *
   * @param ms the number of milliseconds to wait (1,000
   *        milliseconds = 1 second)
   * @param ns the number of nanoseconds to wait over and
   *        above ms (1,000,000 nanoseconds = 1 millisecond)
   * @throws IllegalArgumentException if ms &lt; 0 or ns is not
   *         in the range 0 to 999,999
   * @throws IllegalMonitorStateException if this Thread
   *         does not own a lock on this Object
   * @throws InterruptedException if some other Thread
   *         interrupts this Thread
   * @see #notify()
   * @see #notifyAll()
   * @see #wait()
   * @see #wait(long)
   * @see Thread
   */
  public final native void wait(long timeout, int nanos)
    throws InterruptedException;

  /**
   * Determine whether this Object is semantically equal
   * to another Object.
   *
   * <p>There are some fairly strict requirements on this
   * method which subclasses must follow:<br>
   * <ul>
   * <li>It must be transitive.  If <code>a.equals(b)</code> and
   *     <code>b.equals(c)</code>, then <code>a.equals(c)</code>
   *     must be true as well.</li>
   * <li>It must be symmetric.  <code>a.equals(b)</code> and
   *     <code>b.equals(a)</code> must have the same value.</li>
   * <li>It must be reflexive.  <code>a.equals(a)</code> must
   *     always be true.</li>
   * <li>It must be consistent.  Whichever value a.equals(b)
   *     returns on the first invocation must be the value
   *     returned on all later invocations.</li>
   * <li><code>a.equals(null)</code> must be false.</li>
   * <li>It must be consistent with hashCode().  That is,
   *     <code>a.equals(b)</code> must imply
   *     <code>a.hashCode() == b.hashCode()</code>.
   *     The reverse is not true; two objects that are not
   *     equal may have the same hashcode, but that has
   *     the potential to harm hashing performance.</li>
   * </ul>
   *
   * <p>This is typically overridden to throw a {@link ClassCastException}
   * if the argument is not comparable to the class performing
   * the comparison, but that is not a requirement.  It is legal
   * for <code>a.equals(b)</code> to be true even though
   * <code>a.getClass() != b.getClass()</code>.  Also, it
   * is typical to never cause a {@link NullPointerException}.
   *
   * <p>In general, the Collections API ({@link java.util}) use the
   * <code>equals</code> method rather than the <code>==</code>
   * operator to compare objects.  However, {@link java.util.IdentityHashMap}
   * is an exception to this rule, for its own good reasons.
   *
   * <p>The default implementation returns <code>this == o</code>.
   *
   * @param obj the Object to compare to
   * @return whether this Object is semantically equal to another
   * @see #hashCode()
   */
  public boolean equals(Object obj)
  {
    return this == obj;
  }

  /**
   * The basic constructor.  Object is special, because it has no
   * superclass, so there is no call to super().
   *
   * @throws OutOfMemoryError Technically, this constructor never
   *         throws an OutOfMemoryError, because the memory has
   *         already been allocated by this point.  But as all
   *         instance creation expressions eventually trace back
   *         to this constructor, and creating an object allocates
   *         memory, we list that possibility here.
   */
  public Object()
  {
  }

  /**
   * Convert this Object to a human-readable String.
   * There are no limits placed on how long this String
   * should be or what it should contain.  We suggest you
   * make it as intuitive as possible to be able to place
   * it into {@link java.io.PrintStream#println() System.out.println()}
   * and such.
   *
   * <p>It is typical, but not required, to ensure that this method
   * never completes abruptly with a {@link RuntimeException}.
   *
   * <p>This method will be called when performing string
   * concatenation with this object.  If the result is
   * <code>null</code>, string concatenation will instead
   * use <code>"null"</code>.
   *
   * <p>The default implementation returns
   * <code>getClass().getName() + "@" +
   *      Integer.toHexString(hashCode())</code>.
   *
   * @return the String representing this Object, which may be null
   * @throws OutOfMemoryError The default implementation creates a new
   *         String object, therefore it must allocate memory
   * @see #getClass()
   * @see #hashCode()
   * @see Class#getName()
   * @see Integer#toHexString(int)
   */
  public String toString()
  {
    return getClass().getName() + '@' + Integer.toHexString(hashCode());
  }

  /**
   * Waits indefinitely for notify() or notifyAll() to be
   * called on the Object in question.  Implementation is
   * identical to wait(0).
   *
   * <p>The Thread that calls wait must have a lock on this Object,
   * obtained by a <code>synchronized</code> method or statement.
   * After calling wait, the thread loses the lock on this
   * object until the method completes (abruptly or normally),
   * at which time it regains the lock.  All locks held on
   * other objects remain in force, even though the thread is
   * inactive. Therefore, caution must be used to avoid deadlock.
   *
   * <p>While it is typical that this method will complete abruptly
   * with an {@link InterruptedException}, it is not guaranteed.  So,
   * it is typical to call wait inside an infinite loop:<br>
   *
   * <pre>
   * try
   *   {
   *     while (true)
   *       lock.wait();
   *   }
   * catch (InterruptedException e)
   *   {
   *   }
   * </pre>
   *
   * @throws IllegalMonitorStateException if this Thread
   *         does not own a lock on this Object
   * @throws InterruptedException if some other Thread
   *         interrupts this Thread
   * @see #notify()
   * @see #notifyAll()
   * @see #wait(long)
   * @see #wait(long, int)
   * @see Thread
   */
  public final void wait() throws InterruptedException
  {
    wait(0, 0);
  }

  /**
   * Waits a specified amount of time (or indefinitely if
   * the time specified is 0) for someone to call notify()
   * or notifyAll() on this Object, waking up this Thread.
   *
   * <p>The Thread that calls wait must have a lock on this Object,
   * obtained by a <code>synchronized</code> method or statement.
   * After calling wait, the thread loses the lock on this
   * object until the method completes (abruptly or normally),
   * at which time it regains the lock.  All locks held on
   * other objects remain in force, even though the thread is
   * inactive. Therefore, caution must be used to avoid deadlock.
   *
   * <p>Usually, this call will complete normally if the time
   * expires, or abruptly with {@link InterruptedException}
   * if another thread called notify, but neither result
   * is guaranteed.
   *
   * <p>The waiting period is only *roughly* the amount of time
   * you requested.  It cannot be exact because of the overhead
   * of the call itself.  Most Virtual Machiness treat the
   * argument as a lower limit on the time spent waiting, but
   * even that is not guaranteed.  Besides, some other thread
   * may hold the lock on the object when the time expires, so
   * the current thread may still have to wait to reobtain the
   * lock.
   *
   * @param timeout the minimum number of milliseconds to wait (1000
   *        milliseconds = 1 second), or 0 for an indefinite wait
   * @throws IllegalArgumentException if ms &lt; 0
   * @throws IllegalMonitorStateException if this Thread
   *         does not own a lock on this Object
   * @throws InterruptedException if some other Thread
   *         interrupts this Thread
   * @see #notify()
   * @see #notifyAll()
   * @see #wait()
   * @see #wait(long, int)
   * @see Thread
   */
  public final void wait(long timeout) throws InterruptedException
  {
    wait(timeout, 0);
  }

  /**
   * This method may be called to create a new copy of the
   * Object.  The typical behavior is as follows:<br>
   * <ul>
   *  <li><code>o == o.clone()</code> is false</li>
   *  <li><code>o.getClass() == o.clone().getClass()</code>
   *      is true</li>
   *  <li><code>o.equals(o)</code> is true</li>
   * </ul>
   *
   * <p>However, these are not strict requirements, and may
   * be violated if necessary.  Of the three requirements, the
   * last is the most commonly violated, particularly if the
   * subclass does not override {@link #equals(Object)}.
   *
   * <p>If the Object you call clone() on does not implement
   * {@link Cloneable} (which is a placeholder interface), then
   * a CloneNotSupportedException is thrown.  Notice that
   * Object does not implement Cloneable; this method exists
   * as a convenience for subclasses that do.
   *
   * <p>Object's implementation of clone allocates space for the
   * new Object using the correct class, without calling any
   * constructors, and then fills in all of the new field values
   * with the old field values.  Thus, it is a shallow copy.
   * However, subclasses are permitted to make a deep copy.
   *
   * <p>All array types implement Cloneable, and override
   * this method as follows (it should never fail):<br>
   * <pre>
   * public Object clone()
   * {
   *   try
   *     {
   *       super.clone();
   *     }
   *   catch (CloneNotSupportedException e)
   *     {
   *       throw new InternalError(e.getMessage());
   *     }
   * }
   * </pre>
   *
   * @return a copy of the Object
   * @throws CloneNotSupportedException If this Object does not
   *         implement Cloneable
   * @throws OutOfMemoryError Since cloning involves memory allocation,
   *         even though it may bypass constructors, you might run
   *         out of memory
   * @see Cloneable
   */
  protected native Object clone() throws CloneNotSupportedException;

  // This initializes the sync_info member.  It is here for
  // completeness (some day we'll be able to auto-generate Object.h).
  private final native void sync_init();

  // If we fail to find a method at class loading time we put the
  // vtable index of this method in its place: any attempt to call
  // that method will result in an error.
  void throwNoSuchMethodError()
  {
    throw new NoSuchMethodError("in " + getClass());
  }

  // Note that we don't mention the sync_info field here.  If we do,
  // jc1 will not work correctly.
}
