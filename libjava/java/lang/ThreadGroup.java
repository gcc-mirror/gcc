/* java.lang.ThreadGroup
   Copyright (C) 1998, 2000, 2001 Free Software Foundation, Inc.

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
 
package java.lang;

import java.util.Vector;
import java.util.Enumeration;

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 from http://www.javasoft.com.
 * Status:  Complete for 1.2.  Some parts from the JDK 1.0 spec only are
 * not implemented. 
 */
 
/**
 * ThreadGroup allows you to group Threads together.  There is a
 * hierarchy of ThreadGroups, and only the initial ThreadGroup has
 * no parent.  A Thread may access information about its own
 * ThreadGroup, but not its parents or others outside the tree.
 *
 * @author John Keiser
 * @author Tom Tromey
 * @author Bryce McKinlay
 * @version 1.2.0
 * @since JDK1.0
 */

public class ThreadGroup
{
  /* The Initial, top-level ThreadGroup. */
  static ThreadGroup root = new ThreadGroup();
  /* This flag is set if an uncaught exception occurs. The runtime should 
  check this and exit with an error status if it is set. */
  static boolean had_uncaught_exception = false;

  private ThreadGroup parent;
  private String name;
  private Vector threads = new Vector();
  private Vector groups = new Vector();
  private boolean daemon_flag = false;
  private int maxpri = Thread.MAX_PRIORITY;

  private ThreadGroup()
  {
    name = "main";    
  }

  /** Create a new ThreadGroup using the given name and the
   *  current thread's ThreadGroup as a parent.
   *  @param name the name to use for the ThreadGroup.
   */
  public ThreadGroup(String name)
  {
    this (Thread.currentThread().getThreadGroup(), name);
  }

  /** Create a new ThreadGroup using the given name and
   *  parent group.
   *  @param name the name to use for the ThreadGroup.
   *  @param parent the ThreadGroup to use as a parent.
   *  @exception NullPointerException if parent is null.
   *  @exception SecurityException if you cannot change
   *             the intended parent group.
   */
  public ThreadGroup(ThreadGroup parent, String name)
  {
    parent.checkAccess();
    this.parent = parent;
    if (parent.isDestroyed())
      throw new IllegalArgumentException ();
    this.name = name;
    maxpri = parent.maxpri;
    daemon_flag = parent.daemon_flag;
    parent.addGroup(this);
  }

  /** Get the name of this ThreadGroup.
   *  @return the name of this ThreadGroup.
   */
  public final String getName()
  {
    return name;
  }

  /** Get the parent of this ThreadGroup.
   *  @return the parent of this ThreadGroup.
   */
  public final ThreadGroup getParent()
  {
    return parent;
  }

  /** Set the maximum priority for Threads in this ThreadGroup. setMaxPriority
   *  can only be used to reduce the current maximum. If maxpri
   *  is greater than the current Maximum, the current value is not changed.
   *  Calling this does not effect threads already in this ThreadGroup.
   *  @param maxpri the new maximum priority for this ThreadGroup.
   *  @exception SecurityException if you cannoy modify this ThreadGroup.
   */
  public final synchronized void setMaxPriority(int maxpri)
  {
    checkAccess();
    if (maxpri < this.maxpri
        && maxpri >= Thread.MIN_PRIORITY
	&& maxpri <= Thread.MAX_PRIORITY)
      {
	this.maxpri = maxpri;        
      }  
  }

  /** Get the maximum priority of Threads in this ThreadGroup.
   *  @return the maximum priority of Threads in this ThreadGroup.
   */
  public final int getMaxPriority()
  {
    return maxpri;
  }

  /** Set whether this ThreadGroup is a daemon group.  A daemon
   *  group will be destroyed when its last thread is stopped and
   *  its last thread group is destroyed.
   *  @specnote The Java API docs indicate that the group is destroyed
   * 		when either of those happen, but that doesn't make
   * 		sense.
   *  @param daemon whether this ThreadGroup should be a daemon group.
   *  @exception SecurityException if you cannoy modify this ThreadGroup.
   */
  public final void setDaemon (boolean daemon)
  {
    checkAccess();
    daemon_flag = daemon;
  }
   
  /** Tell whether this ThreadGroup is a daemon group.  A daemon
    * group will be destroyed when its last thread is stopped and
    * its last thread group is destroyed.
    * @specnote The Java API docs indicate that the group is destroyed
    *		when either of those happen, but that doesn't make
    *		sense.
    * @return whether this ThreadGroup is a daemon group.
    */
  public final boolean isDaemon()
  {
    return daemon_flag;
  }

  /** Tell whether this ThreadGroup has been destroyed or not.
    * @return whether this ThreadGroup has been destroyed or not.
    */
  public synchronized boolean isDestroyed()
  {
    return parent == null && this != root;
  }

  /** Check whether this ThreadGroup is an ancestor of the
    * specified ThreadGroup, or if they are the same.
    *
    * @param g the group to test on.
    * @return whether this ThreadGroup is a parent of the
    *	      specified group.
    */
  public final boolean parentOf(ThreadGroup tg)
  {
    while (tg != null)
      {
        if (tg == this)
          return true;
        tg = tg.parent;
      }
    return false;
  }

  /** Return the total number of active threads in this ThreadGroup
    * and all its descendants.<P>
    *
    * This cannot return an exact number, since the status of threads
    * may change after they were counted.  But it should be pretty
    * close.<P>
    *
    * @return the number of active threads in this ThreadGroup and
    *	      its descendants.
    * @specnote it isn't clear what the definition of an "Active" thread is.
    *           Current JDKs regard a thread as active if has been
    *           started and not finished.  We implement this behaviour.
    *           There is a JDC bug, <A HREF="http://developer.java.sun.com/developer/bugParade/bugs/4089701.html">
    *           4089701</A>, regarding this issue.
    *           
    */
  public synchronized int activeCount()
  {
    int total = 0;
    for (int i = 0; i < threads.size(); ++i)
      {
	if (((Thread) threads.elementAt(i)).isAlive ())
	  ++total;
      }

    for (int i=0; i < groups.size(); i++)
      {
        ThreadGroup g = (ThreadGroup) groups.elementAt(i);
        total += g.activeCount();
      }
    return total;
  }

  /** Get the number of active groups in this ThreadGroup.  This group
    * itself is not included in the count.
    * @specnote it is unclear what exactly constitutes an
    *		active ThreadGroup.  Currently we assume that
    *		all sub-groups are active, per current JDKs.
    * @return the number of active groups in this ThreadGroup.
    */
  public synchronized int activeGroupCount()
  {
    int total = groups.size();
    for (int i=0; i < groups.size(); i++)
      {
	ThreadGroup g = (ThreadGroup) groups.elementAt(i);
	total += g.activeGroupCount();
      }
    return total;
  }

  /** Copy all of the active Threads from this ThreadGroup and
    * its descendants into the specified array.  If the array is
    * not big enough to hold all the Threads, extra Threads will
    * simply not be copied.
    *
    * @param threads the array to put the threads into.
    * @return the number of threads put into the array.
    */
  public int enumerate(Thread[] threads)
  {
    return enumerate(threads, 0, true);
  }

  /** Copy all of the active Threads from this ThreadGroup and,
    * if desired, from its descendants, into the specified array.
    * If the array is not big enough to hold all the Threads,
    * extra Threads will simply not be copied.
    *
    * @param threads the array to put the threads into.
    * @param useDescendants whether to count Threads in this
    *	     ThreadGroup's descendants or not.
    * @return the number of threads put into the array.
    */
  public int enumerate(Thread[] threads, boolean useDescendants)
  {
    return enumerate(threads, 0, useDescendants);
  }

  // This actually implements enumerate.
  private synchronized int enumerate(Thread[] list, int next_index, 
				     boolean recurse)
  {
    Enumeration e = threads.elements();
    while (e.hasMoreElements() && next_index < list.length)
      {
	Thread t = (Thread) e.nextElement();
	if (t.isAlive ())
	  list[next_index++] = t;
      }
    if (recurse && next_index != list.length)
      {
	e = groups.elements();
	while (e.hasMoreElements() && next_index < list.length)
	  {
	    ThreadGroup g = (ThreadGroup) e.nextElement();
	    next_index = g.enumerate(list, next_index, true);
	  }
      }
    return next_index;
  }

  /** Copy all active ThreadGroups that are descendants of this
    * ThreadGroup into the specified array.  If the array is not
    * large enough to hold all active ThreadGroups, extra
    * ThreadGroups simply will not be copied.
    *
    * @param groups the array to put the ThreadGroups into.
    * @return the number of ThreadGroups copied into the array.
    */
  public int enumerate(ThreadGroup[] groups)
  {
    return enumerate(groups, 0, true);
  }

  /** Copy all active ThreadGroups that are children of this
    * ThreadGroup into the specified array, and if desired, also
    * copy all active descendants into the array.  If the array
    * is not large enough to hold all active ThreadGroups, extra
    * ThreadGroups simply will not be copied.
    *
    * @param groups the array to put the ThreadGroups into.
    * @param recurse whether to include all descendants
    *	     of this ThreadGroup's children in determining
    *	     activeness.
    * @return the number of ThreadGroups copied into the array.
    */
  public int enumerate(ThreadGroup[] groups, boolean recurse)
  {
    return enumerate(groups, 0, recurse);
  }

  // This actually implements enumerate.
  private synchronized int enumerate (ThreadGroup[] list, int next_index, 
				      boolean recurse)
  {
    Enumeration e = groups.elements();
    while (e.hasMoreElements() && next_index < list.length)
      {
	ThreadGroup g = (ThreadGroup) e.nextElement();
	list[next_index++] = g;
	if (recurse && next_index != list.length)
	  next_index = g.enumerate(list, next_index, true);
      }
    return next_index;
  }

  /** Interrupt all Threads in this ThreadGroup and its sub-groups.
    * @exception SecurityException if you cannot modify this
    *		 ThreadGroup or any of its Threads or children
    *		 ThreadGroups.
    * @since JDK1.2
    */
  public final synchronized void interrupt()
  {
    checkAccess();
    for (int i=0; i < threads.size(); i++)
      {
        Thread t = (Thread) threads.elementAt(i);
        t.interrupt();
      }
    for (int i=0; i < groups.size(); i++)
      {
        ThreadGroup tg = (ThreadGroup) groups.elementAt(i);
        tg.interrupt();
      }
  }

  /** Stop all Threads in this ThreadGroup and its descendants.
    * @exception SecurityException if you cannot modify this
    *		 ThreadGroup or any of its Threads or children
    *		 ThreadGroups.
    * @deprecated This method calls Thread.stop(), which is dangerous.
    */
  public final synchronized void stop()
  {
    checkAccess();
    for (int i=0; i<threads.size(); i++)
      {
        Thread t = (Thread) threads.elementAt(i);
	t.stop();
      }
    for (int i=0; i < groups.size(); i++)
      {
        ThreadGroup tg = (ThreadGroup) groups.elementAt(i);
        tg.stop();
      }
  }

  /** Suspend all Threads in this ThreadGroup and its descendants.
    * @exception SecurityException if you cannot modify this
    *		 ThreadGroup or any of its Threads or children
    *		 ThreadGroups.
    * @deprecated This method calls Thread.suspend(), which is dangerous.
    */
  public final synchronized void suspend()
  {
    checkAccess();
    for (int i=0; i<threads.size(); i++)
      {
        Thread t = (Thread) threads.elementAt(i);
        t.suspend();
      }
    for (int i=0; i < groups.size(); i++)
      {
        ThreadGroup tg = (ThreadGroup) groups.elementAt(i);
        tg.suspend();
      }
  }

  /** Resume all Threads in this ThreadGroup and its descendants.
    * @exception SecurityException if you cannot modify this
    *		 ThreadGroup or any of its Threads or children
    *		 ThreadGroups.
    * @deprecated This method relies on Thread.suspend(), which is dangerous.
    */
  public final synchronized void resume()
  {
    checkAccess();
    for (int i=0; i < threads.size(); i++)
      {
        Thread t = (Thread) threads.elementAt(i);
	t.resume();
      }
    for (int i=0; i < groups.size(); i++)
      {
        ThreadGroup tg = (ThreadGroup) groups.elementAt(i);
        tg.resume();
      }
  }

  // This is a helper that is used to implement the destroy method.
  private synchronized void checkDestroy ()
  {
    if (! threads.isEmpty())
      throw new IllegalThreadStateException ("ThreadGroup has threads");
    for (int i=0; i < groups.size(); i++)
      {
        ThreadGroup tg = (ThreadGroup) groups.elementAt(i);
	tg.checkDestroy();
      }
  }

  /** Destroy this ThreadGroup.  There can be no Threads in it,
    * and none of its descendants (sub-groups) may have Threads in them.
    * All its descendants will be destroyed as well.
    * @exception IllegalThreadStateException if the ThreadGroup or
    *		 its descendants have Threads remaining in them, or
    *		 if the ThreadGroup in question is already destroyed.
    * @exception SecurityException if you cannot modify this
    *		 ThreadGroup or any of its descendants.
    */
  public final synchronized void destroy()
  {
    checkAccess();
    if (isDestroyed())
      throw new IllegalThreadStateException("Already destroyed.");
    checkDestroy ();
    if (parent != null)
      parent.removeGroup(this);
    parent = null;

    for (int i=0; i < groups.size(); i++)
      {
        ThreadGroup tg = (ThreadGroup) groups.elementAt(i);
	tg.destroy();
      }
  }
  
  /** Print out information about this ThreadGroup to System.out.
    */
  public void list()
  {
    list("");
  }

  private synchronized void list(String indentation)
  {
    System.out.print(indentation);
    System.out.println(toString ());
    String sub = indentation + "    ";
    for (int i=0; i < threads.size(); i++)
      {
        Thread t = (Thread) threads.elementAt(i);
	System.out.print(sub);
	System.out.println(t.toString());
      }
    for (int i=0; i < groups.size(); i++)
      {
        ThreadGroup tg = (ThreadGroup) groups.elementAt(i);
	tg.list(sub);
      }
  }

  /** When a Thread in this ThreadGroup does not catch an exception,
    * this method of the ThreadGroup is called.<P>
    *
    * ThreadGroup's implementation does the following:<BR>
    * <OL>
    * <LI>If there is a parent ThreadGroup, call uncaughtException()
    *	  in the parent.</LI>
    * <LI>If the Throwable passed is a ThreadDeath, don't do
    *	  anything.</LI>
    * <LI>Otherwise, call <CODE>exception.printStackTrace().</CODE></LI>
    * </OL>
    *
    * @param thread the thread that exited.
    * @param exception the uncaught exception.
    */
  public void uncaughtException(Thread thread, Throwable t)
  {
    if (parent != null)
      parent.uncaughtException (thread, t);
    else if (! (t instanceof ThreadDeath))
      {
        if (thread != null)
          System.err.print ("Exception in thread \""
			    + thread.getName() + "\" ");
	try
	  {
	    t.printStackTrace(System.err);
	  }
	catch (Throwable x)
	  {
	    // This means that something is badly screwed up with the runtime,
	    // or perhaps someone is messing with the SecurityManager. In any
	    // case, try to deal with it gracefully.
	    System.err.println(t);
	    System.err.println("*** Got " + x.toString() + 
			       " while trying to print stack trace");
	  }
	had_uncaught_exception = true;
      }
  }

  /** Tell the VM whether it may suspend Threads in low memory
    * situations.
    * @deprecated This method is unimplemented, because it would rely on
    *		  suspend(), which is deprecated. There is no way for a Java
    *		  program to determine whether this has any effect whatsoever,
    *		  so we don't need it.
    * @return false
    */
  public boolean allowThreadSuspension(boolean allow)
  {
    return false;
  }

  /** Get a human-readable representation of this ThreadGroup.
    * @return a String representing this ThreadGroup.
    * @specnote Language Spec and Class Libraries book disagree a bit here.
    *		We follow the Spec, but add "ThreadGroup" per the book.  We
    *		include "java.lang" based on the list() example in the Class
    *		Libraries book.
    */
  public String toString ()
  {
    return "java.lang.ThreadGroup[name=" + name + 
           ",maxpri=" + maxpri + "]";
  }

  /** Find out if the current Thread can modify this ThreadGroup.
    * Calls the current SecurityManager's checkAccess() method to
    * find out.  If there is none, it assumes everything's OK.
    * @exception SecurityException if the current Thread cannot
    *		 modify this ThreadGroup.
    */
  public final void checkAccess()
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkAccess(this);
  }

  // This is called to add a Thread to our internal list.
  final synchronized void addThread(Thread t)
  {
    if (isDestroyed())
      throw new IllegalThreadStateException ("ThreadGroup is destroyed");
  
    threads.addElement(t);
  }

  // This is called to remove a Thread from our internal list.
  final synchronized void removeThread(Thread t)
  {
    if (isDestroyed())
      throw new IllegalThreadStateException ();
  
    threads.removeElement(t);
    // Daemon groups are automatically destroyed when all their threads die.
    if (daemon_flag && groups.size() == 0 && threads.size() == 0)
      {
	// We inline destroy to avoid the access check.
	if (parent != null)
	  parent.removeGroup(this);
	parent = null;
      }
  }

  // This is called to add a ThreadGroup to our internal list.
  final synchronized void addGroup(ThreadGroup g)
  {
    groups.addElement(g);
  }

  // This is called to remove a ThreadGroup from our internal list.
  final synchronized void removeGroup(ThreadGroup g)
  {
    groups.removeElement(g);
    // Daemon groups are automatically destroyed when all their threads die.
    if (daemon_flag && groups.size() == 0 && threads.size() == 0)
      {
	// We inline destroy to avoid the access check.
	if (parent != null)
	  parent.removeGroup(this);
	parent = null;
      }
  }
}
