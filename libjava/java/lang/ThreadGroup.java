// ThreadGroup.java - ThreadGroup class.

/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang;

import java.util.Enumeration;
import java.util.Vector;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date August 25, 1998 
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Complete for 1.1.  Parts from the JDK 1.0 spec only are
 * not implemented.  Parts of the 1.2 spec are also not implemented.
 */

public class ThreadGroup
{
  public int activeCount ()
  {
    int ac = threads.size();
    Enumeration e = groups.elements();
    while (e.hasMoreElements())
      {
	ThreadGroup g = (ThreadGroup) e.nextElement();
	ac += g.activeCount();
      }
    return ac;
  }

  public int activeGroupCount ()
  {
    int ac = groups.size();
    Enumeration e = groups.elements();
    while (e.hasMoreElements())
      {
	ThreadGroup g = (ThreadGroup) e.nextElement();
	ac += g.activeGroupCount();
      }
    return ac;
  }

  // Deprecated in 1.2.
  public boolean allowThreadSuspension (boolean allow)
  {
    // There is no way for a Java program to determine whether this
    // has any effect whatsoever.  We don't need it.
    return true;
  }

  public final void checkAccess ()
  {
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkAccess(this);
  }

  // This is called to remove a ThreadGroup from our internal list.
  private final void remove (ThreadGroup g)
  {
    groups.removeElement(g);
    if (daemon_flag && groups.size() == 0 && threads.size() == 0)
      {
	// We inline destroy to avoid the access check.
	destroyed_flag = true;
	if (parent != null)
	  parent.remove(this);
      }
  }

  // This is called by the Thread code to remove a Thread from our
  // internal list.
  final void remove (Thread t)
  {
    threads.removeElement(t);
    if (daemon_flag && groups.size() == 0 && threads.size() == 0)
      {
	// We inline destroy to avoid the access check.
	destroyed_flag = true;
	if (parent != null)
	  parent.remove(this);
      }
  }

  // This is called by the Thread code to add a Thread to our internal
  // list.
  final void add (Thread t)
  {
    if (destroyed_flag)
      throw new IllegalThreadStateException ();

    threads.addElement(t);
  }

  // This is a helper that is used to implement the destroy method.
  private final boolean canDestroy ()
  {
    if (! threads.isEmpty())
      return false;
    Enumeration e = groups.elements();
    while (e.hasMoreElements())
      {
	ThreadGroup g = (ThreadGroup) e.nextElement();
	if (! g.canDestroy())
	  return false;
      }
    return true;
  }

  public final void destroy ()
  {
    checkAccess ();
    if (! canDestroy ())
      throw new IllegalThreadStateException ();
    destroyed_flag = true;
    if (parent != null)
      parent.remove(this);
  }

  // This actually implements enumerate.
  private final int enumerate (Thread[] ts, int next_index, boolean recurse)
  {
    Enumeration e = threads.elements();
    while (e.hasMoreElements() && next_index < ts.length)
      ts[next_index++] = (Thread) e.nextElement();
    if (recurse && next_index != ts.length)
      {
	e = groups.elements();
	while (e.hasMoreElements() && next_index < ts.length)
	  {
	    ThreadGroup g = (ThreadGroup) e.nextElement();
	    next_index = g.enumerate(ts, next_index, true);
	  }
      }
    return next_index;
  }

  public int enumerate (Thread[] ts)
  {
    return enumerate (ts, 0, true);
  }

  public int enumerate (Thread[] ts, boolean recurse)
  {
    return enumerate (ts, 0, recurse);
  }

  // This actually implements enumerate.
  private final int enumerate (ThreadGroup[] ts, int next_index,
			       boolean recurse)
  {
    Enumeration e = groups.elements();
    while (e.hasMoreElements() && next_index < ts.length)
      {
	ThreadGroup g = (ThreadGroup) e.nextElement();
	ts[next_index++] = g;
	if (recurse && next_index != ts.length)
	  next_index = g.enumerate(ts, next_index, true);
      }
    return next_index;
  }

  public int enumerate (ThreadGroup[] gs)
  {
    return enumerate (gs, 0, true);
  }

  public int enumerate (ThreadGroup[] gs, boolean recurse)
  {
    return enumerate (gs, 0, recurse);
  }

  public final int getMaxPriority ()
  {
    return maxpri;
  }

  public final String getName ()
  {
    return name;
  }

  public final ThreadGroup getParent ()
  {
    return parent;
  }

  // JDK 1.2.
  // public void interrupt ();

  public final boolean isDaemon ()
  {
    return daemon_flag;
  }

  public synchronized boolean isDestroyed ()
  {
    return destroyed_flag;
  }

  private final void list (String indentation)
  {
    System.out.print(indentation);
    System.out.println(toString ());
    String sub = indentation + "    ";
    Enumeration e = threads.elements();
    while (e.hasMoreElements())
      {
	Thread t = (Thread) e.nextElement();
	System.out.print(sub);
	System.out.println(t.toString());
      }
    e = groups.elements();
    while (e.hasMoreElements())
      {
	ThreadGroup g = (ThreadGroup) e.nextElement();
	g.list(sub);
      }
  }

  public void list ()
  {
    list ("");
  }

  public final boolean parentOf (ThreadGroup g)
  {
    while (g != null)
      {
	if (this == g)
	  return true;
	g = g.parent;
      }
    return false;
  }

  // Deprecated in 1.2.
  public final void resume ()
  {
    checkAccess ();
    Enumeration e = threads.elements();
    while (e.hasMoreElements())
      {
	Thread t = (Thread) e.nextElement();
	t.resume();
      }
    e = groups.elements();
    while (e.hasMoreElements())
      {
	ThreadGroup g = (ThreadGroup) e.nextElement();
	g.resume();
      }
  }

  public final void setDaemon (boolean daemon)
  {
    checkAccess ();
    daemon_flag = daemon;
    // FIXME: the docs don't say you are supposed to do this.  But
    // they don't say you aren't, either.
    if (groups.size() == 0 && threads.size() == 0)
      destroy ();
  }

  public final void setMaxPriority (int pri)
  {
    checkAccess ();

    // FIXME: JDK 1.2 behaviour is different: if the newMaxPriority
    // argument is < MIN_PRIORITY or > MAX_PRIORITY an
    // IllegalArgumentException should be thrown.
    if (pri >= Thread.MIN_PRIORITY && pri <= maxpri)
      {
	maxpri = pri;
	
	Enumeration e = groups.elements();
	while (e.hasMoreElements())
	  {
	    ThreadGroup g = (ThreadGroup) e.nextElement();
	    g.setMaxPriority (maxpri);
	  }
      }
  }

  // Deprecated in 1.2.
  public final void stop ()
  {
    checkAccess ();
    Enumeration e = threads.elements();
    while (e.hasMoreElements())
      {
	Thread t = (Thread) e.nextElement();
	t.stop();
      }
    e = groups.elements();
    while (e.hasMoreElements())
      {
	ThreadGroup g = (ThreadGroup) e.nextElement();
	g.stop();
      }
  }

  // Deprecated in 1.2.
  public final void suspend ()
  {
    checkAccess ();
    Enumeration e = threads.elements();
    while (e.hasMoreElements())
      {
	Thread t = (Thread) e.nextElement();
	t.suspend();
      }
    e = groups.elements();
    while (e.hasMoreElements())
      {
	ThreadGroup g = (ThreadGroup) e.nextElement();
	g.suspend();
      }
  }

  // This constructor appears in the Class Libraries book but in
  // neither the Language Spec nor the 1.2 docs.
  public ThreadGroup ()
  {
    this (Thread.currentThread().getThreadGroup(), null);
  }

  public ThreadGroup (String n)
  {
    this (Thread.currentThread().getThreadGroup(), n);
  }

  public ThreadGroup (ThreadGroup p, String n)
  {
    checkAccess ();
    if (p.destroyed_flag)
      throw new IllegalArgumentException ();

    parent = p;
    name = n;
    maxpri = p.maxpri;
    threads = new Vector ();
    groups = new Vector ();
    daemon_flag = p.daemon_flag;
    destroyed_flag = false;
    p.groups.addElement(this);
  }

  // This is the constructor that is used when creating the very first
  // ThreadGroup.  We have an arbitrary argument here just to
  // differentiate this constructor from the others.
  ThreadGroup (int dummy)
  {
    parent = null;
    name = "main";
    maxpri = Thread.MAX_PRIORITY;
    threads = new Vector ();
    groups = new Vector ();
    daemon_flag = false;
    destroyed_flag = false;
  }

  public String toString ()
  {
    // Language Spec and Class Libraries book disagree a bit here.  We
    // follow the Spec, but add "ThreadGroup" per the book.  We
    // include "java.lang" based on the list() example in the Class
    // Libraries book.
    return "java.lang.ThreadGroup[name=" + name + ",maxpri=" + maxpri + "]";
  }

  public void uncaughtException (Thread thread, Throwable e)
  {
    // FIXME: in 1.2, this has different semantics.  In particular if
    // this group has a parent, the exception is passed upwards and
    // not processed locally.
    if (! (e instanceof ThreadDeath))
      {
	e.printStackTrace();
      }
  }

  // Private data.
  private ThreadGroup parent;
  private String name;
  private int maxpri;
  private Vector threads;
  private Vector groups;
  private boolean daemon_flag;
  private boolean destroyed_flag;
}
