/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util;
 
/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date September 2, 1998.
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */
 
public class Observable
{
  /* tracks whether this object has changed */
  private boolean changed;

  /* list of the Observers registered as interested in this Observable */
  private Vector observerVec;

  /* TBD: This might be better implemented as an Observer[]
   * but that would mean writing more code rather than making use of
   * the existing Vector class (this also implies a larger text code
   * space in resulting executables).  The tradeoff is one of speed
   * (manipulating the Observer[] directly) vs. size/reuse.  In the future,
   * we may decide to make the tradeoff and reimplement with an Observer[].
   */

  public Observable()
  {
    changed = false;
    observerVec = new Vector();
  }

  public synchronized void addObserver(Observer obs)
  {
    // JDK 1.2 spec says not to add this if it is already there
    if (!observerVec.contains(obs))
      observerVec.addElement(obs);
  }

  protected synchronized void clearChanged()
  {
    changed = false;
  }

  public synchronized int countObservers()
  {
    return observerVec.size();
  }

  public synchronized void deleteObserver(Observer obs)
  {
    observerVec.removeElement(obs);
  }

  public synchronized void deleteObservers()
  {
    observerVec.removeAllElements();
  }

  public synchronized boolean hasChanged()
  {
    return changed;
  }

  public void notifyObservers()
  {
    notifyObservers(null);
  }

  public void notifyObservers(Object arg)
  {
    if (changed)
      {
	/* The JDK 1.2 spec states that though the order of notification
	 * is unspecified in subclasses, in Observable it is in the order
	 * of registration.
	 */
        for (int i = 0, numObs = observerVec.size(); i < numObs; i++)
          ((Observer) (observerVec.elementAt(i))).update(this, arg);
        changed = false;
      }
  }

  protected synchronized void setChanged()
  {
    changed = true;
  }
}
