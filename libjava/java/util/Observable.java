/* java.util.Observable
   Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package java.util;

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date September 2, 1998.
 */
public class Observable
{
  /** tracks whether this object has changed */
  private boolean changed;

  /* list of the Observers registered as interested in this Observable */
  private Vector observers;

  /* TBD: This might be better implemented as an Observer[]
   * but that would mean writing more code rather than making use of
   * the existing Vector class (this also implies a larger text code
   * space in resulting executables).  The tradeoff is one of speed
   * (manipulating the Observer[] directly) vs. size/reuse.  In the future,
   * we may decide to make the tradeoff and reimplement with an Observer[].
   */

  /**
   * Constructs an Observable with zero Observers.
   */
  public Observable()
  {
    changed = false;
    observers = new Vector();
  }

  /**
   * Adds an Observer. If the observer was already added this method does
   * nothing.
   *
   * @param observer Observer to add.
   */
  public synchronized void addObserver(Observer observer)
  {
    if (!observers.contains(observer))
      observers.addElement(observer);
  }

  /**
   * Reset this Observable's state to unchanged.
   */
  protected synchronized void clearChanged()
  {
    changed = false;
  }

  /**
   * @return Number of Observers for this Observable.
   */
  public synchronized int countObservers()
  {
    return observers.size();
  }

  /**
   * Deletes an Observer of this Observable.
   *
   * @param victim Observer to delete.
   */
  public synchronized void deleteObserver(Observer victim)
  {
    observers.removeElement(victim);
  }

  /**
   * Deletes all Observers of this Observable.
   */
  public synchronized void deleteObservers()
  {
    observers.removeAllElements();
  }

  /**
   * @return Whether or not this Observable has changed.
   */
  public synchronized boolean hasChanged()
  {
    return changed;
  }

  /**
   * If the Observable has actually changed then tell all Observers about it,
   * then resets state to unchanged.
   */
  public void notifyObservers()
  {
    notifyObservers(null);
  }

  /**
   * If the Observable has actually changed then tell all Observers about it,
   * then resets state to unchanged. 
   * Note that though the order of notification is unspecified in subclasses,
   * in Observable it is in the order of registration.
   *
   * @param obj Arguement to Observer's update method.
   */
  public void notifyObservers(Object obj)
  {
    if (!hasChanged())
      return;
    Vector ob1 = (Vector) observers.clone();

    for (int i = 0; i < ob1.size(); i++)
      ((Observer) ob1.elementAt(i)).update(this, obj);

    clearChanged();
  }

  /**
   * Marks this Observable as having changed.
   */
  protected synchronized void setChanged()
  {
    changed = true;
  }
}
