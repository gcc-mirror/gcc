/* Observable.java -- an object to be observed
   Copyright (C) 1999, 2000, 2001, 2002 Free Software Foundation, Inc.

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


package java.util;

/**
 * This class represents an object which is observable.  Other objects may
 * register their intent to be notified when this object changes; and when
 * this object does change, it will trigger the <code>update</code> method
 * of each observer.
 *
 * Note that the <code>notifyObservers()</code> method of this class is
 * unrelated to the <code>notify()</code> of Object.
 *
 * @author Warren Levy <warrenl@cygnus.com>
 * @author Eric Blake <ebb9@email.byu.edu>
 * @see Observer
 * @status updated to 1.4
 */
public class Observable
{
  /** Tracks whether this object has changed. */
  private boolean changed;

  /* List of the Observers registered as interested in this Observable. */
  private LinkedHashSet observers;

  /**
   * Constructs an Observable with zero Observers.
   */
  public Observable()
  {
    observers = new LinkedHashSet();
  }

  /**
   * Adds an Observer. If the observer was already added this method does
   * nothing.
   *
   * @param observer Observer to add
   * @throws NullPointerException if observer is null
   */
  public synchronized void addObserver(Observer observer)
  {
    observers.add(observer);
  }

  /**
   * Reset this Observable's state to unchanged. This is called automatically
   * by <code>notifyObservers</code> once all observers have been notified.
   *
   * @see #notifyObservers()
   */
  protected synchronized void clearChanged()
  {
    changed = false;
  }

  /**
   * Returns the number of observers for this object.
   *
   * @return number of Observers for this
   */
  public synchronized int countObservers()
  {
    return observers.size();
  }

  /**
   * Deletes an Observer of this Observable.
   *
   * @param victim Observer to delete
   */
  public synchronized void deleteObserver(Observer victim)
  {
    observers.remove(victim);
  }

  /**
   * Deletes all Observers of this Observable.
   */
  public synchronized void deleteObservers()
  {
    observers.clear();
  }

  /**
   * True if <code>setChanged</code> has been called more recently than
   * <code>clearChanged</code>.
   *
   * @return whether or not this Observable has changed
   */
  public synchronized boolean hasChanged()
  {
    return changed;
  }

  /**
   * If the Observable has actually changed then tell all Observers about it,
   * then reset state to unchanged.
   *
   * @see #notifyObservers(Object)
   * @see Observer#update(Observable, Object)
   */
  public void notifyObservers()
  {
    notifyObservers(null);
  }

  /**
   * If the Observable has actually changed then tell all Observers about it,
   * then reset state to unchanged. Note that though the order of
   * notification is unspecified in subclasses, in Observable it is in the
   * order of registration.
   *
   * @param obj argument to Observer's update method
   * @see Observer#update(Observable, Object)
   */
  public void notifyObservers(Object obj)
  {
    if (! hasChanged())
      return;
    // Create clone inside monitor, as that is relatively fast and still
    // important to keep threadsafe, but update observers outside of the
    // lock since update() can call arbitrary code.
    Set s;
    synchronized (this)
      {
        s = (Set) observers.clone();
      }
    int i = s.size();
    Iterator iter = s.iterator();
    while (--i >= 0)
      ((Observer) iter.next()).update(this, obj);
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
