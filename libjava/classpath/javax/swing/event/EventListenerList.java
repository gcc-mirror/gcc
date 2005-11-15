/* EventListenerList.java --
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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

package javax.swing.event;

import java.io.Serializable;
import java.lang.reflect.Array;
import java.util.EventListener;


/**
 * A utility class for keeping track of {@link EventListener}s.
 *
 * <p><b>Example for using this class:</b>
 *
 * <blockquote><pre> import java.util.EventListener;
 * import javax.swing.event.EventListenerList;
 *
 * class Foo
 * {
 *   protected final EventListenerList listeners = new EventListenerList();
 *   protected BarClosedEvent barClosedEvent = null;
 *
 *   public void addBarListener(BarListener l)
 *   {
 *     listeners.<a href="#add(java.lang.Class, java.util.EventListener)"
 *               >add</a>(BarListener.class, l);
 *   }
 *
 *   public void removeBarListener(BarListener l)
 *   {
 *     listeners.<a href="#remove(java.lang.Class, java.util.EventListener)"
 *               >remove</a>(BarListener.class, l);
 *   }
 *
 *   protected void fireBarClosedEvent()
 *   {
 *     Object[] l = listeners.<a href="#getListenerList()"
 *                            >getListenerList()</a>;
 *
 *     for (int i = l.length - 2; i >= 0; i -= 2)
 *       if (l[i] == BarListener.class)
 *         {
 *           // Create the event on demand, when it is needed the first time.
 *           if (barClosedEvent == null)
 *             barClosedEvent = new BarClosedEvent(this);
 *
 *           ((BarClosedListener) l[i + 1]).barClosed(barClosedEvent);
 *         }
 *   }
 * }</pre></blockquote>
 *
 * @author Andrew Selkirk (aselkirk@sympatico.ca)
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class EventListenerList
  implements Serializable
{
  /**
   * An ID for serializing instances of this class; verified with the
   * serialver tool of Sun J2SE 1.4.1_01.
   */
  static final long serialVersionUID = -5677132037850737084L;


  /**
   * An empty array that is shared by all instances of this class that
   * have no listeners.
   */
  private static final Object[] NO_LISTENERS = new Object[0];
  
  
  /**
   * An array with all currently registered listeners.  The array has
   * twice as many elements as there are listeners.  For an even
   * integer <code>i</code>, <code>listenerList[i]</code> indicates
   * the registered class, and <code>listenerList[i+1]</code> is the
   * listener.
   */
  protected transient Object[] listenerList = NO_LISTENERS;

  
  /**
   * EventListenerList constructor
   */
  public EventListenerList()
  {
    // Nothing to do here.
  }


  /**
   * Registers a listener of a specific type.
   *
   * @param t the type of the listener.
   *
   * @param listener the listener to add, which must be an instance of
   * <code>t</code>, or of a subclass of <code>t</code>.
   *
   * @throws IllegalArgumentException if <code>listener</code> is not
   * an instance of <code>t</code> (or a subclass thereof).
   *
   * @throws NullPointerException if <code>t</code> is <code>null</code>.
   */
  public void add(Class t, EventListener listener)
  {
    int oldLength;
    Object[] newList;

    if (listener == null)
      return;

    if (!t.isInstance(listener))
      throw new IllegalArgumentException();

    oldLength = listenerList.length;
    newList = new Object[oldLength + 2];
    if (oldLength > 0)
      System.arraycopy(listenerList, 0, newList, 0, oldLength);

    newList[oldLength] = t;
    newList[oldLength + 1] = listener;
    listenerList = newList;
  }


  /**
   * Determines the number of listeners.
   */
  public int getListenerCount()
  {
    return listenerList.length / 2;
  }


  /**
   * Determines the number of listeners of a particular class.
   *
   * @param t the type of listeners to be counted. In order to get
   * counted, a subscribed listener must be exactly of class
   * <code>t</code>. Thus, subclasses of <code>t</code> will not be
   * counted.
   */
  public int getListenerCount(Class t)
  {
    int result = 0;
    for (int i = 0; i < listenerList.length; i += 2)
      if (t == listenerList[i])
        ++result;

    return result;
  }


  /**
   * Get a list of listenerType/listener pairs
   * @returns Listener list
   */
  public Object[] getListenerList()
  {
    return listenerList;
  }


  /**
   * Retrieves the currently subscribed listeners of a particular
   * type.  For a listener to be returned, it must have been
   * registered with exactly the type <code>c</code>; subclasses are
   * not considered equal.
   *
   * <p>The returned array can always be cast to <code>c[]</code>.
   * Since it is a newly allocated copy, the caller may arbitrarily
   * modify the array.
   *
   * @param c the class which was passed to {@link #add}.
   *
   * @throws ClassCastException if <code>c</code> does not implement
   * the {@link EventListener} interface.
   *
   * @throws NullPointerException if <code>c</code> is
   * <code>null</code>.
   *
   * @returns an array of <code>c</code> whose elements are the
   * currently subscribed listeners of the specified type.  If there
   * are no such listeners, an empty array is returned.
   *
   * @since 1.3
   */
  public EventListener[] getListeners(Class c)
  {
    int count, f;
    EventListener[] result;

    count = getListenerCount(c);
    result = (EventListener[]) Array.newInstance(c, count);
    f = 0;
    for (int i = 0; i < listenerList.length; i += 2)
      if (listenerList[i] == c)
        result[f++] = (EventListener) listenerList[i + 1];
    
    return result;
  }


  /**
   * Removes a listener of a specific type.
   *
   * @param t the type of the listener.
   *
   * @param listener the listener to remove, which must be an instance
   * of <code>t</code>, or of a subclass of <code>t</code>.
   *
   * @throws IllegalArgumentException if <code>listener</code> is not
   * an instance of <code>t</code> (or a subclass thereof).
   *
   * @throws NullPointerException if <code>t</code> is <code>null</code>.
   */
  public void remove(Class t, EventListener listener)
  {
    Object[] oldList, newList;
    int oldLength;

    if (listener == null)
      return;

    if (!t.isInstance(listener))
      throw new IllegalArgumentException();

    oldList = listenerList;
    oldLength = oldList.length;
    for (int i = 0; i < oldLength; i += 2)
      if (oldList[i] == t && oldList[i + 1] == listener)
        {
          if (oldLength == 2)
            newList = NO_LISTENERS;
          else
            {
              newList = new Object[oldLength - 2];
              if (i > 0)
                System.arraycopy(oldList, 0, newList, 0, i);
              if (i < oldLength - 2)
                System.arraycopy(oldList, i + 2, newList, i,
                                 oldLength - 2 - i);
            }
          listenerList = newList;
          return;
        }
  }


  /**
   * Returns a string representation of this object that may be useful
   * for debugging purposes.
   */
  public String toString()
  {
    StringBuffer buf = new StringBuffer("EventListenerList: ");
    buf.append(listenerList.length / 2);
    buf.append(" listeners: ");
    for (int i = 0; i < listenerList.length; i += 2)
      {
        buf.append(" type ");
        buf.append(((Class) listenerList[i]).getName());
        buf.append(" listener ");
        buf.append(listenerList[i + 1]);
      }
    return buf.toString();
  }
}
