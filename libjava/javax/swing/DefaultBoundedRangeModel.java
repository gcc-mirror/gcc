/* DefaultBoundedRangeModel.java -- Default implementation
   of BoundedRangeModel.
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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

package javax.swing;

import java.io.Serializable;
import java.util.EventListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.EventListenerList;


/**
 * A default implementation of BoundedRangeModel.
 *
 * @author <a href="mailto:aselkirk@sympatico.ca">Andrew Selkirk</a>
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class DefaultBoundedRangeModel
  implements BoundedRangeModel, Serializable
{
  /**
   * The identifier of this class in object serialization. Verified
   * using the serialver tool of Sun J2SE 1.4.1_01.
   */
  static final long serialVersionUID = 5034068491295259790L;


  /**
   * An event that is sent to all registered {@link ChangeListener}s
   * when the state of this range model has changed.
   *
   * <p>The event object is created on demand, the first time it
   * is actually needed.
   *
   * @see #fireStateChanged()
   */
  protected transient ChangeEvent changeEvent;


  /**
   * The list of the currently registered EventListeners.
   */
  protected EventListenerList listenerList = new EventListenerList();


  /**
   * The current value of the range model, which is always between
   * {@link #minimum} and ({@link #maximum} - {@link #extent}). In a
   * scroll bar visualization of a {@link BoundedRangeModel}, the
   * <code>value</code> is displayed as the position of the thumb.
   */
  private int value;


  /**
   * The current extent of the range model, which is a number greater
   * than or equal to zero. In a scroll bar visualization of a {@link
   * BoundedRangeModel}, the <code>extent</code> is displayed as the
   * size of the thumb.
   */
  private int extent;


  /**
   * The current minimum value of the range model, which is always
   * less than or equal to {@link #maximum}.
   */
  private int minimum;


  /**
   * The current maximum value of the range model, which is always
   * greater than or equal to {@link #minimum}.
   */
  private int maximum;


  /**
   * A property that indicates whether the value of this {@link
   * BoundedRangeModel} is going to change in the immediate future.
   */
  private boolean isAdjusting;


  /**
   * Constructs a <code>DefaultBoundedRangeModel</code> with default
   * values for the properties. The properties <code>value</code>,
   * <code>extent</code> and <code>minimum</code> will be initialized
   * to zero; <code>maximum</code> will be set to 100; the property
   * <code>valueIsAdjusting</code> will be <code>false</code>.
   */
  public DefaultBoundedRangeModel()
  {
    // The fields value, extent, minimum have the default value 0, and
    // isAdjusting is already false. These fields no not need to be
    // set explicitly.
    maximum = 100;
  }


  /**
   * Constructs a <code>DefaultBoundedRangeModel</code> with the
   * specified values for some properties.
   *
   * @param value the initial value of the range model, which must be
   * a number between <code>minimum</code> and <code>(maximum -
   * extent)</code>. In a scroll bar visualization of a {@link
   * BoundedRangeModel}, the <code>value</code> is displayed as the
   * position of the thumb.
   *
   * @param extent the initial extent of the range model, which is a
   * number greater than or equal to zero. In a scroll bar
   * visualization of a {@link BoundedRangeModel}, the
   * <code>extent</code> is displayed as the size of the thumb.
   *
   * @param minimum the initial minimal value of the range model.
   *
   * @param maximum the initial maximal value of the range model.
   *
   * @throws IllegalArgumentException if the following condition is
   * not satisfied: <code>minimum <= value <= value + extent <=
   * maximum</code>.
   */
  public DefaultBoundedRangeModel(int value, int extent, int minimum,
                                  int maximum)
  {
    if (!(minimum <= value && extent >= 0 && (value + extent) <= maximum))
      throw new IllegalArgumentException();

    this.value = value;
    this.extent = extent;
    this.minimum = minimum;
    this.maximum = maximum;

    // The isAdjusting field already has a false value by default.
  }


  /**
   * Returns a string with all relevant properties of this range
   * model.
   */
  public String toString()
  {
    return getClass().getName()
      + "[value=" + value
      + ", extent=" + extent
      + ", min=" + minimum
      + ", max=" + maximum
      + ", adj=" + isAdjusting
      + ']';
  }


  /**
   * Returns the current value of this bounded range model.  In a
   * scroll bar visualization of a {@link BoundedRangeModel}, the
   * <code>value</code> is displayed as the position of the thumb.
   */
  public int getValue()
  {
    return value;
  }


  /**
   * Changes the current value of this bounded range model. In a
   * scroll bar visualization of a {@link BoundedRangeModel}, the
   * <code>value</code> is displayed as the position of the thumb;
   * changing the <code>value</code> of a scroll bar&#x2019;s model
   * thus moves the thumb to a different position.
   */
  public void setValue(int value)
  {
    value = Math.max(minimum, value);
    if (value + extent > maximum)
      value = maximum - extent;

    if (value != this.value)
      {
        this.value = value;
        fireStateChanged();
      }
  }


  /**
   * Returns the current extent of this bounded range model, which is
   * a number greater than or equal to zero. In a scroll bar
   * visualization of a {@link BoundedRangeModel}, the
   * <code>extent</code> is displayed as the size of the thumb.
   */
  public int getExtent()
  {
    return extent;
  }


  /**
   * Changes the current extent of this bounded range model. In a
   * scroll bar visualization of a {@link BoundedRangeModel}, the
   * <code>extent</code> is displayed as the size of the thumb.
   *
   * @param extent the new extent of the range model, which is a
   * number greater than or equal to zero.
   */
  public void setExtent(int extent)
  {
    extent = Math.max(extent, 0);
    if (value + extent > maximum)
      extent = maximum - value;

    if (extent != this.extent)
      {
        this.extent = extent;
        fireStateChanged();
      }
  }


  /**
   * Returns the current minimal value of this bounded range model.
   */
  public int getMinimum()
  {
    return minimum;
  }


  /**
   * Changes the current minimal value of this bounded range model.
   *
   * @param minimum the new minimal value.
   */
  public void setMinimum(int minimum)
  {
    int value, maximum;

    maximum = Math.max(minimum, this.maximum);
    value = Math.max(minimum, this.value);

    setRangeProperties(value, extent, minimum, maximum, isAdjusting);
  }


  /**
   * Returns the current maximal value of this bounded range model.
   */
  public int getMaximum()
  {
    return maximum;
  }


  /**
   * Changes the current maximal value of this bounded range model.
   *
   * @param maximum the new maximal value.
   */
  public void setMaximum(int maximum)
  {
    int value, extent, minimum;

    minimum = Math.min(this.minimum, maximum);
    extent = Math.min(this.extent, maximum - minimum);
    value = Math.min(this.value, maximum - extent);

    setRangeProperties(value, extent, minimum, maximum, isAdjusting);
  }


  /**
   * Returns whether or not the value of this bounded range model is
   * going to change in the immediate future. Scroll bars set this
   * property to <code>true</code> while the thumb is being dragged
   * around; when the mouse is relased, they set the property to
   * <code>false</code> and post a final {@link ChangeEvent}.
   *
   * @returns <code>true</code> if the value will change soon again;
   * <code>false</code> if the value will probably not change soon.
   */
  public boolean getValueIsAdjusting()
  {
    return isAdjusting;
  }


  /**
   * Specifies whether or not the value of this bounded range model is
   * going to change in the immediate future. Scroll bars set this
   * property to <code>true</code> while the thumb is being dragged
   * around; when the mouse is relased, they set the property to
   * <code>false</code>.
   *
   * @param isAdjusting <code>true</code> if the value will change
   * soon again; <code>false</code> if the value will probably not
   * change soon.
   */
  public void setValueIsAdjusting(boolean isAdjusting)
  {
    if (isAdjusting == this.isAdjusting)
      return;

    this.isAdjusting = isAdjusting;
    fireStateChanged();
  }


  /**
   * setRangeProperties
   *
   * @param value the new value of the range model.  In a scroll bar
   * visualization of a {@link BoundedRangeModel}, the
   * <code>value</code> is displayed as the position of the thumb.
   *
   * @param extent the new extent of the range model, which is a
   * number greater than or equal to zero. In a scroll bar
   * visualization of a {@link BoundedRangeModel}, the
   * <code>extent</code> is displayed as the size of the thumb.
   *
   * @param minimum the new minimal value of the range model.
   *
   * @param maximum the new maximal value of the range model.

   * @param isAdjusting whether or not the value of this bounded range
   * model is going to change in the immediate future. Scroll bars set
   * this property to <code>true</code> while the thumb is being
   * dragged around; when the mouse is relased, they set the property
   * to <code>false</code>.
   */
  public void setRangeProperties(int value, int extent, int minimum,
                                 int maximum, boolean isAdjusting)
  {
    minimum = Math.min(Math.min(minimum, maximum), value);
    maximum = Math.max(value, maximum);
    if (extent + value > maximum)
      extent = maximum - value;
    extent = Math.max(0, extent);

    if ((value == this.value)
        && (extent == this.extent)
        && (minimum == this.minimum)
        && (maximum == this.maximum)
        && (isAdjusting == this.isAdjusting))
      return;

    this.value = value;
    this.extent = extent;
    this.minimum = minimum;
    this.maximum = maximum;
    this.isAdjusting = isAdjusting;
		
    fireStateChanged();
  }


  /**
   * Subscribes a ChangeListener to state changes.
   *
   * @param listener the listener to be subscribed.
   */
  public void addChangeListener(ChangeListener listener)
  {
    listenerList.add(ChangeListener.class, listener);
  }


  /**
   * Cancels the subscription of a ChangeListener.
   *
   * @param listener the listener to be unsubscribed.
   */
  public void removeChangeListener(ChangeListener listener)
  {
    listenerList.remove(ChangeListener.class, listener);
  }


  /**
   * Sends a {@link ChangeEvent} to any registered {@link
   * ChangeListener}s.
   *
   * @see #addChangeListener(ChangeListener)
   * @see #removeChangeListener(ChangeListener)
   */
  protected void fireStateChanged()
  {
    Object[] listeners;

    listeners = listenerList.getListenerList();
    for (int i = listeners.length - 2; i >= 0; i -= 2)
      if (listeners[i] == ChangeListener.class)
        {
          if (changeEvent == null)
            changeEvent = new ChangeEvent(this);
          ((ChangeListener) listeners[i + 1]).stateChanged(changeEvent);
        }
  }


  /**
   * Retrieves the current listeners of the specified class.
   *
   * @param c the class of listeners; usually {@link
   * ChangeListener}<code>.class</code>.
   *
   * @return an array with the currently subscribed listeners, or
   * an empty array if there are currently no listeners.
   *
   * @since 1.3
   */
  public EventListener[] getListeners(Class listenerType)
  {
    return listenerList.getListeners(listenerType);
  }


  /**
   * Returns all <code>ChangeListeners</code> that are currently
   * subscribed for changes to this
   * <code>DefaultBoundedRangeModel</code>.
   *
   * @return an array with the currently subscribed listeners, or
   * an empty array if there are currently no listeners.
   *
   * @since 1.4
   */
  public ChangeListener[] getChangeListeners()
  {
    return (ChangeListener[]) getListeners(ChangeListener.class);
  }
}
