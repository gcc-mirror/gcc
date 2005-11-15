/* BoundedRangeModel.java --
   Copyright (C) 2002, 2004, 2005 Free Software Foundation, Inc.

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


package javax.swing;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

/**
 * The data model that represents a <i>range</i> that is constrained to fit 
 * within specified <i>bounds</i>.  The range is defined as <code>value</code> 
 * to <code>value + extent</code>, where both <code>value</code> and 
 * <code>extent</code> are integers, and <code>extent >= 0</code>.   The bounds
 * are defined by integers <code>minimum</code> and <code>maximum</code>.  
 * <p>
 * This type of model is used in components that display a range of values,
 * like {@link JProgressBar} and {@link JSlider}.
 *
 * @author Andrew Selkirk
 */
public interface BoundedRangeModel
{
  /**
   * Returns the current value for the model.
   * 
   * @return The current value for the model.
   *
   * @see #setValue(int)
   */
  int getValue();

  /**
   * Sets the value for the model and sends a {@link ChangeEvent} to
   * all registered listeners.  The new value must satisfy the constraint
   * <code>min <= value <= value + extent <= max</code>.
   * 
   * @param value the value
   *
   * @see #getValue()
   */
  void setValue(int value);

  /**
   * Returns the lower bound for the model.  The start of the model's range 
   * (see {@link #getValue()}) cannot be less than this lower bound.
   * 
   * @return The lower bound for the model.
   *
   * @see #setMinimum(int)
   * @see #getMaximum()
   */
  int getMinimum();

  /**
   * Sets the lower bound for the model and sends a {@link ChangeEvent} to all
   * registered listeners.  The new minimum must be less than or equal to the
   * start value of the model's range (as returned by {@link #getValue()}).
   * 
   * @param minimum the minimum value
   *
   * @see #getMinimum()
   */
  void setMinimum(int minimum);

  /**
   * Returns the upper bound for the model.  This sets an upper limit for the
   * end value of the model's range ({@link #getValue()} + 
   * {@link #getExtent()}).
   * 
   * @return The upper bound for the model.
   *
   * @see #setMaximum(int)
   * @see #getMinimum()
   */
  int getMaximum();

  /**
   * Sets the upper bound for the model and sends a {@link ChangeEvent} to all
   * registered listeners.  The new maximum must be greater than or equal to the
   * end value of the model's range (as returned by {@link #getValue()} + 
   * {@link #getExtent()}).
   * 
   * @param maximum the maximum value
   *
   * @see #getMaximum()
   */
  void setMaximum(int maximum);

  /**
   * Returns the value of the <code>valueIsAdjusting</code> property.
   * 
   * @return <code>true</code> if value is adjusting,
   * otherwise <code>false</code>
   *
   * @see #setValueIsAdjusting(boolean)
   */
  boolean getValueIsAdjusting();

  /**
   * Sets the <code>valueIsAdjusting</code> property.
   * 
   * @param adjusting <code>true</code> if adjusting,
   * <code>false</code> otherwise
   *
   * @see #getValueIsAdjusting()
   */
  void setValueIsAdjusting(boolean adjusting);

  /**
   * Returns the current extent.
   *
   * @return the extent
   *
   * @see #setExtent(int)
   */
  int getExtent();

  /**
   * Sets the extent, which is the length of the model's range, and sends a
   * {@link ChangeEvent} to all registered listeners.
   * 
   * @param extent the extent
   *
   * @see #getExtent()
   */
  void setExtent(int extent);

  /**
   * Sets all the properties for the model in a single call.
   * 
   * @param value the value
   * @param extent the extent
   * @param minimum the minimum value
   * @param maximum the maximum value
   * @param adjusting a flag that indicates the model is being adjusted 
   *                  continuously.
   */
  void setRangeProperties(int value, int extent, int minimum, int maximum,
                          boolean adjusting);

  /**
   * Adds a <code>ChangeListener</code> to this object.
   * 
   * @param listener the listener to add
   * 
   * @see #removeChangeListener(ChangeListener)
   */
  void addChangeListener(ChangeListener listener);

  /**
   * Removes a <code>ChangeListener</code> from this object.
   * 
   * @param listener the listener to remove
   *
   * @see #addChangeListener(ChangeListener)
   */
  void removeChangeListener(ChangeListener listener);
}
