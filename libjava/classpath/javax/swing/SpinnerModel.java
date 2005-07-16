/* SpinnerModel.java -- 
   Copyright (C) 2003, 2004  Free Software Foundation, Inc.

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

import javax.swing.event.ChangeListener;

/**
 * The data model that is used in {@link JSpinner}s.
 *
 * @since 1.4
 */
public interface SpinnerModel
{
  /**
   * Sets the current value of the model to that specified.
   * Implementations can choose to refuse to accept the value
   * and throw an exception instead.  For example, a date model
   * may throw invalid dates, or a list model may throw out
   * values which don't exist in the underlying list.  Models
   * may also throw out unusual values, such as null.  The decision
   * is left to the discretion of the implementator.  If the
   * operation succeeds, the implementation should also notify
   * any registered <code>ChangeListener</code>s.
   *
   * @param value The new value of the model.
   * @throws IllegalArgumentException if the model does not accept
   *         the given value.
   */
  void setValue(Object value);

  /**
   * Returns the current value of the model.
   *
   * @return The current value.
   */
  Object getValue();

  /**
   * Returns the next value from the model.  If the model is bounded,
   * this method may return null when the upper bound is met.
   * The current value is not changed.
   *
   * @return The next value, or null if there are no more values
   *         to retrieve.
   */
  Object getNextValue();

  /**
   * Returns the previous value from the model.  If the model is
   * bounded, this method may return null when the lower bound is
   * met.  The current value is not changed.
   *
   * @return The previous value, or null if there are no more
   *         values to retrieve.
   */
  Object getPreviousValue();

  /**
   * Adds a <code>ChangeListener</code> to the list of registered
   * listeners.  Each listener is notified when the current value
   * is changed.
   *
   * @param listener The new listener to register.
   */
  void addChangeListener(ChangeListener listener);

  /**
   * Removes a given <code>ChangeListener</code> from the list
   * of registered listeners.
   *
   * @param listener The listener to remove.
   */
  void removeChangeListener(ChangeListener listener);
    
}
