/* Adjustable.java -- Objects with a numeric adjustment scale
   Copyright (C) 1999, 2002 Free Software Foundation, Inc.

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


package java.awt;

import java.awt.event.AdjustmentListener;

/**
 * This interface is for objects that take a numeric value that can be
 * adjusted within a bounded range.  For example, a scroll bar.
 *
 * @author Aaron M. Renn <arenn@urbanophile.com>
 * @since 1.0
 * @status updated to 1.4
 */
public interface Adjustable
{
  /** Constant for an adjustable object with horizontal orientation. */
  int HORIZONTAL = 0;

  /** Constant for an adjustable object with vertical orientation. */
  int VERTICAL = 1;

  /** Constant for an adjustable object with no orientation. */
  int NO_ORIENTATION = 2;

  /**
   * Returns a constant representing the orientation of the object.
   *
   * @return the orientation of this object
   * @see #HORIZONTAL
   * @see #VERTICAL
   * @see #NO_ORIENTATION
   */
  int getOrientation();

  /**
   * Sets the minimum value this object can have.
   *
   * @param minimum the new minimum value
   */
  void setMinimum(int minimum);

  /**
   * Returns the minimum value this object can have.
   *
   * @return the minimum value
   */
  int getMinimum();

  /**
   * Sets the maximum value this object can have.
   *
   * @param maximum the new maximum value
   */
  void setMaximum(int maximum);

  /**
   * Returns the maximum value this object can have.
   *
   * @return the maximum value
   */
  int getMaximum();

  /**
   * Sets the increment value for incrementing the value by units.
   *
   * @param increment the unit increment value
   */
  void setUnitIncrement(int increment);

  /**
   * Returns the increment value for incrementing the value by units.
   *
   * @return the unit increment value
   */
  int getUnitIncrement();

  /**
   * Sets the increment value for incrementing the value by blocks.
   *
   * @param increment the block increment value
   */
  void setBlockIncrement(int increment);

  /**
   * Returns the increment value for incrementing the value by blocks.
   *
   * @return the block increment value
   */
  int getBlockIncrement();

  /**
   * Sets the length of the indicator for this object to the specified value.
   *
   * @param length the indicator length
   */
  void setVisibleAmount(int length);

  /**
   * Returns the length of the indicator for this object.
   *
   * @return the indicator length
   */
  int getVisibleAmount();

  /**
   * Sets the current value of the object.
   *
   * @param value the new value
   */
  void setValue(int value);

  /**
   * Returns the current value of the object.
   *
   * @return the current value
   */
  int getValue();

  /**
   * Adds a listener that will receive adjustment events for this object.
   *
   * @param listener the adjustment listener to add
   * @see AdjustmentEvent
   */
  void addAdjustmentListener(AdjustmentListener listener);

  /**
   * Removes an adjustment listener from this object.
   *
   * @param listener the adjustment listener to remove
   * @see AdjustmentEvent
   */
  void removeAdjustmentListener(AdjustmentListener listener);
} // interface Adjustable
