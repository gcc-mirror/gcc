/* Adjustable.java -- Objects with a numeric adjustment scale.
   Copyright (C) 1999 Free Software Foundation, Inc.

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
  * This interface is for objects that take a numeric value that
  * can be adjusted within a bounded range.  For example, a scroll bar.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public interface Adjustable
{

/*
 * Static Variables
 */

/**
  * Constant for a horizontal orientation
  */
public static final int HORIZONTAL = 0;

/**
  * Constant for a vertical orientation
  */
public static final int VERTICAL = 1;

/*************************************************************************/

/*
 * Instance Methods
 */

/**
  * Returns the current value of the object.
  *
  * @return The current value of the object.
  */
public abstract int
getValue();

/*************************************************************************/

/**
  * Sets the current value of the object.
  *
  * @param value The current value of the object.
  */
public abstract void
setValue(int value);

/*************************************************************************/

/**
  * Returns the orientation of the object, either <code>HORIZONTAL</code>
  * or <code>VERTICAL</code>.
  *
  * @return The orientation of this object.
  */
public abstract int
getOrientation();

/*************************************************************************/

/**
  * Returns the minimum value this object can take.
  *
  * @return The minimum value this object can take.
  */
public abstract int
getMinimum();

/*************************************************************************/

/**
  * Sets the minimum value this object can take to the specified value.
  *
  * @param minimum The new minimum value for this object.
  */
public abstract void
setMinimum(int minimum);

/*************************************************************************/

/**
  * Returns the maximum value this object can take.
  *
  * @return The maximum value this object can take.
  */
public abstract int
getMaximum();

/*************************************************************************/

/**
  * Sets the maximum value this object can take to the specified value.
  *
  * @param maximum The new maximum value for this object.
  */
public abstract void
setMaximum(int maximum);

/*************************************************************************/

/**
  * Returns the increment value for incrementing by units.
  *
  * @return The unit increment value.
  */
public abstract int
getUnitIncrement();

/*************************************************************************/

/**
  * Sets the increment value for incrementing by units to the specified value.
  *
  * @param increment The unit increment value.
  */
public abstract void
setUnitIncrement(int increment);

/*************************************************************************/

/**
  * Returns the increment value for incrementing by blocks.
  *
  * @return The block increment value.
  */
public abstract int
getBlockIncrement();

/*************************************************************************/

/**
  * Sets the increment value for incrementing by blocks to the specified value.
  *
  * @param increment The block increment value.
  */
public abstract void
setBlockIncrement(int increment);

/*************************************************************************/

/**
  * Returns the length of the indicator for this object.
  *
  * @return The indicator length.
  */
public abstract int
getVisibleAmount();

/*************************************************************************/

/**
  * Sets the length of the indicator for this object to the specified value.
  *
  * @param length The indicator length
  */
public abstract void
setVisibleAmount(int length);

/*************************************************************************/

/**
  * Adds a listener that will receive adjustment events for this object.
  * 
  * @param listener The adjustment listener to add.
  */
public abstract void
addAdjustmentListener(AdjustmentListener listener);

/*************************************************************************/

/**
  * Removes an adjustment listener from this object.  It will no longer
  * receive adjustment events.
  *
  * @param listener The adjustment listener to remove.
  */
public abstract void
removeAdjustmentListener(AdjustmentListener listener);

} // interface Adjustable

