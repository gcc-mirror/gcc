/* AdjustmentEvent.java -- an adjustable value was changed
   Copyright (C) 1999, 2002, 2004, 2005  Free Software Foundation, Inc.

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


package java.awt.event;

import java.awt.AWTEvent;
import java.awt.Adjustable;

/**
 * This class represents an event that is generated when an adjustable
 * value is changed.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @see Adjustable
 * @see AdjustmentListener
 * @since 1.1
 * @status updated to 1.4
 */
public class AdjustmentEvent extends AWTEvent
{
  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = 5700290645205279921L;

  /** This is the first id in the range of ids used by adjustment events. */
  public static final int ADJUSTMENT_FIRST = 601;

  /** This is the last id in the range of ids used by adjustment events. */
  public static final int ADJUSTMENT_LAST = 601;

  /** This is the id indicating an adjustment value changed. */
  public static final int ADJUSTMENT_VALUE_CHANGED = 601;

  /** Adjustment type for unit increments. */
  public static final int UNIT_INCREMENT = 1;

  /** Adjustment type for unit decrements. */
  public static final int UNIT_DECREMENT = 2;

  /** Adjustment type for block decrements. */
  public static final int BLOCK_DECREMENT = 3;

  /** Adjustment type for block increments. */
  public static final int BLOCK_INCREMENT = 4;

  /** Adjustment type for tracking adjustments. */
  public static final int TRACK = 5;

  /**
   * The adjustable object that caused the event.
   *
   * @see #getAdjustable()
   * @serial the cause
   */
  private final Adjustable adjustable;

  /**
   * The type of adjustment, one of {@link #UNIT_INCREMENT},
   * {@link #UNIT_DECREMENT}, {@link #BLOCK_INCREMENT},
   * {@link #BLOCK_DECREMENT}, or {@link #TRACK}.
   *
   * @see #getAdjustmentType()
   * @serial the adjustment type
   */
  private final int adjustmentType;

  /**
   * The new value of the adjustable; it should be in the range of the
   * adjustable cause.
   *
   * @see #getValue()
   * @serial the adjustment value
   */
  private final int value;

  /**
   * True if this is in a series of multiple adjustment events.
   *
   * @see #getValueIsAdjusting()
   * @serial true if this is not the last adjustment
   * @since 1.4
   */
  private final boolean isAdjusting;

  /**
   * Initializes an instance of <code>AdjustmentEvent</code> with the
   * specified source, id, type, and value. Note that an invalid id leads to
   * unspecified results.
   *
   * @param source the source of the event
   * @param id the event id
   * @param type the event type, one of the constants of this class
   * @param value the value of the adjustment
   * @throws IllegalArgumentException if source is null
   */
  public AdjustmentEvent(Adjustable source, int id, int type, int value)
  {
    this(source, id, type, value, false);
  }

  /**
   * Initializes an instance of <code>AdjustmentEvent</code> with the
   * specified source, id, type, and value. Note that an invalid id leads to
   * unspecified results.
   *
   * @param source the source of the event
   * @param id the event id
   * @param type the event type, one of the constants of this class
   * @param value the value of the adjustment
   * @param isAdjusting if this event is in a chain of adjustments
   * @throws IllegalArgumentException if source is null
   * @since 1.4
   */
  public AdjustmentEvent(Adjustable source, int id, int type, int value,
                         boolean isAdjusting)
  {
    super(source, id);
    this.adjustmentType = type;
    this.value = value;
    adjustable = source;
    this.isAdjusting = isAdjusting;
  }

  /**
   * This method returns the source of the event as an <code>Adjustable</code>.
   *
   * @return the <code>Adjustable</code> source of the event
   */
  public Adjustable getAdjustable()
  {
    return adjustable;
  }

  /**
   * Returns the new value of the adjustable object.
   *
   * @return the value of the event
   */
  public int getValue()
  {
    return value;
  }

  /**
   * Returns the type of the event, which will be one of
   * {@link #UNIT_INCREMENT}, {@link #UNIT_DECREMENT},
   * {@link #BLOCK_INCREMENT}, {@link #BLOCK_DECREMENT}, or {@link #TRACK}.
   *
   * @return the type of the event
   */
  public int getAdjustmentType()
  {
    return adjustmentType;
  }

  /**
   * Test if this event is part of a sequence of multiple adjustements.
   *
   * @return true if this is not the last adjustment
   * @since 1.4
   */
  public boolean getValueIsAdjusting()
  {
    return isAdjusting;
  }

  /**
   * Returns a string that describes the event. This is in the format
   * <code>"ADJUSTMENT_VALUE_CHANGED,adjType=" + &lt;type&gt; + ",value="
   * + getValue() + ",isAdjusting=" + getValueIsAdjusting()</code>, where
   * type is the name of the constant returned by getAdjustmentType().
   *
   * @return a string that describes the event
   */
  public String paramString()
  {
    return (id == ADJUSTMENT_VALUE_CHANGED
            ? "ADJUSTMENT_VALUE_CHANGED,adjType=" : "unknown type,adjType=")
      + (adjustmentType == UNIT_INCREMENT ? "UNIT_INCREMENT,value="
         : adjustmentType == UNIT_DECREMENT ? "UNIT_DECREMENT,value="
         : adjustmentType == BLOCK_INCREMENT ? "BLOCK_INCREMENT,value="
         : adjustmentType == BLOCK_DECREMENT ? "BLOCK_DECREMENT,value="
         : adjustmentType == TRACK ? "TRACK,value=" : "unknown type,value=")
      + value + ",isAdjusting=" + isAdjusting;
  }
} // class AdjustmentEvent
