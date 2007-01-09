/* ScrollPaneAdjustable.java -- Scrollbars for a ScrollPane
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


package java.awt;

import java.awt.event.AdjustmentListener;
import java.io.Serializable;

/**
 * Need this class since the serialization spec for ScrollPane
 * uses it.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @since 1.4
 */
public class ScrollPaneAdjustable
  implements Adjustable, Serializable
{
  private static final long serialVersionUID = -3359745691033257079L;
 
  ScrollPane sp;
  int orientation;
  int value;
  int minimum;
  int maximum;
  int visibleAmount;
  int unitIncrement = 1;
  int blockIncrement = 1;
  AdjustmentListener adjustmentListener;

  private transient boolean valueIsAdjusting = false;

  ScrollPaneAdjustable (ScrollPane sp, int orientation)
  {
    this.sp = sp;
    this.orientation = orientation;
  }
  
  ScrollPaneAdjustable (ScrollPane sp, int orientation, int value, int minimum,
                        int maximum, int visibleAmount, int unitIncrement,
                        int blockIncrement)
  {
    this.sp = sp;
    this.orientation = orientation;
    this.value = value;
    this.minimum = minimum;
    this.maximum = maximum;
    this.visibleAmount = visibleAmount;
    this.unitIncrement = unitIncrement;
    this.blockIncrement = blockIncrement;
  }
  
  public void addAdjustmentListener (AdjustmentListener listener)
  {
    if (listener == null)
      return;
    adjustmentListener = AWTEventMulticaster.add (adjustmentListener, listener);
  }
  
  public void removeAdjustmentListener (AdjustmentListener listener)
  {
    if (listener == null)
      return;
    adjustmentListener = AWTEventMulticaster.remove (adjustmentListener, listener);
  }
  
  public AdjustmentListener[] getAdjustmentListeners ()
  {
    return (AdjustmentListener[]) AWTEventMulticaster.getListeners
                               (adjustmentListener, AdjustmentListener.class);
  }

  public int getBlockIncrement ()
  {
    return blockIncrement;
  }

  public int getMaximum ()
  {
    return maximum;
  }

  public int getMinimum ()
  {
    return minimum;
  }

  public int getOrientation ()
  {
    return orientation;
  }

  public int getUnitIncrement ()
  {
    return unitIncrement;
  }
  
  public int getValue ()
  {
    return value;
  }

  public int getVisibleAmount ()
  {
    return visibleAmount;
  }

  public void setBlockIncrement (int blockIncrement)
  {
    this.blockIncrement = blockIncrement;
  }
    
  /**
   * This method should never be called.
   * 
   * @param maximum The maximum value to be set.
   * @throws AWTError Always throws this error when called.
   */
  public void setMaximum (int maximum) throws AWTError
  {
    throw new AWTError("Can be set by scrollpane only");
  }

  /**
   * This method should never be called.
   * 
   * @param minimum The minimum value to be set.
   * @throws AWTError Always throws this error when called.
   */
  public void setMinimum (int minimum)
  {
    throw new AWTError("Can be set by scrollpane only");
  }

  public void setUnitIncrement (int unitIncrement)
  {
    this.unitIncrement = unitIncrement;
  }

  public void setValue (int value)
  {
    this.value = value;

    if (value < minimum)
      minimum = value;

    if (value > maximum)
      maximum = value;
  }
  
  /**
   * This method should never be called.
   * 
   * @param visibleAmount The visible amount to be set.
   * @throws AWTError Always throws this error when called.
   */
  public void setVisibleAmount (int visibleAmount)
  {
    throw new AWTError("Can be set by scrollpane only");
  }

  public String paramString ()
  {
    return paramStringHelper() 
         + ",[" + getMinimum() + ".." + getMaximum() 
         + "],val=" + getValue() 
         + ",vis=" + getVisibleAmount() 
         + ",unit=" + getUnitIncrement()
         + ",block=" + getBlockIncrement() 
         + ",isAdjusting=" + valueIsAdjusting;
  }

  private String paramStringHelper()
  {
    if (getOrientation() == HORIZONTAL)
      return "horizontal";
    else
      return "vertical";
  }
  
  public String toString()
  {
    return getClass().getName() + "[" + paramString() + "]";
  }

  /**
   * Returns true if the value is in the process of changing.
   *
   * @since 1.4
   */
  public boolean getValueIsAdjusting ()
  {
    return valueIsAdjusting;
  }

  /**
   * Sets the value of valueIsAdjusting.
   *
   * @since 1.4
   */
  public void setValueIsAdjusting (boolean valueIsAdjusting)
  {
    this.valueIsAdjusting = valueIsAdjusting;
  }
  
} // class ScrollPaneAdjustable

