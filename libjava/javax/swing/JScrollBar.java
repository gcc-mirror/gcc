/* JScrollBar.java --
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

import java.awt.Adjustable;
import java.awt.Dimension;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleStateSet;
import javax.accessibility.AccessibleValue;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.ScrollBarUI;


/**
 * The JScrollBar. Two buttons control how the values that the 
 * scroll bar can take. You can also drag the thumb or click the track
 * to move the scroll bar. Typically, the JScrollBar is used with
 * other components to translate the value of the bar to the viewable
 * contents of the other components.
 */
public class JScrollBar extends JComponent implements Adjustable, Accessible
{
  /**
   * DOCUMENT ME!
   */
  protected class AccessibleJScrollBar extends JComponent.AccessibleJComponent
    implements AccessibleValue
  {
    private static final long serialVersionUID = -7758162392045586663L;
    
    /**
     * Creates a new AccessibleJSlider object.
     *
     * @param value0 DOCUMENT ME!
     */
    protected AccessibleJScrollBar()
    {
      super();
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public AccessibleStateSet getAccessibleStateSet()
    {
      return null;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public AccessibleRole getAccessibleRole()
    {
      return null;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public AccessibleValue getAccessibleValue()
    {
      return null;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Number getCurrentAccessibleValue()
    {
      return null;
    }

    /**
     * setCurrentAccessibleValue
     *
     * @param value0 TODO
     *
     * @return boolean
     */
    public boolean setCurrentAccessibleValue(Number value0)
    {
      return false;
    }

    /**
     * getMinimumAccessibleValue
     *
     * @return Number
     */
    public Number getMinimumAccessibleValue()
    {
      return null;
    }

    /**
     * getMaximumAccessibleValue
     *
     * @return Number
     */
    public Number getMaximumAccessibleValue()
    {
      return null;
    }
  }

  private static final long serialVersionUID = -8195169869225066566L;
  
  /** Fired in a PropertyChangeEvent when the "blockIncrement" changes. */
  public static final String BLOCK_INCREMENT_CHANGED_PROPERTY = "blockIncrement";

  /** Fired in a PropertyChangeEvent when the "model" changes. */
  public static final String MODEL_CHANGED_PROPERTY = "model";

  /** Fired in a PropertyChangeEvent when the "orientation" changes. */
  public static final String ORIENTATION_CHANGED_PROPERTY = "orientation";

  /** Fired in a PropertyChangeEvent when the "unitIncrement" changes. */
  public static final String UNIT_INCREMENT_CHANGED_PROPERTY = "unitIncrement";

  /** How much the thumb moves when moving in a block. */
  protected int blockIncrement = 10;

  /** The model that holds the scroll bar's data. */
  protected BoundedRangeModel model;

  /** The orientation of the scroll bar. */
  protected int orientation = SwingConstants.VERTICAL;

  /** How much the thumb moves when moving in a unit. */
  protected int unitIncrement = 1;

  /** The ChangeListener that listens to the model. */
  private transient ChangeListener changeListener;

  /** The ChangeEvent that's fired. */
  private transient ChangeEvent changeEvent;

  /** 
   * Creates a new horizontal JScrollBar object with a minimum
   * of 0, a maxmium of 100, a value of 0 and an extent of 10.
   */
  public JScrollBar()
  {
    this(SwingConstants.VERTICAL, 0, 10, 0, 100);
  }

  /**
   * Creates a new JScrollBar object with a minimum of 0, a 
   * maximum of 100, a value of 0, an extent of 10 and the given
   * orientation.
   *
   * @param orientation The orientation of the JScrollBar.
   */
  public JScrollBar(int orientation)
  {
    this(orientation, 0, 10, 0, 100);
  }

  /**
   * Creates a new JScrollBar object with the given orientation, 
   * value, min, max, and extent.
   *
   * @param orientation The orientation to use.
   * @param value The value to use.
   * @param extent The extent to use.
   * @param min The minimum value of the scrollbar.
   * @param max The maximum value of the scrollbar.
   */
  public JScrollBar(int orientation, int value, int extent, int min, int max)
  {
    model = new DefaultBoundedRangeModel(value, extent, min, max);
    if (orientation != SwingConstants.HORIZONTAL
        && orientation != SwingConstants.VERTICAL)
      throw new IllegalArgumentException(orientation
                                         + " is not a legal orientation");
    this.orientation = orientation;
    changeListener = createChangeListener();
    model.addChangeListener(changeListener);
    updateUI();
  }

  /**
   * This method sets the UI of this scrollbar to
   * the given UI.
   *
   * @param ui The UI to use with this scrollbar.
   */
  public void setUI(ScrollBarUI ui)
  {
    super.setUI(ui);
  }

  /**
   * This method returns the UI that is being used
   * with this scrollbar.
   *
   * @return The scrollbar's current UI.
   */
  public ScrollBarUI getUI()
  {
    return (ScrollBarUI) ui;
  }

  /**
   * This method changes the UI to be the
   * default for the current look and feel.
   */
  public void updateUI()
  {
    setUI((ScrollBarUI) UIManager.getUI(this));
    invalidate();
    repaint();
  }

  /**
   * This method returns an identifier to 
   * choose the correct UI delegate for the
   * scrollbar.
   *
   * @return The identifer to choose the UI delegate; "ScrollBarUI"
   */
  public String getUIClassID()
  {
    return "ScrollBarUI";
  }

  /**
   * This method returns the orientation of the scrollbar.
   *
   * @return The orientation of the scrollbar.
   */
  public int getOrientation()
  {
    return orientation;
  }

  /**
   * This method sets the orientation of the scrollbar.
   *
   * @param orientation The orientation of the scrollbar.
   */
  public void setOrientation(int orientation)
  {
    if (orientation != SwingConstants.HORIZONTAL
        && orientation != SwingConstants.VERTICAL)
      throw new IllegalArgumentException("orientation must be one of HORIZONTAL or VERTICAL");
    if (orientation != this.orientation)
      {
	int oldOrientation = this.orientation;
	this.orientation = orientation;
	firePropertyChange(ORIENTATION_CHANGED_PROPERTY, oldOrientation,
	                   this.orientation);
      }
  }

  /**
   * This method returns the model being used with 
   * the scrollbar.
   *
   * @return The scrollbar's model.
   */
  public BoundedRangeModel getModel()
  {
    return model;
  }

  /**
   * This method sets the model to use with
   * the scrollbar.
   *
   * @param newModel The new model to use with the scrollbar.
   */
  public void setModel(BoundedRangeModel newModel)
  {
    if (model != newModel)
      {
	BoundedRangeModel oldModel = model;
	model = newModel;
	oldModel.removeChangeListener(changeListener);
	model.addChangeListener(changeListener);
	firePropertyChange(MODEL_CHANGED_PROPERTY, oldModel, model);
      }
  }

  /**
   * This method returns how much the scrollbar's value
   * should change for a unit increment depending on the 
   * given direction.
   *
   * @param direction The direction to scroll in.
   *
   * @return The amount the scrollbar's value will change given the direction.
   */
  public int getUnitIncrement(int direction)
  {
    return direction * unitIncrement;
  }

  /**
   * This method sets the unitIncrement property.
   *
   * @param unitIncrement The new unitIncrement.
   */
  public void setUnitIncrement(int unitIncrement)
  {
    if (unitIncrement != this.unitIncrement)
      {
	int oldInc = this.unitIncrement;
	this.unitIncrement = unitIncrement;
	firePropertyChange(UNIT_INCREMENT_CHANGED_PROPERTY, oldInc,
	                   this.unitIncrement);
      }
  }

  /**
   * The method returns how much the scrollbar's value
   * should change for a block increment depending on
   * the given direction.
   *
   * @param direction The direction to scroll in.
   *
   * @return The amount the scrollbar's value will change given the direction.
   */
  public int getBlockIncrement(int direction)
  {
    return direction * blockIncrement;
  }

  /**
   * This method sets the blockIncrement property.
   *
   * @param blockIncrement The new blockIncrement.
   */
  public void setBlockIncrement(int blockIncrement)
  {
    if (blockIncrement != this.blockIncrement)
      {
	int oldInc = this.blockIncrement;
	this.blockIncrement = blockIncrement;
	firePropertyChange(BLOCK_INCREMENT_CHANGED_PROPERTY, oldInc,
	                   this.blockIncrement);
      }
  }

  /**
   * This method returns the unitIncrement.
   *
   * @return The unitIncrement.
   */
  public int getUnitIncrement()
  {
    return unitIncrement;
  }

  /**
   * This method returns the blockIncrement.
   *
   * @return The blockIncrement.
   */
  public int getBlockIncrement()
  {
    return blockIncrement;
  }

  /**
   * This method returns the value of the scrollbar.
   *
   * @return The value of the scrollbar.
   */
  public int getValue()
  {
    return model.getValue();
  }

  /**
   * This method changes the value of the scrollbar.
   *
   * @param value The new value of the scrollbar.
   */
  public void setValue(int value)
  {
    if (isEnabled() && value != getValue())
    {
      model.setValue(value);
      fireAdjustmentValueChanged(AdjustmentEvent.ADJUSTMENT_VALUE_CHANGED,
                                 AdjustmentEvent.TRACK, value);
    }
  }

  /**
   * This method returns the visible amount (AKA extent). 
   * The visible amount can be used by UI delegates to 
   * determine the size of the thumb.
   *
   * @return The visible amount (AKA extent).
   */
  public int getVisibleAmount()
  {
    return model.getExtent();
  }

  /**
   * This method sets the visible amount (AKA extent).
   *
   * @param extent The visible amount (AKA extent).
   */
  public void setVisibleAmount(int extent)
  {
    if (extent != getVisibleAmount())
    {
      model.setExtent(extent);
      fireAdjustmentValueChanged(AdjustmentEvent.ADJUSTMENT_VALUE_CHANGED,
                                 AdjustmentEvent.TRACK, extent);
    }
  }

  /**
   * This method returns the minimum value of the scrollbar.
   *
   * @return The minimum value of the scrollbar.
   */
  public int getMinimum()
  {
    return model.getMinimum();
  }

  /**
   * This method sets the minimum value of the scrollbar.
   *
   * @param minimum The minimum value of the scrollbar.
   */
  public void setMinimum(int minimum)
  {
    if (minimum != getMinimum())
    {
      model.setMinimum(minimum);
      fireAdjustmentValueChanged(AdjustmentEvent.ADJUSTMENT_VALUE_CHANGED,
                                 AdjustmentEvent.TRACK, minimum);
    }
  }

  /**
   * This method returns the maximum value of the scrollbar.
   *
   * @return The maximum value of the scrollbar.
   */
  public int getMaximum()
  {
    return model.getMaximum();
  }

  /**
   * This method sets the maximum value of the scrollbar.
   *
   * @param maximum The maximum value of the scrollbar.
   */
  public void setMaximum(int maximum)
  {
    if (maximum != getMaximum())
    {
      model.setMaximum(maximum);
      fireAdjustmentValueChanged(AdjustmentEvent.ADJUSTMENT_VALUE_CHANGED,
                                 AdjustmentEvent.TRACK, maximum);
    }
  }

  /**
   * This method returns the model's isAjusting value.
   *
   * @return The model's isAdjusting value.
   */
  public boolean getValueIsAdjusting()
  {
    return model.getValueIsAdjusting();
  }

  /**
   * This method sets the model's isAdjusting value.
   *
   * @param b The new isAdjusting value.
   */
  public void setValueIsAdjusting(boolean b)
  {
    model.setValueIsAdjusting(b);
  }

  /**
   * This method sets the value, extent, minimum and 
   * maximum.
   *
   * @param newValue The new value.
   * @param newExtent The new extent.
   * @param newMin The new minimum.
   * @param newMax The new maximum.
   */
  public void setValue(int newValue, int newExtent, int newMin, int newMax)
  {
    if (!isEnabled())
      newValue = model.getValue();
    // It seems to be that on any change the value is fired.
    if (newValue != getValue() || newExtent != getVisibleAmount() ||
        newMin != getMinimum() || newMax != getMaximum())
    {
      model.setRangeProperties(newValue, newExtent, newMin, newMax,
                               model.getValueIsAdjusting());
      fireAdjustmentValueChanged(AdjustmentEvent.ADJUSTMENT_VALUE_CHANGED,
                                 AdjustmentEvent.TRACK, newValue);
    }
  }

  /**
   * This method creates a new ChangeListener.
   *
   * @return A new ChangeListener.
   */
  private ChangeListener createChangeListener()
  {
    return new ChangeListener()
      {
	public void stateChanged(ChangeEvent e)
	{
	  fireStateChanged();
	}
      };
  }

  /**
   * This method is called whenever the model fires a ChangeEvent. It should
   * propagate the ChangeEvent to its listeners with a new ChangeEvent that
   * identifies the scroll bar as the source.
   */
  private void fireStateChanged()
  {
    Object[] changeListeners = listenerList.getListenerList();
    if (changeEvent == null)
      changeEvent = new ChangeEvent(this);
    for (int i = changeListeners.length - 2; i >= 0; i -= 2)
      {
	if (changeListeners[i] == ChangeListener.class)
	  ((ChangeListener) changeListeners[i + 1]).stateChanged(changeEvent);
      }
  }

  /**
   * This method adds a ChangeListener to the scroll bar.
   *
   * @param listener The listener to add.
   */
  public void addChangeListener(ChangeListener listener)
  {
    listenerList.add(ChangeListener.class, listener);
  }

  /**
   * This method removes a ChangeListener from the scroll bar.
   *
   * @param listener The listener to remove.
   */
  public void removeChangeListener(ChangeListener listener)
  {
    listenerList.remove(ChangeListener.class, listener);
  }

  /**
   * This method returns an array of all ChangeListeners listening to this
   * scroll bar.
   *
   * @return An array of ChangeListeners listening to this scroll bar.
   */
  public ChangeListener[] getChangeListeners()
  {
    return (ChangeListener[]) listenerList.getListeners(ChangeListener.class);
  }

  /**
   * This method adds an AdjustmentListener to the scroll bar.
   *
   * @param listener The listener to add.
   */
  public void addAdjustmentListener(AdjustmentListener listener)
  {
    listenerList.add(AdjustmentListener.class, listener);
  }

  /**
   * This method removes an AdjustmentListener from the scroll bar. 
   *
   * @param listener The listener to remove.
   */
  public void removeAdjustmentListener(AdjustmentListener listener)
  {
    listenerList.remove(AdjustmentListener.class, listener);
  }

  /**
   * This method returns an arry of all AdjustmentListeners listening to 
   * this scroll bar.
   *
   * @return An array of AdjustmentListeners listening to this scroll bar.
   */
  public AdjustmentListener[] getAdjustmentListeners()
  {
    return (AdjustmentListener[]) listenerList.getListeners(AdjustmentListener.class);
  }

  /**
   * This method is called to fired AdjustmentEvents to the listeners
   * of this scroll bar. All AdjustmentEvents that are fired
   * will have an ID of ADJUSTMENT_VALUE_CHANGED and a type of
   * TRACK. 
   *
   * @param id The ID of the adjustment event.
   * @param type The Type of change.
   * @param value The new value for the property that was changed..
   */
  protected void fireAdjustmentValueChanged(int id, int type, int value)
  {
    Object[] adjustmentListeners = listenerList.getListenerList();
    AdjustmentEvent adjustmentEvent = new AdjustmentEvent(this, 
                                            AdjustmentEvent.ADJUSTMENT_VALUE_CHANGED,
					    AdjustmentEvent.TRACK,
					    value);
    for (int i = adjustmentListeners.length - 2; i >= 0; i -= 2)
      {
	if (adjustmentListeners[i] == AdjustmentListener.class)
	  ((AdjustmentListener) adjustmentListeners[i + 1]).adjustmentValueChanged(adjustmentEvent);
      }
  }

  /**
   * This method returns the minimum size for this scroll bar.
   *
   * @return The minimum size.
   */
  public Dimension getMinimumSize()
  {
    return ui.getMinimumSize(this);
  }

  /**
   * This method returns the maximum size for this scroll bar.
   *
   * @return The maximum size.
   */
  public Dimension getMaximumSize()
  {
    return ui.getMaximumSize(this);
  }

  /**
   * This method overrides the setEnabled in JComponent.
   * When the scroll bar is disabled, the knob cannot
   * be moved.
   *
   * @param x Whether the scrollbar is enabled.
   */
  public void setEnabled(boolean x)
  {
    // nothing special needs to be done here since we 
    // just check the enabled setting before changing the value.
    super.setEnabled(x);
  }

  /**
   * A string that describes this JScrollBar. Normally only used
   * for debugging.
   *
   * @return A string describing this JScrollBar.
   */
  protected String paramString()
  {
    return "JScrollBar";
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJScrollBar();
    return accessibleContext;
  }
}
