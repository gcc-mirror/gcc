/* JSlider.java --
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


package javax.swing;

import java.awt.Dimension;
import java.awt.MenuContainer;
import java.awt.image.ImageObserver;
import java.beans.PropertyChangeEvent;
import java.io.Serializable;
import java.util.Dictionary;
import java.util.Enumeration;
import java.util.Hashtable;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleStateSet;
import javax.accessibility.AccessibleValue;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.SliderUI;

/**
 * The JSlider is a Swing component that allows selection of a value within a
 * range by adjusting a thumb in a track. The values for the minimum,
 * maximum, extent and value are stored in a {@link
 * DefaultBoundedRangeModel}.
 * 
 * <p>
 * JSliders have the following properties:
 * </p>
 * 
 * <table>
 * <tr><th> Property         </th><th> Stored in </th><th> Bound? </th></tr>
 * <tr><td> extent           </td><td> model     </td><td> no     </td></tr>
 * <tr><td> inverted         </td><td> slider    </td><td> yes    </td></tr>
 * <tr><td> labelTable       </td><td> slider    </td><td> yes    </td></tr>
 * <tr><td> majorTickSpacing </td><td> slider    </td><td> yes    </td></tr> 
 * <tr><td> maximum          </td><td> model     </td><td> no     </td></tr>
 * <tr><td> minimum          </td><td> model     </td><td> no     </td></tr>
 * <tr><td> minorTickSpacing </td><td> slider    </td><td> yes    </td></tr>
 * <tr><td> model            </td><td> slider    </td><td> yes    </td></tr> 
 * <tr><td> orientation      </td><td> slider    </td><td> yes    </td></tr>
 * <tr><td> paintLabels      </td><td> slider    </td><td> yes    </td></tr>
 * <tr><td> paintTicks       </td><td> slider    </td><td> yes    </td></tr>
 * <tr><td> snapToTicks      </td><td> slider    </td><td> no     </td></tr>
 * <tr><td> value            </td><td> model     </td><td> no     </td></tr>
 * <tr><td> valueIsAdjusting </td><td> model     </td><td> no     </td></tr>
 * </table>
 * 
 * <p>
 * The various behavioral aspects of these properties follows:
 * </p>
 * 
 * <ul>
 * <li>
 * When non-bound properties stored in the slider change, the slider fires
 * ChangeEvents to its ChangeListeners.
 * </li>
 * <li>
 * When bound properties stored in the slider change, the slider fires
 * PropertyChangeEvents to its PropertyChangeListeners
 * </li>
 * <li>
 * If any of the model's properties change, it fires a ChangeEvent to its
 * ChangeListeners, which include the slider.
 * </li>
 * <li>
 * If the slider receives a ChangeEvent from its model, it will propagate the
 * ChangeEvent to its ChangeListeners, with the ChangeEvent's "source"
 * property set to refer to the slider, rather than the model.
 * </li>
 * </ul>
 */
public class JSlider extends JComponent implements SwingConstants, Accessible,
                                                   ImageObserver,
                                                   MenuContainer, Serializable
{
  /** DOCUMENT ME! */
  private static final long serialVersionUID = -1441275936141218479L;

  /**
   * DOCUMENT ME!
   */
  protected class AccessibleJSlider extends JComponent.AccessibleJComponent
    implements AccessibleValue
  {
    private static final long serialVersionUID = -6301740148041106789L;
  
    /**
     * Creates a new AccessibleJSlider object.
     */
    protected AccessibleJSlider()
    {
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

  /** Whether or not this slider paints its ticks. */
  private transient boolean paintTicks = false;

  /** Whether or not this slider paints its track. */
  private transient boolean paintTrack = true;

  /** Whether or not this slider paints its labels. */
  private transient boolean paintLabels = false;

  /**
   * A dictionary of (Integer, Component) pairs where each Component is a
   * JLabel and the Integer determines where the label will be painted.
   */
  private transient Dictionary labelTable;

  /** The model used to describe the slider. */
  protected BoundedRangeModel sliderModel;

  /** The space between major ticks. */
  protected int majorTickSpacing;

  /** The space between minor ticks. */
  protected int minorTickSpacing;

  /** Whether the slider snaps its values to ticks. */
  protected boolean snapToTicks = false;

  /** The orientation of the slider. */
  protected int orientation = HORIZONTAL;

  /** Whether the slider is inverted. */
  private transient boolean isInverted;

  /** The ChangeListener that listens to the model. */
  protected ChangeListener changeListener;

  /** The ChangeEvent that is passed to all listeners of this slider. */
  protected transient ChangeEvent changeEvent;

  /**
   * Creates a new horizontal JSlider object with a minimum of 0, a maximum of
   * 100, and a value of 50.
   */
  public JSlider()
  {
    this(HORIZONTAL, 0, 100, 50);
  }

  /**
   * Creates a new JSlider object with the given orientation and a minimum of
   * 0, a maximum of 100, and a value of 50.
   *
   * @param orientation The orientation of the slider ({@link #HORIZONTAL} or
   *                    {@link #VERTICAL}).
   * 
   * @throws IllegalArgumentException if <code>orientation</code> is not one of
   *         the specified values.
   */
  public JSlider(int orientation)
  {
    this(orientation, 0, 100, 50);
  }

  /**
   * Creates a new horizontal JSlider object with the given maximum and
   * minimum and a value that is  halfway between the minimum and the
   * maximum.
   *
   * @param minimum The minimum value of the JSlider.
   * @param maximum The maximum value of the JSlider.
   */
  public JSlider(int minimum, int maximum)
  {
    this(HORIZONTAL, minimum, maximum, (maximum + minimum) / 2);
  }

  /**
   * Creates a new horizontal JSlider object with the given minimum, maximum,
   * and value.
   *
   * @param minimum The minimum value of the JSlider.
   * @param maximum The maximum value of the JSlider.
   * @param value The initial value of the JSlider.
   */
  public JSlider(int minimum, int maximum, int value)
  {
    this(HORIZONTAL, minimum, maximum, value);
  }

  /**
   * Creates a new JSlider object with the given orientation, minimum,
   * maximum, and value.
   *
   * @param orientation The orientation of the slider ({@link #HORIZONTAL} or
   *                    {@link #VERTICAL}).
   * @param minimum The minimum value of the JSlider.
   * @param maximum The maximum value of the JSlider.
   * @param value The initial value of the JSlider.
   * 
   * @throws IllegalArgumentException if <code>orientation</code> is not one of
   *         the specified values.
   */
  public JSlider(int orientation, int minimum, int maximum, int value)
  {
    sliderModel = new DefaultBoundedRangeModel(value, 0, minimum, maximum);
    if (orientation != HORIZONTAL && orientation != VERTICAL)
      throw new IllegalArgumentException(orientation + " is not a legal orientation");
    this.orientation = orientation;
    changeListener = createChangeListener();
    sliderModel.addChangeListener(changeListener);
    updateUI();
  }

  /**
   * Creates a new horizontal JSlider object with the given model.
   *
   * @param model The model (<code>null</code> not permitted).
   * 
   * @throws NullPointerException if <code>model</code> is <code>null</code>.
   */
  public JSlider(BoundedRangeModel model)
  {
    sliderModel = model;
    changeListener = createChangeListener();
    sliderModel.addChangeListener(changeListener);
    updateUI();
  }

  /**
   * This method returns the current value of the slider.
   *
   * @return The value of the slider stored in the model.
   */
  public int getValue()
  {
    return sliderModel.getValue();
  }

  /**
   * This method sets the value of the slider.
   *
   * @param value The slider's new value.
   */
  public void setValue(int value)
  {
    sliderModel.setValue(value);
  }

  /**
   * This method returns the slider's UI delegate.
   *
   * @return The slider's UI delegate.
   */
  public SliderUI getUI()
  {
    return (SliderUI) ui;
  }

  /**
   * This method sets the slider's UI delegate.
   *
   * @param ui A SliderUI object to use with this slider.
   */
  public void setUI(SliderUI ui)
  {
    super.setUI(ui);
  }

  /**
   * This method sets this slider's UI to the UIManager's default for the
   * current look and feel.
   */
  public void updateUI()
  {
    setUI((SliderUI) UIManager.getUI(this));
    invalidate();
    repaint();
  }

  /**
   * This method returns a name to identify which look and feel class will be
   * the UI delegate for the slider.
   *
   * @return The Look and Feel classID. "SliderUI"
   */
  public String getUIClassID()
  {
    return "SliderUI";
  }

  /**
   * Creates a ChangeListener for this Slider.
   *
   * @return A new ChangeListener.
   */
  protected ChangeListener createChangeListener()
  {
    return new ChangeListener()
      {
	public void stateChanged(ChangeEvent ce)
	{
	  // No need to trigger a repaint since the UI listens to the model
	  // as well. All we need to do is pass on the stateChanged event 
	  // to our listeners.
	  fireStateChanged();
	}
      };
  }

  /**
   * This method registers a listener to this slider. The listener will be
   * informed of new ChangeEvents.
   *
   * @param listener The listener to register.
   */
  public void addChangeListener(ChangeListener listener)
  {
    listenerList.add(ChangeListener.class, listener);
  }

  /**
   * This method removes a listener from this slider.
   *
   * @param listener The listener to remove.
   */
  public void removeChangeListener(ChangeListener listener)
  {
    listenerList.remove(ChangeListener.class, listener);
  }

  /**
   * This method is called whenever the model fires a ChangeEvent. It should
   * propagate the ChangeEvent to its listeners with a new ChangeEvent that
   * identifies the slider as the source.
   */
  protected void fireStateChanged()
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
   * This method returns an array of all ChangeListeners listening to this
   * slider.
   *
   * @return An array of ChangeListeners listening to this slider.
   */
  public ChangeListener[] getChangeListeners()
  {
    return (ChangeListener[]) listenerList.getListeners(ChangeListener.class);
  }

  /**
   * This method returns the model of the slider.
   *
   * @return The slider's model.
   */
  public BoundedRangeModel getModel()
  {
    return sliderModel;
  }

  /**
   * This method changes the "model" property. It also needs  to unregister
   * any listeners to the old model and register any listeners to the new
   * model.
   *
   * @param model The model to use with the slider.
   */
  public void setModel(BoundedRangeModel model)
  {
    // I didn't do the null pointer check on purpose.
    // If you try it with Sun's, it'll go ahead and set it to null
    // and bork the next time it tries to access the model.
    if (model != sliderModel)
      {
	BoundedRangeModel oldModel = sliderModel;
	sliderModel = model;
	oldModel.removeChangeListener(changeListener);
	sliderModel.addChangeListener(changeListener);
	firePropertyChange("model", oldModel, sliderModel);
      }
  }

  /**
   * This method returns the minimum value of the slider.
   *
   * @return The minimum value of the slider.
   */
  public int getMinimum()
  {
    return sliderModel.getMinimum();
  }

  /**
   * This method sets the minimum value of the slider.
   *
   * @param minimum The minimum value of the slider.
   */
  public void setMinimum(int minimum)
  {
    int old = sliderModel.getMinimum();
    sliderModel.setMinimum(minimum);
    if (minimum != old)
      firePropertyChange("minimum", old, minimum);
  }

  /**
   * This method returns the maximum value of the slider.
   *
   * @return The maximum value of the slider.
   */
  public int getMaximum()
  {
    return sliderModel.getMaximum();
  }

  /**
   * This method sets the maximum value of the slider.
   *
   * @param maximum The maximum value of the slider.
   */
  public void setMaximum(int maximum)
  {
    int old = sliderModel.getMaximum();
    sliderModel.setMaximum(maximum);
    if (maximum != old)
      firePropertyChange("maximum", old, maximum);
  }

  /**
   * This method returns this slider's isAdjusting value which is true if the
   * thumb is being dragged.
   *
   * @return The slider's isAdjusting value.
   */
  public boolean getValueIsAdjusting()
  {
    return sliderModel.getValueIsAdjusting();
  }

  /**
   * This method sets the isAdjusting value for the slider.
   *
   * @param adjusting The slider's isAdjusting value.
   */
  public void setValueIsAdjusting(boolean adjusting)
  {
    sliderModel.setValueIsAdjusting(adjusting);
  }

  /**
   * This method returns the extent value for this slider.
   *
   * @return The extent value for this slider.
   */
  public int getExtent()
  {
    return sliderModel.getExtent();
  }

  /**
   * This method sets the extent value for this slider.
   *
   * @param extent The extent value for this slider.
   */
  public void setExtent(int extent)
  {
    sliderModel.setExtent(extent);
  }

  /**
   * This method returns the slider orientation.
   *
   * @return The orientation of the slider.
   */
  public int getOrientation()
  {
    return orientation;
  }

  /**
   * This method changes the "orientation" property of this slider. If the
   * orientation is not VERTICAL or HORIZONTAL, this method does nothing.
   *
   * @param orientation The orientation of this slider.
   */
  public void setOrientation(int orientation)
  {
    if (orientation != VERTICAL && orientation != HORIZONTAL)
      throw new IllegalArgumentException("orientation must be one of: VERTICAL, HORIZONTAL");
    if (orientation != this.orientation)
      {
	int oldOrientation = this.orientation;
	this.orientation = orientation;
	firePropertyChange("orientation", oldOrientation,
	                   this.orientation);
      }
  }

  /**
   * This method returns the label table for this slider.
   *
   * @return The label table for this slider.
   */
  public Dictionary getLabelTable()
  {
    return labelTable;
  }

  /**
   * This method changes the "labelTable" property of this slider.
   *
   * @param table The label table for this slider.
   */
  public void setLabelTable(Dictionary table)
  {
    if (table != labelTable)
      {
	Dictionary oldTable = labelTable;
	labelTable = table;
	firePropertyChange("labelTable", oldTable, labelTable);
      }
  }

  /**
   * This method is called to reset UI delegates for the labels in the
   * labelTable to a default for the current look and feel.
   */
  protected void updateLabelUIs()
  {
    if (labelTable == null)
      return;
    for (Enumeration list = labelTable.elements(); list.hasMoreElements();)
      {
	JLabel label = (JLabel) list.nextElement();
	label.updateUI();
      }
  }

  /**
   * Creates a hashtable of (Integer, JLabel) pairs that can be used as a
   * label table for this slider. The labels will start from the sliders
   * minimum and increase by the increment. Each  label will have a text
   * string indicating their integer value.
   *
   * @param increment The increment between labels (must be > 0).
   *
   * @return A hashtable with the labels and their keys.
   *
   * @throws IllegalArgumentException if <code>increment</code> is not greater
   *         than zero.
   */
  public Hashtable createStandardLabels(int increment)
  {
    return createStandardLabels(increment, sliderModel.getMinimum());
  }

  /**
   * Creates a hashtable of (Integer, JLabel) pairs that can be used as a
   * label table for this slider. The labels will start from the given start
   * value and increase by the increment. Each  label will have a text string
   * indicating its integer value.
   *
   * @param increment The increment between labels (must be > 0).
   * @param start The value to start from.
   *
   * @return A hashtable with the labels and their keys.
   *
   * @throws IllegalArgumentException if <code>increment</code> is not greater
   *         than zero, or <code>start</code> is not within the range of the
   *         model.
   */
  public Hashtable createStandardLabels(int increment, int start)
  {
    if (increment <= 0) 
      throw new IllegalArgumentException("Requires 'increment' > 0.");
    if (start < getMinimum() || start > getMaximum())
      throw new IllegalArgumentException("The 'start' value is out of range.");
    Hashtable table = new Hashtable();
    JLabel label;
    Dimension dim;

    int max = sliderModel.getMaximum();

    for (int i = start; i <= max; i += increment)
      {
	label = new JLabel(String.valueOf(i));
	label.setVerticalAlignment(CENTER);
	label.setHorizontalAlignment(CENTER);
	
	// Make sure these labels have the width and height
	// they want.
	dim = label.getPreferredSize();
	label.setBounds(label.getX(), label.getY(),
	                (int) dim.getWidth(),
			(int) dim.getHeight()); 
	table.put(new Integer(i), label);
      }
    return table;
  }

  /**
   * This method returns whether the slider is inverted. Horizontal sliders
   * that are not inverted will have the minimums on the left. If they are
   * inverted, the minimums will be  on the right. Vertical sliders that are
   * not inverted will have the minimums at the bottom. If they are inverted,
   * the minimums will be at the top.
   *
   * @return Whether this slider is inverted.
   */
  public boolean getInverted()
  {
    return isInverted;
  }

  /**
   * This method changes the "inverted" property for this slider.Horizontal
   * sliders  that are not inverted will have the minimums on the left. If
   * they are inverted, the minimums will be  on the right. Vertical sliders
   * that are not inverted will have the minimums at the bottom. If they are
   * inverted, the minimums will be at the top. However, if the slider's
   * componentOrientation is set to RIGHT_TO_LEFT, then everything gets
   * reversed again.
   *
   * @param inverted Whether the slider should be inverted.
   */
  public void setInverted(boolean inverted)
  {
    if (isInverted != inverted)
      {
	boolean oldInverted = isInverted;
	isInverted = inverted;
	firePropertyChange("inverted", oldInverted, isInverted);
      }
  }

  /**
   * This method returns the amount of units between each major tick mark.
   *
   * @return The amount of units between each major tick mark.
   */
  public int getMajorTickSpacing()
  {
    return majorTickSpacing;
  }

  /**
   * This method changes the "majorTickSpacing" property for this slider. The
   * major tick spacing is the amount of units between each major tick mark.
   *
   * @param spacing The amount of units between each major tick mark.
   */
  public void setMajorTickSpacing(int spacing)
  {
    if (majorTickSpacing != spacing)
      {
	int oldSpacing = majorTickSpacing;
	majorTickSpacing = spacing;
	firePropertyChange("majorTickSpacing", oldSpacing,
	                   majorTickSpacing);
      }
  }

  /**
   * This method returns the amount of units between each minor tick mark.
   *
   * @return The amount of units between each minor tick mark.
   */
  public int getMinorTickSpacing()
  {
    return minorTickSpacing;
  }

  /**
   * This method changes the "minorTickSpacing" property for this slider. The
   * minor tick spacing is the amount of units between each minor tick mark.
   *
   * @param spacing The amount of units between each minor tick mark.
   */
  public void setMinorTickSpacing(int spacing)
  {
    if (minorTickSpacing != spacing)
      {
	int oldSpacing = minorTickSpacing;
	minorTickSpacing = spacing;
	firePropertyChange("minorTickSpacing", oldSpacing,
	                   minorTickSpacing);
      }
  }

  /**
   * This method returns whether this slider is snapping to ticks.  Sliders
   * that snap to ticks will automatically move the thumb to the nearest tick
   * mark.
   *
   * @return Whether this slider snaps to ticks.
   */
  public boolean getSnapToTicks()
  {
    return snapToTicks;
  }

  /**
   * This method sets whether this slider will snap to ticks. Sliders that
   * snap to ticks will automatically move the thumb to the nearest tick
   * mark.
   *
   * @param snap Whether this slider snaps to ticks.
   */
  public void setSnapToTicks(boolean snap)
  {
    if (snap != snapToTicks)
      {
	snapToTicks = snap;
	firePropertyChange("snapToTicks", !snap, snap);
      }
  }

  /**
   * This method returns whether the slider will paint its tick marks. In
   * addition to setting this property to true, one of minor tick spacing  or
   * major tick spacing must be set to a value greater than 0 in order for
   * ticks to be painted.
   *
   * @return Whether ticks will be painted.
   */
  public boolean getPaintTicks()
  {
    return paintTicks;
  }

  /**
   * This method changes the "paintTicks" property for this slider. In
   * addition to setting this property to true, one of minor tick spacing  or
   * major tick spacing must be set to a value greater than 0 in order for
   * ticks to be painted.
   *
   * @param paint Whether ticks will be painted.
   */
  public void setPaintTicks(boolean paint)
  {
    if (paint != paintTicks)
      {
	boolean oldPaintTicks = paintTicks;
	paintTicks = paint;
	firePropertyChange("paintTicks", oldPaintTicks, paintTicks);
      }
  }

  /**
   * This method returns whether the track will be painted.
   *
   * @return Whether the track will be painted.
   */
  public boolean getPaintTrack()
  {
    return paintTrack;
  }

  /**
   * Sets the flag that controls whether or not the track is painted, and
   * sends a {@link PropertyChangeEvent} (for the "paintTrack" property) to all
   * registered listeners.
   *
   * @param paint Whether the track will be painted.
   */
  public void setPaintTrack(boolean paint)
  {
    if (paintTrack != paint)
    {
      paintTrack = paint;
      firePropertyChange("paintTrack", !paint, paint);
    }
  }

  /**
   * This method returns whether labels will be painted.
   *
   * @return Whether labels will be painted.
   */
  public boolean getPaintLabels()
  {
    return paintLabels;
  }

  /**
   * This method changes the "paintLabels" property.
   *
   * @param paint Whether labels will be painted.
   */
  public void setPaintLabels(boolean paint)
  {
    if (paint != paintLabels)
      {
	paintLabels = paint;
        if (paint && majorTickSpacing > 0)
          labelTable = createStandardLabels(majorTickSpacing);
	firePropertyChange("paintLabels", !paint, paint);
      }
  }

  /**
   * This method is used primarily for debugging purposes and returns a string
   * that can be used to represent this slider.
   *
   * @return A string representing this slider.
   */
  protected String paramString()
  {
    return "JSlider";
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJSlider();
    
    return accessibleContext;
  }
}
