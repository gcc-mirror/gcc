/* JSlider.java --
   Copyright (C) 2002, 2004, 2005, 2006,  Free Software Foundation, Inc.

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

import gnu.java.lang.CPStringBuilder;

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
import javax.accessibility.AccessibleState;
import javax.accessibility.AccessibleStateSet;
import javax.accessibility.AccessibleValue;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.SliderUI;
import javax.swing.plaf.UIResource;

/**
 * A visual component that allows selection of a value within a
 * range by adjusting a thumb in a track. The values for the minimum,
 * maximum, extent and value are stored in a {@link
 * DefaultBoundedRangeModel}.
 * <p>
 * A <code>JSlider</code> component has the following properties:
 * </p>
 *
 * <table>
 * <tr><th> Property         </th><th> Stored in </th><th> Bound? </th></tr>
 * <tr><td> extent           </td><td> model     </td><td> no     </td></tr>
 * <tr><td> inverted         </td><td> slider    </td><td> yes    </td></tr>
 * <tr><td> labelTable       </td><td> slider    </td><td> yes    </td></tr>
 * <tr><td> majorTickSpacing </td><td> slider    </td><td> yes    </td></tr>
 * <tr><td> maximum          </td><td> model     </td><td> yes     </td></tr>
 * <tr><td> minimum          </td><td> model     </td><td> yes     </td></tr>
 * <tr><td> minorTickSpacing </td><td> slider    </td><td> yes    </td></tr>
 * <tr><td> model            </td><td> slider    </td><td> yes    </td></tr>
 * <tr><td> orientation      </td><td> slider    </td><td> yes    </td></tr>
 * <tr><td> paintLabels      </td><td> slider    </td><td> yes    </td></tr>
 * <tr><td> paintTicks       </td><td> slider    </td><td> yes    </td></tr>
 * <tr><td> snapToTicks      </td><td> slider    </td><td> yes     </td></tr>
 * <tr><td> value            </td><td> model     </td><td> no     </td></tr>
 * <tr><td> valueIsAdjusting </td><td> model     </td><td> no     </td></tr>
 * </table>
 *
 * <p>
 * The various behavioural aspects of these properties follows:
 * </p>
 *
 * <ul>
 * <li>
 * When a non-bound property stored in the slider changes, the slider fires
 * a {@link ChangeEvent} to its change listeners.
 * </li>
 * <li>
 * When a bound property stored in the slider changes, the slider fires a
 * {@link PropertyChangeEvent} to its property change listeners.
 * </li>
 * <li>
 * If any of the model's properties change, it fires a {@link ChangeEvent} to
 * its listeners, which include the slider.
 * </li>
 * <li>
 * If the slider receives a {@link ChangeEvent} from its model, it will
 * propagate the event to its own change listeners, with the event's "source"
 * property set to refer to the slider, rather than the model.
 * </li>
 * </ul>
 */
public class JSlider extends JComponent implements SwingConstants, Accessible,
                                                   ImageObserver,
                                                   MenuContainer, Serializable
{

  /**
   * A little testing shows that the reference implementation creates
   * labels from a class named LabelUIResource.
   */
  private class LabelUIResource
    extends JLabel
    implements UIResource
  {
    LabelUIResource(String text, int align)
    {
      super(text, align);
      setName("Slider.label");
    }
  }

  private static final long serialVersionUID = -1441275936141218479L;

  /**
   * Provides the accessibility features for the <code>JSlider</code>
   * component.
   */
  protected class AccessibleJSlider extends JComponent.AccessibleJComponent
    implements AccessibleValue
  {
    private static final long serialVersionUID = -6301740148041106789L;

    /**
     * Creates a new <code>AccessibleJSlider</code> instance.
     */
    protected AccessibleJSlider()
    {
      // Nothing to do here.
    }

    /**
     * Returns a set containing the current state of the {@link JSlider}
     * component.
     *
     * @return The accessible state set.
     */
    public AccessibleStateSet getAccessibleStateSet()
    {
      AccessibleStateSet result = super.getAccessibleStateSet();
      if (orientation == JSlider.HORIZONTAL)
        result.add(AccessibleState.HORIZONTAL);
      else if (orientation == JSlider.VERTICAL)
        result.add(AccessibleState.VERTICAL);
      return result;
    }

    /**
     * Returns the accessible role for the <code>JSlider</code> component.
     *
     * @return {@link AccessibleRole#SLIDER}.
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.SLIDER;
    }

    /**
     * Returns an object that provides access to the current, minimum and
     * maximum values for the {@link JSlider}.  Since this class implements
     * {@link AccessibleValue}, it returns itself.
     *
     * @return The accessible value.
     */
    public AccessibleValue getAccessibleValue()
    {
      return this;
    }

    /**
     * Returns the current value of the {@link JSlider} component, as an
     * {@link Integer}.
     *
     * @return The current value of the {@link JSlider} component.
     */
    public Number getCurrentAccessibleValue()
    {
      return new Integer(getValue());
    }

    /**
     * Sets the current value of the {@link JSlider} component and sends a
     * {@link PropertyChangeEvent} (with the property name
     * {@link AccessibleContext#ACCESSIBLE_VALUE_PROPERTY}) to all registered
     * listeners.  If the supplied value is <code>null</code>, this method
     * does nothing and returns <code>false</code>.
     *
     * @param value  the new slider value (<code>null</code> permitted).
     *
     * @return <code>true</code> if the slider value is updated, and
     *     <code>false</code> otherwise.
     */
    public boolean setCurrentAccessibleValue(Number value)
    {
      if (value == null)
        return false;
      Number oldValue = getCurrentAccessibleValue();
      setValue(value.intValue());
      firePropertyChange(AccessibleContext.ACCESSIBLE_VALUE_PROPERTY, oldValue,
                         new Integer(getValue()));
      return true;
    }

    /**
     * Returns the minimum value of the {@link JSlider} component, as an
     * {@link Integer}.
     *
     * @return The minimum value of the {@link JSlider} component.
     */
    public Number getMinimumAccessibleValue()
    {
      return new Integer(getMinimum());
    }

    /**
     * Returns the maximum value of the {@link JSlider} component, as an
     * {@link Integer}.
     *
     * @return The maximum value of the {@link JSlider} component.
     */
    public Number getMaximumAccessibleValue()
    {
      return new Integer(getMaximum());
    }
  }

  /** Whether or not this slider paints its ticks. */
  private transient boolean paintTicks;

  /** Whether or not this slider paints its track. */
  private transient boolean paintTrack = true;

  /** Whether or not this slider paints its labels. */
  private transient boolean paintLabels;

  /**
   * A dictionary of (Integer, Component) pairs where each Component is a
   * JLabel and the Integer determines where the label will be painted.
   */
  private transient Dictionary labelTable;

  /** The model used to store the slider's range and current value. */
  protected BoundedRangeModel sliderModel;

  /** The space/distance between major ticks. */
  protected int majorTickSpacing;

  /** The space/distance between minor ticks. */
  protected int minorTickSpacing;

  /** Whether the slider snaps its values to ticks. */
  protected boolean snapToTicks;

  /** The orientation (horizontal or vertical) of the slider. */
  protected int orientation = HORIZONTAL;

  /** Whether the slider is inverted. */
  private transient boolean isInverted;

  /**
   * The listener that monitors the slider's model and forwards events to the
   * slider's listeners (see <code>createChangeListener()</code>).
   */
  protected ChangeListener changeListener;

  /** The change event that is passed to all listeners of this slider. */
  protected transient ChangeEvent changeEvent;

  /**
   * Creates a new horizontal <code>JSlider</code> instance with a minimum of
   * 0, a maximum of 100, and a value of 50.
   */
  public JSlider()
  {
    this(HORIZONTAL, 0, 100, 50);
  }

  /**
   * Creates a new <code>JSlider</code> instance with the given orientation
   * and a minimum of 0, a maximum of 100, and a value of 50.
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
   * Creates a new horizontal <code>JSlider</code> instance with the given
   * maximum and minimum and a value that is halfway between the minimum and the
   * maximum.
   *
   * @param minimum The minimum value.
   * @param maximum The maximum value.
   *
   * @throws IllegalArgumentException if <code>minimum</code> is greater than
   *     <code>maximum</code>.
   */
  public JSlider(int minimum, int maximum)
  {
    this(HORIZONTAL, minimum, maximum, (maximum + minimum) / 2);
  }

  /**
   * Creates a new horizontal <code>JSlider</code> instance with the given
   * minimum, maximum, and value.
   *
   * @param minimum The minimum value.
   * @param maximum The maximum value.
   * @param value The initial value.
   *
   * @throws IllegalArgumentException if <code>value</code> is not in the
   *     specified range.
   * @throws IllegalArgumentException if <code>minimum</code> is greater than
   *     <code>maximum</code>.
   */
  public JSlider(int minimum, int maximum, int value)
  {
    this(HORIZONTAL, minimum, maximum, value);
  }

  /**
   * Creates a new <code>JSlider</code> instance with the given orientation,
   * minimum, maximum, and value.
   *
   * @param orientation The orientation of the slider ({@link #HORIZONTAL} or
   *                    {@link #VERTICAL}).
   * @param minimum The minimum value of the JSlider.
   * @param maximum The maximum value of the JSlider.
   * @param value The initial value of the JSlider.
   *
   * @throws IllegalArgumentException if <code>orientation</code> is not one of
   *     the specified values.
   * @throws IllegalArgumentException if <code>value</code> is not in the
   *     specified range.
   * @throws IllegalArgumentException if <code>minimum</code> is greater than
   *     <code>maximum</code>.
   */
  public JSlider(int orientation, int minimum, int maximum, int value)
  {
    sliderModel = new DefaultBoundedRangeModel(value, 0, minimum, maximum);
    if (orientation != HORIZONTAL && orientation != VERTICAL)
      throw new IllegalArgumentException(orientation
                                         + " is not a legal orientation");
    this.orientation = orientation;
    changeListener = createChangeListener();
    sliderModel.addChangeListener(changeListener);
    updateUI();
  }

  /**
   * Creates a new horizontal <code>JSlider</code> instance with the given
   * model.
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
   * Returns the slider's value (from the slider's model).
   *
   * @return The value of the slider.
   *
   * @see #setValue(int)
   */
  public int getValue()
  {
    return sliderModel.getValue();
  }

  /**
   * Sets the slider's value and sends a {@link ChangeEvent} to all
   * registered listeners.  Note that the model will fire a change event to all
   * of its registered listeners first (with the model as the event source) and
   * then the slider will fire another change event to all of its registered
   * listeners (this time with the slider as the event source).
   *
   * @param value  the new value.
   *
   * @see #getValue()
   */
  public void setValue(int value)
  {
    sliderModel.setValue(value);
  }

  /**
   * Returns the slider's UI delegate.
   *
   * @return The slider's UI delegate.
   */
  public SliderUI getUI()
  {
    return (SliderUI) ui;
  }

  /**
   * Sets the slider's UI delegate.
   *
   * @param ui  the UI delegate.
   */
  public void setUI(SliderUI ui)
  {
    super.setUI(ui);
  }

  /**
   * Sets this slider's UI delegate to the default (obtained from the
   * {@link UIManager}) for the current look and feel.
   */
  public void updateUI()
  {
    updateLabelUIs();
    setUI((SliderUI) UIManager.getUI(this));
  }

  /**
   * Returns the suffix (<code>"SliderUI"</code> in this case) used to
   * determine the class name for a UI delegate that can provide the look and
   * feel for a <code>JSlider</code>.
   *
   * @return <code>"SliderUI"</code>.
   */
  public String getUIClassID()
  {
    return "SliderUI";
  }

  /**
   * Creates a {@link ChangeListener} that is added to the slider's model and
   * forwards change events generated by the model to the listeners that are
   * registered with the <code>JSlider</code> (by calling the
   * {@link #fireStateChanged} method).
   *
   * @return A new listener.
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
   * Registers a listener with the slider so that it will receive
   * {@link ChangeEvent} notifications.  Note that change events generated
   * by the slider's model will be forwarded automatically to the slider's
   * listeners.
   *
   * @param listener  the listener to register.
   *
   * @see #removeChangeListener(ChangeListener)
   */
  public void addChangeListener(ChangeListener listener)
  {
    listenerList.add(ChangeListener.class, listener);
  }

  /**
   * Removes a listener from this slider so that it will no longer receive
   * {@link ChangeEvent} notifications from the slider.
   *
   * @param listener The listener to remove.
   *
   * @see #addChangeListener(ChangeListener)
   */
  public void removeChangeListener(ChangeListener listener)
  {
    listenerList.remove(ChangeListener.class, listener);
  }

  /**
   * Sends a {@link ChangeEvent} to all registered listeners, with this slider
   * as the source.
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
   * Returns an array containing all the {@link ChangeListener} instances
   * registered with this slider.  If no listeners are registered, this method
   * returns an empty array.
   *
   * @return An array array containing all the {@link ChangeListener} instances
   *     registered with this slider (possibly empty, but never
   *     <code>null</code>).
   */
  public ChangeListener[] getChangeListeners()
  {
    return (ChangeListener[]) listenerList.getListeners(ChangeListener.class);
  }

  /**
   * Returns the slider's model, which stores the minimum, maximum and current
   * values.
   *
   * @return The slider's model.
   *
   * @see #setModel(BoundedRangeModel)
   */
  public BoundedRangeModel getModel()
  {
    return sliderModel;
  }

  /**
   * Sets the slider's model and sends a {@link PropertyChangeEvent} (with the
   * property name "model") to all registered listeners.   The change listener
   * that the slider registered with the original model is removed and added
   * to the new model (this ensures that {@link ChangeEvent} notifications
   * generated by the model are automatically forwarded to listeners that are
   * registered with the slider).
   *
   * @param model The model to use with the slider.
   *
   * @see #getModel()
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
   * Returns the minimum value of the slider (from the slider's model).
   *
   * @return The minimum value of the slider.
   *
   * @see #setMinimum(int)
   */
  public int getMinimum()
  {
    return sliderModel.getMinimum();
  }

  /**
   * Sets the minimum value of the slider and fires a
   * {@link PropertyChangeEvent} (with the property name "minimum") to all
   * registered listeners.  Note that:
   * <p>
   * <ul>
   * <li>the minimum value is stored in the slider's model (see
   *     {@link #getModel()});</li>
   * <li>in addition to the property change event, the slider also fires a
   *     {@link ChangeEvent}.</li>
   * </ul>
   *
   * @param minimum The minimum value of the slider.
   *
   * @see #getMinimum()
   */
  public void setMinimum(int minimum)
  {
    int old = sliderModel.getMinimum();
    sliderModel.setMinimum(minimum);
    if (minimum != old)
      firePropertyChange("minimum", old, minimum);
  }

  /**
   * Returns the slider's maximum value (obtained from the slider's model).
   *
   * @return The maximum value of the slider.
   *
   * @see #setMaximum(int)
   */
  public int getMaximum()
  {
    return sliderModel.getMaximum();
  }

  /**
   * Sets the maximum value of the slider and fires a
   * {@link PropertyChangeEvent} (with the property name "maximum") to all
   * registered listeners.  Note that:
   * <p>
   * <ul>
   * <li>the maximum value is stored in the slider's model (see
   *     {@link #getModel()});</li>
   * <li>in addition to the property change event, the slider also fires a
   *     {@link ChangeEvent}.</li>
   * </ul>
   *
   * @param maximum The maximum value of the slider.
   *
   * @see #getMaximum()
   */
  public void setMaximum(int maximum)
  {
    int old = sliderModel.getMaximum();
    sliderModel.setMaximum(maximum);
    if (maximum != old)
      firePropertyChange("maximum", old, maximum);
  }

  /**
   * Returns the <code>valueIsAdjusting</code> flag from the slider's model.
   *
   * @return The <code>valueIsAdjusting</code> flag from the slider's model.
   *
   * @see #setValueIsAdjusting(boolean)
   */
  public boolean getValueIsAdjusting()
  {
    return sliderModel.getValueIsAdjusting();
  }

  /**
   * Sets the <code>valueIsAdjusting</code> flag in the slider's model, and
   * sends a {@link ChangeEvent} to all registered listeners.
   *
   * @param adjusting  the new flag value.
   *
   * @see #getValueIsAdjusting()
   */
  public void setValueIsAdjusting(boolean adjusting)
  {
    sliderModel.setValueIsAdjusting(adjusting);
  }

  /**
   * Returns the slider's extent value, obtained from the slider's model.
   *
   * @return The extent value.
   *
   * @see #setExtent(int)
   */
  public int getExtent()
  {
    return sliderModel.getExtent();
  }

  /**
   * Sets the slider's extent value and sends a {@link ChangeEvent} to all
   * registered listeners.  Note that the model will fire a change event to all
   * of its registered listeners first (with the model as the event source) and
   * then the slider will fire another change event to all of its registered
   * listeners (this time with the slider as the event source).
   *
   * @param extent The extent value for this slider.
   *
   * @see #getExtent()
   */
  public void setExtent(int extent)
  {
    sliderModel.setExtent(extent);
  }

  /**
   * Returns the orientation of the slider, either {@link JSlider#HORIZONTAL}
   * or {@link JSlider#VERTICAL}.
   *
   * @return The orientation of the slider.
   *
   * @see #setOrientation(int)
   */
  public int getOrientation()
  {
    return orientation;
  }

  /**
   * Sets the orientation for the slider and sends a
   * {@link PropertyChangeEvent} (with the property name "orientation") to all
   * registered listeners.
   *
   * @param orientation  the orientation (one of {@link JSlider#HORIZONTAL} or
   *     {@link JSlider#VERTICAL}).
   *
   * @throws IllegalArgumentException if <code>orientation</code> is not one of
   *     the permitted values.
   *
   * @see #getOrientation()
   */
  public void setOrientation(int orientation)
  {
    if (orientation != VERTICAL && orientation != HORIZONTAL)
      throw new IllegalArgumentException(
          "orientation must be one of: VERTICAL, HORIZONTAL");
    if (orientation != this.orientation)
      {
        int oldOrientation = this.orientation;
        this.orientation = orientation;
        firePropertyChange("orientation", oldOrientation, this.orientation);
        revalidate();
      }
  }

  /**
   * Returns the label table for the slider.
   *
   * @return The label table for the slider (possibly <code>null</code>).
   *
   * @see #setLabelTable(Dictionary)
   */
  public Dictionary getLabelTable()
  {
    return labelTable;
  }

  /**
   * Sets the table of labels for the slider and sends a
   * {@link PropertyChangeEvent} (with the property name "labelTable") to all
   * registered listeners.
   *
   * @param table  the table of labels (<code>null</code> permitted).
   *
   * @see #getLabelTable()
   */
  public void setLabelTable(Dictionary table)
  {
    if (table != labelTable)
      {
        Dictionary oldTable = labelTable;
        labelTable = table;
        updateLabelUIs();
        firePropertyChange("labelTable", oldTable, labelTable);
        revalidate();
        repaint();
      }
  }

  /**
   * Resets the UI delegates for the labels in the <code>labelTable</code> to
   * the default for the current look and feel.
   */
  protected void updateLabelUIs()
  {
    if (labelTable != null)
      {
        for (Enumeration list = labelTable.elements(); list.hasMoreElements();)
          {
            Object o = list.nextElement();
            if (o instanceof JComponent)
              {
                JComponent jc = (JComponent) o;
                jc.updateUI();
                jc.setSize(jc.getPreferredSize());
              }
          }
      }
  }

  /**
   * Creates a hashtable of <code>(Integer, JLabel)</code> pairs that can be
   * used as a label table for this slider. The labels will start from the
   * slider's minimum and increase by the increment. Each label will have a text
   * string indicating its integer value.
   *
   * @param increment The increment between labels (must be > 0).
   *
   * @return A hashtable containing the labels.
   *
   * @throws IllegalArgumentException if <code>increment</code> is not greater
   *         than zero.
   */
  public Hashtable createStandardLabels(int increment)
  {
    return createStandardLabels(increment, sliderModel.getMinimum());
  }

  /**
   * Creates a hashtable of <code>(Integer, JLabel)</code> pairs that can be
   * used as a label table for this slider. The labels will start from the
   * given start value and increase by the increment. Each  label will have a
   * text string indicating its integer value.
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
    int max = getMaximum();
    for (int i = start; i <= max; i += increment)
      {
        LabelUIResource label = new LabelUIResource(String.valueOf(i),
                                                    JLabel.CENTER);
        table.put(new Integer(i), label);
      }
    return table;
  }

  /**
   * Returns the flag that controls whether or not the value scale for the
   * slider is inverted (the default value is <code>false</code>).
   *
   * @return The flag that controls whether or not the value scale for the
   *     slider is inverted.
   *
   * @see #setInverted(boolean)
   */
  public boolean getInverted()
  {
    return isInverted;
  }

  /**
   * Sets the flag that controls whether or not the value scale for the
   * slider is inverted and, if the new flag value is different to the old flag
   * value, sends a {@link PropertyChangeEvent} to all registered listeners.
   * Typically, a horizontal slider will display a scale that increases from
   * left to right, but this is reversed if the 'inverted' flag is set to
   * <code>true</code>.  Similarly, a vertical slider will display a scale that
   * increases from bottom to top, and this is reversed if the 'inverted' flag
   * is set to <code>true</code>.
   *
   * @param inverted  the new flag value.
   *
   * @see #getInverted()
   */
  public void setInverted(boolean inverted)
  {
    if (isInverted != inverted)
      {
        boolean oldInverted = isInverted;
        isInverted = inverted;
        firePropertyChange("inverted", oldInverted, isInverted);
        repaint();
      }
  }

  /**
   * Returns the distance between major tick marks along the slider's value
   * scale.
   *
   * @return The amount of units between each major tick mark.
   *
   * @see #setMajorTickSpacing(int)
   */
  public int getMajorTickSpacing()
  {
    return majorTickSpacing;
  }

  /**
   * Sets the distance between major tick marks along the slider's value scale,
   * and sends a {@link PropertyChangeEvent} (with the property name
   * "majorTickSpacing") to all registered listeners.
   *
   * @param spacing  the distance between major tick marks.
   *
   * @see #getMajorTickSpacing()
   */
  public void setMajorTickSpacing(int spacing)
  {
    if (majorTickSpacing != spacing)
      {
        int oldSpacing = majorTickSpacing;
        majorTickSpacing = spacing;
        if (labelTable == null && majorTickSpacing > 0 && getPaintLabels())
          setLabelTable(createStandardLabels(majorTickSpacing));
        firePropertyChange("majorTickSpacing", oldSpacing, majorTickSpacing);
        if (getPaintTicks())
          repaint();
      }
  }

  /**
   * Returns the distance between minor tick marks along the slider's value
   * scale.
   *
   * @return The distance between minor tick marks along the slider's value
   *     scale.
   *
   * @see #setMinorTickSpacing(int)
   */
  public int getMinorTickSpacing()
  {
    return minorTickSpacing;
  }

  /**
   * Sets the distance between minor tick marks along the slider's value scale,
   * and sends a {@link PropertyChangeEvent} (with the property name
   * "minorTickSpacing") to all registered listeners.
   *
   * @param spacing  the distance between minor tick marks.
   *
   * @see #getMinorTickSpacing()
   */
  public void setMinorTickSpacing(int spacing)
  {
    if (minorTickSpacing != spacing)
      {
        int oldSpacing = minorTickSpacing;
        minorTickSpacing = spacing;
        firePropertyChange("minorTickSpacing", oldSpacing, minorTickSpacing);
        if (getPaintTicks())
          repaint();
      }
  }

  /**
   * Returns the flag that controls whether the slider thumb will snap to ticks.
   * Sliders that snap to ticks will automatically move the thumb to the
   * nearest tick mark.
   *
   * @return <code>true</code> if the slider thumb automatically.
   *
   * @see #setSnapToTicks(boolean)
   */
  public boolean getSnapToTicks()
  {
    return snapToTicks;
  }

  /**
   * Sets the flag that controls whether the slider thumb will snap to ticks
   * and sends a {@link PropertyChangeEvent} (with the property name
   * 'snapToTicks') to all registered listeners. Sliders that snap to ticks
   * will automatically move the thumb to the nearest tick mark.
   *
   * @param snap  the new flag value.
   *
   * @see #getSnapToTicks()
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
   * Returns the flag that controls whether or not tick marks are painted along
   * the slider's value scale.
   *
   * @return <code>true</code> if tick marks should be painted, and
   *     <code>false</code> if tick marks should not be painted.
   *
   * @see #setPaintTicks(boolean)
   */
  public boolean getPaintTicks()
  {
    return paintTicks;
  }

  /**
   * Sets the flag that controls whether or not tick marks are painted along
   * the slider's value scale, and sends a {@link PropertyChangeEvent} (with
   * the property name "paintTicks") to all registered listeners. In
   * addition to setting this property to <code>true</code>, one or both of the
   * minor tick spacing and major tick spacing attributes must be set to a
   * value greater than 0 in order for ticks to be painted.
   *
   * @param paint Whether ticks will be painted.
   *
   * @see #getPaintTicks()
   */
  public void setPaintTicks(boolean paint)
  {
    if (paint != paintTicks)
      {
        boolean oldPaintTicks = paintTicks;
        paintTicks = paint;
        firePropertyChange("paintTicks", oldPaintTicks, paintTicks);
        revalidate();
        repaint();
      }
  }

  /**
   * Returns the flag that controls whether or not the track is painted.
   *
   * @return Whether the track will be painted.
   *
   * @see #setPaintTrack(boolean)
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
   *
   * @see #getPaintTrack()
   */
  public void setPaintTrack(boolean paint)
  {
    if (paintTrack != paint)
    {
      paintTrack = paint;
      firePropertyChange("paintTrack", !paint, paint);
      repaint();
    }
  }

  /**
   * Returns the flag that controls whether or not labels are painted for the
   * tick marks along the slider.
   *
   * @return Whether labels will be painted.
   *
   * @see #setPaintLabels(boolean)
   */
  public boolean getPaintLabels()
  {
    return paintLabels;
  }

  /**
   * Sets the flag that controls whether or not labels are painted for the
   * tick marks along the slider and sends a {@link PropertyChangeEvent} (with
   * the property name "paintLabels") to all registered listeners.
   *
   * @param paint Whether labels will be painted.
   *
   * @see #getPaintLabels()
   */
  public void setPaintLabels(boolean paint)
  {
    if (paint != paintLabels)
      {
        paintLabels = paint;
        if (paint && majorTickSpacing > 0 && labelTable == null)
          setLabelTable(createStandardLabels(majorTickSpacing));
        firePropertyChange("paintLabels", !paint, paint);
        revalidate();
        repaint();
      }
  }

  /**
   * Returns an implementation-dependent string describing the attributes of
   * this <code>JSlider</code>.
   *
   * @return A string describing the attributes of this <code>JSlider</code>
   *         (never <code>null</code>).
   */
  protected String paramString()
  {
    String superParamStr = super.paramString();
    CPStringBuilder sb = new CPStringBuilder();
    sb.append(",isInverted=").append(getInverted());
    sb.append(",majorTickSpacing=").append(getMajorTickSpacing());
    sb.append(",minorTickSpacing=").append(getMinorTickSpacing());
    sb.append(",orientation=");
    if (orientation == HORIZONTAL)
      sb.append("HORIZONTAL");
    else
      sb.append("VERTICAL");
    sb.append(",paintLabels=").append(getPaintLabels());
    sb.append(",paintTicks=").append(getPaintTicks());
    sb.append(",paintTrack=").append(getPaintTrack());
    sb.append(",snapToTicks=").append(getSnapToTicks());

    // the following is output by the reference implementation.  We don't
    // strictly need to replicate this. Perhaps it has some meaning, but
    // I couldn't determine it yet...
    sb.append(",snapToValue=true");

    return superParamStr + sb.toString();
  }

  /**
   * Returns the object that provides accessibility features for this
   * <code>JSlider</code> component.
   *
   * @return The accessible context (an instance of {@link AccessibleJSlider}).
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJSlider();

    return accessibleContext;
  }
}
