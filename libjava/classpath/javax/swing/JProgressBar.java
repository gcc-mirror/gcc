/* JProgressBar.java --
   Copyright (C) 2002, 2004, 2005, 2006  Free Software Foundation, Inc.

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

import java.awt.Graphics;
import java.beans.PropertyChangeEvent;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleState;
import javax.accessibility.AccessibleStateSet;
import javax.accessibility.AccessibleValue;
import javax.swing.border.Border;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.ProgressBarUI;

/**
 * A component that displays a visual indicator of the progress of a task. The
 * component has two modes: determinate and indeterminate.  In determinate mode,
 * the <code>JProgressBar</code> fills a percentage of its bar based on its 
 * current value. In indeterminate mode, it creates box and bounces it between 
 * its bounds.
 * <p>
 * This component has the following properties:
 * </p>
 * <table>
 * <tr><th> Property         </th><th> Stored in   </th><th> Bound? </th></tr>
 * <tr><td> borderPainted    </td><td> progressBar </td><td> yes    </td></tr>
 * <tr><td> changeListeners  </td><td> progressBar </td><td> no     </td></tr>
 * <tr><td> indeterminate    </td><td> progressBar </td><td> yes    </td></tr> 
 * <tr><td> maximum          </td><td> model       </td><td> no     </td></tr>
 * <tr><td> minimum          </td><td> model       </td><td> no     </td></tr>
 * <tr><td> model            </td><td> progressBar </td><td> no     </td></tr> 
 * <tr><td> orientation      </td><td> progressBar </td><td> yes    </td></tr>
 * <tr><td> percentComplete  </td><td> progressBar </td><td> no     </td></tr>
 * <tr><td> string           </td><td> progressBar </td><td> yes    </td></tr>
 * <tr><td> stringPainted    </td><td> progressBar </td><td> yes    </td></tr>
 * <tr><td> value            </td><td> model       </td><td> no     </td></tr>
 * </table>
 */
public class JProgressBar extends JComponent implements SwingConstants,
                                                        Accessible
{
  /**
   * Provides the accessibility features for the <code>JProgressBar</code>
   * component.
   */
  protected class AccessibleJProgressBar extends AccessibleJComponent
    implements AccessibleValue
  {
    private static final long serialVersionUID = -2938130009392721813L;
  
    /**
     * Creates a new <code>AccessibleJProgressBar</code> instance.
     */
    protected AccessibleJProgressBar()
    {
      // Nothing to do here.
    } 

    /**
     * Returns a set containing the current state of the {@link JProgressBar} 
     * component.
     *
     * @return The accessible state set.
     */
    public AccessibleStateSet getAccessibleStateSet()
    {
      AccessibleStateSet result = super.getAccessibleStateSet();
      if (orientation == JProgressBar.HORIZONTAL)
        result.add(AccessibleState.HORIZONTAL);
      else if (orientation == JProgressBar.VERTICAL)
        result.add(AccessibleState.VERTICAL);
      return result;
    } 

    /**
     * Returns the accessible role for the <code>JProgressBar</code> component.
     *
     * @return {@link AccessibleRole#PROGRESS_BAR}.
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.PROGRESS_BAR;
    } 

    /**
     * Returns an object that provides access to the current, minimum and 
     * maximum values.
     *
     * @return The accessible value.
     */
    public AccessibleValue getAccessibleValue()
    {
      return this;
    } 

    /**
     * Returns the current value of the {@link JProgressBar} component, as an
     * {@link Integer}.
     *
     * @return The current value of the {@link JProgressBar} component.
     */
    public Number getCurrentAccessibleValue()
    {
      return new Integer(getValue());
    }

    /**
     * Sets the current value of the {@link JProgressBar} component and sends a
     * {@link PropertyChangeEvent} (with the property name 
     * {@link AccessibleContext#ACCESSIBLE_VALUE_PROPERTY}) to all registered
     * listeners.  If the supplied value is <code>null</code>, this method 
     * does nothing and returns <code>false</code>.
     *
     * @param value  the new progress bar value (<code>null</code> permitted).
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
     * Returns the minimum value of the {@link JProgressBar} component, as an
     * {@link Integer}.
     *
     * @return The minimum value of the {@link JProgressBar} component.
     */
    public Number getMinimumAccessibleValue()
    {
      return new Integer(getMinimum());
    }

    /**
     * Returns the maximum value of the {@link JProgressBar} component, as an
     * {@link Integer}.
     *
     * @return The maximum value of the {@link JProgressBar} component.
     */
    public Number getMaximumAccessibleValue()
    {
      return new Integer(getMaximum());
    }
  } 

  private static final long serialVersionUID = 1980046021813598781L;
  
  /** 
   * A flag that determines the mode (<code>true</code> for indeterminate, 
   * <code>false</code> for determinate).
   */
  private transient boolean indeterminate = false;

  /** 
   * The orientation of the <code>JProgressBar</code> 
   * ({@link SwingConstants#HORIZONTAL} or {@link SwingConstants#VERTICAL}). 
   * Defaults to {@link SwingConstants#HORIZONTAL}.
   * @see #setOrientation(int)
   */
  protected int orientation;

  /** 
   * A flag the controls whether or not the component's border is painted.
   * The default is <code>true</code>. 
   * @see #setBorderPainted(boolean)
   */
  protected boolean paintBorder = true;

  /** 
   * The model defining the bounds and current value for the progress bar.
   * @see #setModel(BoundedRangeModel) 
   */
  protected BoundedRangeModel model;

  /** 
   * A custom string for display in the progress bar.  If this is 
   * <code>null</code>, a default string will be generated. 
   * @see #setString(String)
   */
  protected String progressString;

  /** 
   * A flag that controls whether a string is displayed within the progress 
   * bar. 
   * @see #setStringPainted(boolean)
   */
  protected boolean paintString = false;

  /** 
   * A single change event reused for all events.
   * @see #fireStateChanged() 
   */
  protected transient ChangeEvent changeEvent;

  /** 
   * The listener that is registered with the model. */
  protected ChangeListener changeListener;

  /**
   * Creates a new <code>JProgressBar</code> with default attributes.  The 
   * following defaults are used:
   * <p>
   * <ul>
   * <li><code>value</code>: 0;</li>
   * <li><code>minimum</code>: 0;</li>
   * <li><code>maximum</code>: 100;</li>
   * <li><code>orientation</code>: {@link SwingConstants#HORIZONTAL}.</li>
   * </ul>  
   */
  public JProgressBar()
  {
    this(HORIZONTAL, 0, 100);
  }

  /**
   * Creates a new <code>JProgressBar</code> with the specified 
   * <code>orientation</code>.  The following defaults are used:
   * <p>
   * <ul>
   * <li><code>value</code>: 0;</li>
   * <li><code>minimum</code>: 0;</li>
   * <li><code>maximum</code>: 100;</li>
   * </ul>  
   * 
   * @param orientation  the orientation ({@link #HORIZONTAL} or 
   *     {@link #VERTICAL}).
   * 
   * @throws IllegalArgumentException if <code>orientation</code> is not one of
   *     the specified values.
   */
  public JProgressBar(int orientation)
  {
    this(orientation, 0, 100);
  }

  /**
   * Creates a new <code>JProgressBar</code> with the specified value range.
   * The following defaults are used:
   * <p>
   * <ul>
   * <li><code>value</code>: <code>minimum</code>;</li>
   * <li><code>orientation</code>: {@link SwingConstants#HORIZONTAL}.</li>
   * </ul>  
   * 
   * @param minimum  the lower bound of the value range.
   * @param maximum  the upper bound of the value range.
   */
  public JProgressBar(int minimum, int maximum)
  {
    this(HORIZONTAL, minimum, maximum);
  }

  /**
   * Creates a new <code>JProgressBar</code> with the specified range and
   * orientation.  The following defaults are used:
   * <p>
   * <ul>
   * <li><code>value</code>: <code>minimum</code>;</li>
   * </ul>  
   * 
   * @param minimum  the lower bound of the value range.
   * @param maximum  the upper bound of the value range.
   * @param orientation  the orientation ({@link #HORIZONTAL} or 
   *     {@link #VERTICAL}).
   * 
   * @throws IllegalArgumentException if <code>orientation</code> is not one of
   *     the specified values.
   */
  public JProgressBar(int orientation, int minimum, int maximum)
  {
    model = new DefaultBoundedRangeModel(minimum, 0, minimum, maximum);
    if (orientation != HORIZONTAL && orientation != VERTICAL)
      throw new IllegalArgumentException(orientation
                                         + " is not a legal orientation");    
    this.orientation = orientation;
    changeListener = createChangeListener();
    model.addChangeListener(changeListener);
    updateUI();
  }

  /**
   * Creates a new <code>JProgressBar</code> with the specified model.  The
   * following defaults are used:
   * <p>
   * <ul>
   * <li><code>orientation</code>: {@link SwingConstants#HORIZONTAL}.</li>
   * </ul>  
   * 
   * @param model  the model (<code>null</code> not permitted).
   */
  public JProgressBar(BoundedRangeModel model)
  {
    this.model = model;
    changeListener = createChangeListener();
    if (model != null)
      model.addChangeListener(changeListener);
    updateUI();    
  }

  /**
   * Returns the current value for the <code>JProgressBar</code>.  This value 
   * is fetched from the model.
   *
   * @return The current value.
   * 
   * @see #setValue(int)
   */
  public int getValue()
  {
    return model.getValue();
  }

  /**
   * Sets the current value for the <code>JProgressBar</code>.  The value is
   * stored in the component's <code>model</code> (see {@link #getModel()}).  
   * If the new value is different to the old value, a {@link ChangeEvent} is 
   * sent to the model's registered listeners.  In turn, this triggers a call 
   * to {@link #fireStateChanged()} which will send a <code>ChangeEvent</code> 
   * to this component's registered listeners.
   * <p>
   * If <code>value</code> is outside the range <code>minimum</code> to 
   * <code>maximum</code>, it will be set to the nearest of those boundary 
   * values.
   *
   * @param value  the new value.
   * 
   * @see #getValue()
   */
  public void setValue(int value)
  {
    model.setValue(value);
  }

  /**
   * Paints the component's border, but only if {@link #isBorderPainted()}
   * returns <code>true</code>.
   *
   * @param graphics  the graphics object to paint with.
   * 
   * @see #setBorderPainted(boolean)
   */
  protected void paintBorder(Graphics graphics)
  {
    Border border = getBorder();
    if (paintBorder && border != null)
      border.paintBorder(this, graphics, 0, 0, getWidth(), getHeight());
  }

  /**
   * Returns the orientation of the <code>JProgressBar</code> component, which
   * is either {@link SwingConstants#HORIZONTAL} or 
   * {@link SwingConstants#VERTICAL}.  The default orientation is 
   * <code>HORIZONTAL</code>.
   *
   * @return The orientation.
   * 
   * @see #setOrientation(int)
   */
  public int getOrientation()
  {
    return orientation;
  }

  /**
   * Sets the orientation for this <code>JProgressBar</code> component and,
   * if the value changes, sends a {@link PropertyChangeEvent} (with the 
   * property name <code>"orientation"</code>) to all registered listeners.
   *
   * @param orientation  the orientation ({@link #HORIZONTAL} or 
   *     {@link #VERTICAL}).
   * 
   * @throws IllegalArgumentException if <code>orientation</code> is not
   *     one of the listed values.
   *     
   * @see #getOrientation()
   */
  public void setOrientation(int orientation)
  {
    if (orientation != VERTICAL && orientation != HORIZONTAL)
      throw new IllegalArgumentException(orientation
                                         + " is not a legal orientation");    
    if (this.orientation != orientation)
      {
        int oldOrientation = this.orientation;
        this.orientation = orientation;
        firePropertyChange("orientation", oldOrientation, this.orientation);
      }
  }

  /**
   * Returns the flag that controls whether or not the string returned by
   * {@link #getString()} is displayed by the <code>JProgressBar</code> 
   * component.
   *
   * @return <code>true</code> if the string should be displayed, and 
   *     <code>false</code> otherwise.
   * 
   * @see #setStringPainted(boolean)
   */
  public boolean isStringPainted()
  {
    return paintString;
  }

  /**
   * Sets the flag that controls whether or not the string returned by
   * {@link #getString()} is displayed by the <code>JProgressBar</code> 
   * component.  If the flag value changes, a {@link PropertyChangeEvent} (with 
   * the property name <code>"stringPainted"</code>) is sent to all registered 
   * listeners.
   *
   * @param painted  the new flag value.
   * 
   * @see #isStringPainted()
   * @see #setString(String)
   */
  public void setStringPainted(boolean painted)
  {
    if (paintString != painted)
      {
        boolean oldPainted = paintString;
        paintString = painted;
        firePropertyChange("stringPainted", oldPainted, paintString);
      }
  }

  /**
   * Returns the string that is painted on the <code>JProgressBar</code> if 
   * {@link #isStringPainted()} returns <code>true</code>.  If no string has 
   * been explicitly set, this method will return a string displaying the 
   * value of {@link #getPercentComplete()}.
   *
   * @return The string.
   * 
   * @see #setString(String)
   * @see #setStringPainted(boolean)
   */
  public String getString()
  {
    if (progressString != null)
      return progressString;
    else
      return (int) (getPercentComplete() * 100) + "%";
  }

  /**
   * Sets the string to display within the progress bar and, if the new value
   * is different to the old value, sends a {@link PropertyChangeEvent} (with 
   * the property name <code>"string"</code>) to all registered listeners. If 
   * the string is set to <code>null</code>, {@link #getString()} will return
   * a default string.
   *
   * @param string  the string (<code>null</code> permitted).
   * 
   * @see #getString()
   * @see #setStringPainted(boolean)
   */
  public void setString(String string)
  {
    if (((string == null || progressString == null) &&
        string != progressString) || (string != null &&
	! string.equals(progressString)))
      {
        String oldString = progressString;
        progressString = string;
        firePropertyChange("string", oldString, progressString);
      }
  }

  /**
   * Returns the current value expressed as a percentage.  This is calculated 
   * as <code>(value - min) / (max - min)</code>.
   *
   * @return The percentage (a value in the range 0.0 to 1.0).
   */
  public double getPercentComplete()
  {
    if (getMaximum() == getMinimum())
      return 1.0;
    else
      return (double) (model.getValue() - model.getMinimum()) 
          / (model.getMaximum() - model.getMinimum());
  }

  /**
   * Returns a flag that controls whether or not the component's border is
   * painted.  The default value is <code>true</code>.
   *
   * @return <code>true</code> if the component's border should be painted,
   *     and <code>false</code> otherwise.
   *     
   * @see #setBorderPainted(boolean)
   */
  public boolean isBorderPainted()
  {
    return paintBorder;
  }

  /**
   * Sets the flag that controls whether or not the component's border is
   * painted.  If the flag value is changed, this method sends a 
   * {@link PropertyChangeEvent} (with the property name "borderPainted") to 
   * all registered listeners.
   *
   * @param painted  the new flag value.
   * 
   * @see #isBorderPainted()
   * @see #paintBorder
   */
  public void setBorderPainted(boolean painted)
  {
    if (painted != paintBorder)
      {
        boolean oldPainted = paintBorder;
        paintBorder = painted;
        firePropertyChange("borderPainted", oldPainted, paintBorder);
      }
  }

  /**
   * Returns the UI delegate for this <code>JProgressBar</code>.
   *
   * @return The UI delegate.
   */
  public ProgressBarUI getUI()
  {
    return (ProgressBarUI) ui;
  }

  /**
   * Sets the UI delegate for this component.
   *
   * @param ui  the new UI delegate.
   */
  public void setUI(ProgressBarUI ui)
  {
    super.setUI(ui);
  }

  /**
   * Sets this <code>JProgressBar</code>'s UI delegate to the default 
   * (obtained from the {@link UIManager}) for the current look and feel.
   */
  public void updateUI()
  {
    setUI((ProgressBarUI) UIManager.getUI(this));
  }

  /**
   * Returns the suffix (<code>"ProgressBarUI"</code> in this case) used to 
   * determine the class name for a UI delegate that can provide the look and 
   * feel for a <code>JProgressBar</code>.
   *
   * @return <code>"ProgressBarUI"</code>.
   */
  public String getUIClassID()
  {
    return "ProgressBarUI";
  }

  /**
   * Creates a new {@link ChangeListener} that calls 
   * {@link #fireStateChanged()} whenever it receives a {@link ChangeEvent}
   * (typically from the component's <code>model</code>).  This listener is 
   * registered with the progress bar's model, so that changes made to the 
   * model directly will automatically result in the progress bar's listeners 
   * being notified also.
   *
   * @return A new listener.
   */
  protected ChangeListener createChangeListener()
  {
    return new ChangeListener()
      {
        public void stateChanged(ChangeEvent ce)
        {
          fireStateChanged();
	    }
      };
  }

  /**
   * Registers a listener with this component so that it will receive 
   * notification of component state changes.
   *
   * @param listener  the listener.
   * 
   * @see #removeChangeListener(ChangeListener)
   */
  public void addChangeListener(ChangeListener listener)
  {
    listenerList.add(ChangeListener.class, listener);
  }

  /**
   * Deregisters a listener so that it no longer receives notification of
   * component state changes.
   *
   * @param listener  the listener.
   * 
   * @see #addChangeListener(ChangeListener)
   */
  public void removeChangeListener(ChangeListener listener)
  {
    listenerList.remove(ChangeListener.class, listener);
  }
  
  /**
   * Returns an array of the listeners that are registered with this component.
   * The array may be empty, but is never <code>null</code>.
   *
   * @return An array of listeners.
   * 
   * @since 1.4
   */
  public ChangeListener[] getChangeListeners()
  {
    return (ChangeListener[]) listenerList.getListeners(ChangeListener.class);
  }  

  /**
   * Sends a {@link ChangeEvent} to all registered listeners to indicate that
   * the state of the <code>JProgressBar</code> has changed.  
   * 
   * @see #createChangeListener()
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
   * Returns the model for the <code>JProgressBar</code>.
   *
   * @return The model (never <code>null</code>).
   * 
   * @see #setModel(BoundedRangeModel)
   */
  public BoundedRangeModel getModel()
  {
    return model;
  }

  /**
   * Sets the model for the <code>JProgressBar</code> and sends a 
   * {@link ChangeEvent} to all registered listeners.
   *
   * @param model  the model (<code>null</code> not permitted).
   * 
   * @see #getModel()
   */
  public void setModel(BoundedRangeModel model)
  {
    if (model != this.model)
      {
        this.model.removeChangeListener(changeListener);
        this.model = model;
        this.model.addChangeListener(changeListener);
        fireStateChanged();
      }
  }

  /**
   * Returns the minimum value for the <code>JProgressBar</code>. This defines 
   * the lower bound for the current value, and is stored in the component's 
   * <code>model</code>.
   *
   * @return The minimum value.
   * 
   * @see #setMinimum(int)
   */
  public int getMinimum()
  {
    return model.getMinimum();
  }

  /**
   * Sets the minimum value for the <code>JProgressBar</code>.  The value is
   * stored in the component's <code>model</code> (see {@link #getModel()}).  
   * If the new value is different to the old value, a {@link ChangeEvent} is 
   * sent to the model's registered listeners.  In turn, this triggers a call 
   * to {@link #fireStateChanged()} which will send a <code>ChangeEvent</code> 
   * to this component's registered listeners.
   * 
   * @param minimum  the minimum value.
   * 
   * @see #getMinimum()
   */
  public void setMinimum(int minimum)
  {
    model.setMinimum(minimum);
  }

  /**
   * Returns the maximum value for the <code>JProgressBar</code>.  This defines 
   * the upper bound for the current value, and is stored in the component's 
   * <code>model</code>.
   *
   * @return The maximum value.
   * 
   * @see #setMaximum(int)
   */
  public int getMaximum()
  {
    return model.getMaximum();
  }

  /**
   * Sets the maximum value for the <code>JProgressBar</code>.  The value is
   * stored in the component's <code>model</code> (see {@link #getModel()}).  
   * If the new value is different to the old value, a {@link ChangeEvent} is 
   * sent to the model's registered listeners.  In turn, this triggers a call 
   * to {@link #fireStateChanged()} which will send a <code>ChangeEvent</code> 
   * to this component's registered listeners.
   *
   * @param maximum  the maximum value.
   * 
   * @see #getMaximum()
   */
  public void setMaximum(int maximum)
  {
    model.setMaximum(maximum);
  }

  /**
   * Returns an implementation-dependent string describing the attributes of
   * this <code>JProgressBar</code>.
   *
   * @return A string describing the attributes of this 
   *     <code>JProgressBar</code> (never <code>null</code>).
   */
  protected String paramString()
  {
    String superParamStr = super.paramString();
    StringBuffer sb = new StringBuffer();
    sb.append(",orientation=");
    if (orientation == HORIZONTAL)
      sb.append("HORIZONTAL");
    else
      sb.append("VERTICAL");
    sb.append(",paintBorder=").append(isBorderPainted());
    sb.append(",paintString=").append(isStringPainted());
    sb.append(",progressString=");
    if (progressString != null)
      sb.append(progressString);
    sb.append(",indeterminateString=").append(isIndeterminate());
    return superParamStr + sb.toString();
  }

  /**
   * Sets the flag that controls the mode for this <code>JProgressBar</code>
   * (<code>true</code> for indeterminate mode, and <code>false</code> for
   * determinate mode).  If the flag value changes, this method sends a 
   * {@link PropertyChangeEvent} (with the property name 
   * <code>"indeterminate"</code>) to all registered listeners.
   * <p>
   * If the <code>JProgressBar</code> is determinate, it paints a percentage
   * of the bar described by its value. If it is indeterminate, it simply 
   * bounces a box between the ends of the bar; the value of the 
   * <code>JProgressBar</code> is ignored.
   *
   * @param flag  the new flag value.
   * 
   * @see #isIndeterminate()
   * @since 1.4
   */
  public void setIndeterminate(boolean flag)
  {
    if (indeterminate != flag)
      {
        indeterminate = flag;
        firePropertyChange("indeterminate", !flag, indeterminate);
      }
  }

  /**
   * Returns a flag that indicates the mode for this <code>JProgressBar</code>
   * (<code>true</code> for indeterminate mode, and <code>false</code> for 
   * determinate mode).  
   *
   * @return A flag indicating the mode for the <code>JProgressBar</code>.
   * 
   * @see #setIndeterminate(boolean)
   * @since 1.4
   */
  public boolean isIndeterminate()
  {
    return indeterminate;
  }

  /**
   * Returns the object that provides accessibility features for this
   * <code>JProgressBar</code> component.
   *
   * @return The accessible context (an instance of 
   *     {@link AccessibleJProgressBar}).
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJProgressBar();
    
    return accessibleContext;
  } 
}
