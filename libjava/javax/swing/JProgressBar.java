/* JProgressBar.java --
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

import java.awt.Graphics;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleStateSet;
import javax.accessibility.AccessibleValue;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.ProgressBarUI;

/**
 * The ProgressBar is a widget that displays in two modes. In 
 * determinate mode, it displays fills a percentage of its bar
 * based on its current value. In indeterminate mode, it creates
 * box and bounces it between its bounds.
 *
 * <p>
 * JProgressBars have the following properties:
 * </p>
 * 
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
   * AccessibleJProgressBar
   */
  protected class AccessibleJProgressBar extends AccessibleJComponent
    implements AccessibleValue
  {
    private static final long serialVersionUID = -2938130009392721813L;
  
    /**
     * Constructor AccessibleJProgressBar
     *
     * @param component TODO
     */
    protected AccessibleJProgressBar()
    {
    } 

    /**
     * getAccessibleStateSet
     *
     * @return AccessibleStateSet
     */
    public AccessibleStateSet getAccessibleStateSet()
    {
      return null; 
    } 

    /**
     * getAccessibleRole
     *
     * @return AccessibleRole
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.PROGRESS_BAR;
    } 

    /**
     * getAccessibleValue
     *
     * @return AccessibleValue
     */
    public AccessibleValue getAccessibleValue()
    {
      return null;
    } 

    /**
     * getCurrentAccessibleValue
     *
     * @return Number
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

  private static final long serialVersionUID = 1980046021813598781L;
  
  /** Whether the ProgressBar is determinate. */
  private transient boolean indeterminate = false;

  /** The orientation of the ProgressBar */
  protected int orientation = HORIZONTAL;

  /** Whether borders should be painted. */
  protected boolean paintBorder = true;

  /** The model describing this ProgressBar. */
  protected BoundedRangeModel model;

  /** The string that is displayed by the ProgressBar. */
  protected String progressString;

  /** Whether the string should be painted. */
  protected boolean paintString = false;

  /** The static changeEvent passed to all ChangeListeners. */
  protected transient ChangeEvent changeEvent;

  /** The ChangeListener that listens to the model. */
  protected ChangeListener changeListener;

  /**
   * Creates a new horizontally oriented JProgressBar object 
   * with a minimum of 0 and a maximum of 100.
   */
  public JProgressBar()
  {
    this(0, 100, HORIZONTAL);
  }

  /**
   * Creates a new JProgressBar object with a minimum of 0,
   * a maximum of 100, and the given orientation.
   *
   * @param orientation The orientation of the JProgressBar.
   */
  public JProgressBar(int orientation)
  {
    this(0, 100, orientation);
  }

  /**
   * Creates a new horizontally oriented JProgressBar object
   * with the given minimum and maximum.
   *
   * @param minimum The minimum of the JProgressBar.
   * @param maximum The maximum of the JProgressBar.
   */
  public JProgressBar(int minimum, int maximum)
  {
    this(minimum, maximum, HORIZONTAL);
  }

  /**
   * Creates a new JProgressBar object with the given minimum,
   * maximum, and orientation.
   *
   * @param minimum The minimum of the JProgressBar.
   * @param maximum The maximum of the JProgressBar.
   * @param orientation The orientation of the JProgressBar.
   */
  public JProgressBar(int minimum, int maximum, int orientation)
  {
    model = new DefaultBoundedRangeModel(minimum, 0, minimum, maximum);
    if (orientation != HORIZONTAL && orientation != VERTICAL)
      throw new IllegalArgumentException(orientation + " is not a legal orientation");    
    this.orientation = orientation;
    changeListener = createChangeListener();
    model.addChangeListener(changeListener);
    updateUI();
  }

  /**
   * Creates a new horizontally oriented JProgressBar object 
   * with the given model.
   *
   * @param model The model to be used with the JProgressBar.
   */
  public JProgressBar(BoundedRangeModel model)
  {
    this.model = model;
    changeListener = createChangeListener();
    model.addChangeListener(changeListener);
    updateUI();    
  }

  /**
   * This method returns the current value of the JProgressBar.
   *
   * @return The current value of the JProgressBar.
   */
  public int getValue()
  {
    return model.getValue();
  }

  /**
   * This method sets the value of the JProgressBar.
   *
   * @param value The value of the JProgressBar.
   */
  public void setValue(int value)
  {
    model.setValue(value);
  }

  /**
   * This method paints the border of the JProgressBar
   *
   * @param graphics The graphics object to paint with.
   */
  protected void paintBorder(Graphics graphics)
  {
    getBorder().paintBorder(this, graphics, 0, 0,
                            getWidth(),
                            getHeight());
  }

  /**
   * This method returns the orientation of the JProgressBar.
   *
   * @return The orientation of the JProgressBar.
   */
  public int getOrientation()
  {
    return orientation;
  }

  /**
   * This method changes the orientation property. The orientation of the 
   * JProgressBar can be either horizontal or vertical.
   *
   * @param orientation The orientation of the JProgressBar.
   */
  public void setOrientation(int orientation)
  {
    if (orientation != VERTICAL && orientation != HORIZONTAL)
      throw new IllegalArgumentException("orientation must be one of VERTICAL or HORIZONTAL");
    if (this.orientation != orientation)
      {
	int oldOrientation = this.orientation;
	this.orientation = orientation;
	firePropertyChange("orientation", oldOrientation,
	                   this.orientation);
      }
  }

  /**
   * This method returns whether the progressString will be painted.
   *
   * @return Whether the string is painted.
   */
  public boolean isStringPainted()
  {
    return paintString;
  }

  /**
   * This method changes the stringPainted property.
   *
   * @param painted Whether the string is painted.
   */
  public void setStringPainted(boolean painted)
  {
    if (paintString != painted)
      {
	boolean oldPainted = paintString;
	paintString = painted;
	firePropertyChange("stringPainted", oldPainted,
	                   paintString);
      }
  }

  /**
   * This method returns the string that is painted if the 
   * stringPainted property is set to true. If there is no
   * string set, it will return a string containing the 
   * JProgressBar's value as a percent.
   *
   * @return The string that is painted.
   */
  public String getString()
  {
    if (progressString != null)
      return progressString;
    else
      return (int) (getPercentComplete() * 100) + "%";
  }

  /**
   * This method changes the string property. The string
   * given will be the one painted. If you want to 
   * revert to the default string given, set the
   * string to null.
   *
   * @param string The string to be painted.
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
   * This method returns the percent of the bar
   * that is "complete". (This is the amount value / (max - min)).
   *
   * @return DOCUMENT ME!
   */
  public double getPercentComplete()
  {
    if (getMaximum() == getMinimum())
      return 1.0;
    else
      return (double) (model.getValue() - model.getMinimum()) / (model
                                                                 .getMaximum()
             - model.getMinimum());
  }

  /**
   * This method returns whether the border is painted.
   *
   * @return Whether the border is painted.
   */
  public boolean isBorderPainted()
  {
    return paintBorder;
  }

  /**
   * This method changes the borderPainted property.
   *
   * @param painted Whether the border is painted.
   */
  public void setBorderPainted(boolean painted)
  {
    if (painted != paintBorder)
      {
	boolean oldPainted = paintBorder;
	paintBorder = painted;
	firePropertyChange("borderPainted", oldPainted,
	                   paintBorder);
      }
  }

  /**
   * This method returns the JProgressBar's UI delegate.
   *
   * @return This JProgressBar's UI delegate.
   */
  public ProgressBarUI getUI()
  {
    return (ProgressBarUI) ui;
  }

  /**
   * This method changes the UI property for this JProgressBar.
   *
   * @param ui The new UI delegate.
   */
  public void setUI(ProgressBarUI ui)
  {
    super.setUI(ui);
  }

  /**
   * This method reverts the UI delegate for this JProgressBar
   * to the default for this Look and Feel.
   */
  public void updateUI()
  {
    setUI((ProgressBarUI) UIManager.getUI(this));
    invalidate();
  }

  /**
   * This method returns the identifier to allow the UIManager
   * to pick the correct class to act as the UI for
   * this JProgressBar.
   *
   * @return The UIClassID: "ProgressBarUI".
   */
  public String getUIClassID()
  {
    return "ProgressBarUI";
  }

  /**
   * This method returns a ChangeListener that gets registered
   * model. By default, the ChangeListener, propagates the 
   * ChangeEvents to the ChangeListeners of the JProgressBar.
   *
   * @return A new ChangeListener.
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
   * This method adds a ChangeListener to this JProgressBar.
   *
   * @param listener The ChangeListener to add to this JProgressBar.
   */
  public void addChangeListener(ChangeListener listener)
  {
    listenerList.add(ChangeListener.class, listener);
  }

  /**
   * This method removes a ChangeListener from this JProgressBar.
   *
   * @param listener The ChangeListener to remove from this JProgressBar.
   */
  public void removeChangeListener(ChangeListener listener)
  {
    listenerList.remove(ChangeListener.class, listener);
  }
  
  /**
   * This method returns an array of all ChangeListeners listening to this
   * progress bar.
   *
   * @return An array of ChangeListeners listening to this progress bar.
   */
  public ChangeListener[] getChangeListeners()
  {
    return (ChangeListener[]) listenerList.getListeners(ChangeListener.class);
  }  

  /**
   * This method is called when the JProgressBar receives a ChangeEvent
   * from its model. This simply propagates the event (changing the source
   * to the JProgressBar) to the JProgressBar's listeners.
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
   * This method returns the model used with this JProgressBar.
   *
   * @return The model used with this JProgressBar.
   */
  public BoundedRangeModel getModel()
  {
    return model;
  }

  /**
   * This method changes the model property for this JProgressBar.
   *
   * @param model The model to use with this JProgressBar.
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
   * This method returns the minimum value of this JProgressBar.
   *
   * @return The minimum value of this JProgressBar.
   */
  public int getMinimum()
  {
    return model.getMinimum();
  }

  /**
   * This method sets the minimum value of this JProgressBar.
   *
   * @param minimum The minimum value of this JProgressBar.
   */
  public void setMinimum(int minimum)
  {
    model.setMinimum(minimum);
  }

  /**
   * This method returns the maximum value of this JProgressBar.
   *
   * @return The maximum value of this JProgressBar.
   */
  public int getMaximum()
  {
    return model.getMaximum();
  }

  /**
   * This method sets the maximum value of this JProgressBar.
   *
   * @param maximum The maximum value of this JProgressBar.
   */
  public void setMaximum(int maximum)
  {
    model.setMaximum(maximum);
  }

  /**
   * This method returns a string that can be used to 
   * describe this JProgressBar. This method is usually
   * only used for debugging purposes.
   *
   * @return A string that describes this JProgressBar.
   */
  protected String paramString()
  {
    return "JProgressBar";
  }

  /**
   * This method changes the indeterminate property. If the
   * JProgressBar is determinate, it paints a percentage
   * of the bar described by its value. If it is indeterminate,
   * it simply bounces a box between the ends of the bar; the 
   * value of the JProgressBar is ignored.
   *
   * @param newValue Whether the JProgressBar is indeterminate.
   */
  public void setIndeterminate(boolean newValue)
  {
    if (indeterminate != newValue)
      {
	boolean olddeter = indeterminate;
	indeterminate = newValue;
	firePropertyChange("indeterminate", olddeter,
	                   indeterminate);
      }
  }

  /**
   * This method returns whether the JProgressBar is indeterminate.
   *
   * @return Whether this JProgressBar is indeterminate.
   */
  public boolean isIndeterminate()
  {
    return indeterminate;
  }

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJProgressBar();
    
    return accessibleContext;
  } 
}
