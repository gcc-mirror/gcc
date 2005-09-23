/* JToggleButton.java -- 
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

import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleState;
import javax.swing.plaf.ButtonUI;

/**
 * The <code>JToggleButton</code> component provides a stateful button,
 * which can be either selected or unselected.  This provides the basis
 * for the implementations of radio buttons (<code>JRadioButton</code>)
 * and check boxes (<code>JCheckBox</code>).
 *
 * @author Michael Koch  (konqueror@gmx.de)
 * @author Graydon Hoare  (graydon@redhat.com)
 * @author Andrew John Hughes  (gnu_andrew@member.fsf.org)
 * @see JRadioButton
 * @see JCheckBox
 * @since 1.2
 */
public class JToggleButton extends AbstractButton implements Accessible
{
  /**
   * This class provides accessibility support for the toggle button.
   */
  protected class AccessibleJToggleButton
    extends AccessibleAbstractButton
    implements ItemListener
  {
    private static final long serialVersionUID = -8652952712161229225L;

    /**
     * Constructor for the accessible toggle button.
     */
    public AccessibleJToggleButton()
    {
      super();
      /* Register the accessible toggle button as a listener for item events */
      addItemListener(this);
    }

    /**
     * Returns the accessible role for the toggle button.
     *
     * @return An instance of <code>AccessibleRole</code>, describing
     *         the role of the toggle button.
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.TOGGLE_BUTTON;
    }
    
    /**
     * Monitors the toggle button for state changes and fires accessible
     * property change events when they occur.
     *
     * @param event the event that occurred.
     */
    public void itemStateChanged(ItemEvent event)
    {
        /* Fire a state property change event as the button's state has changed */
        if (event.getStateChange() == ItemEvent.SELECTED)
          {
            /* State has changed from unselected (null) to selected */
            firePropertyChange(ACCESSIBLE_STATE_PROPERTY, null, AccessibleState.SELECTED);
          } 
        else
          {
            /* State has changed from selected to unselected (null) */
            firePropertyChange(ACCESSIBLE_STATE_PROPERTY, AccessibleState.ENABLED, null);
          }
    }
    
  }

  /**
   * The model handles the storage and maintenance of the state of
   * the toggle button.  This follows the same paradigm (the MVC
   * or Model-View-Controller design pattern) employed by
   * other Swing components, where the data associated with a component
   * is stored separately from the display aspects.
   */
  public static class ToggleButtonModel extends DefaultButtonModel
  {
    /**
     * Compatible with Sun's JDK.
     */
    private static final long serialVersionUID = -1589950750899943974L;
  
    /**
     * Sets the pressed state of the button.  The selected state
     * of the button also changes follwing the button being pressed.
     *
     * @param p true if the button is pressed down.
     */
    public void setPressed(boolean p)  
    {
      // cannot change PRESSED state unless button is enabled
      if (! isEnabled())
        return;
      
      // if this call does not represent a CHANGE in state, then return
      if ((p && isPressed()) || (!p && !isPressed()))
        return;

      // The JDK first fires events in the following order:
      // 1. ChangeEvent for selected
      // 2. ChangeEvent for pressed
      // 3. ActionEvent
      // So do we.

      // setPressed(false) == mouse release on us,
      // if we were armed, we flip the selected state.
      if (!p && isArmed())
        {
          setSelected(! isSelected());
        }

      // make the change
      if (p)
        stateMask = stateMask | PRESSED;
      else
        stateMask = stateMask & (~PRESSED);
      
      // notify interested ChangeListeners
      fireStateChanged();

      if (!p && isArmed())
        {
          fireActionPerformed(new ActionEvent(this,
                                              ActionEvent.ACTION_PERFORMED,
                                              actionCommand));
        }

    }
  }

  /**
   * Compatible with Sun's JDK.
   */
  private static final long serialVersionUID = -3128248873429850443L;
    
  /**
   * Constructs an unselected toggle button with no text or icon.
   */ 
  public JToggleButton()
  {
    this(null, null, false);
  }

  /**
   * Constructs a toggle button using the labelling, state
   * and icon specified by the supplied action.
   *
   * @param a the action to use to define the properties of the button.
   */
  public JToggleButton(Action a)
  {
    this();
    setAction(a);
  }

  /**
   * Constructs an unselected toggle button with the supplied icon
   * and no text.
   *
   * @param icon the icon to use.
   */
  public JToggleButton(Icon icon)
  { 
    this(null, icon, false);
  }    
  
  /**
   * Constructs a toggle button with the supplied icon and state.
   *
   * @param icon the icon to use.
   * @param selected if true, the toggle button is initially in the
   *        selected state.  Otherwise, the button is unselected.
   */
  public JToggleButton(Icon icon, boolean selected) 
  {
    this(null, icon, selected);
  }
  
  /**
   * Constructs an unselected toggle button using the supplied text
   * and no icon.
   *
   * @param text the text to use.
   */ 
  public JToggleButton(String text)
  {
    this(text, null, false);
  }
      
  /**
   * Constructs a toggle button with the supplied text and state.
   *
   * @param text the text to use.
   * @param selected if true, the toggle button is initially in the
   *        selected state.  Otherwise, the button is unselected.
   */
  public JToggleButton(String text, boolean selected)
  {
    this(text, null, selected);
  }

  /**
   * Constructs an unselected toggle button with the supplied text
   * and icon.
   *
   * @param text the text to use.
   * @param icon the icon to use.
   */
  public JToggleButton(String text, Icon icon)
  {
    this(text, icon, false);
  }

  /**
   * Constructs a toggle button with the supplied text, icon and state.
   *
   * @param text the text to use.
   * @param icon the icon to use.
   * @param selected if true, the toggle button is initially in the
   *        selected state.  Otherwise, the button is unselected.
   */
  public JToggleButton (String text, Icon icon, boolean selected) 
  {
    super();
    init(text, icon);

    setModel(new ToggleButtonModel());	
    model.setSelected(selected);
  }

  /**
   * Gets the AccessibleContext associated with this <code>JToggleButton</code>.
   * The context is created, if necessary.
   *
   * @return the associated context
   */
  public AccessibleContext getAccessibleContext()
  {
    /* Create the context if this is the first request */
    if (accessibleContext == null)
      {
        /* Create the context */
	accessibleContext = new AccessibleJToggleButton();
      }
    return accessibleContext;
  }
  
  /**
   * Returns a string that specifies the name of the Look and Feel
   * class that renders this component.
   *
   * @return The Look and Feel UI class in <code>String</code> form.
   */
  public String getUIClassID()
  {
    return "ToggleButtonUI";
  }
  
  /**
   * Returns a textual representation of this component for debugging.
   * Users should not depend on anything as regards the content or formatting
   * of this string, except for the fact that the returned string may never be
   * null (only empty).
   *
   * @return the component in <code>String</code> form for debugging.
   */
  protected  String paramString()
  {
    return super.paramString();
  }
  
  /**
   * This method resets the toggle button's UI delegate to the default UI for
   * the current look and feel.
   */
  public void updateUI()
  {	
    setUI((ButtonUI)UIManager.getUI(this));
  }

}



