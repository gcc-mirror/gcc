/* JRadioButton.java -- 
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

import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.swing.plaf.ButtonUI;

/**
 * The <code>JRadioButton</code> component provides a visually selectable
 * button with mutually exclusive behaviour within a <code>ButtonGroup</code>.
 * A series of radio buttons can be used to provide options to the user,
 * where the user can only select one of the available options.  The state
 * of the button is provided by the superclass, <code>JToggleButton</code>.
 * <code>JRadioButton</code> adds the additional behaviour, that if two
 * or more radio buttons are grouped together, the selection of one implies
 * the deselection of the other buttons within the group.
 * <p>
 *
 * Buttons are grouped by adding each instance to a <code>ButtonGroup</code>.
 * The existence of such a grouping is not reflected visually, so other means
 * should be used to denote this.  For instance, the grouped buttons can be placed
 * within the same panel, possibly with an appropriate border to denote
 * the connection between the components.
 *
 * @author Michael Koch  (konqueror@gmx.de)
 * @author Graydon Hoare  (graydon@redhat.com)
 * @author Andrew John Hughes  (gnu_andrew@member.fsf.org)
 * @see JToggleButton
 * @see ButtonGroup
 * @since 1.2
 */
public class JRadioButton extends JToggleButton
{
  /**
   * Compatible with Sun's JDK.
   */
  private static final long serialVersionUID = 7751949583255506856L;

  /**
   * This class provides accessibility support for the toggle button.
   */
  protected class AccessibleJRadioButton
    extends AccessibleJToggleButton
  {

    /**
     * Constructor for the accessible toggle button.
     */
    protected AccessibleJRadioButton()
    {
        /* Call the superclass to register for events */
        super();
    }

    /**
     * Returns the accessible role for the toggle button.
     *
     * @return An instance of <code>AccessibleRole</code>, describing
     *         the role of the toggle button.
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.RADIO_BUTTON;
    }

  }

  /**
   * Constructs an unselected radio button with no text or icon.
   */ 
  public JRadioButton()
  {
    this(null, null, false);
  }
    
  /**
   * Constructs a radio button using the labelling, state
   * and icon specified by the supplied action.
   *
   * @param a the action to use to define the properties of the button.
   */
  public JRadioButton(Action a)
  {
    this();
    setAction(a);
  }

  /**
   * Constructs an unselected radio button with the supplied icon
   * and no text.
   *
   * @param icon the icon to use.
   */
  public JRadioButton(Icon icon)
  { 
    this(null, icon, false);
  }    
  
  /**
   * Constructs a radio button with the supplied icon and state.
   *
   * @param icon the icon to use.
   * @param selected if true, the radio button is initially in the
   *        selected state.  Otherwise, the button is unselected.
   */
  public JRadioButton(Icon icon, boolean selected)
  { 
    this(null, icon, selected);
  }    
  
  /**
   * Constructs an unselected radio button using the supplied text
   * and no icon.
   *
   * @param text the text to use.
   */
  public JRadioButton(String text)
  {
    this(text, null, false);
  }

  /**
   * Constructs a radio button with the supplied text and state.
   *
   * @param text the text to use.
   * @param selected if true, the radio button is initially in the
   *        selected state.  Otherwise, the button is unselected.
   */
  public JRadioButton(String text, boolean selected)
  {
    this(text, null, selected);
  }
      
  /**
   * Constructs an unselected radio button with the supplied text
   * and icon.
   *
   * @param text the text to use.
   * @param icon the icon to use.
   */
  public JRadioButton(String text, Icon icon)
  {
    this(text, icon, false);
  }
  
  /**
   * Constructs a radio button with the supplied text, icon and state.
   *
   * @param text the text to use.
   * @param icon the icon to use.
   * @param selected if true, the radio button is initially in the
   *        selected state.  Otherwise, the button is unselected.
   */
  public JRadioButton(String text, Icon icon, boolean selected)
  {
    super(text, icon, selected);
    borderPainted = false;
    contentAreaFilled = false;
  }
      
  /**
   * Returns the accessible context for this <code>JRadioButton</code>,
   * in the form of an instance of <code>AccessibleJRadioButton</code>.
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
	accessibleContext = new AccessibleJRadioButton();
      }
    return accessibleContext;
  }
  
  /**
   * Returns a string specifying the name of the Look and Feel UI class
   * that renders this component.
   *
   * @return the Look and Feel UI class for <code>JRadioButton</code>s
   *         as a <code>String</code>.
   */  
  public String getUIClassID()
  {
    return "RadioButtonUI";
  }
  
  /**
   * Returns a string representation of this component for debugging use.
   * Users should not depend on anything as regards the content or formatting
   * of this string, except for the fact that the returned string may never be
   * null (only empty).
   *
   * @return the component in <code>String</code> form for debugging.
   */  
  protected  String paramString()
  {
    return "JRadioButton";
  }
  
  /**
   * This method resets the radio button's UI delegate to the default UI for
   * the current look and feel.
   */
  public void updateUI()
  {
    /* 
       I can't see any difference between this and the superclass one,
       but Sun reimplements it... there is no RadioButtonUI class for it
       to be cast to.
    */
    setUI((ButtonUI) UIManager.getUI(this));
  }

}



