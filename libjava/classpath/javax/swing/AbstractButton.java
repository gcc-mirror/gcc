/* AbstractButton.java -- Provides basic button functionality.
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
import java.awt.Image;
import java.awt.Insets;
import java.awt.ItemSelectable;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.image.ImageObserver;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.Serializable;

import javax.accessibility.AccessibleAction;
import javax.accessibility.AccessibleIcon;
import javax.accessibility.AccessibleRelationSet;
import javax.accessibility.AccessibleStateSet;
import javax.accessibility.AccessibleText;
import javax.accessibility.AccessibleValue;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.ButtonUI;
import javax.swing.text.AttributeSet;


/**
 * Provides an abstract implementation of common button behaviour,
 * data model and look &amp; feel.
 *
 * <p>This class is supposed to serve as a base class for
 * several kinds of buttons with similar but non-identical semantics:
 * toggle buttons (radio buttons and checkboxes), simple push buttons,
 * menu items, etc.</p>
 *
 * <p>Buttons have many properties, some of which are stored in this class
 * while others are delegated to the button's model. The following properties
 * are available:</p>
 *
 * <table>
 * <tr><th>Property               </th><th>Stored in</th><th>Bound?</th></tr>
 *
 * <tr><td>action                 </td><td>button</td> <td>no</td></tr>
 * <tr><td>actionCommand          </td><td>model</td>  <td>no</td></tr>
 * <tr><td>borderPainted          </td><td>button</td> <td>yes</td></tr>
 * <tr><td>contentAreaFilled      </td><td>button</td> <td>yes</td></tr>
 * <tr><td>disabledIcon           </td><td>button</td> <td>yes</td></tr>
 * <tr><td>disabledSelectedIcon   </td><td>button</td> <td>yes</td></tr>
 * <tr><td>displayedMnemonicIndex </td><td>button</td> <td>no</td></tr>
 * <tr><td>enabled                </td><td>model</td>  <td>no</td></tr>
 * <tr><td>focusPainted           </td><td>button</td> <td>yes</td></tr>
 * <tr><td>horizontalAlignment    </td><td>button</td> <td>yes</td></tr>
 * <tr><td>horizontalTextPosition </td><td>button</td> <td>yes</td></tr>
 * <tr><td>icon                   </td><td>button</td> <td>yes</td></tr>
 * <tr><td>iconTextGap            </td><td>button</td> <td>no</td></tr>
 * <tr><td>label (same as text)   </td><td>model</td>  <td>yes</td></tr>
 * <tr><td>margin                 </td><td>button</td> <td>yes</td></tr>
 * <tr><td>multiClickThreshold    </td><td>button</td> <td>no</td></tr>
 * <tr><td>pressedIcon            </td><td>button</td> <td>yes</td></tr>
 * <tr><td>rolloverEnabled        </td><td>button</td> <td>yes</td></tr>
 * <tr><td>rolloverIcon           </td><td>button</td> <td>yes</td></tr>
 * <tr><td>rolloverSelectedIcon   </td><td>button</td> <td>yes</td></tr>
 * <tr><td>selected               </td><td>model</td>  <td>no</td></tr>
 * <tr><td>selectedIcon           </td><td>button</td> <td>yes</td></tr>
 * <tr><td>selectedObjects        </td><td>button</td> <td>no</td></tr>
 * <tr><td>text                   </td><td>model</td>  <td>yes</td></tr>
 * <tr><td>UI                     </td><td>button</td> <td>yes</td></tr>
 * <tr><td>verticalAlignment      </td><td>button</td> <td>yes</td></tr>
 * <tr><td>verticalTextPosition   </td><td>button</td> <td>yes</td></tr>
 *
 * </table>
 *
 * <p>The various behavioral aspects of these properties follows:</p>
 *
 * <ul> 
 *
 * <li>When non-bound properties stored in the button change, the button
 * fires ChangeEvents to its ChangeListeners.</li>
 * 
 * <li>When bound properties stored in the button change, the button fires
 * PropertyChangeEvents to its PropertyChangeListeners</li>
 *
 * <li>If any of the model's properties change, it fires a ChangeEvent to
 * its ChangeListeners, which include the button.</li>
 *
 * <li>If the button receives a ChangeEvent from its model, it will
 * propagate the ChangeEvent to its ChangeListeners, with the ChangeEvent's
 * "source" property set to refer to the button, rather than the model. The
 * the button will request a repaint, to paint its updated state.</li>
 *
 * <li>If the model's "selected" property changes, the model will fire an
 * ItemEvent to its ItemListeners, which include the button, in addition to
 * the ChangeEvent which models the property change. The button propagates
 * ItemEvents directly to its ItemListeners.</li>
 *
 * <li>If the model's armed and pressed properties are simultaneously
 * <code>true</code>, the model will fire an ActionEvent to its
 * ActionListeners, which include the button. The button will propagate
 * this ActionEvent to its ActionListeners, with the ActionEvent's "source"
 * property set to refer to the button, rather than the model.</li>
 *
 * </ul>
 *
 * @author Ronald Veldema (rveldema@cs.vu.nl)
 * @author Graydon Hoare (graydon@redhat.com)
 */

public abstract class AbstractButton extends JComponent
  implements ItemSelectable, SwingConstants
{
  private static final long serialVersionUID = -937921345538462020L;

  /**
   * An extension of ChangeListener to be serializable.
   */
  protected class ButtonChangeListener
    implements ChangeListener, Serializable
  {
    private static final long serialVersionUID = 1471056094226600578L;

    /**
     * Notified when the target of the listener changes its state.
     *
     * @param ev the ChangeEvent describing the change
     */
    public void stateChanged(ChangeEvent ev)
    {
    }
  }

  /** The icon displayed by default. */
  Icon default_icon;

  /** The icon displayed when the button is pressed. */
  Icon pressed_icon;

  /** The icon displayed when the button is disabled. */
  Icon disabeldIcon;

  /** The icon displayed when the button is selected. */
  Icon selectedIcon;

  /** The icon displayed when the button is selected but disabled. */
  Icon disabledSelectedIcon;

  /** The icon displayed when the button is rolled over. */
  Icon rolloverIcon;

  /** The icon displayed when the button is selected and rolled over. */
  Icon rolloverSelectedIcon;

  /** The icon currently displayed. */
  Icon current_icon;

  /** The text displayed in the button. */
  String text;

  /**
   * The gap between icon and text, if both icon and text are
   * non-<code>null</code>.
   */
  int iconTextGap;

  /** The vertical alignment of the button's text and icon. */
  int verticalAlignment;

  /** The horizontal alignment of the button's text and icon. */
  int horizontalAlignment;

  /** The horizontal position of the button's text relative to its icon. */
  int horizontalTextPosition;

  /** The vertical position of the button's text relative to its icon. */
  int verticalTextPosition;

  /** Whether or not the button paints its border. */
  boolean borderPainted;

  /** Whether or not the button paints its focus state. */
  boolean focusPainted;

  /** Whether or not the button fills its content area. */
  boolean contentAreaFilled;
  
  /** Whether rollover is enabled. */
  boolean rollOverEnabled;

  /** The action taken when the button is clicked. */
  Action action;

  /** The button's current state. */
  protected ButtonModel model;

  /** The margin between the button's border and its label. */
  Insets margin;

  /**
   * A hint to the look and feel class, suggesting which character in the
   * button's label should be underlined when drawing the label.
   */
  int mnemonicIndex;

  /** Listener the button uses to receive ActionEvents from its model.  */
  protected ActionListener actionListener;

  /** Listener the button uses to receive ItemEvents from its model.  */
  protected ItemListener itemListener;

  /** Listener the button uses to receive ChangeEvents from its model.  */  
  protected ChangeListener changeListener;

  /**
   * The time in miliseconds in which clicks get coalesced into a single
   * <code>ActionEvent</code>.
   */
  long multiClickThreshhold;
  
  /**
   * Listener the button uses to receive PropertyChangeEvents from its
   * Action.
   */
  PropertyChangeListener actionPropertyChangeListener;
  
  /** ChangeEvent that is fired to button's ChangeEventListeners  */  
  protected ChangeEvent changeEvent = new ChangeEvent(this);
  
  /**
   * Fired in a PropertyChangeEvent when the "borderPainted" property changes.
   */
  public static final String BORDER_PAINTED_CHANGED_PROPERTY = "borderPainted";
  
  /**
   * Fired in a PropertyChangeEvent when the "contentAreaFilled" property
   * changes.
   */
  public static final String CONTENT_AREA_FILLED_CHANGED_PROPERTY =
    "contentAreaFilled";
  
  /**
   * Fired in a PropertyChangeEvent when the "disabledIcon" property changes.
   */
  public static final String DISABLED_ICON_CHANGED_PROPERTY = "disabledIcon";
  
  /**
   * Fired in a PropertyChangeEvent when the "disabledSelectedIcon" property
   * changes.
   */
  public static final String DISABLED_SELECTED_ICON_CHANGED_PROPERTY =
    "disabledSelectedIcon";
  
  /**
   * Fired in a PropertyChangeEvent when the "focusPainted" property changes.
   */
  public static final String FOCUS_PAINTED_CHANGED_PROPERTY = "focusPainted";

  /**
   * Fired in a PropertyChangeEvent when the "horizontalAlignment" property
   * changes.
   */
  public static final String HORIZONTAL_ALIGNMENT_CHANGED_PROPERTY =
    "horizontalAlignment";

  /**
   * Fired in a PropertyChangeEvent when the "horizontalTextPosition" property
   * changes.
   */
  public static final String HORIZONTAL_TEXT_POSITION_CHANGED_PROPERTY =
    "horizontalTextPosition";

  /**
   * Fired in a PropertyChangeEvent when the "icon" property changes. */
  public static final String ICON_CHANGED_PROPERTY = "icon";

  /** Fired in a PropertyChangeEvent when the "margin" property changes. */
  public static final String MARGIN_CHANGED_PROPERTY = "margin";

  /** Fired in a PropertyChangeEvent when the "mnemonic" property changes. */
  public static final String MNEMONIC_CHANGED_PROPERTY = "mnemonic";

  /** Fired in a PropertyChangeEvent when the "model" property changes. */
  public static final String MODEL_CHANGED_PROPERTY = "model";

  /** Fired in a PropertyChangeEvent when the "pressedIcon" property changes. */
  public static final String PRESSED_ICON_CHANGED_PROPERTY = "pressedIcon";

  /**
   * Fired in a PropertyChangeEvent when the "rolloverEnabled" property
   * changes.
   */
  public static final String ROLLOVER_ENABLED_CHANGED_PROPERTY =
    "rolloverEnabled";

  /**
   * Fired in a PropertyChangeEvent when the "rolloverIcon" property changes.
   */
  public static final String ROLLOVER_ICON_CHANGED_PROPERTY = "rolloverIcon";
  
  /**
   * Fired in a PropertyChangeEvent when the "rolloverSelectedIcon" property
   * changes.
   */
  public static final String ROLLOVER_SELECTED_ICON_CHANGED_PROPERTY =
    "rolloverSelectedIcon";
  
  /**
   * Fired in a PropertyChangeEvent when the "selectedIcon" property changes.
   */
  public static final String SELECTED_ICON_CHANGED_PROPERTY = "selectedIcon";

  /** Fired in a PropertyChangeEvent when the "text" property changes. */
  public static final String TEXT_CHANGED_PROPERTY = "text";

  /**
   * Fired in a PropertyChangeEvent when the "verticalAlignment" property
   * changes.
   */
  public static final String VERTICAL_ALIGNMENT_CHANGED_PROPERTY =
    "verticalAlignment";

  /**
   * Fired in a PropertyChangeEvent when the "verticalTextPosition" property
   * changes.
   */
  public static final String VERTICAL_TEXT_POSITION_CHANGED_PROPERTY =
    "verticalTextPosition";

  /**
   * A Java Accessibility extension of the AbstractButton.
   */
  protected abstract class AccessibleAbstractButton
    extends AccessibleJComponent implements AccessibleAction, AccessibleValue,
                                            AccessibleText
  {
    private static final long serialVersionUID = -5673062525319836790L;
    
    protected AccessibleAbstractButton()
    {
    }

    public AccessibleStateSet getAccessibleStateSet()
    {
      return null; // TODO
    }

    public String getAccessibleName()
    {
      return null; // TODO
    }

    public AccessibleIcon[] getAccessibleIcon()
    {
      return null; // TODO
    }

    public AccessibleRelationSet getAccessibleRelationSet()
    {
      return null; // TODO
    }

    public AccessibleAction getAccessibleAction()
    {
      return null; // TODO
    }

    public AccessibleValue getAccessibleValue()
    {
      return null; // TODO
    }

    public int getAccessibleActionCount()
    {
      return 0; // TODO
    }

    public String getAccessibleActionDescription(int value0)
    {
      return null; // TODO
    }

    public boolean doAccessibleAction(int value0)
    {
      return false; // TODO
    }

    public Number getCurrentAccessibleValue()
    {
      return null; // TODO
    }

    public boolean setCurrentAccessibleValue(Number value0)
    {
      return false; // TODO
    }

    public Number getMinimumAccessibleValue()
    {
      return null; // TODO
    }

    public Number getMaximumAccessibleValue()
    {
      return null; // TODO
    }

    public AccessibleText getAccessibleText()
    {
      return null; // TODO
    }

    public int getIndexAtPoint(Point value0)
    {
      return 0; // TODO
    }

    public Rectangle getCharacterBounds(int value0)
    {
      return null; // TODO
    }

    public int getCharCount()
    {
      return 0; // TODO
    }

    public int getCaretPosition()
    {
      return 0; // TODO
    }

    public String getAtIndex(int value0, int value1)
    {
      return null; // TODO
    }

    public String getAfterIndex(int value0, int value1)
    {
      return null; // TODO
    }

    public String getBeforeIndex(int value0, int value1)
    {
      return null; // TODO
    }

    public AttributeSet getCharacterAttribute(int value0)
    {
      return null; // TODO
    }

    public int getSelectionStart()
    {
      return 0; // TODO
    }

    public int getSelectionEnd()
    {
      return 0; // TODO
    }

    public String getSelectedText()
    {
      return null; // TODO
    }

    private Rectangle getTextRectangle()
    {
      return null; // TODO
    }
  }

  /**
   * Creates a new AbstractButton object.
   */
  public AbstractButton()
  {
    init("", null);
    updateUI();
  }

  /**
   * Get the model the button is currently using.
   *
   * @return The current model
   */
  public ButtonModel getModel()
  {
    return model;
  }

  /**
   * Set the model the button is currently using. This un-registers all 
   * listeners associated with the current model, and re-registers them
   * with the new model.
   *
   * @param newModel The new model
   */
  public void setModel(ButtonModel newModel)
  {
    if (newModel == model)
      return;

    if (model != null)
      {
        model.removeActionListener(actionListener);
        model.removeChangeListener(changeListener);
        model.removeItemListener(itemListener);
      }
    ButtonModel old = model;
    model = newModel;
    if (model != null)
      {
        model.addActionListener(actionListener);
        model.addChangeListener(changeListener);
        model.addItemListener(itemListener);
      }
    firePropertyChange(MODEL_CHANGED_PROPERTY, old, model);
    revalidate();
    repaint();
  }

 protected void init(String text, Icon icon) 
 {
    // If text is null, we fall back to the empty
    // string (which is set using AbstractButton's
    // constructor).
    // This way the behavior of the JDK is matched.
    if(text != null)
        this.text = text;

    if (icon != null)
      default_icon = icon;

    actionListener = createActionListener();
    changeListener = createChangeListener();
    itemListener = createItemListener();

    horizontalAlignment = CENTER;
    horizontalTextPosition = TRAILING;
    verticalAlignment = CENTER;
    verticalTextPosition = CENTER;
    borderPainted = true;
    contentAreaFilled = true;

    focusPainted = true;
    setFocusable(true);

    setAlignmentX(LEFT_ALIGNMENT);
    setAlignmentY(CENTER_ALIGNMENT);

    setDisplayedMnemonicIndex(-1);    
 }
 
  /**
   * <p>Returns the action command string for this button's model.</p>
   *
   * <p>If the action command was set to <code>null</code>, the button's
   * text (label) is returned instead.</p>
   *
   * @return The current action command string from the button's model
   */
  public String getActionCommand()
  {
    String ac = model.getActionCommand();
    if (ac != null)
      return ac;
    else
      return text;
  }

  /**
   * Sets the action command string for this button's model.
   *
   * @param actionCommand The new action command string to set in the button's
   * model.
   */
  public void setActionCommand(String actionCommand)
  {
    model.setActionCommand(actionCommand);
  }

  /**
   * Adds an ActionListener to the button's listener list. When the
   * button's model is clicked it fires an ActionEvent, and these
   * listeners will be called.
   *
   * @param l The new listener to add
   */
  public void addActionListener(ActionListener l)
  {
    listenerList.add(ActionListener.class, l);
  }

  /**
   * Removes an ActionListener from the button's listener list.
   *
   * @param l The listener to remove
   */
  public void removeActionListener(ActionListener l)
  {
    listenerList.remove(ActionListener.class, l);
  }

  /**
   * Returns all added <code>ActionListener</code> objects.
   * 
   * @return an array of listeners
   * 
   * @since 1.4
   */
  public ActionListener[] getActionListeners()
  {
    return (ActionListener[]) listenerList.getListeners(ActionListener.class);
  }

  /**
   * Adds an ItemListener to the button's listener list. When the button's
   * model changes state (between any of ARMED, ENABLED, PRESSED, ROLLOVER
   * or SELECTED) it fires an ItemEvent, and these listeners will be
   * called.
   *
   * @param l The new listener to add
   */
  public void addItemListener(ItemListener l)
  {
    listenerList.add(ItemListener.class, l);
  }

  /**
   * Removes an ItemListener from the button's listener list.
   *
   * @param l The listener to remove
   */
  public void removeItemListener(ItemListener l)
  {
    listenerList.remove(ItemListener.class, l);
  }

  /**
   * Returns all added <code>ItemListener</code> objects.
   * 
   * @return an array of listeners
   * 
   * @since 1.4
   */
  public ItemListener[] getItemListeners()
  {
    return (ItemListener[]) listenerList.getListeners(ItemListener.class);
  }

  /**
   * Adds a ChangeListener to the button's listener list. When the button's
   * model changes any of its (non-bound) properties, these listeners will be
   * called. 
   *
   * @param l The new listener to add
   */
  public void addChangeListener(ChangeListener l)
  {
    listenerList.add(ChangeListener.class, l);
  }

  /**
   * Removes a ChangeListener from the button's listener list.
   *
   * @param l The listener to remove
   */
  public void removeChangeListener(ChangeListener l)
  {
    listenerList.remove(ChangeListener.class, l);
  }

  /**
   * Returns all added <code>ChangeListener</code> objects.
   * 
   * @return an array of listeners
   * 
   * @since 1.4
   */
  public ChangeListener[] getChangeListeners()
  {
    return (ChangeListener[]) listenerList.getListeners(ChangeListener.class);
  }

  /**
   * Calls {@link ItemListener#itemStateChanged} on each ItemListener in
   * the button's listener list.
   *
   * @param e The event signifying that the button's model changed state
   */
  protected void fireItemStateChanged(ItemEvent e)
  {
    e.setSource(this);
    ItemListener[] listeners = getItemListeners();
 
    for (int i = 0; i < listeners.length; i++)
      listeners[i].itemStateChanged(e);
  }

  /**
   * Calls {@link ActionListener#actionPerformed} on each {@link
   * ActionListener} in the button's listener list.
   *
   * @param e The event signifying that the button's model was clicked
   */
  protected void fireActionPerformed(ActionEvent e)
  {
	// Dispatch a copy of the given ActionEvent in order to
	// set the source and action command correctly.
    ActionEvent ae = new ActionEvent(
        this,
        e.getID(),
        getActionCommand(),
        e.getWhen(),
        e.getModifiers());

    ActionListener[] listeners = getActionListeners();
    
    for (int i = 0; i < listeners.length; i++)
      listeners[i].actionPerformed(ae);
  }

  /**
   * Calls {@link ChangeListener#stateChanged} on each {@link ChangeListener}
   * in the button's listener list.
   */
  protected void fireStateChanged()
  {
    ChangeListener[] listeners = getChangeListeners();

    for (int i = 0; i < listeners.length; i++)
      listeners[i].stateChanged(changeEvent);
  }

  /**
   * Get the current keyboard mnemonic value. This value corresponds to a
   * single key code (one of the {@link java.awt.event.KeyEvent} VK_*
   * codes) and is used to activate the button when pressed in conjunction
   * with the "mouseless modifier" of the button's look and feel class, and
   * when focus is in one of the button's ancestors.
   *
   * @return The button's current keyboard mnemonic
   */
  public int getMnemonic()
  {
    return getModel().getMnemonic();
  }

  /**
   * Set the current keyboard mnemonic value. This value corresponds to a
   * single key code (one of the {@link java.awt.event.KeyEvent} VK_*
   * codes) and is used to activate the button when pressed in conjunction
   * with the "mouseless modifier" of the button's look and feel class, and
   * when focus is in one of the button's ancestors.
   *
   * @param mne A new mnemonic to use for the button
   */
  public void setMnemonic(char mne)
  {
    setMnemonic((int) mne);
  }

  /**
   * Set the current keyboard mnemonic value. This value corresponds to a
   * single key code (one of the {@link java.awt.event.KeyEvent} VK_*
   * codes) and is used to activate the button when pressed in conjunction
   * with the "mouseless modifier" of the button's look and feel class, and
   * when focus is in one of the button's ancestors.
   *
   * @param mne A new mnemonic to use for the button
   */
  public void setMnemonic(int mne)
  {
    int old = getModel().getMnemonic();

    if (old != mne)
      {
        getModel().setMnemonic(mne);

        if (text != null && !text.equals(""))
          {
            // Since lower case char = upper case char for
            // mnemonic, we will convert both text and mnemonic
            // to upper case before checking if mnemonic character occurs
            // in the menu item text.
            int upperCaseMne = Character.toUpperCase((char) mne);
            String upperCaseText = text.toUpperCase();
            setDisplayedMnemonicIndex(upperCaseText.indexOf(upperCaseMne));
          }

        firePropertyChange(MNEMONIC_CHANGED_PROPERTY, old, mne);
        revalidate();
        repaint();
      }
  }

  /** 
   * Sets the button's mnemonic index. The mnemonic index is a hint to the
   * look and feel class, suggesting which character in the button's label
   * should be underlined when drawing the label. If the mnemonic index is
   * -1, no mnemonic will be displayed. 
   * 
   * If no mnemonic index is set, the button will choose a mnemonic index
   * by default, which will be the first occurrence of the mnemonic
   * character in the button's text.
   *
   * @param index An offset into the "text" property of the button
   * @throws IllegalArgumentException If <code>index</code> is not within the
   * range of legal offsets for the "text" property of the button.
   * @since 1.4
   */

  public void setDisplayedMnemonicIndex(int index)
  {
    if (index < -1 || (text != null && index >= text.length()))
      throw new IllegalArgumentException();
  
    mnemonicIndex = index;
  }
  
  /** 
   * Get the button's mnemonic index, which is an offset into the button's
   * "text" property.  The character specified by this offset should be
   * underlined when the look and feel class draws this button.
   *
   * @return An index into the button's "text" property
   */
  public int getDisplayedMnemonicIndex()
  {
    return mnemonicIndex;
  }
  

  /**
   * Set the "rolloverEnabled" property. When rollover is enabled, and the
   * look and feel supports it, the button will change its icon to
   * rolloverIcon, when the mouse passes over it.
   *
   * @param r Whether or not to enable rollover icon changes
   */
  public void setRolloverEnabled(boolean r)
  {
    if (rollOverEnabled != r)
      {
        rollOverEnabled = r;
        firePropertyChange(ROLLOVER_ENABLED_CHANGED_PROPERTY, !r, r);
        revalidate();
        repaint();
      }
  }

  /**
   * Returns whether or not rollover icon changes are enabled on the
   * button.
   *
   * @return The state of the "rolloverEnabled" property
   */
  public boolean isRolloverEnabled()
  {
    return rollOverEnabled;
  }

  /**
   * Set the value of the button's "selected" property. Selection is only
   * meaningful for toggle-type buttons (check boxes, radio buttons).
   *
   * @param s New value for the property
   */
  public void setSelected(boolean s)
  {
    getModel().setSelected(s);
  }

  /**
   * Get the value of the button's "selected" property. Selection is only
   * meaningful for toggle-type buttons (check boxes, radio buttons).
   *
   * @return The value of the property
   */
  public boolean isSelected()
  {
    return getModel().isSelected();
  }

  /**
   * Enables or disables the button. A button will neither be selectable
   * nor preform any actions unless it is enabled.
   *
   * @param b Whether or not to enable the button
   */
  public void setEnabled(boolean b)
  {
    super.setEnabled(b);
    getModel().setEnabled(b);
  }

  /** 
   * Set the horizontal alignment of the button's text and icon. The
   * alignment is a numeric constant from {@link SwingConstants}. It must
   * be one of: <code>RIGHT</code>, <code>LEFT</code>, <code>CENTER</code>,
   * <code>LEADING</code> or <code>TRAILING</code>.  The default is
   * <code>RIGHT</code>.
   * 
   * @return The current horizontal alignment
   */
  public int getHorizontalAlignment()
  {
    return horizontalAlignment;
  }

  /**
   * Set the horizontal alignment of the button's text and icon. The
   * alignment is a numeric constant from {@link SwingConstants}. It must
   * be one of: <code>RIGHT</code>, <code>LEFT</code>, <code>CENTER</code>,
   * <code>LEADING</code> or <code>TRAILING</code>.  The default is
   * <code>RIGHT</code>.
   *
   * @param a The new horizontal alignment
   * @throws IllegalArgumentException If alignment is not one of the legal
   * constants.
   */
  public void setHorizontalAlignment(int a)
  {
    if (horizontalAlignment == a)
      return;

    int old = horizontalAlignment;
    horizontalAlignment = a;
    firePropertyChange(HORIZONTAL_ALIGNMENT_CHANGED_PROPERTY, old, a);
    revalidate();
    repaint();
  }

  /**
   * Get the horizontal position of the button's text relative to its
   * icon. The position is a numeric constant from {@link
   * SwingConstants}. It must be one of: <code>RIGHT</code>,
   * <code>LEFT</code>, <code>CENTER</code>, <code>LEADING</code> or
   * <code>TRAILING</code>.  The default is <code>TRAILING</code>.
   *
   * @return The current horizontal text position
   */
  public int getHorizontalTextPosition()
  {
    return horizontalTextPosition;
  }

  /**
   * Set the horizontal position of the button's text relative to its
   * icon. The position is a numeric constant from {@link
   * SwingConstants}. It must be one of: <code>RIGHT</code>,
   * <code>LEFT</code>, <code>CENTER</code>, <code>LEADING</code> or
   * <code>TRAILING</code>. The default is <code>TRAILING</code>.
   *
   * @param t The new horizontal text position
   * @throws IllegalArgumentException If position is not one of the legal
   * constants.
   */
  public void setHorizontalTextPosition(int t)
  {
    if (horizontalTextPosition == t)
      return;

    int old = horizontalTextPosition;
    horizontalTextPosition = t;
    firePropertyChange(HORIZONTAL_TEXT_POSITION_CHANGED_PROPERTY, old, t);
    revalidate();
    repaint();
  }

  /**
   * Get the vertical alignment of the button's text and icon. The
   * alignment is a numeric constant from {@link SwingConstants}. It must
   * be one of: <code>CENTER</code>, <code>TOP</code>, or
   * <code>BOTTOM</code>. The default is <code>CENTER</code>.
   *
   * @return The current vertical alignment
   */
  public int getVerticalAlignment()
  {
    return verticalAlignment;
  }

  /**
   * Set the vertical alignment of the button's text and icon. The
   * alignment is a numeric constant from {@link SwingConstants}. It must
   * be one of: <code>CENTER</code>, <code>TOP</code>, or
   * <code>BOTTOM</code>. The default is <code>CENTER</code>.
   *
   * @param a The new vertical alignment
   * @throws IllegalArgumentException If alignment is not one of the legal
   * constants.
   */
  public void setVerticalAlignment(int a)
  {
    if (verticalAlignment == a)
      return;
    
    int old = verticalAlignment;
    verticalAlignment = a;
    firePropertyChange(VERTICAL_ALIGNMENT_CHANGED_PROPERTY, old, a);
    revalidate();
    repaint();
  }

  /**
   * Get the vertical position of the button's text relative to its
   * icon. The alignment is a numeric constant from {@link
   * SwingConstants}. It must be one of: <code>CENTER</code>,
   * <code>TOP</code>, or <code>BOTTOM</code>. The default is
   * <code>CENTER</code>.
   *
   * @return The current vertical position
   */
  public int getVerticalTextPosition()
  {
    return verticalTextPosition;
  }

  /**
   * Set the vertical position of the button's text relative to its
   * icon. The alignment is a numeric constant from {@link
   * SwingConstants}. It must be one of: <code>CENTER</code>,
   * <code>TOP</code>, or <code>BOTTOM</code>. The default is
   * <code>CENTER</code>.
   *
   * @param t The new vertical position
   * @throws IllegalArgumentException If position is not one of the legal
   * constants.
   */
  public void setVerticalTextPosition(int t)
  {
    if (verticalTextPosition == t)
      return;
    
    int old = verticalTextPosition;
    verticalTextPosition = t;
    firePropertyChange(VERTICAL_TEXT_POSITION_CHANGED_PROPERTY, old, t);
    revalidate();
    repaint();
  }

  /**
   * Set the value of the "borderPainted" property. If set to
   * <code>false</code>, the button's look and feel class should not paint
   * a border for the button. The default is <code>true</code>.
   *
   * @return The current value of the property.
   */
  public boolean isBorderPainted()
  {
    return borderPainted;
  }

  /**
   * Set the value of the "borderPainted" property. If set to
   * <code>false</code>, the button's look and feel class should not paint
   * a border for the button. The default is <code>true</code>.
   *
   * @param b The new value of the property.
   */
  public void setBorderPainted(boolean b)
  {
    if (borderPainted == b)
      return;
    
    boolean old = borderPainted;
    borderPainted = b;
    firePropertyChange(BORDER_PAINTED_CHANGED_PROPERTY, old, b);
    revalidate();
    repaint();
  }

  /**
   * Get the value of the "action" property. 
   *
   * @return The current value of the "action" property
   */
  public Action getAction()
  {
    return action;
  }

  /**
   * <p>Set the button's "action" property, subscribing the new action to the
   * button, as an ActionListener, if it is not already subscribed. The old
   * Action, if it exists, is unsubscribed, and the button is unsubscribed
   * from the old Action if it was previously subscribed as a
   * PropertyChangeListener.</p>
   *
   * <p>This method also configures several of the button's properties from
   * the Action, by calling {@link #configurePropertiesFromAction}, and
   * subscribes the button to the Action as a PropertyChangeListener.
   * Subsequent changes to the Action will thus reconfigure the button 
   * automatically.</p>
   *
   * @param a The new value of the "action" property
   */
  public void setAction(Action a)
  {
    if (action != null)
      {
        action.removePropertyChangeListener(actionPropertyChangeListener);
        removeActionListener(action);
        if (actionPropertyChangeListener != null)
          {
            action.removePropertyChangeListener(actionPropertyChangeListener);
            actionPropertyChangeListener = null;
          }
      }

    Action old = action;
    action = a;
    configurePropertiesFromAction(action);
    if (action != null)
      {
        actionPropertyChangeListener = createActionPropertyChangeListener(a);      
        action.addPropertyChangeListener(actionPropertyChangeListener);
        addActionListener(action);
      }
  }

  /**
   * Return the button's default "icon" property.
   *
   * @return The current default icon
   */
  public Icon getIcon()
  {
    return default_icon;
  }

  /**
   * Set the button's default "icon" property. This icon is used as a basis
   * for the pressed and disabled icons, if none are explicitly set.
   *
   * @param i The new default icon
   */
  public void setIcon(Icon i)
  {
    if (default_icon == i)
      return;
    
    Icon old = default_icon;      
    default_icon = i;      
    firePropertyChange(ICON_CHANGED_PROPERTY, old, i);
    revalidate();
    repaint();
  }

  /**
   * Return the button's "text" property. This property is synonymous with
   * the "label" property.
   *
   * @return The current "text" property
   */
  public String getText()
  {
    return text;
  }

  /**
   * Set the button's "label" property. This property is synonymous with the
   * "text" property.
   *
   * @param label The new "label" property
   *
   * @deprecated use <code>setText(text)</code>
   */
  public void setLabel(String label)
  {
    setText(label);
  }

  /**
   * Return the button's "label" property. This property is synonymous with
   * the "text" property.
   *
   * @return The current "label" property
   *
   * @deprecated use <code>getText()</code>
   */
  public String getLabel()
  {
    return getText();
  }

  /**
   * Set the button's "text" property. This property is synonymous with the
   * "label" property.
   *
   * @param t The new "text" property
   */
  public void setText(String t)
  {
    if (text == t)
      return;
    
    String old = text;
    text = t;
    firePropertyChange(TEXT_CHANGED_PROPERTY, old, t);
    revalidate();
    repaint();
  }

  /**
   * Set the value of the {@link #iconTextGap} property.
   * 
   * @param i The new value of the property
   */
  public void setIconTextGap(int i)
  {
    if (iconTextGap == i)
      return;
    
    int old = iconTextGap;
    iconTextGap = i;
    fireStateChanged();
    revalidate();
    repaint();
  }

  /**
   * Get the value of the {@link #iconTextGap} property.
   *
   * @return The current value of the property
   */
  public int getIconTextGap()
  {
    return iconTextGap;
  }

  /**
   * Return the button's "margin" property, which is an {@link Insets} object
   * describing the distance between the button's border and its text and
   * icon.
   *
   * @return The current "margin" property
   */
  public Insets getMargin()
  {
    return margin;
  }

  /**
   * Set the button's "margin" property, which is an {@link Insets} object
   * describing the distance between the button's border and its text and
   * icon.
   *
   * @param m The new "margin" property
   */
  public void setMargin(Insets m)
  {
    if (margin == m)
      return;
    
    Insets old = margin;
    margin = m;
    firePropertyChange(MARGIN_CHANGED_PROPERTY, old, m);
    revalidate();
    repaint();
  }

  /**
   * Return the button's "pressedIcon" property. The look and feel class
   * should paint this icon when the "pressed" property of the button's
   * {@link ButtonModel} is <code>true</code>. This property may be
   * <code>null</code>, in which case the default icon is used.
   *
   * @return The current "pressedIcon" property
   */
  public Icon getPressedIcon()
  {
    return pressed_icon;
  }

  /**
   * Set the button's "pressedIcon" property. The look and feel class
   * should paint this icon when the "pressed" property of the button's
   * {@link ButtonModel} is <code>true</code>. This property may be
   * <code>null</code>, in which case the default icon is used.
   *
   * @param pressedIcon The new "pressedIcon" property
   */
  public void setPressedIcon(Icon pressedIcon)
  {
    if (pressed_icon == pressedIcon)
      return;
    
    Icon old = pressed_icon;
    pressed_icon = pressedIcon;
    firePropertyChange(PRESSED_ICON_CHANGED_PROPERTY, old, pressed_icon);
    revalidate();
    repaint();
  }

  /**
   * Return the button's "disabledIcon" property. The look and feel class
   * should paint this icon when the "enabled" property of the button's
   * {@link ButtonModel} is <code>false</code>. This property may be
   * <code>null</code>, in which case an icon is constructed, based on the
   * default icon.
   *
   * @return The current "disabledIcon" property
   */
  public Icon getDisabledIcon()
  {
    if (disabeldIcon == null && default_icon instanceof ImageIcon)
      {
        Image iconImage = ((ImageIcon) default_icon).getImage();
        Image grayImage = GrayFilter.createDisabledImage(iconImage);
        disabeldIcon = new ImageIcon(grayImage);
      }
      
    return disabeldIcon;
  }

  /**
   * Set the button's "disabledIcon" property. The look and feel class should
   * paint this icon when the "enabled" property of the button's {@link
   * ButtonModel} is <code>false</code>. This property may be
   * <code>null</code>, in which case an icon is constructed, based on the
   * default icon.
   *
   * @param d The new "disabledIcon" property
   */
  public void setDisabledIcon(Icon d)
  {
    disabeldIcon = d;
    revalidate();
    repaint();
  }

  /**
   * Return the button's "paintFocus" property. This property controls
   * whether or not the look and feel class will paint a special indicator
   * of focus state for the button. If it is false, the button still paints
   * when focused, but no special decoration is painted to indicate the
   * presence of focus.
   *
   * @return The current "paintFocus" property
   */
  public boolean isFocusPainted()
  {
    return focusPainted;
  }

  /**
   * Set the button's "paintFocus" property. This property controls whether
   * or not the look and feel class will paint a special indicator of focus
   * state for the button. If it is false, the button still paints when
   * focused, but no special decoration is painted to indicate the presence
   * of focus.
   *
   * @param p The new "paintFocus" property
   */
  public void setFocusPainted(boolean p)
  {
    if (focusPainted == p)
      return;
    
    boolean old = focusPainted;
    focusPainted = p;
    firePropertyChange(FOCUS_PAINTED_CHANGED_PROPERTY, old, p);
    revalidate();
    repaint();
  }

  /**
   * Verifies that a particular key is one of the valid constants used for
   * describing horizontal alignment and positioning. The valid constants
   * are the following members of {@link SwingConstants}:
   * <code>RIGHT</code>, <code>LEFT</code>, <code>CENTER</code>,
   * <code>LEADING</code> or <code>TRAILING</code>.
   *
   * @param key The key to check
   * @param exception A message to include in an IllegalArgumentException
   *
   * @return the value of key
   *
   * @throws IllegalArgumentException If key is not one of the valid constants
   *
   * @see #setHorizontalTextPosition(int)
   * @see #setHorizontalAlignment(int)
   */
  protected  int checkHorizontalKey(int key, String exception)
  {
    switch (key)
      {
      case SwingConstants.RIGHT:
      case SwingConstants.LEFT:
      case SwingConstants.CENTER:
      case SwingConstants.LEADING:
      case SwingConstants.TRAILING:
        break;
      default:
        throw new IllegalArgumentException(exception);
      }
    return key;
  }

  /**
   * Verifies that a particular key is one of the valid constants used for
   * describing vertical alignment and positioning. The valid constants are
   * the following members of {@link SwingConstants}: <code>TOP</code>,
   * <code>BOTTOM</code> or <code>CENTER</code>.
   *
   * @param key The key to check
   * @param exception A message to include in an IllegalArgumentException
   *
   * @return the value of key
   *
   * @throws IllegalArgumentException If key is not one of the valid constants
   *
   * @see #setVerticalTextPosition(int)
   * @see #setVerticalAlignment(int)
   */
  protected  int checkVerticalKey(int key, String exception)
  {
    switch (key)
      {
      case SwingConstants.TOP:
      case SwingConstants.BOTTOM:
      case SwingConstants.CENTER:
        break;
      default:
        throw new IllegalArgumentException(exception);
      }
    return key;
  }

  /**
   * Configure various properties of the button by reading properties
   * of an {@link Action}. The mapping of properties is as follows:
   *
   * <table>
   *
   * <tr><th>Action keyed property</th> <th>AbstractButton property</th></tr>
   *
   * <tr><td>NAME                 </td> <td>text                   </td></tr>
   * <tr><td>SMALL_ICON           </td> <td>icon                   </td></tr>
   * <tr><td>SHORT_DESCRIPTION    </td> <td>toolTipText            </td></tr>
   * <tr><td>MNEMONIC_KEY         </td> <td>mnemonic               </td></tr>
   * <tr><td>ACTION_COMMAND_KEY   </td> <td>actionCommand          </td></tr>
   *
   * </table>
   *
   * <p>In addition, this method always sets the button's "enabled" property to
   * the value of the Action's "enabled" property.</p>
   *
   * <p>If the provided Action is <code>null</code>, the text, icon, and
   * toolTipText properties of the button are set to <code>null</code>, and
   * the "enabled" property is set to <code>true</code>; the mnemonic and
   * actionCommand properties are unchanged.</p>
   *
   * @param a An Action to configure the button from
   */
  protected void configurePropertiesFromAction(Action a)
  {
    if (a == null)
      {
        setText(null);
        setIcon(null);
        setEnabled(true);
        setToolTipText(null);
      }
    else
      {
        setText((String) (a.getValue(Action.NAME)));
        setIcon((Icon) (a.getValue(Action.SMALL_ICON)));
        setEnabled(a.isEnabled());
        setToolTipText((String) (a.getValue(Action.SHORT_DESCRIPTION)));
        if (a.getValue(Action.MNEMONIC_KEY) != null)
          setMnemonic(((Integer) (a.getValue(Action.MNEMONIC_KEY))).intValue());
        String actionCommand = (String) (a.getValue(Action.ACTION_COMMAND_KEY));

        // Set actionCommand to button's text by default if it is not specified
        if (actionCommand != null)
          setActionCommand((String) (a.getValue(Action.ACTION_COMMAND_KEY)));
        else
          setActionCommand(getText());
      }
  }

  /**
   * <p>A factory method which should return an {@link ActionListener} that
   * propagates events from the button's {@link ButtonModel} to any of the
   * button's ActionListeners. By default, this is an inner class which
   * calls {@link AbstractButton#fireActionPerformed} with a modified copy
   * of the incoming model {@link ActionEvent}.</p>
   *
   * <p>The button calls this method during construction, stores the
   * resulting ActionListener in its <code>actionListener</code> member
   * field, and subscribes it to the button's model. If the button's model
   * is changed, this listener is unsubscribed from the old model and
   * subscribed to the new one.</p>
   *
   * @return A new ActionListener 
   */
  protected  ActionListener createActionListener()
  {
    return new ActionListener()
      {
        public void actionPerformed(ActionEvent e)
        {
          AbstractButton.this.fireActionPerformed(e);
        }
      };
  }

  /**
   * <p>A factory method which should return a {@link PropertyChangeListener}
   * that accepts changes to the specified {@link Action} and reconfigure
   * the {@link AbstractButton}, by default using the {@link
   * #configurePropertiesFromAction} method.</p>
   *
   * <p>The button calls this method whenever a new Action is assigned to
   * the button's "action" property, via {@link #setAction}, and stores the
   * resulting PropertyChangeListener in its
   * <code>actionPropertyChangeListener</code> member field. The button
   * then subscribes the listener to the button's new action. If the
   * button's action is changed subsequently, the listener is unsubscribed
   * from the old action and subscribed to the new one.</p>
   *
   * @param a The Action which will be listened to, and which should be 
   * the same as the source of any PropertyChangeEvents received by the
   * new listener returned from this method.
   *
   * @return A new PropertyChangeListener
   */
  protected  PropertyChangeListener createActionPropertyChangeListener(Action a)
  {
    return new PropertyChangeListener()
      {
        public void propertyChange(PropertyChangeEvent e)
        {
          Action act = (Action) (e.getSource());
          if (e.getPropertyName().equals("enabled"))
            setEnabled(act.isEnabled());
          else if (e.getPropertyName().equals(Action.NAME))
            setText((String) (act.getValue(Action.NAME)));
          else if (e.getPropertyName().equals(Action.SMALL_ICON))
            setIcon((Icon) (act.getValue(Action.SMALL_ICON)));
          else if (e.getPropertyName().equals(Action.SHORT_DESCRIPTION))
            setToolTipText((String) (act.getValue(Action.SHORT_DESCRIPTION)));
          else if (e.getPropertyName().equals(Action.MNEMONIC_KEY))
            if (act.getValue(Action.MNEMONIC_KEY) != null)
              setMnemonic(((Integer) (act.getValue(Action.MNEMONIC_KEY)))
                          .intValue());
          else if (e.getPropertyName().equals(Action.ACTION_COMMAND_KEY))
            setActionCommand((String) (act.getValue(Action.ACTION_COMMAND_KEY)));
        }
      };
  }

  /**
   * <p>Factory method which creates a {@link ChangeListener}, used to
   * subscribe to ChangeEvents from the button's model. Subclasses of
   * AbstractButton may wish to override the listener used to subscribe to
   * such ChangeEvents. By default, the listener just propagates the
   * {@link ChangeEvent} to the button's ChangeListeners, via the {@link
   * AbstractButton#fireStateChanged} method.</p>
   *
   * <p>The button calls this method during construction, stores the
   * resulting ChangeListener in its <code>changeListener</code> member
   * field, and subscribes it to the button's model. If the button's model
   * is changed, this listener is unsubscribed from the old model and
   * subscribed to the new one.</p>
   *
   * @return The new ChangeListener
   */
  protected  ChangeListener createChangeListener()
  {
    return new ChangeListener()
      {
        public void stateChanged(ChangeEvent e)
        {
          AbstractButton.this.fireStateChanged();
          AbstractButton.this.repaint();          
        }
      };
  }

  /**
   * <p>Factory method which creates a {@link ItemListener}, used to
   * subscribe to ItemEvents from the button's model. Subclasses of
   * AbstractButton may wish to override the listener used to subscribe to
   * such ItemEvents. By default, the listener just propagates the
   * {@link ItemEvent} to the button's ItemListeners, via the {@link
   * AbstractButton#fireItemStateChanged} method.</p>
   *
   * <p>The button calls this method during construction, stores the
   * resulting ItemListener in its <code>changeListener</code> member
   * field, and subscribes it to the button's model. If the button's model
   * is changed, this listener is unsubscribed from the old model and
   * subscribed to the new one.</p>
   *
   * <p>Note that ItemEvents are only generated from the button's model
   * when the model's <em>selected</em> property changes. If you want to
   * subscribe to other properties of the model, you must subscribe to
   * ChangeEvents.
   *
   * @return The new ItemListener
   */
  protected  ItemListener createItemListener()
  {
    return new ItemListener()
      {
        public void itemStateChanged(ItemEvent e)
        {
          AbstractButton.this.fireItemStateChanged(e);
        }
      };
  }

  /**
   * Programmatically perform a "click" on the button: arming, pressing,
   * waiting, un-pressing, and disarming the model.
   */
  public void doClick()
  {
    doClick(100);
  }

  /**
   * Programmatically perform a "click" on the button: arming, pressing,
   * waiting, un-pressing, and disarming the model.
   *
   * @param pressTime The number of milliseconds to wait in the pressed state
   */
  public void doClick(int pressTime)
  {
    getModel().setArmed(true);
    getModel().setPressed(true);
    try
      {
        java.lang.Thread.sleep(pressTime);
      }
    catch (java.lang.InterruptedException e)
      {
        // probably harmless
      }
    getModel().setPressed(false);
    getModel().setArmed(false);
  }

  /**
   * Return the button's disabled selected icon. The look and feel class
   * should paint this icon when the "enabled" property of the button's model
   * is <code>false</code> and its "selected" property is
   * <code>true</code>. This icon can be <code>null</code>, in which case
   * it is synthesized from the button's selected icon.
   *
   * @return The current disabled selected icon
   */
  public Icon getDisabledSelectedIcon()
  {
    return disabledSelectedIcon;
  }

  /**
   * Set the button's disabled selected icon. The look and feel class
   * should paint this icon when the "enabled" property of the button's model
   * is <code>false</code> and its "selected" property is
   * <code>true</code>. This icon can be <code>null</code>, in which case
   * it is synthesized from the button's selected icon.
   *
   * @param icon The new disabled selected icon
   */
  public void setDisabledSelectedIcon(Icon icon)
  {
    if (disabledSelectedIcon == icon)
      return;
    
    Icon old = disabledSelectedIcon;
    disabledSelectedIcon = icon;
    firePropertyChange(DISABLED_SELECTED_ICON_CHANGED_PROPERTY, old, icon);
    revalidate();
    repaint();        
  }

  /**
   * Return the button's rollover icon. The look and feel class should
   * paint this icon when the "rolloverEnabled" property of the button is
   * <code>true</code> and the mouse rolls over the button.
   *
   * @return The current rollover icon
   */
  public Icon getRolloverIcon()
  {
    return rolloverIcon;
  }

  /**
   * Set the button's rollover icon. The look and feel class should
   * paint this icon when the "rolloverEnabled" property of the button is
   * <code>true</code> and the mouse rolls over the button.
   *
   * @param r The new rollover icon
   */
  public void setRolloverIcon(Icon r)
  {
    if (rolloverIcon == r)
      return;
    
    Icon old = rolloverIcon;
    rolloverIcon = r;
    firePropertyChange(ROLLOVER_ICON_CHANGED_PROPERTY, old, rolloverIcon);
    revalidate();
    repaint();
  }

  /**
   * Return the button's rollover selected icon. The look and feel class
   * should paint this icon when the "rolloverEnabled" property of the button
   * is <code>true</code>, the "selected" property of the button's model is
   * <code>true</code>, and the mouse rolls over the button.
   *
   * @return The current rollover selected icon
   */
  public Icon getRolloverSelectedIcon()
  {
    return rolloverSelectedIcon;
  }

  /**
   * Set the button's rollover selected icon. The look and feel class
   * should paint this icon when the "rolloverEnabled" property of the button
   * is <code>true</code>, the "selected" property of the button's model is
   * <code>true</code>, and the mouse rolls over the button.
   *
   * @param r The new rollover selected icon
   */
  public void setRolloverSelectedIcon(Icon r)
  {
    if (rolloverSelectedIcon == r)
      return;
    
    Icon old = rolloverSelectedIcon;
    rolloverSelectedIcon = r;
    firePropertyChange(ROLLOVER_SELECTED_ICON_CHANGED_PROPERTY, old, r);
    revalidate();
    repaint();
  }

  /**
   * Return the button's selected icon. The look and feel class should
   * paint this icon when the "selected" property of the button's model is
   * <code>true</code>, and either the "rolloverEnabled" property of the
   * button is <code>false</code> or the mouse is not currently rolled
   * over the button.
   *
   * @return The current selected icon
   */
  public Icon getSelectedIcon()
  {
    return selectedIcon;
  }

  /**
   * Set the button's selected icon. The look and feel class should
   * paint this icon when the "selected" property of the button's model is
   * <code>true</code>, and either the "rolloverEnabled" property of the
   * button is <code>false</code> or the mouse is not currently rolled
   * over the button.
   *
   * @param s The new selected icon
   */
  public void setSelectedIcon(Icon s)
  {
    if (selectedIcon == s)
      return;
    
    Icon old = selectedIcon;
    selectedIcon = s;
    firePropertyChange(SELECTED_ICON_CHANGED_PROPERTY, old, s);
    revalidate();
    repaint();
  }

  /**
   * Returns an single-element array containing the "text" property of the
   * button if the "selected" property of the button's model is
   * <code>true</code>, otherwise returns <code>null</code>.
   *
   * @return The button's "selected object" array
   */
  public Object[] getSelectedObjects()
  {
    if (isSelected())
      {
        Object[] objs = new Object[1];
        objs[0] = getText();
        return objs;
      }
    else
      {
        return null;
      }
  }

  /**
   * Called when image data becomes available for one of the button's icons.
   *
   * @param img The image being updated
   * @param infoflags One of the constant codes in {@link ImageObserver} used
   *     to describe updated portions of an image.
   * @param x X coordinate of the region being updated
   * @param y Y coordinate of the region being updated
   * @param w Width of the region beign updated
   * @param h Height of the region being updated
   *
   * @return <code>true</code> if img is equal to the button's current icon,
   *     otherwise <code>false</code>
   */
  public boolean imageUpdate(Image img, int infoflags, int x, int y, int w,
                             int h)
  {
    return current_icon == img;
  }

  /**
   * Returns the value of the button's "contentAreaFilled" property. This
   * property indicates whether the area surrounding the text and icon of
   * the button should be filled by the look and feel class.  If this
   * property is <code>false</code>, the look and feel class should leave
   * the content area transparent.
   *
   * @return The current value of the "contentAreaFilled" property
   */
  public boolean isContentAreaFilled()
  {
    return contentAreaFilled;
  }

  /**
   * Sets the value of the button's "contentAreaFilled" property. This
   * property indicates whether the area surrounding the text and icon of
   * the button should be filled by the look and feel class.  If this
   * property is <code>false</code>, the look and feel class should leave
   * the content area transparent.
   *
   * @param b The new value of the "contentAreaFilled" property
   */
  public void setContentAreaFilled(boolean b)
  {
    if (contentAreaFilled == b)
      return;
    
    boolean old = contentAreaFilled;
    contentAreaFilled = b;
    firePropertyChange(CONTENT_AREA_FILLED_CHANGED_PROPERTY, old, b);
    // The JDK sets the opaque property to the value of the contentAreaFilled
    // property, so should we do.
    setOpaque(b);
   }

  /**
   * Paints the button's border, if the button's "borderPainted" property is
   * <code>true</code>, by out calling to the button's look and feel class.
   *
   * @param g The graphics context used to paint the border
   */
  protected void paintBorder(Graphics g)
  {
    if (isBorderPainted())
      super.paintBorder(g);
  }

  /**
   * Returns a string, used only for debugging, which identifies or somehow
   * represents this button. The exact value is implementation-defined.
   *
   * @return A string representation of the button
   */
  protected String paramString()
  {
    StringBuffer sb = new StringBuffer();
    sb.append(super.paramString());
    sb.append(",defaultIcon=");
    if (getIcon() != null)
      sb.append(getIcon());
    sb.append(",disabledIcon=");
    if (getDisabledIcon() != null)
      sb.append(getDisabledIcon());
    sb.append(",disabledSelectedIcon=");
    if (getDisabledSelectedIcon() != null)
      sb.append(getDisabledSelectedIcon());
    sb.append(",margin=");
    if (getMargin() != null)
      sb.append(getMargin());
    sb.append(",paintBorder=").append(isBorderPainted());
    sb.append(",paintFocus=").append(isFocusPainted());
    sb.append(",pressedIcon=");
    if (getPressedIcon() != null)
      sb.append(getPressedIcon());
    sb.append(",rolloverEnabled=").append(isRolloverEnabled());
    sb.append(",rolloverIcon=");
    if (getRolloverIcon() != null)
      sb.append(getRolloverIcon());
    sb.append(",rolloverSelected=");
    if (getRolloverSelectedIcon() != null)
      sb.append(getRolloverSelectedIcon());
    sb.append(",selectedIcon=");
    if (getSelectedIcon() != null)
      sb.append(getSelectedIcon());
    sb.append(",text=");
    if (getText() != null)
      sb.append(getText());
    return sb.toString();
  }

  /**
   * Set the "UI" property of the button, which is a look and feel class
   * responsible for handling the button's input events and painting it.
   *
   * @param ui The new "UI" property
   */
  public void setUI(ButtonUI ui)
  {
    super.setUI(ui);
  }
  
  /**
   * Set the "UI" property of the button, which is a look and feel class
   * responsible for handling the button's input events and painting it.
   *
   * @return The current "UI" property
   */
  public ButtonUI getUI()
  {
    return (ButtonUI) ui;
  }
  
  /**
   * Set the "UI" property to a class constructed, via the {@link
   * UIManager}, from the current look and feel. This should be overridden
   * for each subclass of AbstractButton, to retrieve a suitable {@link
   * ButtonUI} look and feel class.
   */
  public void updateUI()
  {
  }

  /**
   * Returns the current time in milliseconds in which clicks gets coalesced
   * into a single <code>ActionEvent</code>.
   *
   * @return the time in milliseconds
   * 
   * @since 1.4
   */
  public long getMultiClickThreshhold()
  {
    return multiClickThreshhold;
  }

  /**
   * Sets the time in milliseconds in which clicks gets coalesced into a single
   * <code>ActionEvent</code>.
   *
   * @param threshhold the time in milliseconds
   * 
   * @since 1.4
   */
  public void setMultiClickThreshhold(long threshhold)
  {
    if (threshhold < 0)
      throw new IllegalArgumentException();

    multiClickThreshhold = threshhold;
  }
}
