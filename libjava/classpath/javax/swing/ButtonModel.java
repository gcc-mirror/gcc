/* ButtonModel.java --
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

import java.awt.ItemSelectable;
import java.awt.event.ActionListener;
import java.awt.event.ItemListener;

import javax.swing.event.ChangeListener;

/**
 * The data model that is used in all kinds of buttons.
 */
public interface ButtonModel extends ItemSelectable
{  

  /**
   * Returns <code>true</code> if the button is armed, <code>false</code>
   * otherwise.
   *
   * A button is armed, when the user has pressed the mouse over it, but has
   * not yet released the mouse.
   * 
   * @return <code>true</code> if the button is armed, <code>false</code>
   *         otherwise
   *
   * @see #setArmed(boolean)
   */
  boolean isArmed();

  /**
   * Sets the armed flag of the button.
   *
   * A button is armed, when the user has pressed the mouse over it, but has
   * not yet released the mouse.
   *
   * @param b <code>true</code> if the button is armed, <code>false</code>
   *        otherwise
   *
   * @see #isArmed()
   */
  void setArmed(boolean b);

  /**
   * Returns <code>true</code> if the button is enabled, <code>false</code>
   * otherwise.
   *
   * When a button is disabled, it is usually grayed out and the user cannot
   * change its state.
   *
   * @return <code>true</code> if the button is enabled, <code>false</code>
   *         otherwise
   *
   * @see #setEnabled(boolean)
   */
  boolean isEnabled();

  /**
   * Sets the enabled flag of the button.
   *
   * When a button is disabled, it is usually grayed out and the user cannot
   * change its state.
   *
   * @param b <code>true</code> if the button is enabled, <code>false</code>
   *        otherwise
   *
   * @see #isEnabled()
   */
  void setEnabled(boolean b);

  /**
   * Sets the pressed flag of the button.
   *
   * The button usually gets pressed when the user clicks on a button, it will
   * be un-pressed when the user releases the mouse.
   *
   * @param b <code>true</code> if the button is pressed, <code>false</code>
   *        otherwise
   *
   * @see #isPressed()
   */
  void setPressed(boolean b);

  /**
   * Returns <code>true</code> if the button is pressed, <code>false</code>
   * otherwise.
   *
   * The button usually gets pressed when the user clicks on a button, it will
   * be un-pressed when the user releases the mouse.
   *
   * @return <code>true</code> if the button is pressed, <code>false</code>
   *         otherwise
   *
   * @see #setPressed(boolean)
   */
  boolean isPressed();

  /**
   * Removes an {@link ActionListener} from the list of registered listeners.
   *
   * @param l the action listener to remove
   *
   * @see #addActionListener(ActionListener)
   */
  void removeActionListener(ActionListener l);

  /**
   * Adds an {@link ActionListener} to the list of registered listeners.
   *
   * An <code>ActionEvent</code> is usually fired when the user clicks on a
   * button.
   * 
   * @param l the action listener to add
   *
   * @see #removeActionListener(ActionListener)
   */
  void addActionListener(ActionListener l);

  /**
   * Adds an {@link ItemListener} to the list of registered listeners.
   *
   * An <code>ItemEvent</code> is usually fired when a button's selected
   * state changes. This applies only to buttons that support the selected
   * flag.
   *
   * @param l the item listener to add
   *
   * @see #removeItemListener(ItemListener)
   */
  void addItemListener(ItemListener l);

  /**
   * Adds an {@link ItemListener} to the list of registered listeners.
   *
   * @param l the item listener to add
   *
   * @see #removeItemListener(ItemListener)
   */
  void removeItemListener(ItemListener l);

  /**
   * Adds an {@link ChangeListener} to the list of registered listeners.
   *
   * A <code>ChangeEvent</code> is fired when any one of the button's flags
   * changes.
   *
   * @param l the change listener to add
   *
   * @see #removeChangeListener(ChangeListener)
   */
  void addChangeListener(ChangeListener l);

  /**
   * Adds an {@link ChangeListener} to the list of registered listeners.
   *
   * @param l the change listener to add
   *
   * @see #removeChangeListener(ChangeListener)
   */
  void removeChangeListener(ChangeListener l);

  /**
   * Sets the rollover flag of the button.
   *
   * A button is rollover-ed, when the user has moved the mouse over it, but has
   * not yet pressed the mouse.
   *
   * @param b <code>true</code> if the button is rollover, <code>false</code>
   *        otherwise
   *
   * @see #isRollover()
   */
  void setRollover(boolean b);

  /**
   * Returns <code>true</code> if the button is rollover-ed, <code>false</code>
   * otherwise.
   *
   * A button is rollover-ed, when the user has moved the mouse over it, but has
   * not yet pressed the mouse.
   *
   * @return <code>true</code> if the button is rollover, <code>false</code>
   *         otherwise
   *
   * @see #setRollover(boolean)
   */
  boolean isRollover();

  /**
   * Returns the keyboard mnemonic for the button. This specifies a shortcut
   * or accelerator key that can be used to activate the button.
   * 
   * @return the keyboard mnemonic for the button
   *
   * @see #setMnemonic(int)
   */
  int  getMnemonic();

  /**
   * Sets the keyboard mnemonic for the button. This specifies a shortcut
   * or accelerator key that can be used to activate the button.
   * 
   * @param key the keyboard mnemonic for the button
   *
   * @see #getMnemonic()
   */
  void setMnemonic(int key);

  /**
   * Sets the action command for the button. This will be used in
   * <code>ActionEvents</code> fired by the button.
   *
   * @param s the action command to set
   *
   * @see #getActionCommand()
   */
  void setActionCommand(String s);

  /**
   * Returns the action command of the button.
   *
   * @return the action command of the button
   *
   * @see #setActionCommand(String)
   */
  String getActionCommand();

  /**
   * Sets the button group for the button. Some kinds of button (e.g. radio
   * buttons) allow only one button within a button group selected at any one
   * time.
   *
   * @param group the button group to set
   */
  void setGroup(ButtonGroup group);

  /**
   * Sets the selected flag of the button.
   *
   * Some kinds of buttons (e.g. toggle buttons, check boxes, radio buttons)
   * can be in one of two states: selected or unselected. The selected state
   * is usually toggled by clicking on the button.
   * 
   * @param b <code>true</code> if the button is selected, <code>false</code>
   *        otherwise
   *
   * @see #isSelected()
   */
  void setSelected(boolean b);

  /**
   * Returns <code>true</code> if the button is selected, <code>false</code>
   * otherwise.
   *
   * Some kinds of buttons (e.g. toggle buttons, check boxes, radio buttons)
   * can be in one of two states: selected or unselected. The selected state
   * is usually toggled by clicking on the button.
   * 
   * @return <code>true</code> if the button is selected, <code>false</code>
   *         otherwise
   *
   * @see #setSelected(boolean)
   */
  boolean isSelected();
}
