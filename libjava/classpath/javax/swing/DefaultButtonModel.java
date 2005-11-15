/* DefaultButtonModel.java --
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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.io.Serializable;
import java.util.EventListener;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.EventListenerList;

/**
 * The default implementation of {@link ButtonModel}.
 * The purpose of this class is to model the dynamic state of an abstract
 * button. The concrete button type holding this state may be a a "toggle"
 * button (checkbox, radio button) or a "push" button (menu button, button).
 * If the model is disabled, only the "selected" property can be changed. An
 * attempt to change the "armed", "rollover" or "pressed" properties  while
 * the model is disabled will be blocked. Any successful (non-blocked) change
 * to the model's properties will trigger the firing of a ChangeEvent. Any
 * change to the "selected" property will trigger the firing of an ItemEvent
 * in addition to ChangeEvent. This is true whether the model is enabled or
 * not. One other state change is special: the transition from "enabled,
 * armed and pressd" to "enabled, armed and not-pressed". This is considered
 * the "trailing edge" of a successful mouse click, and therefore fires an
 * ActionEvent in addition to a ChangeEvent. In all other respects this class
 * is just a container of boolean flags.
 *
 * @author Graydon Hoare (graydon_at_redhat.com)
 */
public class DefaultButtonModel implements ButtonModel, Serializable
{
  /** DOCUMENT ME! */
  private static final long serialVersionUID = -5342609566534980231L;

  /**
   * Indicates that the button is <em>partially</em> committed to being
   * pressed, but not entirely. This usually happens when a user has pressed
   * but not yet released the mouse button.
   */
  public static final int ARMED = 1;

  /**
   * State constant indicating that the button is enabled. Buttons cannot be
   * pressed or selected unless they are enabled.
   */
  public static final int ENABLED = 8;

  /**
   * State constant indicating that the user is holding down the button. When
   * this transitions from true to false, an ActionEvent may be fired,
   * depending on the value of the "armed" property.
   */
  public static final int PRESSED = 4;

  /**
   * State constant indicating that the mouse is currently positioned over the
   * button.
   */
  public static final int ROLLOVER = 16;

  /**
   * State constant indicating that the button is selected. This constant is
   * only meaningful for toggle-type buttons (radio buttons, checkboxes).
   */
  public static final int SELECTED = 2;

  /**
   * Represents the "state properties" (armed, enabled, pressed, rollover and
   * selected) by a bitwise combination of integer constants.
   */
  protected int stateMask = ENABLED;

  /**
   * List of ItemListeners, ChangeListeners, and ActionListeners registered on
   * this model.
   */
  protected EventListenerList listenerList = new EventListenerList();

  /** The single ChangeEvent this model (re)uses to call its ChangeListeners. */
  protected ChangeEvent changeEvent = new ChangeEvent(this);

  /**
   * The group this model belongs to. Only one button in a group may be
   * selected at any given time.
   */
  protected ButtonGroup group;

  /**
   * The key code (one of {@link java.awt.event.KeyEvent} VK_) used to press
   * this button via a keyboard interface.
   */
  protected int mnemonic = KeyEvent.VK_UNDEFINED;

  /**
   * The string used as the "command" property of any ActionEvent this model
   * sends.
   */
  protected String actionCommand;

  /**
   * Creates a new DefaultButtonModel object.
   */
  public DefaultButtonModel()
  {
    // Nothing to do here.
  }

  /**
   * Return <code>null</code>. Use {@link AbstractButton} if you wish to
   * interface with a button via an {@link ItemSelectable} interface.
   *
   * @return <code>null</code>
   */
  public Object[] getSelectedObjects()
  {
    return null;
  }

  /**
   * Returns a specified class of listeners.
   *
   * @param listenerType the type of listener to return
   *
   * @return array of listeners
   */
  public EventListener[] getListeners(Class listenerType)
  {
    return listenerList.getListeners(listenerType);
  }

  /**
   * Add an ActionListener to the model. Usually only called to subscribe an
   * AbstractButton's listener to the model.
   *
   * @param l The listener to add
   */
  public void addActionListener(ActionListener l)
  {
    listenerList.add(ActionListener.class, l);
  }

  /**
   * Remove an ActionListener to the model. Usually only called to unsubscribe
   * an AbstractButton's listener to the model.
   *
   * @param l The listener to remove
   */
  public void removeActionListener(ActionListener l)
  {
    listenerList.remove(ActionListener.class, l);
  }

  /**
   * Returns all registered <code>ActionListener</code> objects.
   *
   * @return array of <code>ActionListener</code> objects
   */
  public ActionListener[] getActionListeners()
  {
    return (ActionListener[]) listenerList.getListeners(ActionListener.class);
  }

  /**
   * Add an ItemListener to the model. Usually only called to subscribe an
   * AbstractButton's listener to the model.
   *
   * @param l The listener to add
   */
  public void addItemListener(ItemListener l)
  {
    listenerList.add(ItemListener.class, l);
  }

  /**
   * Remove an ItemListener to the model. Usually only called to unsubscribe
   * an AbstractButton's listener to the model.
   *
   * @param l The listener to remove
   */
  public void removeItemListener(ItemListener l)
  {
    listenerList.remove(ItemListener.class, l);
  }

  /**
   * Returns all registered <code>ItemListener</code> objects.
   *
   * @return array of <code>ItemListener</code> objects
   */
  public ItemListener[] getItemListeners()
  {
    return (ItemListener[]) listenerList.getListeners(ItemListener.class);
  }

  /**
   * Add a ChangeListener to the model. Usually only called to subscribe an
   * AbstractButton's listener to the model.
   *
   * @param l The listener to add
   */
  public void addChangeListener(ChangeListener l)
  {
    listenerList.add(ChangeListener.class, l);
  }

  /**
   * Remove a ChangeListener to the model. Usually only called to unsubscribe
   * an AbstractButton's listener to the model.
   *
   * @param l The listener to remove
   */
  public void removeChangeListener(ChangeListener l)
  {
    listenerList.remove(ChangeListener.class, l);
  }

  /**
   * Returns all registered <code>ChangeListener</code> objects.
   *
   * @return array of <code>ChangeListener</code> objects
   */
  public ChangeListener[] getChangeListeners()
  {
    return (ChangeListener[]) listenerList.getListeners(ChangeListener.class);
  }

  /**
   * Inform each ItemListener in the {@link #listenerList} that an ItemEvent
   * has occurred. This happens in response to any change to the {@link
   * #stateMask} field.
   *
   * @param e The ItemEvent to fire
   */
  protected void fireItemStateChanged(ItemEvent e)
  {
    ItemListener[] ll = getItemListeners();

    for (int i = 0; i < ll.length; i++)
      ll[i].itemStateChanged(e);
  }

  /**
   * Inform each ActionListener in the {@link #listenerList} that an
   * ActionEvent has occurred. This happens in response to the any change to
   * the {@link #stateMask} field which makes the enabled, armed and pressed
   * properties all simultaneously <code>true</code>.
   *
   * @param e The ActionEvent to fire
   */
  protected void fireActionPerformed(ActionEvent e)
  {
    ActionListener[] ll = getActionListeners();

    for (int i = 0; i < ll.length; i++)
      ll[i].actionPerformed(e);
  }

  /**
   * Inform each ChangeListener in the {@link #listenerList} that a ChangeEvent
   * has occurred. This happens in response to the any change to a property
   * of the model.
   */
  protected void fireStateChanged()
  {
    ChangeListener[] ll = getChangeListeners();

    for (int i = 0; i < ll.length; i++)
      ll[i].stateChanged(changeEvent);
  }

  /**
   * Get the value of the model's "armed" property.
   *
   * @return The current "armed" property
   */
  public boolean isArmed()
  {
    return (stateMask & ARMED) == ARMED;
  }

  /**
   * Set the value of the model's "armed" property.
   *
   * @param a The new "armed" property
   */
  public void setArmed(boolean a)
  {
    // if this call does not represent a CHANGE in state, then return
    if ((a && isArmed()) || (!a && !isArmed()))
      return;
    
    // cannot change ARMED state unless button is enabled
    if (!isEnabled())
      return;

    // make the change
    if (a)
      stateMask = stateMask | ARMED;
    else
      stateMask = stateMask & (~ARMED);

    // notify interested ChangeListeners
    fireStateChanged();
  }

  /**
   * Get the value of the model's "enabled" property.
   *
   * @return The current "enabled" property.
   */
  public boolean isEnabled()
  {
    return (stateMask & ENABLED) == ENABLED;
  }

  /**
   * Set the value of the model's "enabled" property.
   *
   * @param e The new "enabled" property
   */
  public void setEnabled(boolean e)
  {
    // if this call does not represent a CHANGE in state, then return
    if ((e && isEnabled()) || (!e && !isEnabled()))
      return;

    // make the change
    if (e)
      stateMask = stateMask | ENABLED;
    else
      stateMask = stateMask & (~ENABLED);

    // notify interested ChangeListeners
    fireStateChanged();
  }

  /**
   * Set the value of the model's "pressed" property.
   *
   * @param p The new "pressed" property
   */
  public void setPressed(boolean p)
  {
    // if this call does not represent a CHANGE in state, then return
    if ((p && isPressed()) || (!p && !isPressed()))
      return;

    // cannot changed PRESSED state unless button is enabled
    if (!isEnabled())
      return;

    // make the change
    if (p)
      stateMask = stateMask | PRESSED;
    else
      stateMask = stateMask & (~PRESSED);

    // if button is armed and was released, fire action event
    if (!p && isArmed())
      fireActionPerformed(new ActionEvent(this, ActionEvent.ACTION_PERFORMED,
                                          actionCommand));

    // notify interested ChangeListeners
    fireStateChanged();
  }

  /**
   * Get the value of the model's "pressed" property.
   *
   * @return The current "pressed" property
   */
  public boolean isPressed()
  {
    return (stateMask & PRESSED) == PRESSED;
  }

  /**
   * Set the value of the model's "rollover" property.
   *
   * @param r The new "rollover" property
   */
  public void setRollover(boolean r)
  {
    // if this call does not represent a CHANGE in state, then return
    if ((r && isRollover()) || (!r && !isRollover()))
      return;
    
    // cannot set ROLLOVER property unless button is enabled
    if (!isEnabled())
      return;

    // make the change
    if (r)
      stateMask = stateMask | ROLLOVER;
    else
      stateMask = stateMask & (~ROLLOVER);

    // notify interested ChangeListeners
    fireStateChanged();
  }

  /**
   * Set the value of the model's "selected" property.
   *
   * @param s The new "selected" property
   */
  public void setSelected(boolean s)
  {
    // if this call does not represent a CHANGE in state, then return
    if ((s && isSelected()) || (!s && !isSelected()))
      return;
    
    // make the change
    if (s)
      stateMask = stateMask | SELECTED;
    else
      stateMask = stateMask & (~SELECTED);

    // notify interested ChangeListeners
    fireStateChanged();

    // fire ItemStateChanged events
    if (s)
      {
        fireItemStateChanged(new ItemEvent(this, ItemEvent.ITEM_STATE_CHANGED,
                                           null, ItemEvent.SELECTED));
        if (group != null)
          group.setSelected(this, true);
      }
    else
      {
        fireItemStateChanged(new ItemEvent(this, ItemEvent.ITEM_STATE_CHANGED,
                                           null, ItemEvent.DESELECTED));
        if (group != null)
          group.setSelected(this, false);
      }
  }

  /**
   * Get the value of the model's "selected" property.
   *
   * @return The current "selected" property
   */
  public boolean isSelected()
  {
    return (stateMask & SELECTED) == SELECTED;
  }

  /**
   * Get the value of the model's "rollover" property.
   *
   * @return The current "rollover" property
   */
  public boolean isRollover()
  {
    return (stateMask & ROLLOVER) == ROLLOVER;
  }

  /**
   * Get the value of the model's "mnemonic" property.
   *
   * @return The current "mnemonic" property
   */
  public int getMnemonic()
  {
    return mnemonic;
  }

  /**
   * Set the value of the model's "mnemonic" property.
   *
   * @param key The new "mnemonic" property
   */
  public void setMnemonic(int key)
  {
    if (mnemonic != key)
      {
        mnemonic = key;
        fireStateChanged();
      }
  }

  /**
   * Set the value of the model's "actionCommand" property. This property is
   * used as the "command" property of the {@link ActionEvent} fired from the
   * model.
   *
   * @param s The new "actionCommand" property.
   */
  public void setActionCommand(String s)
  {
    if (actionCommand != s)
      {
        actionCommand = s;
        fireStateChanged();
      }
  }

  /**
   * Returns the current value of the model's "actionCommand" property.
   *
   * @return The current "actionCommand" property
   */
  public String getActionCommand()
  {
    return actionCommand;
  }

  /**
   * Set the value of the model's "group" property. The model is said to be a
   * member of the {@link ButtonGroup} held in its "group" property, and only
   * one model in a given group can have their "selected" property be
   * <code>true</code> at a time.
   *
   * @param g The new "group" property
   */
  public void setGroup(ButtonGroup g)
  {
    if (group != g)
      {
        group = g;
        fireStateChanged();
      }
  }

  /**
   * Returns the current value of the model's "group" property.
   *
   * @return The value of the "group" property
   */
  public ButtonGroup getGroup()
  {
    return group;
  }
}
