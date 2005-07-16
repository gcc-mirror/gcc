/* Action.java --
   Copyright (C) 2002, 2004, 2005 Free Software Foundation, Inc.

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

import java.awt.event.ActionListener;
import java.beans.PropertyChangeListener;

/**
 * An action provides a convenient central point of control for some task
 * that can be triggered by more than one control in a Swing user interface
 * (for example, a menu item and a toolbar button).
 * 
 * @see AbstractButton#setAction(Action)
 * 
 * @author Ronald Veldema (rveldema@cs.vu.nl)
 * @author Andrew Selkirk
 */
public interface Action extends ActionListener {

  /**
   * A key to access the default property for the action (this is not used).
   */
  String DEFAULT = "Default";

  /**
   * A key to access the long description for the action.
   */
  String LONG_DESCRIPTION = "LongDescription";

  /**
   * A key to access the name for the action.
   */
  String NAME = "Name";

  /**
   * A key to access the short description for the action (the short
   * description is typically used as the tool tip text).
   */
  String SHORT_DESCRIPTION = "ShortDescription";

  /**
   * A key to access the icon for the action.
   */
  String SMALL_ICON = "SmallIcon";

  /**
   * A key to access the {@link KeyStroke} used as the accelerator for the
   * action.
   */
  String ACCELERATOR_KEY = "AcceleratorKey";

  /**
   * A key to access the action command string for the action.
   */
  String ACTION_COMMAND_KEY = "ActionCommandKey";

  /**
   * A key to access the mnemonic for the action.
   */
  String MNEMONIC_KEY = "MnemonicKey";

  /**
   * Returns the value associated with the specified key.
   * 
   * @param key  the key (not <code>null</code>).
   * 
   * @return The value associated with the specified key, or 
   *         <code>null</code> if the key is not found.
   */
  Object getValue(String key);

  /**
   * Sets the value associated with the specified key and sends a 
   * {@link java.beans.PropertyChangeEvent} to all registered listeners.  
   * The standard keys are defined in this interface: {@link #NAME}, 
   * {@link #SHORT_DESCRIPTION}, {@link #LONG_DESCRIPTION}, 
   * {@link #SMALL_ICON}, {@link #ACTION_COMMAND_KEY}, 
   * {@link #ACCELERATOR_KEY} and {@link #MNEMONIC_KEY}. Any existing value 
   * associated with the key will be overwritten.  
   * 
   * @param key  the key (not <code>null</code>).
   * @param value  the value (<code>null</code> permitted).
   */
  void putValue(String key, Object value);

  /**
   * Returns the flag that indicates whether or not this action is enabled.
   * 
   * @return The flag.
   */
  boolean isEnabled();

  /**
   * Sets the flag that indicates whether or not this action is enabled.  If
   * the value changes, a {@link java.beans.PropertyChangeEvent} is sent to 
   * all registered listeners.
   * 
   * @param b  the new value of the flag.
   */
  void setEnabled(boolean b);

  /**
   * Registers a listener to receive notification whenever one of the
   * action's properties is modified.
   * 
   * @param listener  the listener.
   */
  void addPropertyChangeListener(PropertyChangeListener listener);

  /**
   * Deregisters a listener so that it no longer receives notification of
   * changes to the action's properties. 
   * 
   * @param listener  the listener.
   */
  void removePropertyChangeListener(PropertyChangeListener listener);

} // Action
