/* AbstractAction.java --
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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.Serializable;
import java.util.HashMap;

import javax.swing.event.SwingPropertyChangeSupport;

/**
 * A base class for implementing the {@link Action} interface.
 *
 * @author Andrew Selkirk
 */
public abstract class AbstractAction
  implements Action, Cloneable, Serializable
{
  private static final long serialVersionUID = -6803159439231523484L;

  /**
   * A flag that indicates whether or not the action is enabled.
   */
  protected boolean enabled = true;

  /**
   * Provides support for property change event notification.
   */
  protected SwingPropertyChangeSupport changeSupport =
    new SwingPropertyChangeSupport(this);

  /**
   * store
   */
  private transient HashMap store = new HashMap();

  /**
   * Creates a new action with no properties set.
   */
  public AbstractAction()
  {
    // Nothing to do.
  }

  /**
   * Creates a new action with the specified name.  The name is stored as a
   * property with the key {@link Action#NAME}, and no other properties are
   * initialised.
   *
   * @param name  the name (<code>null</code> permitted).
   */
  public AbstractAction(String name)
  {
    putValue(NAME, name);
  }

  /**
   * Creates a new action with the specified name and icon.  The name is stored
   * as a property with the key {@link Action#NAME}, the icon is stored as a
   * property with the key {@link Action#SMALL_ICON}, and no other properties
   * are initialised.
   *
   * @param name  the name (<code>null</code> permitted).
   * @param icon  the icon (<code>null</code> permitted).
   */
  public AbstractAction(String name, Icon icon)
  {
    putValue(NAME, name);
    putValue(SMALL_ICON, icon);
  }

  /**
   * Returns a clone of the action.
   *
   * @return A clone of the action.
   *
   * @exception CloneNotSupportedException if there is a problem cloning the
   *            action.
   */
  protected Object clone() throws CloneNotSupportedException
  {
    AbstractAction copy = (AbstractAction) super.clone();
    copy.store = (HashMap) store.clone();
    return copy;
  }

  /**
   * Returns the value associated with the specified key.
   *
   * @param key  the key (not <code>null</code>).
   *
   * @return The value associated with the specified key, or
   *         <code>null</code> if the key is not found.
   *
   * @see #putValue(String, Object)
   */
  public Object getValue(String key)
  {
    return store.get(key);
  }

  /**
   * Sets the value associated with the specified key and sends a
   * {@link java.beans.PropertyChangeEvent} to all registered listeners.
   * The standard keys are:
   * <ul>
   * <li>{@link #NAME}</li>
   * <li>{@link #SHORT_DESCRIPTION}</li>
   * <li>{@link #LONG_DESCRIPTION}</li>
   * <li>{@link #SMALL_ICON}</li>
   * <li>{@link #ACTION_COMMAND_KEY}</li>
   * <li>{@link #ACCELERATOR_KEY}</li>
   * <li>{@link #MNEMONIC_KEY}</li>
   * </ul>
   * Any existing value associated with the key will be overwritten.
   *
   * @param key  the key (not <code>null</code>).
   * @param value  the value (<code>null</code> permitted).
   */
  public void putValue(String key, Object value)
  {
    Object old = getValue(key);
    if ((old == null && value != null) || (old != null && !old.equals(value)))
    {
      store.put(key, value);
      firePropertyChange(key, old, value);
    }
  }

  /**
   * Returns the flag that indicates whether or not the action is enabled.
   *
   * @return The flag.
   *
   * @see #setEnabled(boolean)
   */
  public boolean isEnabled()
  {
    return enabled;
  }

  /**
   * Sets the flag that indicates whether or not the action is enabled and, if
   * the value of the flag changed from the previous setting, sends a
   * {@link java.beans.PropertyChangeEvent} to all registered listeners (using
   * the property name 'enabled').
   *
   * @param enabled  the new flag value.
   *
   * @see #isEnabled()
   */
  public void setEnabled(boolean enabled)
  {
    if (enabled != this.enabled)
    {
      this.enabled = enabled;
      firePropertyChange("enabled", !this.enabled, this.enabled);
    }
  }

  /**
   * Returns an array of the keys for the property values that have been
   * defined via the {@link #putValue(String, Object)} method (or the class
   * constructor).
   *
   * @return An array of keys.
   */
  public Object[] getKeys()
  {
    return store.keySet().toArray();
  }

  /**
   * Sends a {@link PropertyChangeEvent} for the named property to all
   * registered listeners.
   *
   * @param propertyName  the property name.
   * @param oldValue  the old value of the property.
   * @param newValue  the new value of the property.
   */
  protected void firePropertyChange(String propertyName, Object oldValue,
                                    Object newValue)
  {
    changeSupport.firePropertyChange(propertyName, oldValue, newValue);
  }

  /**
   * Sends a {@link PropertyChangeEvent} for the named property to all
   * registered listeners.  This private method is called by the
   * {@link #setEnabled(boolean)} method.
   *
   * @param propertyName  the property name.
   * @param oldValue  the old value of the property.
   * @param newValue  the new value of the property.
   */
  private void firePropertyChange(String propertyName, boolean oldValue,
                                  boolean newValue)
  {
    changeSupport.firePropertyChange(propertyName, oldValue, newValue);
  }

  /**
   * Registers a listener to receive {@link PropertyChangeEvent} notifications
   * from this action.
   *
   * @param listener the listener.
   *
   * @see #removePropertyChangeListener(PropertyChangeListener)
   */
  public void addPropertyChangeListener(PropertyChangeListener listener)
  {
    changeSupport.addPropertyChangeListener(listener);
  }

  /**
   * Deregisters a listener so that it no longer receives
   * {@link PropertyChangeEvent} notifications from this action.
   *
   * @param listener the listener.
   *
   * @see #addPropertyChangeListener(PropertyChangeListener)
   */
  public void removePropertyChangeListener(PropertyChangeListener listener)
  {
    changeSupport.removePropertyChangeListener(listener);
  }

  /**
   * Returns all registered listeners.
   *
   * @return An array of listeners.
   *
   * @since 1.4
   */
  public PropertyChangeListener[] getPropertyChangeListeners()
  {
    return changeSupport.getPropertyChangeListeners();
  }
}
