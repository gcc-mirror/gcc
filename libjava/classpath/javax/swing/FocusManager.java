/* FocusManager.java --
   Copyright (C) 2002, 2004  Free Software Foundation, Inc.

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

import java.awt.AWTEvent;
import java.awt.Component;
import java.awt.Container;
import java.awt.DefaultKeyboardFocusManager;
import java.awt.FocusTraversalPolicy;
import java.awt.KeyEventDispatcher;
import java.awt.KeyEventPostProcessor;
import java.awt.KeyboardFocusManager;
import java.awt.Window;
import java.awt.event.KeyEvent;
import java.beans.PropertyChangeListener;
import java.beans.VetoableChangeListener;
import java.util.Set;

/**
 * This class has been obsoleted by the new
 * {@link java.awt.KeyboardFocusManager} and
 * {@link java.awt.DefaultKeyboardFocusManager} API.
 *
 * @author Andrew Selkirk
 */
public abstract class FocusManager
  extends DefaultKeyboardFocusManager
{
  /**
   * A FocusManager that wraps an AWT KeyboardFocusManager and forwards all
   * method calls to it. This is used for compatibility with the new focus
   * system.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  private static class WrappingFocusManager
    extends FocusManager
  {
    /**
     * The wrapped KeyboardFocusManager.
     */
    private KeyboardFocusManager wrapped;

    /**
     * Creates a new instance of WrappedFocusManager.
     *
     * @param fm the focus manager to wrap
     */
    WrappingFocusManager(KeyboardFocusManager fm)
    {
      wrapped = fm;
    }

    /**
     * Wraps {@link DefaultKeyboardFocusManager#dispatchEvent(AWTEvent)}.
     *
     * @param ev the event to dispatch
     *
     * @return <code>true</code> if the event has been dispatched,
     *         <code>false</code> otherwise
     */
    public boolean dispatchEvent(AWTEvent ev)
    {
      return wrapped.dispatchEvent(ev);
    }

    /**
     * Wraps {@link DefaultKeyboardFocusManager#dispatchKeyEvent(KeyEvent)}.
     *
     * @param ev the event to dispatch
     *
     * @return <code>true</code> if the event has been dispatched,
     *         <code>false</code> otherwise
     */
    public boolean dispatchKeyEvent(KeyEvent ev)
    {
      return wrapped.dispatchKeyEvent(ev);
    }

    /**
     * Wraps {@link DefaultKeyboardFocusManager#downFocusCycle(Container)}.
     *
     * @param c the container
     */
    public void downFocusCycle(Container c)
    {
      wrapped.downFocusCycle(c);
    }

    /**
     * Wraps {@link DefaultKeyboardFocusManager#upFocusCycle(Container)}.
     *
     * @param c the container
     */
    public void upFocusCycle(Container c)
    {
      wrapped.upFocusCycle(c);
    }

    /**
     * Wraps {@link DefaultKeyboardFocusManager#focusNextComponent(Component)}.
     *
     * @param c the component
     */
    public void focusNextComponent(Component c)
    {
      wrapped.focusNextComponent(c);
    }

    /**
     * Wraps
     * {@link DefaultKeyboardFocusManager#focusPreviousComponent(Component)}.
     *
     * @param c the component
     */
    public void focusPreviousComponent(Component c)
    {
      wrapped.focusPreviousComponent(c);
    }

    /**
     * Wraps {@link DefaultKeyboardFocusManager#postProcessKeyEvent(KeyEvent)}.
     *
     * @param e the key event
     *
     * @return a boolead
     */
    public boolean postProcessKeyEvent(KeyEvent e)
    {
      return wrapped.postProcessKeyEvent(e);
    }

    /**
     * Wraps
     * {@link DefaultKeyboardFocusManager#processKeyEvent(Component, KeyEvent)}.
     *
     * @param c the component
     * @param e the key event
     */
    public void processKeyEvent(Component c, KeyEvent e)
    {
      wrapped.processKeyEvent(c, e);
    }

    /**
     * Wraps
     * {@link KeyboardFocusManager#addKeyEventDispatcher(KeyEventDispatcher)}.
     *
     * @param d the dispatcher
     */
    public void addKeyEventDispatcher(KeyEventDispatcher d)
    {
      wrapped.addKeyEventDispatcher(d);
    }

    /**
     * Wraps
     * {@link KeyboardFocusManager#addKeyEventPostProcessor(KeyEventPostProcessor)}.
     *
     * @param p the post processor
     */
    public void addKeyEventPostProcessor(KeyEventPostProcessor p)
    {
      wrapped.addKeyEventPostProcessor(p);
    }
 
    /**
     * Wraps {@link KeyboardFocusManager#addPropertyChangeListener(PropertyChangeListener)}.
     *
     * @param l the property change listener
     */
    public void addPropertyChangeListener(PropertyChangeListener l)
    {
      wrapped.addPropertyChangeListener(l);
    }

    /**
     * Wraps {@link KeyboardFocusManager#addPropertyChangeListener(String, PropertyChangeListener)}.
     *
     * @param p the property name
     * @param l the property change listener
     */
    public void addPropertyChangeListener(String p, PropertyChangeListener l)
    {
      wrapped.addPropertyChangeListener(p, l);
    }

    /**
     * Wraps {@link KeyboardFocusManager#addVetoableChangeListener(String, VetoableChangeListener)}.
     *
     * @param p the property name
     * @param l the vetoable change listener
     */
    public void addVetoableChangeListener(String p, VetoableChangeListener l)
    {
      wrapped.addVetoableChangeListener(p, l);
    }

    /**
     * Wraps {@link KeyboardFocusManager#addVetoableChangeListener(VetoableChangeListener)}.
     *
     * @param l the vetoable change listener
     */
    public void addVetoableChangeListener(VetoableChangeListener l)
    {
      wrapped.addVetoableChangeListener(l);
    }

    /**
     * Wraps {@link KeyboardFocusManager#clearGlobalFocusOwner()}.
     */
    public void clearGlobalFocusOwner()
    {
      wrapped.clearGlobalFocusOwner();
    }

    /**
     * Wraps {@link KeyboardFocusManager#getActiveWindow()}.
     *
     * @return the active window
     */
    public Window getActiveWindow()
    {
      return wrapped.getActiveWindow();
    }

    /**
     * Wraps {@link KeyboardFocusManager#getCurrentFocusCycleRoot()}.
     *
     * @return the focus cycle root
     */
    public Container getCurrentFocusCycleRoot()
    {
      return wrapped.getCurrentFocusCycleRoot();
    }

    /**
     * Wraps {@link KeyboardFocusManager#getDefaultFocusTraversalKeys(int)}.
     *
     * @param i the ID
     *
     * @return the focus traversal keys
     */
    public Set getDefaultFocusTraversalKeys(int i)
    {
      return wrapped.getDefaultFocusTraversalKeys(i);
    }

    /**
     * Wraps {@link KeyboardFocusManager#getDefaultFocusTraversalPolicy()}.
     *
     * @return the focus traversal policy
     */
    public FocusTraversalPolicy getDefaultFocusTraversalPolicy()
    {
      return wrapped.getDefaultFocusTraversalPolicy();
    }

    /**
     * Wraps {@link KeyboardFocusManager#getFocusedWindow()}.
     *
     * @return the focused window
     */
    public Window getFocusedWindow()
    {
      return wrapped.getFocusedWindow();
    }

    /**
     * Wraps {@link KeyboardFocusManager#getFocusOwner()}.
     *
     * @return the focus owner
     */
    public Component getFocusOwner()
    {
      return wrapped.getFocusOwner();
    }

    /**
     * Wraps {@link KeyboardFocusManager#getPermanentFocusOwner()}.
     *
     * @return the focus owner
     */
    public Component getPermanentFocusOwner()
    {
      return wrapped.getPermanentFocusOwner();
    }

    /**
     * Wraps {@link KeyboardFocusManager#getPropertyChangeListeners()}.
     *
     * @return the property change listeners
     */
    public PropertyChangeListener[] getPropertyChangeListeners()
    {
      return wrapped.getPropertyChangeListeners();
    }

    /**
     * Wraps {@link KeyboardFocusManager#getPropertyChangeListeners(String)}.
     *
     * @param n the property name
     *
     * @return the property change listeners
     */
    public PropertyChangeListener[] getPropertyChangeListeners(String n)
    {
      return wrapped.getPropertyChangeListeners(n);
    }

    /**
     * Wraps {@link KeyboardFocusManager#getVetoableChangeListeners()}.
     *
     * @return the vetoable change listeners
     */
    public VetoableChangeListener[] getVetoableChangeListeners()
    {
      return wrapped.getVetoableChangeListeners();
    }

    /**
     * Wraps {@link KeyboardFocusManager#getVetoableChangeListeners(String)}.
     *
     * @param n the property name
     *
     * @return the vetoable change listeners
     */
    public VetoableChangeListener[] getVetoableChangeListeners(String n)
    {
      return wrapped.getVetoableChangeListeners(n);
    }

    
    /**
     * Wraps
     * {@link KeyboardFocusManager#removeKeyEventDispatcher(KeyEventDispatcher)}.
     *
     * @param d the key event dispatcher to remove
     */
    public void removeKeyEventDispatcher(KeyEventDispatcher d)
    {
      wrapped.removeKeyEventDispatcher(d);
    }

    /**
     * Wraps
     * {@link KeyboardFocusManager#removeKeyEventPostProcessor(KeyEventPostProcessor)}.
     *
     * @param p the post processor
     */
    public void removeKeyEventPostProcessor(KeyEventPostProcessor p)
    {
      wrapped.removeKeyEventPostProcessor(p);
    }

    /**
     * Wraps
     * {@link KeyboardFocusManager#removePropertyChangeListener(PropertyChangeListener)}.
     *
     * @param l the listener
     */
    public void removePropertyChangeListener(PropertyChangeListener l)
    {
      wrapped.removePropertyChangeListener(l);
    }

    /**
     * Wraps
     * {@link KeyboardFocusManager#removePropertyChangeListener(String, PropertyChangeListener)}.
     *
     * @param n the property name
     * @param l the listener
     */
    public void removePropertyChangeListener(String n, PropertyChangeListener l)
    {
      wrapped.removePropertyChangeListener(n, l);
    }

    /**
     * Wraps
     * {@link KeyboardFocusManager#removeVetoableChangeListener(VetoableChangeListener)}.
     *
     * @param l the listener
     */
    public void removeVetoableChangeListener(VetoableChangeListener l)
    {
      wrapped.removeVetoableChangeListener(l);
    }

    /**
     * Wraps
     * {@link KeyboardFocusManager#removeVetoableChangeListener(String, VetoableChangeListener)}.
     *
     * @param n the property name
     * @param l the listener
     */
    public void removeVetoableChangeListener(String n, VetoableChangeListener l)
    {
      wrapped.removeVetoableChangeListener(n, l);
    }

    /**
     * Wraps
     * {@link KeyboardFocusManager#setDefaultFocusTraversalKeys(int, Set)}.
     *
     * @param id the ID
     * @param k the keystrokes
     */
    public void setDefaultFocusTraversalKeys(int id, Set k)
    {
      wrapped.setDefaultFocusTraversalKeys(id, k);
    }

    /**
     * Wraps {@link KeyboardFocusManager#setDefaultFocusTraversalPolicy(FocusTraversalPolicy)}.
     *
     * @param p the focus traversal policy
     */
    public void setDefaultFocusTraversalPolicy(FocusTraversalPolicy p)
    {
      wrapped.setDefaultFocusTraversalPolicy(p);
    }

    /**
     * Wraps
     * {@link KeyboardFocusManager#setGlobalCurrentFocusCycleRoot(Container)}.
     *
     * @param r the focus cycle root
     */
    public void setGlobalCurrentFocusCycleRoot(Container r)
    {
      wrapped.setGlobalCurrentFocusCycleRoot(r);
    }
  }

  /**
   * FOCUS_MANAGER_CLASS_PROPERTY
   */
  public static final String FOCUS_MANAGER_CLASS_PROPERTY =
    "FocusManagerClassName";

  /**
   * Constructor FocusManager
   */
  public FocusManager()
  {
    super();
  }

  /**
   * getCurrentManager
   * @return FocusManager
   */
  public static FocusManager getCurrentManager()
  {
    KeyboardFocusManager m =
      KeyboardFocusManager.getCurrentKeyboardFocusManager(); 
    return new WrappingFocusManager(m);
  }

  /**
   * setCurrentManager
   * @param manager TODO
   */
  public static void setCurrentManager(FocusManager manager)
  {
    KeyboardFocusManager.setCurrentKeyboardFocusManager(manager);
  }

  /**
   * disableSwingFocusManager
   * @deprecated 1.4
   */
  public static void disableSwingFocusManager()
  {
    // TODO
  }

  /**
   * isFocusManagerEnabled
   * @return boolean
   * @deprecated 1.4
   */
  public static boolean isFocusManagerEnabled()
  {
    return false; // TODO
  }
}
