/* KeyboardFocusManager.java -- manage component focusing via the keyboard
   Copyright (C) 2002 Free Software Foundation

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


package java.awt;

import java.awt.event.KeyEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;
import java.beans.VetoableChangeSupport;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

/**
 *
 * @author Eric Blake <ebb9@email.byu.edu>
 * @since 1.4
 * @status partially updated to 1.4, needs documentation.
 */
public abstract class KeyboardFocusManager
  implements KeyEventDispatcher, KeyEventPostProcessor
{
  public static final int FORWARD_TRAVERSAL_KEYS = 0;
  public static final int BACKWARD_TRAVERSAL_KEYS = 1;
  public static final int UP_CYCLE_TRAVERSAL_KEYS = 2;
  public static final int DOWN_CYCLE_TRAVERSAL_KEYS = 3;

  private static final Set DEFAULT_FORWARD_KEYS;
  private static final Set DEFAULT_BACKWARD_KEYS;
  static
  {
    Set s = new HashSet();
    s.add(AWTKeyStroke.getAWTKeyStroke(KeyEvent.VK_TAB, 0));
    s.add(AWTKeyStroke.getAWTKeyStroke(KeyEvent.VK_TAB,
                                       KeyEvent.CTRL_DOWN_MASK));
    DEFAULT_FORWARD_KEYS = Collections.unmodifiableSet(s);
    s = new HashSet();
    s.add(AWTKeyStroke.getAWTKeyStroke(KeyEvent.VK_TAB,
                                       KeyEvent.SHIFT_DOWN_MASK));
    s.add(AWTKeyStroke.getAWTKeyStroke(KeyEvent.VK_TAB,
                                       KeyEvent.SHIFT_DOWN_MASK
                                       | KeyEvent.CTRL_DOWN_MASK));
    DEFAULT_BACKWARD_KEYS = Collections.unmodifiableSet(s);
  }

  private static KeyboardFocusManager current
    = new DefaultKeyboardFocusManager();

  // XXX Not implemented correctly. I think a good implementation here may
  // be to have permanentFocusOwner be null, and fall back to focusOwner,
  // unless a temporary focus change is in effect.
  private static Component focusOwner;
  private static Component permanentFocusOwner;

  private static Window focusedWindow;
  private static Window activeWindow;
  private static Container focusCycleRoot;

  private FocusTraversalPolicy defaultPolicy;
  private Set[] defaultFocusKeys = new Set[] {
    DEFAULT_FORWARD_KEYS, DEFAULT_BACKWARD_KEYS,
    Collections.EMPTY_SET, Collections.EMPTY_SET
  };

  private final PropertyChangeSupport propertyChangeSupport
    = new PropertyChangeSupport(this);
  private final VetoableChangeSupport vetoableChangeSupport
    = new VetoableChangeSupport(this);
  private final ArrayList keyEventDispatchers = new ArrayList();
  private final ArrayList keyEventPostProcessors = new ArrayList();


  public KeyboardFocusManager()
  {
  }

  public static KeyboardFocusManager getCurrentKeyboardFocusManager()
  {
    // XXX Need a way to divide this into contexts.
    return current;
  }

  public static void setCurrentKeyboardFocusManager(KeyboardFocusManager m)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPermission(new AWTPermission("replaceKeyboardFocusManager"));
    // XXX Need a way to divide this into contexts.
    current = m == null ? new DefaultKeyboardFocusManager() : m;
  }

  public Component getFocusOwner()
  {
    // XXX Need an easy way to test if this thread is in the context of the
    // global focus owner, to avoid creating the exception in the first place.
    try
      {
        return getGlobalFocusOwner();
      }
    catch (SecurityException e)
      {
        return null;
      }
  }

  protected Component getGlobalFocusOwner()
  {
    // XXX Need a way to test if this thread is in the context of the focus
    // owner, and throw a SecurityException if that is the case.
    // XXX Implement.
    return focusOwner;
  }

  protected void setGlobalFocusOwner(Component owner)
  {
    // XXX Should this send focus events to the components involved?
    if (owner == null || owner.focusable)
      {
        firePropertyChange("focusOwner", focusOwner, owner);
        try
          {
            fireVetoableChange("focusOwner", focusOwner, owner);
            focusOwner = owner;
          }
        catch (PropertyVetoException e)
          {
          }
      }
  }

  public void clearGlobalFocusOwner()
  {
    // XXX Is this enough?
    setGlobalFocusOwner(null);
  }

  public Component getPermanentFocusOwner()
  {
    // XXX Need an easy way to test if this thread is in the context of the
    // global focus owner, to avoid creating the exception in the first place.
    try
      {
        return getGlobalPermanentFocusOwner();
      }
    catch (SecurityException e)
      {
        return null;
      }
  }

  protected Component getGlobalPermanentFocusOwner()
  {
    // XXX Need a way to test if this thread is in the context of the focus
    // owner, and throw a SecurityException if that is the case.
    // XXX Implement.
    return permanentFocusOwner == null ? focusOwner : permanentFocusOwner;
  }

  protected void setGlobalPermanentFocusOwner(Component focusOwner)
  {
    // XXX Should this send focus events to the components involved?
    if (focusOwner == null || focusOwner.focusable)
      {
        firePropertyChange("permanentFocusOwner", permanentFocusOwner,
                           focusOwner);
        try
          {
            fireVetoableChange("permanentFocusOwner", permanentFocusOwner,
                               focusOwner);
            permanentFocusOwner = focusOwner;
          }
        catch (PropertyVetoException e)
          {
          }
      }
  }

  public Window getFocusedWindow()
  {
    // XXX Need an easy way to test if this thread is in the context of the
    // global focus owner, to avoid creating the exception in the first place.
    try
      {
        return getGlobalFocusedWindow();
      }
    catch (SecurityException e)
      {
        return null;
      }
  }

  protected Window getGlobalFocusedWindow()
  {
    // XXX Need a way to test if this thread is in the context of the focus
    // owner, and throw a SecurityException if that is the case.
    // XXX Implement.
    return focusedWindow;
  }

  protected void setGlobalFocusedWindow(Window window)
  {
    // XXX Should this send focus events to the windows involved?
    if (window == null || window.focusable)
      {
        firePropertyChange("focusedWindow", focusedWindow, window);
        try
          {
            fireVetoableChange("focusedWindow", focusedWindow, window);
            focusedWindow = window;
          }
        catch (PropertyVetoException e)
          {
          }
      }
  }

  public Window getActiveWindow()
  {
    // XXX Need an easy way to test if this thread is in the context of the
    // global focus owner, to avoid creating the exception in the first place.
    try
      {
        return getGlobalActiveWindow();
      }
    catch (SecurityException e)
      {
        return null;
      }
  }

  protected Window getGlobalActiveWindow()
  {
    // XXX Need a way to test if this thread is in the context of the focus
    // owner, and throw a SecurityException if that is the case.
    // XXX Implement.
    return activeWindow;
  }

  protected void setGlobalActiveWindow(Window window)
  {
    // XXX Should this send focus events to the windows involved?
    firePropertyChange("activeWindow", activeWindow, window);
    try
      {
        fireVetoableChange("activeWindow", activeWindow, window);
        activeWindow = window;
      }
    catch (PropertyVetoException e)
      {
      }
  }

  public FocusTraversalPolicy getDefaultFocusTraversalPolicy()
  {
    if (defaultPolicy == null)
      defaultPolicy = new DefaultFocusTraversalPolicy();
    return defaultPolicy;
  }

  public void setDefaultFocusTraversalPolicy(FocusTraversalPolicy policy)
  {
    if (policy == null)
      throw new IllegalArgumentException();
    firePropertyChange("defaultFocusTraversalPolicy", defaultPolicy, policy);
    defaultPolicy = policy;
  }

  public void setDefaultFocusTraversalKeys(int id, Set keystrokes)
  {
    if (keystrokes == null)
      throw new IllegalArgumentException();
    Set sa;
    Set sb;
    Set sc;
    String type;
    switch (id)
      {
      case FORWARD_TRAVERSAL_KEYS:
        sa = defaultFocusKeys[BACKWARD_TRAVERSAL_KEYS];
        sb = defaultFocusKeys[UP_CYCLE_TRAVERSAL_KEYS];
        sc = defaultFocusKeys[DOWN_CYCLE_TRAVERSAL_KEYS];
        type = "forwardDefaultFocusTraversalKeys";
        break;
      case BACKWARD_TRAVERSAL_KEYS:
        sa = defaultFocusKeys[FORWARD_TRAVERSAL_KEYS];
        sb = defaultFocusKeys[UP_CYCLE_TRAVERSAL_KEYS];
        sc = defaultFocusKeys[DOWN_CYCLE_TRAVERSAL_KEYS];
        type = "backwardDefaultFocusTraversalKeys";
        break;
      case UP_CYCLE_TRAVERSAL_KEYS:
        sa = defaultFocusKeys[FORWARD_TRAVERSAL_KEYS];
        sb = defaultFocusKeys[BACKWARD_TRAVERSAL_KEYS];
        sc = defaultFocusKeys[DOWN_CYCLE_TRAVERSAL_KEYS];
        type = "upCycleDefaultFocusTraversalKeys";
        break;
      case DOWN_CYCLE_TRAVERSAL_KEYS:
        sa = defaultFocusKeys[FORWARD_TRAVERSAL_KEYS];
        sb = defaultFocusKeys[BACKWARD_TRAVERSAL_KEYS];
        sc = defaultFocusKeys[UP_CYCLE_TRAVERSAL_KEYS];
        type = "downCycleDefaultFocusTraversalKeys";
        break;
      default:
        throw new IllegalArgumentException();
      }
    int i = keystrokes.size();
    Iterator iter = keystrokes.iterator();
    while (--i >= 0)
      {
        Object o = iter.next();
        if (! (o instanceof AWTKeyStroke)
            || sa.contains(o) || sb.contains(o) || sc.contains(o)
            || ((AWTKeyStroke) o).keyCode == KeyEvent.VK_UNDEFINED)
          throw new IllegalArgumentException();
      }
    keystrokes = Collections.unmodifiableSet(new HashSet(keystrokes));
    firePropertyChange(type, defaultFocusKeys[id], keystrokes);
    defaultFocusKeys[id] = keystrokes;
  }

  public Set getDefaultFocusTraversalKeys(int id)
  {
    if (id < FORWARD_TRAVERSAL_KEYS || id > DOWN_CYCLE_TRAVERSAL_KEYS)
      throw new IllegalArgumentException();
    return defaultFocusKeys[id];
  }

  public Container getCurrentFocusCycleRoot()
  {
    // XXX Need an easy way to test if this thread is in the context of the
    // global focus owner, to avoid creating the exception in the first place.
    try
      {
        return getGlobalCurrentFocusCycleRoot();
      }
    catch (SecurityException e)
      {
        return null;
      }
  }

  protected Container getGlobalCurrentFocusCycleRoot()
  {
    // XXX Need a way to test if this thread is in the context of the focus
    // owner, and throw a SecurityException if that is the case.
    // XXX Implement.
    return focusCycleRoot;
  }

  protected void setGlobalCurrentFocusCycleRoot(Container cycleRoot)
  {
    firePropertyChange("currentFocusCycleRoot", focusCycleRoot, cycleRoot);
    focusCycleRoot = cycleRoot;
  }

  public void addPropertyChangeListener(PropertyChangeListener l)
  {
    if (l != null)
      propertyChangeSupport.addPropertyChangeListener(l);
  }

  public void removePropertyChangeListener(PropertyChangeListener l)
  {
    if (l != null)
      propertyChangeSupport.removePropertyChangeListener(l);
  }

  public PropertyChangeListener[] getPropertyChangeListeners()
  {
    return propertyChangeSupport.getPropertyChangeListeners();
  }

  public void addPropertyChangeListener(String name, PropertyChangeListener l)
  {
    if (l != null)
      propertyChangeSupport.addPropertyChangeListener(name, l);
  }

  public void removePropertyChangeListener(String name,
                                           PropertyChangeListener l)
  {
    if (l != null)
      propertyChangeSupport.removePropertyChangeListener(name, l);
  }

  public PropertyChangeListener[] getPropertyChangeListeners(String name)
  {
    return propertyChangeSupport.getPropertyChangeListeners(name);
  }

  protected void firePropertyChange(String name, Object o, Object n)
  {
    propertyChangeSupport.firePropertyChange(name, o, n);
  }

  public void addVetoableChangeListener(VetoableChangeListener l)
  {
    if (l != null)
      vetoableChangeSupport.addVetoableChangeListener(l);
  }

  public void removeVetoableChangeListener(VetoableChangeListener l)
  {
    if (l != null)
      vetoableChangeSupport.removeVetoableChangeListener(l);
  }

  public VetoableChangeListener[] getVetoableChangeListeners()
  {
    return vetoableChangeSupport.getVetoableChangeListeners();
  }

  public void addVetoableChangeListener(String name, VetoableChangeListener l)
  {
    if (l != null)
      vetoableChangeSupport.addVetoableChangeListener(name, l);
  }

  public void removeVetoableChangeListener(String name,
                                           VetoableChangeListener l)
  {
    if (l != null)
      vetoableChangeSupport.removeVetoableChangeListener(name, l);
  }

  public VetoableChangeListener[] getVetoableChangeListeners(String name)
  {
    return vetoableChangeSupport.getVetoableChangeListeners(name);
  }

  protected void fireVetoableChange(String name, Object o, Object n)
    throws PropertyVetoException
  {
    vetoableChangeSupport.fireVetoableChange(name, o, n);
  }

  public void addKeyEventDispatcher(KeyEventDispatcher dispatcher)
  {
    if (dispatcher != null)
      keyEventDispatchers.add(dispatcher);
  }

  public void removeKeyEventDispatcher(KeyEventDispatcher dispatcher)
  {
    keyEventDispatchers.remove(dispatcher);
  }

  protected List getKeyEventDispatchers()
  {
    return (List) keyEventDispatchers.clone();
  }

  public void addKeyEventPostProcessor(KeyEventPostProcessor postProcessor)
  {
    if (postProcessor != null)
      keyEventPostProcessors.add(postProcessor);
  }

  public void removeKeyEventPostProcessor(KeyEventPostProcessor postProcessor)
  {
    keyEventPostProcessors.remove(postProcessor);
  }

  protected List getKeyEventPostProcessors()
  {
    return (List) keyEventPostProcessors.clone();
  }

  public abstract boolean dispatchEvent(AWTEvent e);

  public final void redispatchEvent(Component target, AWTEvent e)
  {
    throw new Error("not implemented");
  }

  public abstract boolean dispatchKeyEvent(KeyEvent e);

  public abstract boolean postProcessKeyEvent(KeyEvent e);

  public abstract void processKeyEvent(Component focused, KeyEvent e);

  protected abstract void enqueueKeyEvents(long after, Component untilFocused);

  protected abstract void dequeueKeyEvents(long after, Component untilFocused);

  protected abstract void discardKeyEvents(Component comp);

  public abstract void focusNextComponent(Component comp);

  public abstract void focusPreviousComponent(Component comp);

  public abstract void upFocusCycle(Component comp);

  public abstract void downFocusCycle(Container cont);

  public final void focusNextComponent()
  {
    focusNextComponent(focusOwner);
  }

  public final void focusPreviousComponent()
  {
    focusPreviousComponent(focusOwner);
  }

  public final void upFocusCycle()
  {
    upFocusCycle(focusOwner);
  }

  public final void downFocusCycle()
  {
    if (focusOwner instanceof Container
        && ((Container) focusOwner).isFocusCycleRoot())
      downFocusCycle((Container) focusOwner);
  }
} // class KeyboardFocusManager
