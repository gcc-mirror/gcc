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
import java.awt.event.FocusEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;
import java.beans.VetoableChangeSupport;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

// FIXME: finish documentation

/**
 *
 * FIXME: discuss applet contexts and thread groups and codebases
 * being insulated.
 *
 * FIXME: discuss where default focus traversal key sets apply
 * (inherited by child Components etc.)
 *
 * @author Eric Blake <ebb9@email.byu.edu>
 * @author Thomas Fitzsimmons <fitzsim@redhat.com>
 * @since 1.4
 * @status partially updated to 1.4, needs documentation.
 */
public abstract class KeyboardFocusManager
  implements KeyEventDispatcher, KeyEventPostProcessor
{
  /** Identifies {@link AWTKeyStroke}s that move the focus forward in
      the focus cycle. */
  public static final int FORWARD_TRAVERSAL_KEYS = 0;

  /** Identifies {@link AWTKeyStroke}s that move the focus backward in
      the focus cycle. */
  public static final int BACKWARD_TRAVERSAL_KEYS = 1;

  /** Identifies {@link AWTKeyStroke}s that move the focus up to the
      parent focus cycle root. */
  public static final int UP_CYCLE_TRAVERSAL_KEYS = 2;

  /** Identifies {@link AWTKeyStroke}s that move the focus down to the
      child focus cycle root. */
  public static final int DOWN_CYCLE_TRAVERSAL_KEYS = 3;

  /** The set of {@link AWTKeyStroke}s that cause focus to be moved to
      the next focusable Component in the focus cycle. */
  private static final Set DEFAULT_FORWARD_KEYS;

  /** The set of {@link AWTKeyStroke}s that cause focus to be moved to
      the previous focusable Component in the focus cycle. */
  private static final Set DEFAULT_BACKWARD_KEYS;

  /** Populate the DEFAULT_FORWARD_KEYS and DEFAULT_BACKWARD_KEYS
      {@link java.util.Set}s. */
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

  /** The global object {@link java.util.Map}s. */

  /** For security reasons, {@link java.applet.Applet}s in different
      codebases must be insulated from one another.  Since {@link
      KeyboardFocusManager}s have the ability to return {@link
      Component}s from a given {@link java.applet.Applet}, each
      codebase must have an independent {@link KeyboardFocusManager}.
      Since each codebase has its own {@link ThreadGroup} in which its
      {@link Applet}s run, it makes sense to partition {@link
      KeyboardFocusManager}s according to {@link
      java.lang.ThreadGroup}.  Thus, currentKeyboardFocusManagers is a
      {@link java.util.Map} keyed on {@link java.lang.ThreadGroup}. */
  private static Map currentKeyboardFocusManagers = new HashMap ();

  /** {@link java.applet.Applet}s in one codebase must not be allowed
      to access {@link Component}s in {@link java.applet.Applet}s in
      other codebases.  To enforce this restriction, we key the
      following {@link java.util.Map}s on {@link java.lang.ThreadGroup}s (which
      are per-codebase).  For example, if {@link
      java.lang.ThreadGroup} A calls {@link #setGlobalFocusOwner},
      passing {@link Component} C, currentFocusOwners[A] is assigned
      C, and all other currentFocusOwners values are nullified.  Then
      if {@link java.lang.ThreadGroup} A subsequently calls {@link
      #getGlobalFocusOwner}, it will return currentFocusOwners[A],
      that is, {@link Component} C.  If another {@link
      java.lang.ThreadGroup} K calls {@link #getGlobalFocusOwner}, it
      will return currentFocusOwners[K], that is, null.

      Since this is a static field, we ensure that there is only one
      focused {@link Component} per class loader. */
  private static Map currentFocusOwners = new HashMap ();

  /** A {@link java.util.Map} keyed on {@link java.lang.ThreadGroup}s
      that stores the {@link Component} that owns the permanent
      keyboard focus. @see currentFocusOwners */
  private static Map currentPermanentFocusOwners = new HashMap ();

  /** A {@link java.util.Map} keyed on {@link java.lang.ThreadGroup}s
      that stores the focused {@link Window}. @see
      currentFocusOwners */
  private static Map currentFocusedWindows = new HashMap ();

  /** A {@link java.util.Map} keyed on {@link java.lang.ThreadGroup}s
      that stores the active {@link Window}. @see
      currentFocusOwners */
  private static Map currentActiveWindows = new HashMap ();

  /** A {@link java.util.Map} keyed on {@link java.lang.ThreadGroup}s
      that stores the focus cycle root {@link Container}. @see
      currentFocusOwners */
  private static Map currentFocusCycleRoots = new HashMap ();

  /** The default {@link FocusTraveralPolicy} that focus-managing
      {@link Container}s will use to define their initial focus
      traversal policy. */
  private FocusTraversalPolicy defaultPolicy;

  /** An array that stores the {@link #FORWARD_TRAVERSAL_KEYS}, {@link
      #BACKWARD_TRAVERSAL_KEYS}, {@link #UP_CYCLE_TRAVERSAL_KEYS} and
      {@link #DOWN_CYCLE_TRAVERSAL_KEYS} {@link AWTKeyStroke}s {@link
      java.util.Set}s. */
  private Set[] defaultFocusKeys = new Set[]
  {
    DEFAULT_FORWARD_KEYS, DEFAULT_BACKWARD_KEYS,
    Collections.EMPTY_SET, Collections.EMPTY_SET
  };

  private final PropertyChangeSupport propertyChangeSupport = new PropertyChangeSupport (this);
  private final VetoableChangeSupport vetoableChangeSupport = new VetoableChangeSupport (this);

  /** A list of {@link KeyEventDispatcher}s that process {@link
      KeyEvent}s before they are processed the default keyboard focus
      manager. */
  private final ArrayList keyEventDispatchers = new ArrayList();

  /** A list of {@link KeyEventPostProcessor}s that process unconsumed
      {@link KeyEvent}s. */
  private final ArrayList keyEventPostProcessors = new ArrayList();

  /**
   * Construct a KeyboardFocusManager.
   */
  public KeyboardFocusManager ()
  {
  }

  /**
   * Retrieve the keyboard focus manager associated with the {@link
   * java.lang.ThreadGroup} to which the calling thread belongs.
   *
   * @return the keyboard focus manager associated with the current
   * thread group
   */
  public static KeyboardFocusManager getCurrentKeyboardFocusManager ()
  {
    ThreadGroup currentGroup = Thread.currentThread ().getThreadGroup ();
    return (KeyboardFocusManager) currentKeyboardFocusManagers.get (currentGroup);
  }

  /**
   * Set the keyboard focus manager associated with the {@link
   * java.lang.ThreadGroup} to which the calling thread belongs.
   *
   * @param m the keyboard focus manager for the current thread group
   */
  public static void setCurrentKeyboardFocusManager (KeyboardFocusManager m)
  {
    SecurityManager sm = System.getSecurityManager ();
    if (sm != null)
      sm.checkPermission (new AWTPermission ("replaceKeyboardFocusManager"));

    ThreadGroup currentGroup = Thread.currentThread ().getThreadGroup ();
    KeyboardFocusManager manager;

    if (m == null)
      manager = new DefaultKeyboardFocusManager ();
    else
      manager = m;

    currentKeyboardFocusManagers.put (currentGroup, manager);
  }

  /**
   * Retrieve the {@link Component} that has the keyboard focus, or
   * null if the focus owner was not set by a thread in the current
   * {@link java.lang.ThreadGroup}.
   *
   * @return the keyboard focus owner or null
   */
  public Component getFocusOwner ()
  {
    Component owner = (Component) getObject (currentFocusOwners);
    if (owner == null)
      owner = (Component) getObject (currentPermanentFocusOwners);
    return owner;
  }

  /**
   * Retrieve the {@link Component} that has the keyboard focus,
   * regardless of whether or not it was set by a thread in the
   * current {@link java.lang.ThreadGroup}.  If there is no temporary
   * focus owner in effect then this method will return the same value
   * as {@link #getGlobalPermanentFocusOwner}.
   *
   * @return the keyboard focus owner
   * @throws SecurityException if this is not the keyboard focus
   * manager associated with the current {@link java.lang.ThreadGroup}
   */
  protected Component getGlobalFocusOwner ()
  {
    // Check if there is a temporary focus owner.
    Component focusOwner = (Component) getGlobalObject (currentFocusOwners);

    return (focusOwner == null) ? getGlobalPermanentFocusOwner () : focusOwner;
  }

  /**
   * Set the {@link Component} that will be returned by {@link
   * #getFocusOwner} (when it is called from the current {@link
   * java.lang.ThreadGroup}) and {@link #getGlobalFocusOwner}.  This
   * method does not actually transfer the keyboard focus.
   *
   * @param owner the Component to return from getFocusOwner and
   * getGlobalFocusOwner
   *
   * @see Component.requestFocus ()
   * @see Component.requestFocusInWindow ()
   */
  protected void setGlobalFocusOwner (Component owner)
  {
    if (owner == null || owner.focusable)
      setGlobalObject (currentFocusOwners, owner, "focusOwner");
  }

  /**
   * Clear the global focus owner and deliver a FOCUS_LOST event to
   * the previously-focused {@link Component}.  Until another {@link
   * Component} becomes the keyboard focus owner, key events will be
   * discarded by top-level windows.
   */
  public void clearGlobalFocusOwner ()
  {
    synchronized (currentFocusOwners)
      {
        Component focusOwner = getGlobalFocusOwner ();
        Component permanentFocusOwner = getGlobalPermanentFocusOwner ();

        setGlobalFocusOwner (null);
        setGlobalPermanentFocusOwner (null);

        // Inform the old focus owner that it has lost permanent
        // focus.
        if (focusOwner != null)
          {
            // We can't cache the event queue, because of
            // bootstrapping issues.  We need to set the default
            // KeyboardFocusManager in EventQueue before the event
            // queue is started.
            EventQueue q = Toolkit.getDefaultToolkit ().getSystemEventQueue ();
            if (focusOwner != permanentFocusOwner)
              q.postEvent (new FocusEvent (focusOwner, FocusEvent.FOCUS_LOST, true));
            else
              q.postEvent (new FocusEvent (focusOwner, FocusEvent.FOCUS_LOST, false));
          }

        if (focusOwner != permanentFocusOwner)
          {
            EventQueue q = Toolkit.getDefaultToolkit ().getSystemEventQueue ();
            q.postEvent (new FocusEvent (permanentFocusOwner, FocusEvent.FOCUS_LOST, false));
          }
      }
  }

  /**
   * Retrieve the {@link Component} that has the permanent keyboard
   * focus, or null if the focus owner was not set by a thread in the
   * current {@link java.lang.ThreadGroup}.
   *
   * @return the keyboard focus owner or null
   */
  public Component getPermanentFocusOwner ()
  {
    return (Component) getObject (currentPermanentFocusOwners);
  }

  /**
   * Retrieve the {@link Component} that has the permanent keyboard
   * focus, regardless of whether or not it was set by a thread in the
   * current {@link java.lang.ThreadGroup}.
   *
   * @return the keyboard focus owner
   * @throws SecurityException if this is not the keyboard focus
   * manager associated with the current {@link java.lang.ThreadGroup}
   */
  protected Component getGlobalPermanentFocusOwner ()
  {
    return (Component) getGlobalObject (currentPermanentFocusOwners);
  }

  /**
   * Set the {@link Component} that will be returned by {@link
   * #getPermanentFocusOwner} (when it is called from the current
   * {@link java.lang.ThreadGroup}) and {@link
   * #getGlobalPermanentFocusOwner}.  This method does not actually
   * transfer the keyboard focus.
   *
   * @param focusOwner the Component to return from
   * getPermanentFocusOwner and getGlobalPermanentFocusOwner
   *
   * @see Component.requestFocus ()
   * @see Component.requestFocusInWindow ()
   */
  protected void setGlobalPermanentFocusOwner (Component focusOwner)
  {
    if (focusOwner == null || focusOwner.focusable)
      setGlobalObject (currentPermanentFocusOwners, focusOwner,
		       "permanentFocusOwner");
  }

  /**
   * Retrieve the {@link Window} that is or contains the keyboard
   * focus owner, or null if the focused window was not set by a
   * thread in the current {@link java.lang.ThreadGroup}.
   *
   * @return the focused window or null
   */
  public Window getFocusedWindow ()
  {
    return (Window) getObject (currentFocusedWindows);
  }

  /**
   * Retrieve the {@link Window} that is or contains the focus owner,
   * regardless of whether or not the {@link Window} was set focused
   * by a thread in the current {@link java.lang.ThreadGroup}.
   *
   * @return the focused window
   * @throws SecurityException if this is not the keyboard focus
   * manager associated with the current {@link java.lang.ThreadGroup}
   */
  protected Window getGlobalFocusedWindow ()
  {
    return (Window) getGlobalObject (currentFocusedWindows);
  }

  /**
   * Set the {@link Window} that will be returned by {@link
   * #getFocusedWindow} (when it is called from the current {@link
   * java.lang.ThreadGroup}) and {@link #getGlobalFocusedWindow}.
   * This method does not actually cause <code>window</code> to become
   * the focused {@link Window}.
   *
   * @param window the Window to return from getFocusedWindow and
   * getGlobalFocusedWindow
   */
  protected void setGlobalFocusedWindow (Window window)
  {
    if (window == null || window.focusable)
      setGlobalObject (currentFocusedWindows, window, "focusedWindow");
  }

  /**
   * Retrieve the active {@link Window}, or null if the active window
   * was not set by a thread in the current {@link
   * java.lang.ThreadGroup}.
   *
   * @return the active window or null
   */
  public Window getActiveWindow()
  {
    return (Window) getObject (currentActiveWindows);
  }

  /**
   * Retrieve the active {@link Window}, regardless of whether or not
   * the {@link Window} was made active by a thread in the current
   * {@link java.lang.ThreadGroup}.
   *
   * @return the active window
   * @throws SecurityException if this is not the keyboard focus
   * manager associated with the current {@link java.lang.ThreadGroup}
   */
  protected Window getGlobalActiveWindow()
  {
    return (Window) getGlobalObject (currentActiveWindows);
  }

  /**
   * Set the {@link Window} that will be returned by {@link
   * #getActiveWindow} (when it is called from the current {@link
   * java.lang.ThreadGroup}) and {@link #getGlobalActiveWindow}.  This
   * method does not actually cause <code>window</code> to be made
   * active.
   *
   * @param window the Window to return from getActiveWindow and
   * getGlobalActiveWindow
   */
  protected void setGlobalActiveWindow(Window window)
  {
    setGlobalObject (currentActiveWindows, window, "activeWindow");
  }

  /**
   * Retrieve the default {@link FocusTraversalPolicy}.
   * Focus-managing {@link Container}s use the returned object to
   * define their initial focus traversal policy.
   *
   * @return a non-null default FocusTraversalPolicy object
   */
  public FocusTraversalPolicy getDefaultFocusTraversalPolicy ()
  {
    if (defaultPolicy == null)
      defaultPolicy = new DefaultFocusTraversalPolicy ();
    return defaultPolicy;
  }

  /**
   * Set the {@link FocusTraversalPolicy} returned by {@link
   * #getDefaultFocusTraversalPolicy}.  Focus-managing {@link
   * Container}s created after this call will use policy as their
   * initial focus traversal policy.  Existing {@link Container}s'
   * focus traversal policies will not be affected by calls to this
   * method.
   *
   * @param policy the FocusTraversalPolicy that will be returned by
   * subsequent calls to getDefaultFocusTraversalPolicy
   * @throws IllegalArgumentException if policy is null
   */
  public void setDefaultFocusTraversalPolicy (FocusTraversalPolicy policy)
  {
    if (policy == null)
      throw new IllegalArgumentException ();
    firePropertyChange ("defaultFocusTraversalPolicy", defaultPolicy, policy);
    defaultPolicy = policy;
  }

  /**
   * Set the default {@link java.util.Set} of focus traversal keys for
   * one of the focus traversal directions.
   *
   * @param id focus traversal direction identifier
   * @param keystrokes set of AWTKeyStrokes
   *
   * @see #FORWARD_TRAVERSAL_KEYS
   * @see #BACKWARD_TRAVERSAL_KEYS
   * @see #UP_CYCLE_TRAVERSAL_KEYS
   * @see #DOWN_CYCLE_TRAVERSAL_KEYS
   */
  public void setDefaultFocusTraversalKeys (int id, Set keystrokes)
  {
    if (id != KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS &&
        id != KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS &&
        id != KeyboardFocusManager.UP_CYCLE_TRAVERSAL_KEYS &&
        id != KeyboardFocusManager.DOWN_CYCLE_TRAVERSAL_KEYS)
      throw new IllegalArgumentException ();

    if (keystrokes == null)
      throw new IllegalArgumentException ();

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
        throw new IllegalArgumentException ();
      }
    int i = keystrokes.size ();
    Iterator iter = keystrokes.iterator ();
    while (--i >= 0)
      {
        Object o = iter.next ();
        if (!(o instanceof AWTKeyStroke)
            || sa.contains (o) || sb.contains (o) || sc.contains (o)
            || ((AWTKeyStroke) o).keyCode == KeyEvent.VK_UNDEFINED)
          throw new IllegalArgumentException ();
      }
    keystrokes = Collections.unmodifiableSet (new HashSet (keystrokes));
    firePropertyChange (type, defaultFocusKeys[id], keystrokes);
    defaultFocusKeys[id] = keystrokes;
  }

  /**
   * Retrieve the default {@link java.util.Set} of focus traversal
   * keys for one of the focus traversal directions.
   *
   * @param id focus traversal direction identifier
   *
   * @return the default set of AWTKeyStrokes
   *
   * @see #FORWARD_TRAVERSAL_KEYS
   * @see #BACKWARD_TRAVERSAL_KEYS
   * @see #UP_CYCLE_TRAVERSAL_KEYS
   * @see #DOWN_CYCLE_TRAVERSAL_KEYS
   */
  public Set getDefaultFocusTraversalKeys (int id)
  {
    if (id < FORWARD_TRAVERSAL_KEYS || id > DOWN_CYCLE_TRAVERSAL_KEYS)
      throw new IllegalArgumentException ();
    return defaultFocusKeys[id];
  }

  /**
   * Retrieve the current focus cycle root, or null if the focus owner
   * was not set by a thread in the current {@link
   * java.lang.ThreadGroup}.
   *
   * @return the current focus cycle root or null
   */
  public Container getCurrentFocusCycleRoot ()
  {
    return (Container) getObject (currentFocusCycleRoots);
  }

  /**
   * Retrieve the current focus cycle root, regardless of whether or
   * not it was made set by a thread in the current {@link
   * java.lang.ThreadGroup}.
   *
   * @return the current focus cycle root
   * @throws SecurityException if this is not the keyboard focus
   * manager associated with the current {@link java.lang.ThreadGroup}
   */
  protected Container getGlobalCurrentFocusCycleRoot ()
  {
    return (Container) getGlobalObject (currentFocusCycleRoots);
  }

  /**
   * Set the {@link Container} that will be returned by {@link
   * #getCurrentFocusCycleRoot} (when it is called from the current
   * {@link java.lang.ThreadGroup}) and {@link
   * #getGlobalCurrentFocusCycleRoot}.  This method does not actually
   * make <code>cycleRoot</code> the current focus cycle root.
   * 
   * @param cycleRoot the focus cycle root to return from
   * getCurrentFocusCycleRoot and getGlobalCurrentFocusCycleRoot
   */
  public void setGlobalCurrentFocusCycleRoot (Container cycleRoot)
  {
    setGlobalObject (currentFocusCycleRoots, cycleRoot, "currentFocusCycleRoot");
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

  protected List getKeyEventDispatchers ()
  {
    return (List) keyEventDispatchers.clone ();
  }

  public void addKeyEventPostProcessor (KeyEventPostProcessor postProcessor)
  {
    if (postProcessor != null)
      keyEventPostProcessors.add (postProcessor);
  }

  public void removeKeyEventPostProcessor (KeyEventPostProcessor postProcessor)
  {
    keyEventPostProcessors.remove (postProcessor);
  }

  protected List getKeyEventPostProcessors ()
  {
    return (List) keyEventPostProcessors.clone ();
  }

  public abstract boolean dispatchEvent (AWTEvent e);

  public final void redispatchEvent (Component target, AWTEvent e)
  {
    e.setSource (target);
    dispatchEvent (e);
  }

  public abstract boolean dispatchKeyEvent (KeyEvent e);

  public abstract boolean postProcessKeyEvent (KeyEvent e);

  public abstract void processKeyEvent (Component focused, KeyEvent e);

  protected abstract void enqueueKeyEvents (long after, Component untilFocused);

  protected abstract void dequeueKeyEvents (long after, Component untilFocused);

  protected abstract void discardKeyEvents (Component comp);

  public abstract void focusNextComponent (Component comp);

  public abstract void focusPreviousComponent (Component comp);

  public abstract void upFocusCycle (Component comp);

  public abstract void downFocusCycle (Container cont);

  public final void focusNextComponent ()
  {
    focusNextComponent (null);
  }

  public final void focusPreviousComponent ()
  {
    focusPreviousComponent (null);
  }

  public final void upFocusCycle ()
  {
    upFocusCycle (null);
  }

  public final void downFocusCycle ()
  {
    Component focusOwner = getGlobalFocusOwner ();
    if (focusOwner instanceof Container
        && ((Container) focusOwner).isFocusCycleRoot ())
      downFocusCycle ((Container) focusOwner);
  }

  /**
   * Retrieve an object from one of the global object {@link
   * java.util.Map}s, if the object was set by the a thread in the
   * current {@link java.lang.ThreadGroup}.  Otherwise, return null.
   *
   * @param globalMap one of the global object Maps
   *
   * @return a global object set by the current ThreadGroup, or null
   *
   * @see getFocusOwner
   * @see getPermanentFocusOwner
   * @see getFocusedWindow
   * @see getActiveWindow
   * @see getCurrentFocusCycleRoot
   */
  private Object getObject (Map globalMap)
  {
    ThreadGroup currentGroup = Thread.currentThread ().getThreadGroup ();
    return globalMap.get (currentGroup);
  }

  /**
   * Retrieve an object from one of the global object {@link
   * java.util.Map}s, regardless of whether or not the object was set
   * by a thread in the current {@link java.lang.ThreadGroup}.
   *
   * @param globalMap one of the global object Maps
   *
   * @return a global object set by the current ThreadGroup, or null
   *
   * @throws SecurityException if this is not the keyboard focus
   * manager associated with the current {@link java.lang.ThreadGroup}
   *
   * @see getGlobalFocusOwner
   * @see getGlobalPermanentFocusOwner
   * @see getGlobalFocusedWindow
   * @see getGlobalActiveWindow
   * @see getGlobalCurrentFocusCycleRoot
   */
  private Object getGlobalObject (Map globalMap)
  {
    ThreadGroup currentGroup = Thread.currentThread ().getThreadGroup ();
    KeyboardFocusManager managerForCallingThread
      = (KeyboardFocusManager) currentKeyboardFocusManagers.get (currentGroup);

    if (this != managerForCallingThread)
      throw new SecurityException ("Attempted to retrieve an object from a "
                                   + "keyboard focus manager that isn't "
                                   + "associated with the current thread group.");

    synchronized (globalMap)
      {
        Collection globalObjects = globalMap.values ();
        Iterator i = globalObjects.iterator ();
        Component globalObject;

        while (i.hasNext ())
          {
            globalObject = (Component) i.next ();
            if (globalObject != null)
              return globalObject;
          }
      }

    // No Object was found.
    return null;
  }

  /**
   * Set an object in one of the global object {@link java.util.Map}s,
   * that will be returned by subsequent calls to getGlobalObject on
   * the same {@link java.util.Map}.
   *
   * @param globalMap one of the global object Maps
   * @param newObject the object to set
   * @param property the property that will change
   *
   * @see setGlobalFocusOwner
   * @see setGlobalPermanentFocusOwner
   * @see setGlobalFocusedWindow
   * @see setGlobalActiveWindow
   * @see setGlobalCurrentFocusCycleRoot
   */
  private void setGlobalObject (Map globalMap,
                                Object newObject,
                                String property)
  {
    synchronized (globalMap)
      {
        // Save old object.
        Object oldObject = getGlobalObject (globalMap);

        // Nullify old object.
        Collection threadGroups = globalMap.keySet ();
        Iterator i = threadGroups.iterator ();
        while (i.hasNext ())
          {
            ThreadGroup oldThreadGroup = (ThreadGroup) i.next ();
            if (globalMap.get (oldThreadGroup) != null)
              {
                globalMap.put (oldThreadGroup, null);
                // There should only be one object set at a time, so
                // we can short circuit.
                break;
              }
          }

        ThreadGroup currentGroup = Thread.currentThread ().getThreadGroup ();
        firePropertyChange (property, oldObject, newObject);
        try
          {
            fireVetoableChange (property, oldObject, newObject);
            // Set new object.
            globalMap.put (currentGroup, newObject);
          }
        catch (PropertyVetoException e)
          {
          }
      }
  }
}
