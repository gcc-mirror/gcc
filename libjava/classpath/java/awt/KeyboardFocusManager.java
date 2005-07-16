/* KeyboardFocusManager.java -- manage component focusing via the keyboard
   Copyright (C) 2002, 2004  Free Software Foundation

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


package java.awt;

import java.applet.Applet;
import java.awt.event.FocusEvent;
import java.awt.event.KeyEvent;
import java.awt.event.WindowEvent;
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

/**
 * The <code>KeyboardFocusManager</code> handles the focusing of
 * windows for receiving keyboard events.  The manager handles
 * the dispatch of all <code>FocusEvent</code>s and
 * <code>KeyEvent</code>s, along with <code>WindowEvent</code>s
 * relating to the focused window.  Users can use the manager
 * to ascertain the current focus owner and fire events.
 * <br />
 * <br />
 * The focus owner is the <code>Component</code> that receives
 * key events.  The focus owner is either the currently focused
 * window or a component within this window.
 * <br />
 * <br />
 * The underlying native windowing system may denote the active
 * window or its children with special decorations (e.g. a highlighted
 * title bar).  The active window is always either a <code>Frame</code>
 * or <code>Dialog</code>, and is either the currently focused
 * window or its owner.
 * <br />
 * <br />
 * Applets may be partitioned into different applet contexts, according
 * to their code base.  In this case, each context has its own
 * <code>KeyboardFocusManager</code>, as opposed to the global
 * manager maintained by applets which share the same context.
 * Each context is insulated from the others, and they don't interact.
 * The resulting behaviour, as with context division, depends on the browser
 * supporting the applets.  Regardless, there can only ever be
 * one focused window, one active window and one focus owner
 * per <code>ClassLoader</code>.
 * <br />
 * <br />
 * To support this separation of focus managers, the manager instances
 * and the internal state information is grouped by the
 * <code>ThreadGroup</code> to which it pertains.  With respect to
 * applets, each code base has its own <code>ThreadGroup</code>, so the
 * isolation of each context is enforced within the manager.
 * <br />
 * <br />
 * By default, the manager defines TAB and Ctrl+TAB as the
 * forward focus traversal keys and Shift+TAB and Ctrl+Shift+TAB
 * as the backward focus traversal keys.  No up or down cycle
 * traversal keys are defined by default.  Traversal takes effect
 * on the firing of a relevant <code>KEY_PRESSED</code> event.
 * However, all other key events related to the use of the
 * defined focus traversal key sequence are consumed and not
 * dispatched.
 * <br />
 * <br />
 * These default traversal keys come into effect on all windows
 * for which no alternative set of keys is defined.  This also
 * applies recursively to any child components of such a window,
 * which define no traversal keys of their own.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @author Thomas Fitzsimmons (fitzsim@redhat.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.4
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

  /**
   * A utility class to support the handling of events relating to property changes.
   */
  private final PropertyChangeSupport propertyChangeSupport = new PropertyChangeSupport (this);

  /**
   * A utility class to support the handling of events relating to vetoable changes.
   */
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

    if (currentKeyboardFocusManagers.get (currentGroup) == null)
      setCurrentKeyboardFocusManager (null);

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
      manager = createFocusManager();
    else
      manager = m;

    currentKeyboardFocusManagers.put (currentGroup, manager);
  }

  /**
   * Creates a KeyboardFocusManager. The exact class is determined by the
   * system property 'gnu.java.awt.FocusManager'. If this is not set,
   * we default to DefaultKeyboardFocusManager.
   */
  private static KeyboardFocusManager createFocusManager()
  {
    String fmClassName = System.getProperty("gnu.java.awt.FocusManager",
                                       "java.awt.DefaultKeyboardFocusManager");
    try
      {
        Class fmClass = Class.forName(fmClassName);
        KeyboardFocusManager fm = (KeyboardFocusManager) fmClass.newInstance();
        return fm;
      }
    catch (ClassNotFoundException ex)
      {
        System.err.println("The class " + fmClassName + " cannot be found.");
        System.err.println("Check the setting of the system property");
        System.err.println("gnu.java.awt.FocusManager");
        return null;
      }
    catch (InstantiationException ex)
      {
        System.err.println("The class " + fmClassName + " cannot be");
        System.err.println("instantiated.");
        System.err.println("Check the setting of the system property");
        System.err.println("gnu.java.awt.FocusManager");
        return null;
      }
    catch (IllegalAccessException ex)
      {
        System.err.println("The class " + fmClassName + " cannot be");
        System.err.println("accessed.");
        System.err.println("Check the setting of the system property");
        System.err.println("gnu.java.awt.FocusManager");
        return null;
      }
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
   * @see Component#requestFocus()
   * @see Component#requestFocusInWindow()
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
   * @see Component#requestFocus()
   * @see Component#requestFocusInWindow()
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

  /**
   * Registers the supplied property change listener for receiving
   * events caused by the following property changes:
   *
   * <ul>
   * <li>the current focus owner ("focusOwner")</li>
   * <li>the permanent focus owner ("permanentFocusOwner")</li>
   * <li>the focused window ("focusedWindow")</li>
   * <li>the active window ("activeWindow")</li>
   * <li>the default focus traversal policy ("defaultFocusTraversalPolicy")</li>
   * <li>the default set of forward traversal keys ("forwardDefaultFocusTraversalKeys")</li>
   * <li>the default set of backward traversal keys ("backwardDefaultFocusTraversalKeys")</li>
   * <li>the default set of up cycle traversal keys ("upCycleDefaultFocusTraversalKeys")</li>
   * <li>the default set of down cycle traversal keys ("downCycleDefaultFocusTraversalKeys")</li>
   * <li>the current focus cycle root ("currentFocusCycleRoot")</li>
   * </ul>
   *
   * If the supplied listener is null, nothing occurs.
   *
   * @param l the new listener to register.
   * @see KeyboardFocusManager#addPropertyChangeListener(String, java.beans.PropertyChangeListener)
   */
  public void addPropertyChangeListener(PropertyChangeListener l)
  {
    if (l != null)
      propertyChangeSupport.addPropertyChangeListener(l);
  }

  /**
   * Removes the supplied property change listener from the list
   * of registered listeners.  If the supplied listener is null,
   * nothing occurs.
   *
   * @param l the listener to remove.
   */
  public void removePropertyChangeListener(PropertyChangeListener l)
  {
    if (l != null)
      propertyChangeSupport.removePropertyChangeListener(l);
  }

  /**
   * Returns the currently registered property change listeners
   * in array form.  The returned array is empty if no listeners are
   * currently registered.
   *
   * @return an array of registered property change listeners.
   */
  public PropertyChangeListener[] getPropertyChangeListeners()
  {
    return propertyChangeSupport.getPropertyChangeListeners();
  }

  /**
   * Registers a property change listener for receiving events relating
   * to a change to a specified property.  The supplied property name can be
   * either user-defined or one from the following list of properties
   * relevant to this class:
   *
   * <ul>
   * <li>the current focus owner ("focusOwner")</li>
   * <li>the permanent focus owner ("permanentFocusOwner")</li>
   * <li>the focused window ("focusedWindow")</li>
   * <li>the active window ("activeWindow")</li>
   * <li>the default focus traversal policy ("defaultFocusTraversalPolicy")</li>
   * <li>the default set of forward traversal keys ("forwardDefaultFocusTraversalKeys")</li>
   * <li>the default set of backward traversal keys ("backwardDefaultFocusTraversalKeys")</li>
   * <li>the default set of up cycle traversal keys ("upCycleDefaultFocusTraversalKeys")</li>
   * <li>the default set of down cycle traversal keys ("downCycleDefaultFocusTraversalKeys")</li>
   * <li>the current focus cycle root ("currentFocusCycleRoot")</li>
   * </ul>
   *
   * Nothing occurs if a null listener is supplied.  null is regarded as a valid property name.
   *
   * @param name the name of the property to handle change events for.
   * @param l the listener to register for changes to the specified property. 
   * @see KeyboardFocusManager#addPropertyChangeListener(java.beans.PropertyChangeListener)
   */
  public void addPropertyChangeListener(String name, PropertyChangeListener l)
  {
    if (l != null)
      propertyChangeSupport.addPropertyChangeListener(name, l);
  }

  /**
   * Removes the supplied property change listener registered for the
   * specified property from the list of registered listeners.  If the
   * supplied listener is null, nothing occurs.
   *
   * @param name the name of the property the listener is
   *        monitoring changes to.
   * @param l the listener to remove.
   */
  public void removePropertyChangeListener(String name,
                                           PropertyChangeListener l)
  {
    if (l != null)
      propertyChangeSupport.removePropertyChangeListener(name, l);
  }

  /**
   * Returns the currently registered property change listeners
   * in array form, which listen for changes to the supplied property.
   * The returned array is empty, if no listeners are currently registered
   * for events pertaining to the supplied property.
   *
   * @param name The property the returned listeners monitor for changes.
   * @return an array of registered property change listeners which
   *         listen for changes to the supplied property.
   */
  public PropertyChangeListener[] getPropertyChangeListeners(String name)
  {
    return propertyChangeSupport.getPropertyChangeListeners(name);
  }

  /**
   * Fires a property change event as a response to a change to
   * to the specified property.  The event is only fired if a
   * change has actually occurred (i.e. o and n are different).
   *
   * @param name The name of the property to which a change occurred.
   * @param o The old value of the property.
   * @param n The new value of the property.
   */
  protected void firePropertyChange(String name, Object o, Object n)
  {
    propertyChangeSupport.firePropertyChange(name, o, n);
  }

  /**
   * Registers a vetoable property change listener for receiving events
   * relating to the following properties:
   *
   * <ul>
   * <li>the current focus owner ("focusOwner")</li>
   * <li>the permanent focus owner ("permanentFocusOwner")</li>
   * <li>the focused window ("focusedWindow")</li>
   * <li>the active window ("activeWindow")</li>
   * </ul>
   *
   * Nothing occurs if a null listener is supplied.
   *
   * @param l the listener to register. 
   * @see KeyboardFocusManager#addVetoableChangeListener(String, java.beans.VetoableChangeListener)
   */
  public void addVetoableChangeListener(VetoableChangeListener l)
  {
    if (l != null)
      vetoableChangeSupport.addVetoableChangeListener(l);
  }

  /**
   * Removes the supplied vetoable property change listener from
   * the list of registered listeners.  If the supplied listener
   * is null, nothing occurs.
   *
   * @param l the listener to remove.
   */
  public void removeVetoableChangeListener(VetoableChangeListener l)
  {
    if (l != null)
      vetoableChangeSupport.removeVetoableChangeListener(l);
  }

  /**
   * Returns the currently registered vetoable property change listeners
   * in array form.  The returned array is empty if no listeners are
   * currently registered.
   *
   * @return an array of registered vetoable property change listeners.
   * @since 1.4
   */
  public VetoableChangeListener[] getVetoableChangeListeners()
  {
    return vetoableChangeSupport.getVetoableChangeListeners();
  }

  /**
   * Registers a vetoable property change listener for receiving events relating
   * to a vetoable change to a specified property.  The supplied property name can be
   * either user-defined or one from the following list of properties
   * relevant to this class:
   *
   * <ul>
   * <li>the current focus owner ("focusOwner")</li>
   * <li>the permanent focus owner ("permanentFocusOwner")</li>
   * <li>the focused window ("focusedWindow")</li>
   * <li>the active window ("activeWindow")</li>
   * </ul>
   *
   * Nothing occurs if a null listener is supplied.  null is regarded as a valid property name.
   *
   * @param name the name of the property to handle change events for.
   * @param l the listener to register for changes to the specified property. 
   * @see KeyboardFocusManager#addVetoableChangeListener(java.beans.VetoableChangeListener)
   */
  public void addVetoableChangeListener(String name, VetoableChangeListener l)
  {
    if (l != null)
      vetoableChangeSupport.addVetoableChangeListener(name, l);
  }

  /**
   * Removes the supplied vetoable property change listener registered
   * for the specified property from the list of registered listeners.
   * If the supplied listener is null, nothing occurs.
   *
   * @param name the name of the vetoable property the listener is
   *        monitoring changes to.
   * @param l the listener to remove.
   */
  public void removeVetoableChangeListener(String name,
                                           VetoableChangeListener l)
  {
    if (l != null)
      vetoableChangeSupport.removeVetoableChangeListener(name, l);
  }

  /**
   * Returns the currently registered vetoable property change listeners
   * in array form, which listen for changes to the supplied property.
   * The returned array is empty, if no listeners are currently registered
   * for events pertaining to the supplied property.
   *
   * @param name The property the returned listeners monitor for changes.
   * @return an array of registered property change listeners which
   *         listen for changes to the supplied property.
   * @since 1.4
   */
  public VetoableChangeListener[] getVetoableChangeListeners(String name)
  {
    return vetoableChangeSupport.getVetoableChangeListeners(name);
  }

  /**
   * Fires a property change event as a response to a vetoable change to
   * to the specified property.  The event is only fired if a
   * change has actually occurred (i.e. o and n are different).
   * In the event that the property change is vetoed, the following
   * occurs:
   *
   * <ol>
   * <li>
   * This method throws a <code>PropertyVetoException</code> to
   * the proposed change.
   * </li>
   * <li>
   * A new event is fired to reverse the previous change.
   * </li>
   * <li>
   * This method again throws a <code>PropertyVetoException</code>
   * in response to the reversion.
   * </li>
   * </ol>
   *
   * @param name The name of the property to which a change occurred.
   * @param o The old value of the property.
   * @param n The new value of the property.
   * @throws PropertyVetoException if one of the listeners vetos
   *         the change by throwing this exception.
   */
  protected void fireVetoableChange(String name, Object o, Object n)
    throws PropertyVetoException
  {
    vetoableChangeSupport.fireVetoableChange(name, o, n);
  }

  /**
   * Adds a key event dispatcher to the list of registered dispatchers.
   * When a key event is fired, each dispatcher's <code>dispatchKeyEvent</code>
   * method is called in the order that they were added, prior to the manager
   * dispatching the event itself.  Notifications halt when one of the
   * dispatchers returns true.
   * <br />
   * <br />
   * The same dispatcher can exist multiple times within the list
   * of registered dispatchers, and there is no limit on the length
   * of this list.  A null dispatcher is simply ignored.
   *
   * @param dispatcher The dispatcher to register.
   */
  public void addKeyEventDispatcher(KeyEventDispatcher dispatcher)
  {
    if (dispatcher != null)
      keyEventDispatchers.add(dispatcher);
  }

  /**
   * Removes the specified key event dispatcher from the list of
   * registered dispatchers.  The manager always dispatches events,
   * regardless of its existence within the list.  The manager
   * can be added and removed from the list, as with any other
   * dispatcher, but this does not affect its ability to dispatch
   * key events.  Non-existent and null dispatchers are simply ignored
   * by this method.
   *
   * @param dispatcher The dispatcher to remove.
   */
  public void removeKeyEventDispatcher(KeyEventDispatcher dispatcher)
  {
    keyEventDispatchers.remove(dispatcher);
  }

  /**
   * Returns the currently registered key event dispatchers in <code>List</code>
   * form.  At present, this only includes dispatchers explicitly registered
   * via the <code>addKeyEventDispatcher()</code> method, but this behaviour
   * is subject to change and should not be depended on.  The manager itself
   * may be a member of the list, but only if explicitly registered.  If no
   * dispatchers have been registered, the list will be empty.
   *
   * @return A list of explicitly registered key event dispatchers.
   * @see KeyboardFocusManager#addKeyEventDispatcher(java.awt.KeyEventDispatcher)
   */
  protected List getKeyEventDispatchers ()
  {
    return (List) keyEventDispatchers.clone ();
  }

  /**
   * Adds a key event post processor to the list of registered post processors.
   * Post processors work in the same way as key event dispatchers, except
   * that they are invoked after the manager has dispatched the key event,
   * and not prior to this.  Each post processor's <code>postProcessKeyEvent</code>
   * method is called to see if any post processing needs to be performed.  THe
   * processors are called in the order in which they were added to the list,
   * and notifications continue until one returns true.  As with key event
   * dispatchers, the manager is implicitly called following this process,
   * regardless of whether or not it is present within the list.
   * <br />
   * <br />
   * The same post processor can exist multiple times within the list
   * of registered post processors, and there is no limit on the length
   * of this list.  A null post processor is simply ignored.
   *
   * @param postProcessor the post processor to register.
   * @see KeyboardFocusManager#addKeyEventDispatcher(java.awt.KeyEventDispatcher)
   */
  public void addKeyEventPostProcessor (KeyEventPostProcessor postProcessor)
  {
    if (postProcessor != null)
      keyEventPostProcessors.add (postProcessor);
  }

  /**
   * Removes the specified key event post processor from the list of
   * registered post processors.  The manager always post processes events,
   * regardless of its existence within the list.  The manager
   * can be added and removed from the list, as with any other
   * post processor, but this does not affect its ability to post process
   * key events.  Non-existent and null post processors are simply ignored
   * by this method.
   *
   * @param postProcessor the post processor to remove.
   */
  public void removeKeyEventPostProcessor (KeyEventPostProcessor postProcessor)
  {
    keyEventPostProcessors.remove (postProcessor);
  }

  /**
   * Returns the currently registered key event post processors in <code>List</code>
   * form.  At present, this only includes post processors explicitly registered
   * via the <code>addKeyEventPostProcessor()</code> method, but this behaviour
   * is subject to change and should not be depended on.  The manager itself
   * may be a member of the list, but only if explicitly registered.  If no
   * post processors have been registered, the list will be empty.
   *
   * @return A list of explicitly registered key event post processors.
   * @see KeyboardFocusManager#addKeyEventPostProcessor(java.awt.KeyEventPostProcessor)
   */
  protected List getKeyEventPostProcessors ()
  {
    return (List) keyEventPostProcessors.clone ();
  }

  /**
   * The AWT event dispatcher uses this method to request that the manager
   * handle a particular event.  If the manager fails or refuses to
   * dispatch the supplied event (this method returns false), the
   * AWT event dispatcher will try to dispatch the event itself.
   * <br />
   * <br />
   * The manager is expected to handle all <code>FocusEvent</code>s
   * and <code>KeyEvent</code>s, and <code>WindowEvent</code>s
   * relating to the focus.  Dispatch is done with regard to the
   * the focus owner and the currently focused and active windows.
   * In handling the event, the source of the event may be overridden.
   * <br />
   * <br />
   * The actual dispatching is performed by calling
   * <code>redispatchEvent()</code>.  This avoids the infinite recursion
   * of dispatch requests which may occur if this method is called on
   * the target component.  
   *
   * @param e the event to dispatch.
   * @return true if the event was dispatched.
   * @see KeyboardFocusManager#redispatchEvent(java.awt.Component, java.awt.AWTEvent)
   * @see KeyEvent
   * @see FocusEvent
   * @see WindowEvent
   */
  public abstract boolean dispatchEvent (AWTEvent e);

  /**
   * Handles redispatching of an event so that recursion of
   * dispatch requests does not occur.  Event dispatch methods
   * within this manager (<code>dispatchEvent()</code>) and
   * the key event dispatchers should use this method to handle
   * dispatching rather than the dispatch method of the target
   * component.  
   * <br />
   * <br />
   * <strong>
   * This method is not intended for general consumption, and is
   * only for the use of the aforementioned classes.
   * </strong>
   * 
   * @param target the target component to which the event is
   *        dispatched.
   * @param e the event to dispatch.
   */
  public final void redispatchEvent (Component target, AWTEvent e)
  {
    synchronized (e)
      {
        e.setSource (target);
        target.dispatchEvent (e);
      }
  }

  /**
   * Attempts to dispatch key events for which no key event dispatcher
   * has so far succeeded.  This method is usually called by
   * <code>dispatchEvent()</code> following the sending of the key
   * event to any registered key event dispatchers.  If the key
   * event reaches this stage, none of the dispatchers returned
   * true.  This is, of course, always the case if there are no
   * registered dispatchers.
   * <br />
   * <br />
   * If this method also fails to handle the key event, then
   * false is returned to the caller.  In the case of
   * <code>dispatchEvent()</code>, the calling method may try
   * to handle the event itself or simply forward on the
   * false result to its caller.  When the event is dispatched
   * by this method, a true result is propogated through the
   * calling methods.
   *
   * @param e the key event to dispatch.
   * @return true if the event was dispatched successfully.
   */
  public abstract boolean dispatchKeyEvent (KeyEvent e);

  /**
   * Handles the post processing of key events.  By default,
   * this method will map unhandled key events to appropriate
   * <code>MenuShortcut</code>s.  The event is consumed
   * in the process and the shortcut is activated.  This
   * method is usually called by <code>dispatchKeyEvent</code>.
   *
   * @param e the key event to post process.
   * @return true by default, as the event was handled.
   */
  public abstract boolean postProcessKeyEvent (KeyEvent e);

  /**
   * Handles focus traversal operations for key events which
   * represent focus traversal keys in relation to the supplied
   * component.  The supplied component is assumed to have the
   * focus, whether it does so or not, and the operation is
   * carried out as appropriate, with this in mind.
   *
   * @param focused the component on which to perform focus traversal,
   *        on the assumption that this component has the focus.
   * @param e the possible focus traversal key event.
   */
  public abstract void processKeyEvent (Component focused, KeyEvent e);

  /**
   * Delays all key events following the specified timestamp until the
   * supplied component has focus.  The AWT calls this method when it is
   * determined that a focus change may occur within the native windowing
   * system.  Any key events which occur following the time specified by
   * after are delayed until a <code>FOCUS_GAINED</code> event is received
   * for the untilFocused component.  The manager is responsible for ensuring
   * this takes place.
   *
   * @param after the timestamp beyond which all key events are delayed until
   *        the supplied component gains focus.
   * @param untilFocused the component to wait on gaining focus.
   */
  protected abstract void enqueueKeyEvents (long after, Component untilFocused);

  /**
   * Removes the key event block specified by the supplied timestamp and component.
   * All delayed key events are released for normal dispatching following its
   * removal and subsequent key events that would have been blocked are now
   * immediately dispatched.  If the specified timestamp is below 0, then
   * the request with the oldest timestamp is removed.
   *
   * @param after the timestamp of the key event block to be removed, or a
   *        value smaller than 0 if the oldest is to be removed.
   * @param untilFocused the component of the key event block to be removed.
   */
  protected abstract void dequeueKeyEvents (long after, Component untilFocused);

  /**
   * Discards all key event blocks relating to focus requirements for
   * the supplied component, regardless of timestamp.
   *
   * @param comp the component of the key event block(s) to be removed.
   */
  protected abstract void discardKeyEvents (Component comp);

  /**
   * Moves the current focus to the next component following
   * comp, based on the current focus traversal policy.  By
   * default, only visible, displayable, accepted components
   * can receive focus.  <code>Canvas</code>es, <code>Panel</code>s,
   * <code>Label</code>s, <code>ScrollPane</code>s, <code>Scrollbar</code>s,
   * <code>Window</code>s and lightweight components are judged
   * to be unacceptable by default.  See the
   * <code>DefaultFocusTraversalPolicy</code> for more details.
   *
   * @param comp the component prior to the one which will
   *        become the focus, following execution of this method.
   * @see DefaultFocusTraversalPolicy
   */
  public abstract void focusNextComponent(Component comp);

  /**
   * Moves the current focus to the previous component, prior to
   * comp, based on the current focus traversal policy.  By
   * default, only visible, displayable, accepted components
   * can receive focus.  <code>Canvas</code>es, <code>Panel</code>s,
   * <code>Label</code>s, <code>ScrollPane</code>s, <code>Scrollbar</code>s,
   * <code>Window</code>s and lightweight components are judged
   * to be unacceptable by default.  See the
   * <code>DefaultFocusTraversalPolicy</code> for more details.
   *
   * @param comp the component following the one which will
   *        become the focus, following execution of this method.
   * @see DefaultFocusTraversalPolicy
   */
  public abstract void focusPreviousComponent(Component comp);

  /**
   * Moves the current focus upwards by one focus cycle.
   * Both the current focus owner and current focus cycle root
   * become the focus cycle root of the supplied component.
   * However, in the case of a <code>Window</code>, the default
   * focus component becomes the focus owner and the focus cycle
   * root is not changed.
   * 
   * @param comp the component used as part of the focus traversal.
   */ 
  public abstract void upFocusCycle(Component comp);

  /**
   * Moves the current focus downwards by one focus cycle.
   * If the supplied container is a focus cycle root, then this
   * becomes the current focus cycle root and the focus goes
   * to the default component of the specified container.
   * Nothing happens for non-focus cycle root containers. 
   * 
   * @param cont the container used as part of the focus traversal.
   */ 
  public abstract void downFocusCycle(Container cont);

  /**
   * Moves the current focus to the next component, based on the
   * current focus traversal policy.  By default, only visible,
   * displayable, accepted component can receive focus.
   * <code>Canvas</code>es, <code>Panel</code>s,
   * <code>Label</code>s, <code>ScrollPane</code>s, <code>Scrollbar</code>s,
   * <code>Window</code>s and lightweight components are judged
   * to be unacceptable by default.  See the
   * <code>DefaultFocusTraversalPolicy</code> for more details.
   *
   * @see DefaultFocusTraversalPolicy
   */
  public final void focusNextComponent()
  {
    focusNextComponent (null);
  }

  /**
   * Moves the current focus to the previous component, based on the
   * current focus traversal policy.  By default, only visible,
   * displayable, accepted component can receive focus.
   * <code>Canvas</code>es, <code>Panel</code>s,
   * <code>Label</code>s, <code>ScrollPane</code>s, <code>Scrollbar</code>s,
   * <code>Window</code>s and lightweight components are judged
   * to be unacceptable by default.  See the
   * <code>DefaultFocusTraversalPolicy</code> for more details.
   *
   * @see DefaultFocusTraversalPolicy
   */
  public final void focusPreviousComponent()
  {
    focusPreviousComponent (null);
  }

  /**
   * Moves the current focus upwards by one focus cycle,
   * so that the new focus owner is the focus cycle root
   * of the current owner.  The current focus cycle root then
   * becomes the focus cycle root of the new focus owner.
   * However, in the case of the focus cycle root of the
   * current focus owner being a <code>Window</code>, the default
   * component of this window becomes the focus owner and the
   * focus cycle root is not changed.
   */
  public final void upFocusCycle()
  {
    upFocusCycle (null);
  }

  /**
   * Moves the current focus downwards by one focus cycle,
   * iff the current focus cycle root is a <code>Container</code>.
   * Usually, the new focus owner is set to the default component
   * of the container and the current focus cycle root is set
   * to the current focus owner.  Nothing occurs if the current
   * focus cycle root is not a container.
   */
  public final void downFocusCycle()
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
