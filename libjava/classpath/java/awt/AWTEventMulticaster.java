/* AWTEventMulticaster.java -- allows multicast chaining of listeners
   Copyright (C) 1999, 2000, 2002 Free Software Foundation

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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.ContainerEvent;
import java.awt.event.ContainerListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.HierarchyBoundsListener;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.awt.event.InputMethodEvent;
import java.awt.event.InputMethodListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.event.TextEvent;
import java.awt.event.TextListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowFocusListener;
import java.awt.event.WindowListener;
import java.awt.event.WindowStateListener;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.EventListener;

/**
 * This class is used to implement a chain of event handlers.  Dispatching
 * using this class is thread safe.  Here is a quick example of how to
 * add and delete listeners using this class.  For this example, we will
 * assume are firing <code>AdjustmentEvent</code>'s.  However, this
 * same approach is useful for all events in the <code>java.awt.event</code>
 * package, and more if this class is subclassed.
 *
 * <p><code>
 * AdjustmentListener al;
 * public void addAdjustmentListener(AdjustmentListener listener)
 * {
 *   al = AWTEventMulticaster.add(al, listener);
 * }
 * public void removeAdjustmentListener(AdjustmentListener listener)
 * {
 *   al = AWTEventMulticaster.remove(al, listener);
 * }
 * </code>
 *
 * <p>When it come time to process an event, simply call <code>al</code>,
 * assuming it is not <code>null</code>, and all listeners in the chain will
 * be fired.
 *
 * <p>The first time <code>add</code> is called it is passed
 * <code>null</code> and <code>listener</code> as its arguments.  This
 * starts building the chain.  This class returns <code>listener</code>
 * which becomes the new <code>al</code>.  The next time, <code>add</code>
 * is called with <code>al</code> and <code>listener</code> and the
 * new listener is then chained to the old.
 *
 * @author Bryce McKinlay
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Eric Blake (ebb9@email.byu.edu)
 * @since 1.1
 * @status updated to 1.4
 */
public class AWTEventMulticaster
  implements ComponentListener, ContainerListener, FocusListener, KeyListener,
             MouseListener, MouseMotionListener, WindowListener,
             WindowFocusListener, WindowStateListener, ActionListener,
             ItemListener, AdjustmentListener, TextListener,
             InputMethodListener, HierarchyListener, HierarchyBoundsListener,
             MouseWheelListener
{
  /**
   * A variable in the event chain.
   */
  protected final EventListener a;

  /**
   * A variable in the event chain.
   */
  protected final EventListener b;

  /**
   * Initializes a new instance of <code>AWTEventMulticaster</code> with
   * the specified event listener parameters. The parameters should not be
   * null, although it is not required to enforce this with a
   * NullPointerException.
   *
   * @param a the "a" listener object
   * @param b the "b" listener object
   */
  protected AWTEventMulticaster(EventListener a, EventListener b)
  {
    this.a = a;
    this.b = b;
  }

  /**
   * Removes one instance of the specified listener from this multicaster
   * chain. This descends recursively if either child is a multicaster, and
   * returns a multicaster chain with the old listener removed.
   *
   * @param oldl the object to remove from this multicaster
   * @return the resulting multicaster with the specified listener removed
   */
  protected EventListener remove(EventListener oldl)
  {
    // If oldl is an immediate child, return the other child.
    if (a == oldl)
      return b;
    if (b == oldl)
      return a;
    // If a and/or b are Multicaster's, search them recursively.
    if (a instanceof AWTEventMulticaster)
      {
        EventListener newa = ((AWTEventMulticaster) a).remove(oldl);
        if (newa != a)
          return new AWTEventMulticaster(newa, b);
      }
    if (b instanceof AWTEventMulticaster)
      {
        EventListener newb = ((AWTEventMulticaster) b).remove(oldl);
        if (newb != b)
          return new AWTEventMulticaster(a, newb);
      }
    // oldl was not found.
    return this;
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   */
  public void componentResized(ComponentEvent e)
  {
    ((ComponentListener) a).componentResized(e);
    ((ComponentListener) b).componentResized(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   */
  public void componentMoved(ComponentEvent e)
  {
    ((ComponentListener) a).componentMoved(e);
    ((ComponentListener) b).componentMoved(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   */
  public void componentShown(ComponentEvent e)
  {
    ((ComponentListener) a).componentShown(e);
    ((ComponentListener) b).componentShown(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   */
  public void componentHidden(ComponentEvent e)
  {
    ((ComponentListener) a).componentHidden(e);
    ((ComponentListener) b).componentHidden(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   */
  public void componentAdded(ContainerEvent e)
  {
    ((ContainerListener) a).componentAdded(e);
    ((ContainerListener) b).componentAdded(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   */
  public void componentRemoved(ContainerEvent e)
  {
    ((ContainerListener) a).componentRemoved(e);
    ((ContainerListener) b).componentRemoved(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   */
  public void focusGained(FocusEvent e)
  {
    ((FocusListener) a).focusGained(e);
    ((FocusListener) b).focusGained(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   */
  public void focusLost(FocusEvent e)
  {
    ((FocusListener) a).focusLost(e);
    ((FocusListener) b).focusLost(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   */
  public void keyTyped(KeyEvent e)
  {
    ((KeyListener) a).keyTyped(e);
    ((KeyListener) b).keyTyped(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   */
  public void keyPressed(KeyEvent e)
  {
    ((KeyListener) a).keyPressed(e);
    ((KeyListener) b).keyPressed(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   */
  public void keyReleased(KeyEvent e)
  {
    ((KeyListener) a).keyReleased(e);
    ((KeyListener) b).keyReleased(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   */
  public void mouseClicked(MouseEvent e)
  {
    ((MouseListener) a).mouseClicked(e);
    ((MouseListener) b).mouseClicked(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   */
  public void mousePressed(MouseEvent e)
  {
    ((MouseListener) a).mousePressed(e);
    ((MouseListener) b).mousePressed(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   */
  public void mouseReleased(MouseEvent e)
  {
    ((MouseListener) a).mouseReleased(e);
    ((MouseListener) b).mouseReleased(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   */
  public void mouseEntered(MouseEvent e)
  {
    ((MouseListener) a).mouseEntered(e);
    ((MouseListener) b).mouseEntered(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   */
  public void mouseExited(MouseEvent e)
  {
    ((MouseListener) a).mouseExited(e);
    ((MouseListener) b).mouseExited(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   */
  public void mouseDragged(MouseEvent e)
  {
    ((MouseMotionListener) a).mouseDragged(e);
    ((MouseMotionListener) b).mouseDragged(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   */
  public void mouseMoved(MouseEvent e)
  {
    ((MouseMotionListener) a).mouseMoved(e);
    ((MouseMotionListener) b).mouseMoved(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   */
  public void windowOpened(WindowEvent e)
  {
    ((WindowListener) a).windowOpened(e);
    ((WindowListener) b).windowOpened(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   */
  public void windowClosing(WindowEvent e)
  {
    ((WindowListener) a).windowClosing(e);
    ((WindowListener) b).windowClosing(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   */
  public void windowClosed(WindowEvent e)
  {
    ((WindowListener) a).windowClosed(e);
    ((WindowListener) b).windowClosed(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   */
  public void windowIconified(WindowEvent e)
  {
    ((WindowListener) a).windowIconified(e);
    ((WindowListener) b).windowIconified(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   */
  public void windowDeiconified(WindowEvent e)
  {
    ((WindowListener) a).windowDeiconified(e);
    ((WindowListener) b).windowDeiconified(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   */
  public void windowActivated(WindowEvent e)
  {
    ((WindowListener) a).windowActivated(e);
    ((WindowListener) b).windowActivated(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   */
  public void windowDeactivated(WindowEvent e)
  {
    ((WindowListener) a).windowDeactivated(e);
    ((WindowListener) b).windowDeactivated(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   * @since 1.4
   */
  public void windowStateChanged(WindowEvent e)
  {
    ((WindowStateListener) a).windowStateChanged(e);
    ((WindowStateListener) b).windowStateChanged(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   * @since 1.4
   */
  public void windowGainedFocus(WindowEvent e)
  {
    ((WindowFocusListener) a).windowGainedFocus(e);
    ((WindowFocusListener) b).windowGainedFocus(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   * @since 1.4
   */
  public void windowLostFocus(WindowEvent e)
  {
    ((WindowFocusListener) a).windowLostFocus(e);
    ((WindowFocusListener) b).windowLostFocus(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   */
  public void actionPerformed(ActionEvent e)
  {
    ((ActionListener) a).actionPerformed(e);
    ((ActionListener) b).actionPerformed(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   */
  public void itemStateChanged(ItemEvent e)
  {
    ((ItemListener) a).itemStateChanged(e);
    ((ItemListener) b).itemStateChanged(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   */
  public void adjustmentValueChanged(AdjustmentEvent e)
  {
    ((AdjustmentListener) a).adjustmentValueChanged(e);
    ((AdjustmentListener) b).adjustmentValueChanged(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   */
  public void textValueChanged(TextEvent e)
  {
    ((TextListener) a).textValueChanged(e);
    ((TextListener) b).textValueChanged(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   * @since 1.2
   */
  public void inputMethodTextChanged(InputMethodEvent e)
  {
    ((InputMethodListener) a).inputMethodTextChanged(e);
    ((InputMethodListener) b).inputMethodTextChanged(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   * @since 1.2
   */
  public void caretPositionChanged(InputMethodEvent e)
  {
    ((InputMethodListener) a).caretPositionChanged(e);
    ((InputMethodListener) b).caretPositionChanged(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   * @since 1.3
   */
  public void hierarchyChanged(HierarchyEvent e)
  {
    ((HierarchyListener) a).hierarchyChanged(e);
    ((HierarchyListener) b).hierarchyChanged(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   * @since 1.3
   */
  public void ancestorMoved(HierarchyEvent e)
  {
    ((HierarchyBoundsListener) a).ancestorMoved(e);
    ((HierarchyBoundsListener) b).ancestorMoved(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   * @since 1.3
   */
  public void ancestorResized(HierarchyEvent e)
  {
    ((HierarchyBoundsListener) a).ancestorResized(e);
    ((HierarchyBoundsListener) b).ancestorResized(e);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param e the event to handle
   * @since 1.4
   */
  public void mouseWheelMoved(MouseWheelEvent e)
  {
    ((MouseWheelListener) a).mouseWheelMoved(e);
    ((MouseWheelListener) b).mouseWheelMoved(e);
  }

  /**
   * Chain <code>ComponentListener</code> a and b.
   *
   * @param a the "a" listener, may be null
   * @param b the "b" listener, may be null
   * @return latest entry in the chain
   */
  public static ComponentListener add(ComponentListener a, ComponentListener b)
  {
    return (ComponentListener) addInternal(a, b);
  }

  /**
   * Chain <code>ContainerListener</code> a and b.
   *
   * @param a the "a" listener, may be null
   * @param b the "b" listener, may be null
   * @return latest entry in the chain
   */
  public static ContainerListener add(ContainerListener a, ContainerListener b)
  {
    return (ContainerListener) addInternal(a, b);
  }

  /**
   * Chain <code>FocusListener</code> a and b.
   *
   * @param a the "a" listener, may be null
   * @param b the "b" listener, may be null
   * @return latest entry in the chain
   */
  public static FocusListener add(FocusListener a, FocusListener b)
  {
    return (FocusListener) addInternal(a, b);
  }

  /**
   * Chain <code>KeyListener</code> a and b.
   *
   * @param a the "a" listener, may be null
   * @param b the "b" listener, may be null
   * @return latest entry in the chain
   */
  public static KeyListener add(KeyListener a, KeyListener b)
  {
    return (KeyListener) addInternal(a, b);
  }

  /**
   * Chain <code>MouseListener</code> a and b.
   *
   * @param a the "a" listener, may be null
   * @param b the "b" listener, may be null
   * @return latest entry in the chain
   */
  public static MouseListener add(MouseListener a, MouseListener b)
  {
    return (MouseListener) addInternal(a, b);
  }

  /**
   * Chain <code>MouseMotionListener</code> a and b.
   *
   * @param a the "a" listener, may be null
   * @param b the "b" listener, may be null
   * @return latest entry in the chain
   */
  public static MouseMotionListener add(MouseMotionListener a,
                                        MouseMotionListener b)
  {
    return (MouseMotionListener) addInternal(a, b);
  }

  /**
   * Chain <code>WindowListener</code> a and b.
   *
   * @param a the "a" listener, may be null
   * @param b the "b" listener, may be null 
   * @return latest entry in the chain
   */
  public static WindowListener add(WindowListener a, WindowListener b)
  {
    return (WindowListener) addInternal(a, b);
  }

  /**
   * Chain <code>WindowStateListener</code> a and b.
   *
   * @param a the "a" listener, may be null
   * @param b the "b" listener, may be null 
   * @return latest entry in the chain
   * @since 1.4
   */
  public static WindowStateListener add(WindowStateListener a,
                                        WindowStateListener b)
  {
    return (WindowStateListener) addInternal(a, b);
  }

  /**
   * Chain <code>WindowFocusListener</code> a and b.
   *
   * @param a the "a" listener, may be null
   * @param b the "b" listener, may be null 
   * @return latest entry in the chain
   * @since 1.4
   */
  public static WindowFocusListener add(WindowFocusListener a,
                                        WindowFocusListener b)
  {
    return (WindowFocusListener) addInternal(a, b);
  }

  /**
   * Chain <code>ActionListener</code> a and b.
   *
   * @param a the "a" listener, may be null
   * @param b the "b" listener, may be null
   * @return latest entry in the chain
   */
  public static ActionListener add(ActionListener a, ActionListener b)
  {
    return (ActionListener) addInternal(a, b);
  }

  /**
   * Chain <code>ItemListener</code> a and b.
   *
   * @param a the "a" listener, may be null
   * @param b the "b" listener, may be null
   * @return latest entry in the chain
   */
  public static ItemListener add(ItemListener a, ItemListener b)
  {
    return (ItemListener) addInternal(a, b);
  }

  /**
   * Chain <code>AdjustmentListener</code> a and b.
   *
   * @param a the "a" listener, may be null
   * @param b the "b" listener, may be null
   * @return latest entry in the chain
   */
  public static AdjustmentListener add(AdjustmentListener a,
                                       AdjustmentListener b)
  {
    return (AdjustmentListener) addInternal(a, b);
  }

  /**
   * Chain <code>AdjustmentListener</code> a and b.
   *
   * @param a the "a" listener, may be null
   * @param b the "b" listener, may be null
   * @return latest entry in the chain
   */
  public static TextListener add(TextListener a, TextListener b)
  {
    return (TextListener) addInternal(a, b);
  }

  /**
   * Chain <code>InputMethodListener</code> a and b.
   *
   * @param a the "a" listener, may be null
   * @param b the "b" listener, may be null
   * @return latest entry in the chain
   * @since 1.2
   */
  public static InputMethodListener add(InputMethodListener a,
                                        InputMethodListener b)
  {
    return (InputMethodListener) addInternal(a, b);
  }

  /**
   * Chain <code>HierarchyListener</code> a and b.
   *
   * @param a the "a" listener, may be null
   * @param b the "b" listener, may be null
   * @return latest entry in the chain
   * @since 1.3
   */
  public static HierarchyListener add(HierarchyListener a, HierarchyListener b)
  {
    return (HierarchyListener) addInternal(a, b);
  }

  /**
   * Chain <code>HierarchyBoundsListener</code> a and b.
   *
   * @param a the "a" listener, may be null
   * @param b the "b" listener, may be null
   * @return latest entry in the chain
   * @since 1.3
   */
  public static HierarchyBoundsListener add(HierarchyBoundsListener a,
                                            HierarchyBoundsListener b)
  {
    return (HierarchyBoundsListener) addInternal(a, b);
  }

  /**
   * Chain <code>MouseWheelListener</code> a and b.
   *
   * @param a the "a" listener, may be null
   * @param b the "b" listener, may be null
   * @return latest entry in the chain
   * @since 1.4
   */
  public static MouseWheelListener add(MouseWheelListener a,
                                       MouseWheelListener b)
  {
    return (MouseWheelListener) addInternal(a, b);
  }

  /**
   * Removes the listener <code>oldl</code> from the listener <code>l</code>.
   *
   * @param l the listener chain to reduce
   * @param oldl the listener to remove
   * @return the resulting listener chain
   */
  public static ComponentListener remove(ComponentListener l,
                                         ComponentListener oldl)
  {
    return (ComponentListener) removeInternal(l, oldl);
  }

  /**
   * Removes the listener <code>oldl</code> from the listener <code>l</code>.
   *
   * @param l the listener chain to reduce
   * @param oldl the listener to remove
   * @return the resulting listener chain
   */
  public static ContainerListener remove(ContainerListener l,
                                         ContainerListener oldl)
  {
    return (ContainerListener) removeInternal(l, oldl);
  }

  /**
   * Removes the listener <code>oldl</code> from the listener <code>l</code>.
   *
   * @param l the listener chain to reduce
   * @param oldl the listener to remove
   * @return the resulting listener chain
   */
  public static FocusListener remove(FocusListener l, FocusListener oldl)
  {
    return (FocusListener) removeInternal(l, oldl);
  }

  /**
   * Removes the listener <code>oldl</code> from the listener <code>l</code>.
   *
   * @param l the listener chain to reduce
   * @param oldl the listener to remove
   * @return the resulting listener chain
   */
  public static KeyListener remove(KeyListener l, KeyListener oldl)
  {
    return (KeyListener) removeInternal(l, oldl);
  }

  /**
   * Removes the listener <code>oldl</code> from the listener <code>l</code>.
   *
   * @param l the listener chain to reduce
   * @param oldl the listener to remove
   * @return the resulting listener chain
   */
  public static MouseListener remove(MouseListener l, MouseListener oldl)
  {
    return (MouseListener) removeInternal(l, oldl);
  }

  /**
   * Removes the listener <code>oldl</code> from the listener <code>l</code>.
   *
   * @param l the listener chain to reduce
   * @param oldl the listener to remove
   * @return the resulting listener chain
   */
  public static MouseMotionListener remove(MouseMotionListener l,
                                           MouseMotionListener oldl)
  {
    return (MouseMotionListener) removeInternal(l, oldl);
  }

  /**
   * Removes the listener <code>oldl</code> from the listener <code>l</code>.
   *
   * @param l the listener chain to reduce
   * @param oldl the listener to remove
   * @return the resulting listener chain
   */
  public static WindowListener remove(WindowListener l, WindowListener oldl)
  {
    return (WindowListener) removeInternal(l, oldl);
  }

  /**
   * Removes the listener <code>oldl</code> from the listener <code>l</code>.
   *
   * @param l the listener chain to reduce
   * @param oldl the listener to remove
   * @return the resulting listener chain
   * @since 1.4
   */
  public static WindowStateListener remove(WindowStateListener l,
                                           WindowStateListener oldl)
  {
    return (WindowStateListener) removeInternal(l, oldl);
  }

  /**
   * Removes the listener <code>oldl</code> from the listener <code>l</code>.
   *
   * @param l the listener chain to reduce
   * @param oldl the listener to remove
   * @return the resulting listener chain
   * @since 1.4
   */
  public static WindowFocusListener remove(WindowFocusListener l,
                                           WindowFocusListener oldl)
  {
    return (WindowFocusListener) removeInternal(l, oldl);
  }

  /**
   * Removes the listener <code>oldl</code> from the listener <code>l</code>.
   *
   * @param l the listener chain to reduce
   * @param oldl the listener to remove
   * @return the resulting listener chain
   */
  public static ActionListener remove(ActionListener l, ActionListener oldl)
  {
    return (ActionListener) removeInternal(l, oldl);
  }

  /**
   * Removes the listener <code>oldl</code> from the listener <code>l</code>.
   *
   * @param l the listener chain to reduce
   * @param oldl the listener to remove
   * @return the resulting listener chain
   */
  public static ItemListener remove(ItemListener l, ItemListener oldl)
  {
    return (ItemListener) removeInternal(l, oldl);
  }

  /**
   * Removes the listener <code>oldl</code> from the listener <code>l</code>.
   *
   * @param l the listener chain to reduce
   * @param oldl the listener to remove
   * @return the resulting listener chain
   */
  public static AdjustmentListener remove(AdjustmentListener l,
                                          AdjustmentListener oldl)
  {
    return (AdjustmentListener) removeInternal(l, oldl);
  }

  /**
   * Removes the listener <code>oldl</code> from the listener <code>l</code>.
   *
   * @param l the listener chain to reduce
   * @param oldl the listener to remove
   * @return the resulting listener chain
   */
  public static TextListener remove(TextListener l, TextListener oldl)
  {
    return (TextListener) removeInternal(l, oldl);
  }

  /**
   * Removes the listener <code>oldl</code> from the listener <code>l</code>.
   *
   * @param l the listener chain to reduce
   * @param oldl the listener to remove
   * @return the resulting listener chain
   * @since 1.2
   */
  public static InputMethodListener remove(InputMethodListener l,
                                           InputMethodListener oldl)
  {
    return (InputMethodListener) removeInternal(l, oldl);
  }

  /**
   * Removes the listener <code>oldl</code> from the listener <code>l</code>.
   *
   * @param l the listener chain to reduce
   * @param oldl the listener to remove
   * @return the resulting listener chain
   * @since 1.3
   */
  public static HierarchyListener remove(HierarchyListener l,
                                         HierarchyListener oldl)
  {
    return (HierarchyListener) removeInternal(l, oldl);
  }

  /**
   * Removes the listener <code>oldl</code> from the listener <code>l</code>.
   *
   * @param l the listener chain to reduce
   * @param oldl the listener to remove
   * @return the resulting listener chain
   * @since 1.3
   */
  public static HierarchyBoundsListener remove(HierarchyBoundsListener l,
                                               HierarchyBoundsListener oldl)
  {
    return (HierarchyBoundsListener) removeInternal(l, oldl);
  }

  /**
   * Removes the listener <code>oldl</code> from the listener <code>l</code>.
   *
   * @param l the listener chain to reduce
   * @param oldl the listener to remove
   * @return the resulting listener chain
   * @since 1.4
   */
  public static MouseWheelListener remove(MouseWheelListener l,
                                          MouseWheelListener oldl)
  {
    return (MouseWheelListener) removeInternal(l, oldl);
  }

  /**
   * Chain <code>EventListener</code> a and b.
   *
   * @param a the "a" listener, may be null
   * @param b the "b" listener, may be null 
   * @return latest entry in the chain
   */
  protected static EventListener addInternal(EventListener a, EventListener b)
  {
    if (a == null)
      return b;
    if (b == null)
      return a;
    return new AWTEventMulticaster(a, b);
  }

  /**
   * Removes the listener <code>oldl</code> from the listener <code>l</code>.
   *
   * @param l the listener chain to reduce
   * @param oldl the listener to remove
   * @return the resulting listener chain
   */
  protected static EventListener removeInternal(EventListener l,
                                                EventListener oldl)
  {
    if (l == oldl)
      return null;
    if (l instanceof AWTEventMulticaster)
      return ((AWTEventMulticaster) l).remove(oldl);
    return l;
  }

  /**
   * Saves all Serializable listeners to a serialization stream.
   *
   * @param s the stream to save to
   * @param k a prefix stream put before each serializable listener
   * @throws IOException if serialization fails
   */
  protected void saveInternal(ObjectOutputStream s, String k)
    throws IOException
  {
    // This is not documented by Sun, but I think it is correct.
    if (a instanceof AWTEventMulticaster)
      ((AWTEventMulticaster) a).saveInternal(s, k);
    else if (a instanceof Serializable)
      {
        s.writeObject(k);
        s.writeObject(a);
      }
    if (b instanceof AWTEventMulticaster)
      ((AWTEventMulticaster) b).saveInternal(s, k);
    else if (b instanceof Serializable)
      {
        s.writeObject(k);
        s.writeObject(b);
      }
  }

  /**
   * Saves a Serializable listener chain to a serialization stream.
   *
   * @param s the stream to save to
   * @param k a prefix stream put before each serializable listener
   * @param l the listener chain to save
   * @throws IOException if serialization fails
   */
  protected static void save(ObjectOutputStream s, String k, EventListener l)
    throws IOException
  {
    // This is not documented by Sun, but I think it is correct.
    if (l instanceof AWTEventMulticaster)
      ((AWTEventMulticaster) l).saveInternal(s, k);
    else if (l instanceof Serializable)
      {
        s.writeObject(k);
        s.writeObject(l);
      }
  }

  /**
   * Returns an array of all chained listeners of the specified type in the
   * given chain. A null listener returns an empty array, and a listener
   * which is not an AWTEventMulticaster returns an array of one element. If
   * no listeners in the chain are of the specified type, an empty array is
   * returned.
   *
   * @param l the listener chain to convert to an array
   * @param type the type of listeners to collect
   * @return an array of the listeners of that type in the chain
   * @throws ClassCastException if type is not assignable from EventListener
   * @throws NullPointerException if type is null
   * @throws IllegalArgumentException if type is Void.TYPE
   * @since 1.4
   */
  public static <T extends EventListener> T[] getListeners(EventListener l,
							   Class<T> type)
  {
    ArrayList<EventListener> list = new ArrayList<EventListener>();
    if (l instanceof AWTEventMulticaster)
      ((AWTEventMulticaster) l).getListeners(list, type);
    else if (type.isInstance(l))
      list.add(l);
    EventListener[] r = (EventListener[]) Array.newInstance(type, list.size());
    list.toArray(r);
    return (T[]) r;
  }

  /**
   * Collects all instances of the given type in the chain into the list.
   *
   * @param l the list to collect into
   * @param type the type of listeners to collect
   * @throws NullPointerException if type is null
   * @see #getListeners(EventListener, Class)
   */
  private void getListeners(ArrayList l, Class type)
  {
    if (a instanceof AWTEventMulticaster)
      ((AWTEventMulticaster) a).getListeners(l, type);
    else if (type.isInstance(a))
      l.add(a);
    if (b instanceof AWTEventMulticaster)
      ((AWTEventMulticaster) b).getListeners(l, type);
    else if (type.isInstance(b))
      l.add(b);
  }
} // class AWTEventMulticaster
