/* Copyright (C) 1999, 2000, 2002  Free Software Foundation

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

import java.awt.event.*;
import java.util.EventListener;
import java.io.ObjectOutputStream;

/* Written using on-line Java 2 Platform Standard Edition v1.3 API 
 * Specification, as well as "The Java Class Libraries", 2nd edition 
 * (Addison-Wesley, 1998).
 * Status:  Believed complete and correct to J2SE 1.3, except for 
 * serialization support methods, save() and saveInternal(), which are 
 * stubbed.
 */

/**
  * This class is used to implement a chain of event handlers.  Dispatching
  * using this class is thread safe.  Here is a quick example of how to
  * add and delete listeners using this class.  For this example, we will
  * assume are firing <code>AdjustableEvent</code>'s.  However, this 
  * same approach is useful for all events in the <code>java.awt.event</code>
  * package, and more if this class is subclassed.
  * <p>
  * <code> 
  * AdjustmentListener al;
  * 
  * public void 
  * addAdjustmentListener(AdjustmentListener listener)
  * {
  *   al = AWTEventMulticaster.add(al, listener);
  * }
  *
  * public void
  * removeAdjustmentListener(AdjustmentListener listener)
  * {
  *   al = AWTEventMulticaster.remove(al, listener);
  * }
  * </code>
  * <p>
  * When it come time to process an event, simply call <code>al</code>,
  * assuming it is not <code>null</code>.
  * <p>
  * The first time <code>add</code> is called it is passed
  * <code>null</code> and <code>listener</code> as its arguments.  This
  * starts building the chain.  This class returns <code>listener</code>
  * which becomes the new <code>al</code>.  The next time, <code>add</code>
  * is called with <code>al</code> and <code>listener</code> and the
  * new listener is then chained to the old.
  *
  * @author Bryce McKinlay
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class AWTEventMulticaster implements ComponentListener, 
  ContainerListener, FocusListener, KeyListener, MouseListener,
  MouseMotionListener, WindowListener, ActionListener, ItemListener, 
  AdjustmentListener, TextListener, InputMethodListener, HierarchyListener, 
  HierarchyBoundsListener
{
  /**
   * A variable in the event chain.
   */
  protected final EventListener a;

  /**
   * A variable in the event chain
   */
  protected final EventListener b;

  /**
   * Initializes a new instance of <code>AWTEventMulticaster</code> with
   * the specified event listener parameters.
   *
   * @param a The "a" listener object.
   * @param b The "b" listener object.
   */
  protected AWTEventMulticaster(EventListener a,
                        	EventListener b)
  {
    this.a = a;
    this.b = b;
  }

  /**
   * Chain <code>EventListener</code> b to a.
   *
   * @param a - Listener to chain to.
   * @param b - Listener to chain.
   *
   * @return Latest entry in the chain.
   */
  protected static EventListener addInternal(EventListener a, EventListener b)
  {
    if (a == null)
      return b;
    else if (b == null)
      return a;
    else return new AWTEventMulticaster(a, b);
  }

  /**
   * Removes the listener <code>old</code> from the listener <code>lis</code>.
   *
   * @param lis The listener to remove <code>old</code> from.
   * @param old The listener to remove.
   *
   * @return The resulting listener after the remove operation.
   */
  protected static EventListener removeInternal(EventListener l, 
						EventListener oldl)
  {
    if (l == oldl)
      return null;
    else if (l instanceof AWTEventMulticaster)
      {
	AWTEventMulticaster mc = (AWTEventMulticaster) l;
	return mc.remove(oldl);
      }
    return l;
  }

  /**
   * Removes the specified object from this multicaster object.  If the
   * object to remove is not part of this multicaster, then the remove
   * method on the parent multicaster (if it exists) is called and a 
   * new multicaster object is returned based on that object and this
   * multicaster's non-parent object.
   *
   * @param old The object to remove from this multicaster.
   *
   * @return The resulting multicaster with the specified listener removed.
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
        AWTEventMulticaster mc = (AWTEventMulticaster) a;
	EventListener newa = mc.remove(oldl);
	if (newa != a)
	  return new AWTEventMulticaster (newa, b);
      }    
    if (b instanceof AWTEventMulticaster)
      {
        AWTEventMulticaster mc = (AWTEventMulticaster) a;
	EventListener newb = mc.remove(oldl);
	if (newb != b)
	  return new AWTEventMulticaster (a, newb);
      }

    // oldl was not found.
    return this;
  }

  /**
   * Chain <code>ActionListener</code> b to a.
   *
   * @param a - Listener to chain to.
   * @param b - Listener to chain.
   *
   * @return Latest entry in the chain.
   */
  public static ActionListener add(ActionListener a, ActionListener b)
  {
    return (ActionListener) addInternal(a, b);
  }

  /**
   * Chain <code>AdjustmentListener</code> b to a.
   *
   * @param a - Listener to chain to.
   * @param b - Listener to chain.
   *
   * @return Latest entry in the chain.
   */
  public static AdjustmentListener add(AdjustmentListener a, 
				       AdjustmentListener b)
  {
    return (AdjustmentListener) addInternal(a, b);
  }				       

  /**
   * Chain <code>ComponentListener</code> b to a.
   *
   * @param a - Listener to chain to.
   * @param b - Listener to chain.
   *
   * @return Latest entry in the chain.
   */
  public static ComponentListener add(ComponentListener a, ComponentListener b)
  {
    return (ComponentListener) addInternal(a, b);
  }

  /**
   * Chain <code>ContainerListener</code> b to a.
   *
   * @param a - Listener to chain to.
   * @param b - Listener to chain.
   *
   * @return Latest entry in the chain.
   */
  public static ContainerListener add(ContainerListener a, ContainerListener b)
  {
    return (ContainerListener) addInternal(a, b);
  }

  /**
   * Chain <code>FocusListener</code> b to a.
   *
   * @param a - Listener to chain to.
   * @param b - Listener to chain.
   *
   * @return Latest entry in the chain.
   */
  public static FocusListener add(FocusListener a, FocusListener b) 
  {
    return (FocusListener) addInternal(a, b);
  }

  public static HierarchyBoundsListener add(HierarchyBoundsListener a, 
					    HierarchyBoundsListener b)
  {
    return (HierarchyBoundsListener) addInternal(a, b);
  }

  public static HierarchyListener add(HierarchyListener a, HierarchyListener b)
  {
    return (HierarchyListener) addInternal(a, b);
  }

  public static InputMethodListener add(InputMethodListener a, 
					InputMethodListener b)
  {
    return (InputMethodListener) addInternal(a, b);
  }

  /**
   * Chain <code>ItemListener</code> b to a.
   *
   * @param a - Listener to chain to.
   * @param b - Listener to chain.
   *
   * @return Latest entry in the chain.
   */
  public static ItemListener add(ItemListener a, ItemListener b)
  {
    return (ItemListener) addInternal(a, b);
  }

  /**
   * Chain <code>KeyListener</code> b to a.
   *
   * @param a - Listener to chain to.
   * @param b - Listener to chain.
   *
   * @return Latest entry in the chain.
   */
  public static KeyListener add(KeyListener a, KeyListener b)
  {
    return (KeyListener) addInternal(a, b);
  }

  /**
   * Chain <code>MouseListener</code> b to a.
   *
   * @param a - Listener to chain to.
   * @param b - Listener to chain.
   *
   * @return Latest entry in the chain.
   */
  public static MouseListener add(MouseListener a, MouseListener b)
  {
    return (MouseListener) addInternal(a, b);
  }

  /**
   * Chain <code>MouseMotionListener</code> b to a.
   *
   * @param a - Listener to chain to.
   * @param b - Listener to chain.
   *
   * @return Latest entry in the chain.
   */
  public static MouseMotionListener add(MouseMotionListener a, 
					MouseMotionListener b)
  {
    return (MouseMotionListener) addInternal(a, b);
  }

  /**
   * Chain <code>AdjustmentListener</code> b to a.
   *
   * @param a - Listener to chain to.
   * @param b - Listener to chain.
   *
   * @return Latest entry in the chain.
   */
  public static TextListener add(TextListener a, TextListener b)
  {
    return (TextListener) addInternal(a, b);
  }

  /**
   * Chain <code>WindowListener</code> b to a.
   *
   * @param a - Listener to chain to.
   * @param b - Listener to chain.
   *
   * @return Latest entry in the chain.
   */
  public static WindowListener add(WindowListener a, WindowListener b)
  {
    return (WindowListener) addInternal(a, b);
  }

  /**
   * Removes the listener <code>old</code> from the listener <code>lis</code>.
   *
   * @param lis The listener to remove <code>old</code> from.
   * @param old The listener to remove.
   *
   * @return The resulting listener after the remove operation.
   */
  public static ActionListener remove(ActionListener l, ActionListener oldl)
  {
    return (ActionListener) removeInternal(l, oldl);
  }

  /**
   * Removes the listener <code>old</code> from the listener <code>lis</code>.
   *
   * @param lis The listener to remove <code>old</code> from.
   * @param old The listener to remove.
   *
   * @return The resulting listener after the remove operation.
   */
  public static AdjustmentListener remove(AdjustmentListener l, 
					  AdjustmentListener oldl) 
  {
    return (AdjustmentListener) removeInternal(l, oldl);
  }

  /**
   * Removes the listener <code>old</code> from the listener <code>lis</code>.
   *
   * @param lis The listener to remove <code>old</code> from.
   * @param old The listener to remove.
   *
   * @return The resulting listener after the remove operation.
   */
  public static ComponentListener remove(ComponentListener l, 
					 ComponentListener oldl) 
  {
    return (ComponentListener) removeInternal(l, oldl);
  }

  /**
   * Removes the listener <code>old</code> from the listener <code>lis</code>.
   *
   * @param lis The listener to remove <code>old</code> from.
   * @param old The listener to remove.
   *
   * @return The resulting listener after the remove operation.
   */
  public static ContainerListener remove(ContainerListener l, 
					 ContainerListener oldl) 
  {
    return (ContainerListener) removeInternal(l, oldl);
  }

  /**
   * Removes the listener <code>old</code> from the listener <code>lis</code>.
   *
   * @param lis The listener to remove <code>old</code> from.
   * @param old The listener to remove.
   *
   * @return The resulting listener after the remove operation.
   */
  public static FocusListener remove(FocusListener l, FocusListener oldl) 
  {
    return (FocusListener) removeInternal(l, oldl);
  }

  public static HierarchyBoundsListener remove(HierarchyBoundsListener l,
                			       HierarchyBoundsListener oldl) 
  {
    return (HierarchyBoundsListener) removeInternal(l, oldl);
  }

  public static HierarchyListener remove(HierarchyListener l, 
					 HierarchyListener oldl) 
  {
    return (HierarchyListener) removeInternal(l, oldl);
  }

  public static InputMethodListener remove(InputMethodListener l, 
					   InputMethodListener oldl) 
  {
    return (InputMethodListener) removeInternal(l, oldl);
  }

  /**
   * Removes the listener <code>old</code> from the listener <code>lis</code>.
   *
   * @param lis The listener to remove <code>old</code> from.
   * @param old The listener to remove.
   *
   * @return The resulting listener after the remove operation.
   */
  public static ItemListener remove(ItemListener l, ItemListener oldl) 
  {
    return (ItemListener) removeInternal(l, oldl);
  }

  /**
   * Removes the listener <code>old</code> from the listener <code>lis</code>.
   *
   * @param lis The listener to remove <code>old</code> from.
   * @param old The listener to remove.
   *
   * @return The resulting listener after the remove operation.
   */
  public static KeyListener remove(KeyListener l, KeyListener oldl) 
  {
    return (KeyListener) removeInternal(l, oldl);
  }

  /**
   * Removes the listener <code>old</code> from the listener <code>lis</code>.
   *
   * @param lis The listener to remove <code>old</code> from.
   * @param old The listener to remove.
   *
   * @return The resulting listener after the remove operation.
   */
  public static MouseListener remove(MouseListener l, MouseListener oldl) 
  {
    return (MouseListener) removeInternal(l, oldl);
  }

  /**
   * Removes the listener <code>old</code> from the listener <code>lis</code>.
   *
   * @param lis The listener to remove <code>old</code> from.
   * @param old The listener to remove.
   *
   * @return The resulting listener after the remove operation.
   */
  public static MouseMotionListener remove(MouseMotionListener l, 
					   MouseMotionListener oldl) 
  {
    return (MouseMotionListener) removeInternal(l, oldl);
  }

  /**
   * Removes the listener <code>old</code> from the listener <code>lis</code>.
   *
   * @param lis The listener to remove <code>old</code> from.
   * @param old The listener to remove.
   *
   * @return The resulting listener after the remove operation.
   */
  public static TextListener remove(TextListener l, TextListener oldl)                                            
  {
    return (TextListener) removeInternal(l, oldl);
  }

  /**
   * Removes the listener <code>old</code> from the listener <code>lis</code>.
   *
   * @param lis The listener to remove <code>old</code> from.
   * @param old The listener to remove.
   *
   * @return The resulting listener after the remove operation.
   */
  public static WindowListener remove(WindowListener l, WindowListener oldl) 
  {
    return (WindowListener) removeInternal(l, oldl);
  }

  /**
   * Handles this event by dispatching it to the "a" and "b" listener
   * instances.
   *
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
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
   * @param event The event to handle.
   */
  public void windowOpened(WindowEvent e) 
  {
    ((WindowListener) a).windowOpened(e);
    ((WindowListener) b).windowOpened(e);
  }

  protected static void save(ObjectOutputStream s, String k, EventListener l) 
  {
    throw new RuntimeException("Not Implemented");
  }

  protected void saveInternal(ObjectOutputStream s, String k)
  {
    throw new RuntimeException("Not Implemented");
  }
}
