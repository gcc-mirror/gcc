/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

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
 * Convienience class for thread-safe multicasting of AWT events,
 * without synchronization.
 *
 * @author Bryce McKinlay
 */

public class AWTEventMulticaster implements ComponentListener, 
  ContainerListener, FocusListener, KeyListener, MouseListener,
  MouseMotionListener, WindowListener, ActionListener, ItemListener, 
  AdjustmentListener, TextListener, InputMethodListener, HierarchyListener, 
  HierarchyBoundsListener
{
  protected final EventListener a, b;                            

  protected AWTEventMulticaster(EventListener a,
                        	EventListener b)
  {
    this.a = a;
    this.b = b;
  }

  protected static EventListener addInternal(EventListener a, EventListener b)
  {
    if (a == null)
      return b;
    else if (b == null)
      return a;
    else return new AWTEventMulticaster(a, b);
  }
  
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

  /* Remove oldl from this multicaster. */
  protected EventListener remove(EventListener oldl)
  {
    // If oldl is an immediate child, return the other child.
    if (a == oldl)
      return b;
    if (b == oldl)
      return a;

    // If a and/or b are Multicaster's, search them recursivly. 
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

  public static ActionListener add(ActionListener a, ActionListener b)
  {
    return (ActionListener) addInternal(a, b);
  }
  
  public static AdjustmentListener add(AdjustmentListener a, 
				       AdjustmentListener b)
  {
    return (AdjustmentListener) addInternal(a, b);
  }				       
				       
  public static ComponentListener add(ComponentListener a, ComponentListener b)
  {
    return (ComponentListener) addInternal(a, b);
  }
  
  public static ContainerListener add(ContainerListener a, ContainerListener b)
  {
    return (ContainerListener) addInternal(a, b);
  }
  
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
					
  public static ItemListener add(ItemListener a, ItemListener b)
  {
    return (ItemListener) addInternal(a, b);
  }
  
  public static KeyListener add(KeyListener a, KeyListener b)
  {
    return (KeyListener) addInternal(a, b);
  }

  public static MouseListener add(MouseListener a, MouseListener b)
  {
    return (MouseListener) addInternal(a, b);
  }
  
  public static MouseMotionListener add(MouseMotionListener a, 
					MouseMotionListener b)
  {
    return (MouseMotionListener) addInternal(a, b);
  }
					
  public static TextListener add(TextListener a, TextListener b)
  {
    return (TextListener) addInternal(a, b);
  }
  
  public static WindowListener add(WindowListener a, WindowListener b)
  {
    return (WindowListener) addInternal(a, b);
  }
  
  public static ActionListener remove(ActionListener l, ActionListener oldl)
  {
    return (ActionListener) removeInternal(l, oldl);
  }
  
  public static AdjustmentListener remove(AdjustmentListener l, 
					  AdjustmentListener oldl) 
  {
    return (AdjustmentListener) removeInternal(l, oldl);
  }

  public static ComponentListener remove(ComponentListener l, 
					 ComponentListener oldl) 
  {
    return (ComponentListener) removeInternal(l, oldl);
  }

  public static ContainerListener remove(ContainerListener l, 
					 ContainerListener oldl) 
  {
    return (ContainerListener) removeInternal(l, oldl);
  }

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

  public static ItemListener remove(ItemListener l, ItemListener oldl) 
  {
    return (ItemListener) removeInternal(l, oldl);
  }

  public static KeyListener remove(KeyListener l, KeyListener oldl) 
  {
    return (KeyListener) removeInternal(l, oldl);
  }

  public static MouseListener remove(MouseListener l, MouseListener oldl) 
  {
    return (MouseListener) removeInternal(l, oldl);
  }

  public static MouseMotionListener remove(MouseMotionListener l, 
					   MouseMotionListener oldl) 
  {
    return (MouseMotionListener) removeInternal(l, oldl);
  }

  public static TextListener remove(TextListener l, TextListener oldl)                                            
  {
    return (TextListener) removeInternal(l, oldl);
  }

  public static WindowListener remove(WindowListener l, WindowListener oldl) 
  {
    return (WindowListener) removeInternal(l, oldl);
  }

  public void actionPerformed(ActionEvent e) 
  {
    ((ActionListener) a).actionPerformed(e);
    ((ActionListener) b).actionPerformed(e);
  }
  
  public void adjustmentValueChanged(AdjustmentEvent e) 
  {
    ((AdjustmentListener) a).adjustmentValueChanged(e);
    ((AdjustmentListener) b).adjustmentValueChanged(e);
  }
  
  public void componentHidden(ComponentEvent e)
  {
    ((ComponentListener) a).componentHidden(e);
    ((ComponentListener) b).componentHidden(e);
  }
  
  public void componentMoved(ComponentEvent e)
  {
    ((ComponentListener) a).componentMoved(e);
    ((ComponentListener) b).componentMoved(e);
  }
    
  public void componentResized(ComponentEvent e)
  {
    ((ComponentListener) a).componentResized(e);
    ((ComponentListener) b).componentResized(e);
  }
  
  public void componentShown(ComponentEvent e)
  {
    ((ComponentListener) a).componentShown(e);
    ((ComponentListener) b).componentShown(e);
  }
  
  public void componentAdded(ContainerEvent e)
  {
    ((ContainerListener) a).componentAdded(e);
    ((ContainerListener) b).componentAdded(e);
  }

  public void componentRemoved(ContainerEvent e)
  {
    ((ContainerListener) a).componentRemoved(e);
    ((ContainerListener) b).componentRemoved(e);
  }
  
  public void focusGained(FocusEvent e)
  {
    ((FocusListener) a).focusGained(e);
    ((FocusListener) b).focusGained(e);
  }
  
  public void focusLost(FocusEvent e) 
  {
    ((FocusListener) a).focusLost(e);
    ((FocusListener) b).focusLost(e);
  }
  

  public void ancestorMoved(HierarchyEvent e) 
  {
    ((HierarchyBoundsListener) a).ancestorMoved(e);
    ((HierarchyBoundsListener) b).ancestorMoved(e);
  }
  
  public void ancestorResized(HierarchyEvent e) 
  {
    ((HierarchyBoundsListener) a).ancestorResized(e);
    ((HierarchyBoundsListener) b).ancestorResized(e);
  }
  
  public void hierarchyChanged(HierarchyEvent e) 
  {
    ((HierarchyListener) a).hierarchyChanged(e);
    ((HierarchyListener) b).hierarchyChanged(e);
  }

  public void caretPositionChanged(InputMethodEvent e)
  {
    ((InputMethodListener) a).caretPositionChanged(e);
    ((InputMethodListener) b).caretPositionChanged(e);
  }

  public void inputMethodTextChanged(InputMethodEvent e) 
  {
    ((InputMethodListener) a).inputMethodTextChanged(e);
    ((InputMethodListener) b).inputMethodTextChanged(e);
  }

  public void itemStateChanged(ItemEvent e) 
  {
    ((ItemListener) a).itemStateChanged(e);
    ((ItemListener) b).itemStateChanged(e);
  }  

  public void keyPressed(KeyEvent e)
  {
    ((KeyListener) a).keyPressed(e);
    ((KeyListener) b).keyPressed(e);
  }
    
  public void keyReleased(KeyEvent e) 
  {
    ((KeyListener) a).keyReleased(e);
    ((KeyListener) b).keyReleased(e);
  }
  
  public void keyTyped(KeyEvent e) 
  {
    ((KeyListener) a).keyTyped(e);
    ((KeyListener) b).keyTyped(e);
  }
  
  public void mouseClicked(MouseEvent e) 
  {
    ((MouseListener) a).mouseClicked(e);
    ((MouseListener) b).mouseClicked(e);
  }
  
  
  public void mouseEntered(MouseEvent e) 
  {
    ((MouseListener) a).mouseEntered(e);
    ((MouseListener) b).mouseEntered(e);
  }
  
  public void mouseExited(MouseEvent e) 
  {
    ((MouseListener) a).mouseExited(e);
    ((MouseListener) b).mouseExited(e);
  }

  public void mousePressed(MouseEvent e) 
  {
    ((MouseListener) a).mousePressed(e);
    ((MouseListener) b).mousePressed(e);
  }
  
  public void mouseReleased(MouseEvent e) 
  {
    ((MouseListener) a).mouseReleased(e);
    ((MouseListener) b).mouseReleased(e);
  }
  
  public void mouseDragged(MouseEvent e) 
  {
    ((MouseMotionListener) a).mouseDragged(e);
    ((MouseMotionListener) b).mouseDragged(e);
  }

  public void mouseMoved(MouseEvent e) 
  {
    ((MouseMotionListener) a).mouseMoved(e);
    ((MouseMotionListener) b).mouseMoved(e);
  }
    
  public void textValueChanged(TextEvent e) 
  {
    ((TextListener) a).textValueChanged(e);
    ((TextListener) b).textValueChanged(e);
  }
  
  public void windowActivated(WindowEvent e) 
  {
    ((WindowListener) a).windowActivated(e);
    ((WindowListener) b).windowActivated(e);
  }
  
  public void windowClosed(WindowEvent e) 
  {
    ((WindowListener) a).windowClosed(e);
    ((WindowListener) b).windowClosed(e);
  }
  
  public void windowClosing(WindowEvent e) 
  {
    ((WindowListener) a).windowClosing(e);
    ((WindowListener) b).windowClosing(e);
  }
  
  public void windowDeactivated(WindowEvent e) 
  {
    ((WindowListener) a).windowDeactivated(e);
    ((WindowListener) b).windowDeactivated(e);
  }
  
  public void windowDeiconified(WindowEvent e) 
  {
    ((WindowListener) a).windowDeiconified(e);
    ((WindowListener) b).windowDeiconified(e);
  }
  
  public void windowIconified(WindowEvent e) 
  {
    ((WindowListener) a).windowIconified(e);
    ((WindowListener) b).windowIconified(e);
  }
  
  public void windowOpened(WindowEvent e) 
  {
    ((WindowListener) a).windowOpened(e);
    ((WindowListener) b).windowOpened(e);
  }
  
  protected static void save(ObjectOutputStream s, String k, EventListener l) 
  {
    // FIXME
  }

  protected void saveInternal(ObjectOutputStream s, String k)
  {
    // FIXME
  }

}
