/* DefaultCaret.java --
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

package javax.swing.text;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.util.EventListener;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.EventListenerList;

/**
 * The default implementation of the {@link Caret} interface.
 *
 * @author orgininal author unknown
 * @author Roman Kennke (roman@kennke.org)
 */
public class DefaultCaret extends Rectangle
  implements Caret, FocusListener, MouseListener, MouseMotionListener
{
  /**
   * The serial version UID for DefaultCaret.
   */
  private static final long serialVersionUID = 228155774675466193L;

  /**
   * The <code>ChangeEvent</code> that is fired by {@link #fireStateChanged()}.
   */
  protected ChangeEvent changeEvent = new ChangeEvent(this);

  /**
   * Stores all registered event listeners.
   */
  protected EventListenerList listenerList = new EventListenerList();

  /**
   * The text component in which this caret is installed.
   */
  private JTextComponent textComponent;

  /**
   * Indicates if the selection should be visible or not.
   */
  private boolean selectionVisible = true;

  /**
   * The blink rate of this <code>Caret</code>.
   */
  private int blinkRate = 500;

  /**
   * The current dot position.
   */
  private int dot = 0;

  /**
   * The current mark position.
   */
  private int mark = 0;

  /**
   * The current visual caret position.
   */
  private Point magicCaretPosition = null;

  /**
   * Indicates if this <code>Caret</code> is currently visible or not.
   */
  private boolean visible = true;

  /**
   * The current highlight entry.
   */
  private Object highlightEntry;

  /**
   * Moves the caret position when the mouse is dragged over the text
   * component, modifying the selection accordingly.
   *
   * @param event the <code>MouseEvent</code> describing the drag operation
   */
  public void mouseDragged(MouseEvent event)
  {
    // FIXME: Implement this properly.
  }

  /**
   * Indicates a mouse movement over the text component. Does nothing here.
   *
   * @param event the <code>MouseEvent</code> describing the mouse operation
   */
  public void mouseMoved(MouseEvent event)
  {
    // Nothing to do here.
  }

  /**
   * When the click is received from Button 1 then the following actions
   * are performed here:
   *
   * <ul>
   * <li>If we receive a double click, the caret position (dot) is set
   *   to the position associated to the mouse click and the word at
   *   this location is selected.</li>
   * <li>If we receive a triple click, the caret position (dot) is set
   *   to the position associated to the mouse click and the line at
   *   this location is selected.</li>
   * </ul>
   *
   * @param event the <code>MouseEvent</code> describing the click operation
   */
  public void mouseClicked(MouseEvent event)
  {
    // FIXME: Implement this properly.
  }

  /**
   * Indicates that the mouse has entered the text component. Nothing is done
   * here.
   *
   * @param event the <code>MouseEvent</code> describing the mouse operation
   */
  public void mouseEntered(MouseEvent event)
  {
    // Nothing to do here.
  }

  /**
   * Indicates that the mouse has exited the text component. Nothing is done
   * here.
   *
   * @param event the <code>MouseEvent</code> describing the mouse operation
   */
  public void mouseExited(MouseEvent event)
  {
  }

  /**
   * If the button 1 is pressed, the caret position is updated to the
   * position of the mouse click and the text component requests the input
   * focus if it is enabled. If the SHIFT key is held down, the caret will
   * be moved, which might select the text between the old and new location.
   *
   * @param event the <code>MouseEvent</code> describing the press operation
   */
  public void mousePressed(MouseEvent event)
  {
    // FIXME: Implement this properly.
  }

  /**
   * Indicates that a mouse button has been released on the text component.
   * Nothing is done here.
   *
   * @param event the <code>MouseEvent</code> describing the mouse operation
   */
  public void mouseReleased(MouseEvent event)
  {
    // Nothing to do here.
  }

  /**
   * Sets the caret to <code>visible</code> if the text component is editable.
   *
   * @param event the <code>FocusEvent</code>
   */
  public void focusGained(FocusEvent event)
  {
  }

  /**
   * Sets the caret to <code>invisible</code>.
   *
   * @param event the <code>FocusEvent</code>
   */
  public void focusLost(FocusEvent event)
  {
  }

  /**
   * Moves the caret to the position specified in the <code>MouseEvent</code>.
   * This will cause a selection if the dot and mark are different.
   *
   * @param event the <code>MouseEvent</code> from which to fetch the position
   */
  protected void moveCaret(MouseEvent event)
  {
    // FIXME: Implement this properly.
  }

  /**
   * Repositions the caret to the position specified in the
   * <code>MouseEvent</code>.
   *
   * @param event the <code>MouseEvent</code> from which to fetch the position
   */
  protected void positionCaret(MouseEvent event)
  {
    // FIXME: Implement this properly.
  }

  /**
   * Deinstalls this <code>Caret</code> from the specified
   * <code>JTextComponent</code>. This removes any listeners that have been
   * registered by this <code>Caret</code>.
   *
   * @param c the text component from which to install this caret
   */
  public void deinstall(JTextComponent c)
  {
    textComponent.removeFocusListener(this);
    textComponent.removeMouseListener(this);
    textComponent.removeMouseMotionListener(this);
    textComponent = null;
  }

  /**
   * Installs this <code>Caret</code> on the specified
   * <code>JTextComponent</code>. This registers a couple of listeners
   * on the text component.
   *
   * @param c the text component on which to install this caret
   */
  public void install(JTextComponent c)
  {
    textComponent = c;
    textComponent.addFocusListener(this);
    textComponent.addMouseListener(this);
    textComponent.addMouseMotionListener(this);
    repaint();
  }

  /**
   * Sets the current visual position of this <code>Caret</code>.
   *
   * @param p the Point to use for the saved location. May be <code>null</code>
   *        to indicate that there is no visual location
   */
  public void setMagicCaretPosition(Point p)
  {
    magicCaretPosition = p;
  }

  /**
   * Returns the current visual position of this <code>Caret</code>.
   *
   * @return the current visual position of this <code>Caret</code>
   *
   * @see #setMagicCaretPosition
   */
  public Point getMagicCaretPosition()
  {
    return magicCaretPosition;
  }

  /**
   * Returns the current position of the <code>mark</code>. The
   * <code>mark</code> marks the location in the <code>Document</code> that
   * is the end of a selection. If there is no selection, the <code>mark</code>
   * is the same as the <code>dot</code>.
   *
   * @return the current position of the mark
   */
  public int getMark()
  {
    return mark;
  }

  private void handleHighlight()
  {
    Highlighter highlighter = textComponent.getHighlighter();
    
    if (highlighter == null)
      return;
    
    int p0 = Math.min(dot, mark);
    int p1 = Math.max(dot, mark);
    
    if (selectionVisible && p0 != p1)
      {
	try
	  {
	    if (highlightEntry == null)
	      highlightEntry = highlighter.addHighlight(p0, p1, getSelectionPainter());
	    else
	      highlighter.changeHighlight(highlightEntry, p0, p1);
	  }
	catch (BadLocationException e)
	  {
	    // This should never happen.
	    throw new InternalError();
	  }
      }
    else
      {
	if (highlightEntry != null)
	  {
	    highlighter.removeHighlight(highlightEntry);
	    highlightEntry = null;
	  }
      }
  }

  /**
   * Sets the visiblity state of the selection.
   *
   * @param v <code>true</code> if the selection should be visible,
   *        <code>false</code> otherwise
   */
  public void setSelectionVisible(boolean v)
  {
    if (selectionVisible == v)
      return;
    
    selectionVisible = v;
    handleHighlight();
    repaint();
  }

  /**
   * Returns <code>true</code> if the selection is currently visible,
   * <code>false</code> otherwise.
   *
   * @return <code>true</code> if the selection is currently visible,
   *         <code>false</code> otherwise
   */
  public boolean isSelectionVisible()
  {
    return selectionVisible;
  }

  /**
   * Causes the <code>Caret</code> to repaint itself.
   */
  protected final void repaint()
  {
    // FIXME: Is this good? This possibly causes alot of the component
    // hierarchy to be repainted on every caret blink.
    if (textComponent != null)
      textComponent.repaint();
  }

  /**
   * Paints this <code>Caret</code> using the specified <code>Graphics</code>
   * context.
   *
   * @param g the graphics context to use
   */
  public void paint(Graphics g)
  {
    if (textComponent == null)
      return;

    int dot = getDot();
    Rectangle rect = null;

    try
      {
	rect = textComponent.modelToView(dot);
      }
    catch (BadLocationException e)
      {
	// This should never happen as dot should be always valid.
	return;
      }

    if (rect == null)
      return;
    
    // First we need to delete the old caret.
    // FIXME: Implement deleting of old caret.
    
    // Now draw the caret on the new position if visible.
    if (visible)
      {
	g.setColor(textComponent.getCaretColor());
	g.drawLine(rect.x, rect.y, rect.x, rect.y + rect.height);
      }
  }

  /**
   * Returns all registered event listeners of the specified type.
   *
   * @param listenerType the type of listener to return
   *
   * @return all registered event listeners of the specified type
   */
  public EventListener[] getListeners(Class listenerType)
  {
    return listenerList.getListeners(listenerType);
  }

  /**
   * Registers a {@link ChangeListener} that is notified whenever that state
   * of this <code>Caret</code> changes.
   *
   * @param listener the listener to register to this caret
   */
  public void addChangeListener(ChangeListener listener)
  {
    listenerList.add(ChangeListener.class, listener);
  }

  /**
   * Removes a {@link ChangeListener} from the list of registered listeners.
   *
   * @param listener the listener to remove
   */
  public void removeChangeListener(ChangeListener listener)
  {
    listenerList.remove(ChangeListener.class, listener);
  }

  /**
   * Returns all registered {@link ChangeListener}s of this <code>Caret</code>.
   *
   * @return all registered {@link ChangeListener}s of this <code>Caret</code>
   */
  public ChangeListener[] getChangeListeners()
  {
    return (ChangeListener[]) getListeners(ChangeListener.class);
  }

  /**
   * Notifies all registered {@link ChangeListener}s that the state
   * of this <code>Caret</code> has changed.
   */
  protected void fireStateChanged()
  {
    ChangeListener[] listeners = getChangeListeners();

    for (int index = 0; index < listeners.length; ++index)
      listeners[index].stateChanged(changeEvent);
  }

  /**
   * Returns the <code>JTextComponent</code> on which this <code>Caret</code>
   * is installed.
   *
   * @return the <code>JTextComponent</code> on which this <code>Caret</code>
   *         is installed
   */
  protected final JTextComponent getComponent()
  {
    return textComponent;
  }

  /**
   * Returns the blink rate of this <code>Caret</code> in milliseconds.
   * A value of <code>0</code> means that the caret does not blink.
   *
   * @return the blink rate of this <code>Caret</code> or <code>0</code> if
   *         this caret does not blink
   */
  public int getBlinkRate()
  {
    return blinkRate;
  }

  /**
   * Sets the blink rate of this <code>Caret</code> in milliseconds.
   * A value of <code>0</code> means that the caret does not blink.
   *
   * @param rate the new blink rate to set
   */
  public void setBlinkRate(int rate)
  {
    blinkRate = rate;
  }

  /**
   * Returns the current position of this <code>Caret</code> within the
   * <code>Document</code>.
   *
   * @return the current position of this <code>Caret</code> within the
   *         <code>Document</code>
   */
  public int getDot()
  {
    return dot;
  }

  /**
   * Moves the <code>dot</code> location without touching the
   * <code>mark</code>. This is used when making a selection.
   *
   * @param dot the location where to move the dot
   *
   * @see #setDot(int)
   */
  public void moveDot(int dot)
  {
    this.dot = dot;
    handleHighlight();
    repaint();
  }

  /**
   * Sets the current position of this <code>Caret</code> within the
   * <code>Document</code>. This also sets the <code>mark</code> to the
   * new location.
   *
   * @param dot the new position to be set
   *
   * @see #moveDot(int)
   */
  public void setDot(int dot)
  {
    this.dot = dot;
    this.mark = dot;
    handleHighlight();
    repaint();
  }

  /**
   * Returns <code>true</code> if this <code>Caret</code> is currently visible,
   * and <code>false</code> if it is not.
   *
   * @return <code>true</code> if this <code>Caret</code> is currently visible,
   *         and <code>false</code> if it is not
   */
  public boolean isVisible()
  {
    return visible;
  }

  /**
   * Sets the visibility state of the caret. <code>true</code> shows the
   * <code>Caret</code>, <code>false</code> hides it.
   *
   * @param v the visibility to set
   */  
  public void setVisible(boolean v)
  {
    visible = v;
    repaint();
  }

  /**
   * Returns the {@link Highlighter.HighlightPainter} that should be used
   * to paint the selection.
   *
   * @return the {@link Highlighter.HighlightPainter} that should be used
   *         to paint the selection
   */
  protected Highlighter.HighlightPainter getSelectionPainter()
  {
    return DefaultHighlighter.DefaultPainter;
  }
}
