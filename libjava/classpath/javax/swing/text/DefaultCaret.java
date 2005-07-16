/* DefaultCaret.java --
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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


public class DefaultCaret extends Rectangle
  implements Caret, FocusListener, MouseListener, MouseMotionListener
{
  private static final long serialVersionUID = 228155774675466193L;
  
  protected ChangeEvent changeEvent = new ChangeEvent(this);
  protected EventListenerList listenerList = new EventListenerList();
  
  private JTextComponent textComponent;
  
  private boolean selectionVisible = true;
  private int blinkRate = 500;
  private int dot = 0;
  private int mark = 0;
  private Point magicCaretPosition = null;
  private boolean visible = true;
  private Object highlightEntry;

  public void mouseDragged(MouseEvent event)
  {
  }

  public void mouseMoved(MouseEvent event)
  {
  }

  public void mouseClicked(MouseEvent event)
  {
  }

  public void mouseEntered(MouseEvent event)
  {
  }

  public void mouseExited(MouseEvent event)
  {
  }

  public void mousePressed(MouseEvent event)
  {
  }

  public void mouseReleased(MouseEvent event)
  {
  }

  public void focusGained(FocusEvent event)
  {
  }

  public void focusLost(FocusEvent event)
  {
  }

  protected void moveCaret(MouseEvent event)
  {
  }

  protected void positionCaret(MouseEvent event)
  {
  }

  public void deinstall(JTextComponent c)
  {
    textComponent.removeFocusListener(this);
    textComponent.removeMouseListener(this);
    textComponent.removeMouseMotionListener(this);
    textComponent = null;
  }

  public void install(JTextComponent c)
  {
    textComponent = c;
    textComponent.addFocusListener(this);
    textComponent.addMouseListener(this);
    textComponent.addMouseMotionListener(this);
    repaint();
  }

  public void setMagicCaretPosition(Point p)
  {
    magicCaretPosition = p;
  }

  public Point getMagicCaretPosition()
  {
    return magicCaretPosition;
  }

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

  public void setSelectionVisible(boolean v)
  {
    if (selectionVisible == v)
      return;
    
    selectionVisible = v;
    handleHighlight();
    repaint();
  }

  public boolean isSelectionVisible()
  {
    return selectionVisible;
  }

  protected final void repaint()
  {
    if (textComponent != null)
      textComponent.repaint();
  }

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

  public EventListener[] getListeners(Class listenerType)
  {
    return listenerList.getListeners(listenerType);
  }

  public void addChangeListener(ChangeListener listener)
  {
    listenerList.add(ChangeListener.class, listener);
  }

  public void removeChangeListener(ChangeListener listener)
  {
    listenerList.remove(ChangeListener.class, listener);
  }

  public ChangeListener[] getChangeListeners()
  {
    return (ChangeListener[]) getListeners(ChangeListener.class);
  }

  protected void fireStateChanged()
  {
    ChangeListener[] listeners = getChangeListeners();

    for (int index = 0; index < listeners.length; ++index)
      listeners[index].stateChanged(changeEvent);
  }

  protected final JTextComponent getComponent()
  {
    return textComponent;
  }
  
  public int getBlinkRate()
  {
    return blinkRate;
  }

  public void setBlinkRate(int rate)
  {
    blinkRate = rate;
  }

  public int getDot()
  {
    return dot;
  }

  public void moveDot(int dot)
  {
    this.dot = dot;
    handleHighlight();
    repaint();
  }

  public void setDot(int dot)
  {
    this.dot = dot;
    this.mark = dot;
    handleHighlight();
    repaint();
  }

  public boolean isVisible()
  {
    return visible;
  }

  public void setVisible(boolean v)
  {
    visible = v;
    repaint();
  }

  protected Highlighter.HighlightPainter getSelectionPainter()
  {
    return DefaultHighlighter.DefaultPainter;
  }
}
