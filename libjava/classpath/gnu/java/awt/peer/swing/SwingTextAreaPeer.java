/* SwingTextAreaPeer.java -- A Swing based peer for AWT textareas
   Copyright (C)  2006, 2007  Free Software Foundation, Inc.

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

package gnu.java.awt.peer.swing;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.TextArea;
import java.awt.event.ComponentEvent;
import java.awt.event.FocusEvent;
import java.awt.event.HierarchyEvent;
import java.awt.event.InputMethodEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.im.InputMethodRequests;
import java.awt.peer.TextAreaPeer;

import javax.swing.JComponent;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JViewport;
import javax.swing.text.BadLocationException;

public class SwingTextAreaPeer
    extends SwingComponentPeer
    implements TextAreaPeer
{

  /**
   * A spezialized Swing scroller used to hold the textarea.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  private class SwingScrollPane
    extends JScrollPane
    implements SwingComponent
  {

    SwingTextArea textArea;

    SwingScrollPane(SwingTextArea textArea)
    {
      super(textArea, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
            JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);

      this.textArea = textArea;
    }

    /**
     * Returns this label.
     *
     * @return <code>this</code>
     */
    public JComponent getJComponent()
    {
      return this;
    }

    /**
     * Handles mouse events by forwarding it to
     * <code>processMouseEvent()</code>.
     *
     * @param ev the mouse event
     */
    public void handleMouseEvent(MouseEvent ev)
    {
      JViewport viewPort = getViewport();
      if(viewPort.contains(ev.getPoint()))
        {
          ev.setSource(textArea);
          textArea.dispatchEvent(ev);
        }
      else
        {
          ev.setSource(this);
          this.dispatchEvent(ev);
        }
    }

    /**
     * Force lightweight mouse dispatching.
     */
    public boolean isLightweight()
    {
      return false;
    }

    /**
     * Handles mouse motion events by forwarding it to
     * <code>processMouseMotionEvent()</code>.
     *
     * @param ev the mouse motion event
     */
    public void handleMouseMotionEvent(MouseEvent ev)
    {
      textArea.processMouseMotionEvent(ev);
    }

    /**
     * Handles key events by forwarding it to <code>processKeyEvent()</code>.
     *
     * @param ev the mouse event
     */
    public void handleKeyEvent(KeyEvent ev)
    {
      textArea.processKeyEvent(ev);
    }

    /**
     * Handles focus events by forwarding it to
     * <code>processFocusEvent()</code>.
     *
     * @param ev the Focus event
     */
    public void handleFocusEvent(FocusEvent ev)
    {
      textArea.processFocusEvent(ev);
    }

    /**
     * Overridden so that this method returns the correct value even without a
     * peer.
     *
     * @return the screen location of the button
     */
    public Point getLocationOnScreen()
    {
      return SwingTextAreaPeer.this.getLocationOnScreen();
    }

    /**
     * Overridden so that the isShowing method returns the correct value for the
     * swing button, even if it has no peer on its own.
     *
     * @return <code>true</code> if the button is currently showing,
     *         <code>false</code> otherwise
     */
    public boolean isShowing()
    {
      boolean retVal = false;
      if (SwingTextAreaPeer.this.awtComponent != null)
        retVal = SwingTextAreaPeer.this.awtComponent.isShowing();
      return retVal;
    }

    /**
     * Overridden, so that the Swing button can create an Image without its
     * own peer.
     *
     * @param w the width of the image
     * @param h the height of the image
     *
     * @return an image
     */
    public Image createImage(int w, int h)
    {
      return SwingTextAreaPeer.this.createImage(w, h);
    }

    public Graphics getGraphics()
    {
      return SwingTextAreaPeer.this.getGraphics();
    }

    public Container getParent()
    {
      Container par = null;
      if (SwingTextAreaPeer.this.awtComponent != null)
        par = SwingTextAreaPeer.this.awtComponent.getParent();
      return par;
    }

    public void requestFocus() {
        SwingTextAreaPeer.this.requestFocus(awtComponent, false, true, 0);
    }

    public boolean requestFocus(boolean temporary) {
        return SwingTextAreaPeer.this.requestFocus(awtComponent, temporary,
                                                   true, 0);
    }

  }

  private class SwingTextArea extends JTextArea
  {
    /**
     * Make this method accessible in this Package.
     */
    protected final void processComponentKeyEvent(KeyEvent e)
    {
      super.processComponentKeyEvent(e);
    }

    /**
     * Make this method accessible in this Package.
     */
    protected final void processMouseMotionEvent(MouseEvent ev)
    {
      super.processMouseMotionEvent(ev);
    }

    /**
     * Make this method accessible in this Package.
     */
    protected final void processComponentEvent(ComponentEvent e)
    {
      super.processComponentEvent(e);
    }

    /**
     * Make this method accessible in this Package.
     */
    protected final void processFocusEvent(FocusEvent e)
    {
      super.processFocusEvent(e);
    }

    /**
     * Make this method accessible in this Package.
     */
    protected final void processHierarchyBoundsEvent(HierarchyEvent e)
    {
      super.processHierarchyBoundsEvent(e);
    }

    /**
     * Make this method accessible in this Package.
     */
    protected final void processHierarchyEvent(HierarchyEvent e)
    {
      super.processHierarchyEvent(e);
    }

    /**
     * Make this method accessible in this Package.
     */
    protected final void processInputMethodEvent(InputMethodEvent e)
    {
      super.processInputMethodEvent(e);
    }

    /**
     * Make this method accessible in this Package.
     */
    protected final void processMouseEvent(MouseEvent e)
    {
      super.processMouseEvent(e);
    }

    /**
     * Make this method accessible in this Package.
     */
    protected final void processMouseWheelEvent(MouseWheelEvent e)
    {
      super.processMouseWheelEvent(e);
    }

    /**
     * Make this method accessible in this Package.
     */
    protected final void processKeyEvent(KeyEvent e)
    {
      super.processKeyEvent(e);
    }

    public void requestFocus() {
      SwingTextAreaPeer.this.requestFocus(awtComponent, false, true, 0);
    }

    public boolean requestFocus(boolean temporary) {
      return SwingTextAreaPeer.this.requestFocus(awtComponent, temporary,
                                                 true, 0);
    }
  }

  /**
   * The actual JTextArea.
   */
  private SwingTextArea jTextArea;

  public SwingTextAreaPeer(TextArea textArea)
  {
    super();
    jTextArea = new SwingTextArea();
    SwingScrollPane swingArea = new SwingScrollPane(jTextArea);
    init(textArea, swingArea);

    JViewport viewport = new JViewport()
      {
        public Image createImage(int width, int height)
        {
          return awtComponent.createImage(width, height);
        }
      };

    viewport.setView(jTextArea);
    swingArea.setViewport(viewport);
    // Pull over the text from the text area.
    setText(textArea.getText());

    // Pull over the number of rows and columns
    // if non were set use default values
    int columns = textArea.getColumns();
    int rows = textArea.getRows();

    if(columns == 0 && rows == 0)
      {
        columns = 25;
        textArea.setColumns(columns);
        rows = 5;
        textArea.setRows(rows);
      }

    jTextArea.setColumns(columns);
    jTextArea.setRows(rows);
  }

  public Dimension getMinimumSize(int rows, int cols)
  {
    return jTextArea.getMinimumSize();
  }

  public Dimension getPreferredSize(int rows, int cols)
  {
    return jTextArea.getPreferredSize();
  }

  public void insert(String text, int pos)
  {
    jTextArea.insert(text, pos);
  }

  public void insertText(String text, int pos)
  {
    jTextArea.insert(text, pos);
  }

  public Dimension minimumSize()
  {
    return jTextArea.getMinimumSize();
  }

  public Dimension preferredSize()
  {
    return jTextArea.getPreferredSize();
  }

  public Dimension minimumSize(int rows, int cols)
  {
      return jTextArea.getMinimumSize();
  }

  public Dimension preferredSize(int rows, int cols)
  {
      return jTextArea.getPreferredSize();
  }

  public void replaceRange(String text, int start, int end)
  {
    jTextArea.replaceRange(text, start, end);
  }

  public void replaceText(String text, int start, int end)
  {
    jTextArea.replaceRange(text, start, end);
  }

  public long filterEvents(long filter)
  {
    // TODO Auto-generated method stub
    return 0;
  }

  public int getCaretPosition()
  {
    return jTextArea.getCaretPosition();
  }

  public Rectangle getCharacterBounds(int pos)
  {
    Rectangle r;
    try
      {
        return jTextArea.modelToView(pos);
      }
    catch (BadLocationException ex)
      {
        r = null;
      }
    return r;
  }

  public int getIndexAtPoint(int x, int y)
  {
    return jTextArea.viewToModel(new Point(x, y));
  }

  public InputMethodRequests getInputMethodRequests()
  {
    // TODO Auto-generated method stub
    return null;
  }

  public int getSelectionEnd()
  {
    return jTextArea.getSelectionEnd();
  }

  public int getSelectionStart()
  {
    return jTextArea.getSelectionStart();
  }

  public String getText()
  {
    return jTextArea.getText();
  }

  public void select(int start, int end)
  {
    jTextArea.select(start, end);
  }

  public void setCaretPosition(int pos)
  {
    jTextArea.setCaretPosition(pos);
  }

  public void setEditable(boolean editable)
  {
    jTextArea.setEditable(editable);
  }

  public void setText(String text)
  {
    jTextArea.setText(text);
  }

  public void reshape(int x, int y, int width, int height)
  {
    if (swingComponent != null)
      {
        swingComponent.getJComponent().setBounds(x, y, width, height);
        swingComponent.getJComponent().validate();
      }
  }

}
