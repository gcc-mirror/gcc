/* SwingListPeer.java -- A Swing based peer for AWT lists
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

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.List;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.FocusEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.peer.ListPeer;

import javax.swing.DefaultListModel;
import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.JScrollPane;
import javax.swing.ListSelectionModel;

public class SwingListPeer
    extends SwingComponentPeer
    implements ListPeer
{

  /**
   * A spezialized Swing scroller used to hold the list.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  private class SwingList
    extends JScrollPane
    implements SwingComponent
  {

    SwingList(Component comp)
    {
      super(comp, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
            JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
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
      ev.setSource(this);
      dispatchEvent(ev);
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
      processMouseMotionEvent(ev);
    }

    /**
     * Handles key events by forwarding it to <code>processKeyEvent()</code>.
     *
     * @param ev the mouse event
     */
    public void handleKeyEvent(KeyEvent ev)
    {
      processKeyEvent(ev);
    }

    /**
     * Handles focus events by forwarding it to <code>processFocusEvent()</code>.
     *
     * @param ev the Focus event
     */
    public void handleFocusEvent(FocusEvent ev)
    {
      processFocusEvent(ev);
    }


    /**
     * Overridden so that this method returns the correct value even without a
     * peer.
     *
     * @return the screen location of the button
     */
    public Point getLocationOnScreen()
    {
      return SwingListPeer.this.getLocationOnScreen();
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
      if (SwingListPeer.this.awtComponent != null)
        retVal = SwingListPeer.this.awtComponent.isShowing();
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
      return SwingListPeer.this.createImage(w, h);
    }

    public Graphics getGraphics()
    {
      return SwingListPeer.this.getGraphics();
    }

    public Container getParent()
    {
      Container par = null;
      if (SwingListPeer.this.awtComponent != null)
        par = SwingListPeer.this.awtComponent.getParent();
      return par;
    }
  }

  /**
   * The actual Swing JList.
   */
  private JList jList;

  private DefaultListModel listModel;

  public SwingListPeer(List list)
  {
    super();
    listModel = new DefaultListModel();
    jList = new JList(listModel);
    SwingList swingList = new SwingList(jList);
    init(list, swingList);

    // Pull over the items from the list.
    String[] items = list.getItems();
    for (int i = 0 ; i < items.length; i++)
      addItem(items[i], i);
  }

  public void add(String item, int index)
  {
    if (listModel != null)
      listModel.add(index, item);
  }

  public void addItem(String item, int index)
  {
    if (listModel != null)
      listModel.add(index, item);
  }

  public void clear()
  {
    if (listModel != null)
      listModel.clear();
  }

  public void delItems(int startIndex, int endIndex)
  {
    if (listModel != null)
      listModel.removeRange(startIndex, endIndex);
  }

  public void deselect(int index)
  {
    if (jList != null)
      {
        jList.getSelectionModel().removeSelectionInterval(index, index);
      }
  }

  public Dimension getMinimumSize(int s)
  {
    Dimension d = null;
    if (jList != null)
      {
        d = jList.getComponent(s).getMinimumSize();
      }
    return d;
  }

  public Dimension getPreferredSize(int s)
  {
    Dimension d = null;
    if (jList != null)
      {
        d = jList.getComponent(s).getPreferredSize();
      }
    return d;
  }

  public int[] getSelectedIndexes()
  {
    int[] sel = null;
    if (jList != null)
      {
        sel = jList.getSelectedIndices();
      }
    return sel;
  }

  public void makeVisible(int index)
  {
    if (jList != null)
      {
        Component comp = jList.getComponent(index);
        jList.scrollRectToVisible(comp.getBounds());
      }
  }

  public Dimension minimumSize(int s)
  {
    Dimension d = null;
    if (jList != null)
      {
        d = jList.getComponent(s).getMinimumSize();
      }
    return d;
  }

  public Dimension preferredSize(int s)
  {
    Dimension d = null;
    if (jList != null)
      {
        d = jList.getComponent(s).getPreferredSize();
      }
    return d;
  }

  public void removeAll()
  {
    if (jList != null)
      {
        jList.removeAll();
      }
  }

  public void select(int index)
  {
    if (jList != null)
      {
        jList.setSelectedIndex(index);
      }
  }

  public void setMultipleMode(boolean multi)
  {
    if (jList != null)
      {
        jList.setSelectionMode(multi
                               ? ListSelectionModel.MULTIPLE_INTERVAL_SELECTION
                               : ListSelectionModel.SINGLE_SELECTION);
      }
  }

  public void setMultipleSelections(boolean multi)
  {
    if (jList != null)
      {
        jList.setSelectionMode(multi
                               ? ListSelectionModel.MULTIPLE_INTERVAL_SELECTION
                               : ListSelectionModel.SINGLE_SELECTION);
      }
  }

  public void reshape(int x, int y, int width, int height)
  {
    if (swingComponent != null)
      {
        swingComponent.getJComponent().setBounds(x, y, width, height);
        swingComponent.getJComponent().validate();
      }
  }

  protected void peerPaint(Graphics g, boolean update)
  {
    super.peerPaint(g, update);
    jList.doLayout();
    jList.list();

    Rectangle r = getBounds();
    g.setColor(Color.RED);
    g.drawRect(r.x, r.y, r.width, r.height);
  }
}
