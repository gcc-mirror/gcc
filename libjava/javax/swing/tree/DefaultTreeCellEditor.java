/* DefaultTreeCellEditor.java --
   Copyright (C) 2002, 2004  Free Software Foundation, Inc.

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


package javax.swing.tree;

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.EventObject;

import javax.swing.Icon;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.border.Border;
import javax.swing.event.CellEditorListener;
import javax.swing.event.EventListenerList;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;

/**
 * DefaultTreeCellEditor
 * @author Andrew Selkirk
 */
public class DefaultTreeCellEditor
  implements ActionListener, TreeCellEditor, TreeSelectionListener
{
  /**
   * EditorContainer
   */
  public class EditorContainer extends Container
  {
    /**
     * Creates an <code>EditorContainer</code> object.
     */
    public EditorContainer()
    {
      // Do nothing here.
    }

    /**
     * This method only exists for API compatibility and is useless as it does
     * nothing. It got probably introduced by accident.
     */
    public void EditorContainer()
    {
      // Do nothing here.
    }

    /**
     * getPreferredSize
     * @return Dimension
     */
    public Dimension getPreferredSize()
    {
      return null; // TODO
    }

    /**
     * paint
     * @param value0 TODO
     */
    public void paint(Graphics value0)
    {
      // TODO
    }

    /**
     * doLayout
     */
    public void doLayout()
    {
      // TODO
    }
  }

  /**
   * DefaultTextField
   */
  public class DefaultTextField extends JTextField
  {
    /**
     * border
     */
    protected Border border;

    /**
     * Creates a <code>DefaultTextField</code> object.
     *
     * @param border the border to use
     */
    public DefaultTextField(Border border)
    {
      this.border = border;
    }

    /**
     * getFont
     * @return Font
     */
    public Font getFont()
    {
      return null; // TODO
    }

    /**
     * Returns the border of the text field.
     *
     * @return the border
     */
    public Border getBorder()
    {
      return border;
    }

    /**
     * getPreferredSize
     * @return Dimension
     */
    public Dimension getPreferredSize()
    {
      return null; // TODO
    }
  }

  private EventListenerList listenerList = new EventListenerList();

  /**
   * realEditor
   */
  protected TreeCellEditor realEditor;

  /**
   * renderer
   */
  protected DefaultTreeCellRenderer renderer;

  /**
   * editingContainer
   */
  protected Container editingContainer;

  /**
   * editingComponent
   */
  protected transient Component editingComponent;

  /**
   * canEdit
   */
  protected boolean canEdit;

  /**
   * offset
   */
  protected transient int offset;

  /**
   * tree
   */
  protected transient JTree tree;

  /**
   * lastPath
   */
  protected transient TreePath lastPath;

  /**
   * timer
   */
  protected transient javax.swing.Timer timer; // TODO

  /**
   * lastRow
   */
  protected transient int lastRow;

  /**
   * borderSelectionColor
   */
  protected Color borderSelectionColor;

  /**
   * editingIcon
   */
  protected transient Icon editingIcon;

  /**
   * font
   */
  protected Font font;

  /**
   * Constructor DefaultTreeCellEditor
   * @param value0 TODO
   * @param value1 TODO
   */
  public DefaultTreeCellEditor(JTree value0, DefaultTreeCellRenderer value1)
  {
    // TODO
  }

  /**
   * Constructor DefaultTreeCellEditor
   * @param value0 TODO
   * @param value1 TODO
   * @param value2 TODO
   */
  public DefaultTreeCellEditor(JTree value0, DefaultTreeCellRenderer value1,
                               TreeCellEditor value2)
  {
    // TODO
  }

  /**
   * writeObject
   * @param value0 TODO
   * @exception IOException TODO
   */
  private void writeObject(ObjectOutputStream value0) throws IOException
  {
    // TODO
  }

  /**
   * readObject
   * @param value0 TODO
   * @exception IOException TODO
   * @exception ClassNotFoundException TODO
   */
  private void readObject(ObjectInputStream value0)
    throws IOException, ClassNotFoundException
  {
    // TODO
  }

  /**
   * setBorderSelectionColor
   * @param value0 TODO
   */
  public void setBorderSelectionColor(Color value0)
  {
    // TODO
  }

  /**
   * getBorderSelectionColor
   * @return Color
   */
  public Color getBorderSelectionColor()
  {
    return null; // TODO
  }

  /**
   * setFont
   * @param value0 TODO
   */
  public void setFont(Font value0)
  {
    // TODO
  }

  /**
   * getFont
   * @return Font
   */
  public Font getFont()
  {
    return null; // TODO
  }

  /**
   * getTreeCellEditorComponent
   * @param value0 TODO
   * @param value1 TODO
   * @param value2 TODO
   * @param value3 TODO
   * @param value4 TODO
   * @param value5 TODO
   * @return Component
   */
  public Component getTreeCellEditorComponent(JTree value0, Object value1,
                                              boolean value2, boolean value3,
                                              boolean value4, int value5)
  {
    return null; // TODO
  }

  /**
   * getCellEditorValue
   * @return Object
   */
  public Object getCellEditorValue()
  {
    return null; // TODO
  }

  /**
   * isCellEditable
   * @param value0 TODO
   * @return boolean
   */
  public boolean isCellEditable(EventObject value0)
  {
    return false; // TODO
  }

  /**
   * shouldSelectCell
   * @param value0 TODO
   * @return boolean
   */
  public boolean shouldSelectCell(EventObject value0)
  {
    return false; // TODO
  }

  /**
   * stopCellEditing
   * @return boolean
   */
  public boolean stopCellEditing()
  {
    return false; // TODO
  }

  /**
   * cancelCellEditing
   */
  public void cancelCellEditing()
  {
    // TODO
  }

  /**
   * Adds a <code>CellEditorListener</code> object to this editor.
   *
   * @param listener the listener to add
   */
  public void addCellEditorListener(CellEditorListener listener)
  {
    listenerList.add(CellEditorListener.class, listener);
  }

  /**
   * Removes a <code>CellEditorListener</code> object.
   *
   * @param listener the listener to remove
   */
  public void removeCellEditorListener(CellEditorListener listener)
  {
    listenerList.remove(CellEditorListener.class, listener);
  }

  /**
   * Returns all added <code>CellEditorListener</code> objects to this editor.
   *
   * @return an array of listeners
   *
   * @since 1.4
   */
  public CellEditorListener[] getCellEditorListeners()
  {
    return (CellEditorListener[]) listenerList.getListeners(CellEditorListener.class);
  }

  /**
   * valueChanged
   * @param value0 TODO
   */
  public void valueChanged(TreeSelectionEvent value0)
  {
    // TODO
  }

  /**
   * actionPerformed
   * @param value0 TODO
   */
  public void actionPerformed(ActionEvent value0)
  {
    // TODO
  }

  /**
   * setTree
   * @param value0 TODO
   */
  protected void setTree(JTree value0)
  {
    // TODO
  }

  /**
   * shouldStartEditingTimer
   * @param value0 TODO
   * @return boolean
   */
  protected boolean shouldStartEditingTimer(EventObject value0)
  {
    return false; // TODO
  }

  /**
   * startEditingTimer
   */
  protected void startEditingTimer()
  {
    // TODO
  }

  /**
   * canEditImmediately
   * @param value0 TODO
   * @return boolean
   */
  protected boolean canEditImmediately(EventObject value0)
  {
    return false; // TODO
  }

  /**
   * inHitRegion
   * @param value0 TODO
   * @param value1 TODO
   * @return boolean
   */
  protected boolean inHitRegion(int value0, int value1)
  {
    return false; // TODO
  }

  /**
   * determineOffset
   * @param value0 TODO
   * @param value1 TODO
   * @param value2 TODO
   * @param value3 TODO
   * @param value4 TODO
   * @param value5 TODO
   */
  protected void determineOffset(JTree value0, Object value1, boolean value2,
                                 boolean value3, boolean value4, int value5)
  {
    // TODO
  }

  /**
   * prepareForEditing
   */
  protected void prepareForEditing()
  {
    // TODO
  }

  /**
   * createContainer
   * @return Container
   */
  protected Container createContainer()
  {
    return null; // TODO
  }

  /**
   * createTreeCellEditor
   * @return TreeCellEditor
   */
  protected TreeCellEditor createTreeCellEditor()
  {
    return null; // TODO
  }
}
