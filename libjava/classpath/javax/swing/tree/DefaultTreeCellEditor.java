/* DefaultTreeCellEditor.java --
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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


package javax.swing.tree;

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.EventObject;

import javax.swing.DefaultCellEditor;
import javax.swing.Icon;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.event.CellEditorListener;
import javax.swing.event.EventListenerList;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;

/**
 * Participates in the tree cell editing.
 *
 * @author Andrew Selkirk
 * @author Audrius Meskauskas
 */
public class DefaultTreeCellEditor
  implements ActionListener, TreeCellEditor, TreeSelectionListener
{
  /**
   * This container that appears on the tree during editing session.
   * It contains the editing component displays various other editor -
   * specific parts like editing icon.
   */
  public class EditorContainer extends Container
  {
   /**
    * Use v 1.5 serial version UID for interoperability.
    */
    static final long serialVersionUID = 6470339600449699810L;

    /**
     * Creates an <code>EditorContainer</code> object.
     */
    public EditorContainer()
    {
      setLayout(null);
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
     * Overrides Container.paint to paint the node's icon and use the selection
     * color for the background.
     *
     * @param g -
     *          the specified Graphics window
     */
    public void paint(Graphics g)
    {
      // Paint editing icon.
      if (editingIcon != null)
        {
          // From the previous version, the left margin is taken as half
          // of the icon width.
          int y = Math.max(0, (getHeight() - editingIcon.getIconHeight()) / 2);
          editingIcon.paintIcon(this, g, 0, y);
        }
      // Paint border.
      Color c = getBorderSelectionColor();
      if (c != null)
        {
          g.setColor(c);
          g.drawRect(0, 0, getWidth() - 1, getHeight() - 1);
        }
      super.paint(g);
    }

    /**
     * Lays out this Container, moving the editor component to the left
     * (leaving place for the icon).
     */
    public void doLayout()
    {
      if (editingComponent != null)
        {
          editingComponent.getPreferredSize();
          editingComponent.setBounds(offset, 0, getWidth() - offset,
                                     getHeight());
        }
      }

    public Dimension getPreferredSize()
    {
      Dimension dim;
      if (editingComponent != null)
        {
          dim = editingComponent.getPreferredSize();
          dim.width += offset + 5;
          if (renderer != null)
            {
              Dimension r = renderer.getPreferredSize();
              dim.height = Math.max(dim.height, r.height);
            }
          if (editingIcon != null)
            dim.height = Math.max(dim.height, editingIcon.getIconHeight());
          dim.width = Math.max(100, dim.width);
        }
      else
        dim = new Dimension(0, 0);
      return dim;
    }
  }

  /**
   * The default text field, used in the editing sessions.
   */
  public class DefaultTextField extends JTextField
  {
   /**
    * Use v 1.5 serial version UID for interoperability.
    */
    static final long serialVersionUID = -6629304544265300143L;

    /**
     * The border of the text field.
     */
    protected Border border;

    /**
     * Creates a <code>DefaultTextField</code> object.
     *
     * @param aBorder the border to use
     */
    public DefaultTextField(Border aBorder)
    {
      border = aBorder;
    }

    /**
     * Gets the font of this component.
     * @return this component's font; if a font has not been set for
     * this component, the font of its parent is returned (if the parent
     * is not null, otherwise null is returned).
     */
    public Font getFont()
    {
      Font font = super.getFont();
      if (font == null)
        {
          Component parent = getParent();
          if (parent != null)
            return parent.getFont();
          return null;
        }
      return font;
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
     * Overrides JTextField.getPreferredSize to return the preferred size
     * based on current font, if set, or else use renderer's font.
     *
     * @return the Dimension of this textfield.
     */
    public Dimension getPreferredSize()
    {
      Dimension size = super.getPreferredSize();
      if (renderer != null && DefaultTreeCellEditor.this.getFont() == null)
        {
          size.height = renderer.getPreferredSize().height;
        }
      return renderer.getPreferredSize();
    }
  }

  private EventListenerList listenerList = new EventListenerList();

  /**
   * Editor handling the editing.
   */
  protected TreeCellEditor realEditor;

  /**
   * Renderer, used to get border and offsets from.
   */
  protected DefaultTreeCellRenderer renderer;

  /**
   * Editing container, will contain the editorComponent.
   */
  protected Container editingContainer;

  /**
   * Component used in editing, obtained from the editingContainer.
   */
  protected transient Component editingComponent;

  /**
   * As of Java 2 platform v1.4 this field should no longer be used.
   * If you wish to provide similar behavior you should directly
   * override isCellEditable.
   */
  protected boolean canEdit;

  /**
   * Used in editing. Indicates x position to place editingComponent.
   */
  protected transient int offset;

  /**
   * JTree instance listening too.
   */
  protected transient JTree tree;

  /**
   * Last path that was selected.
   */
  protected transient TreePath lastPath;

  /**
   * Used before starting the editing session.
   */
  protected transient javax.swing.Timer timer;

  /**
   * Row that was last passed into getTreeCellEditorComponent.
   */
  protected transient int lastRow;

  /**
   * True if the border selection color should be drawn.
   */
  protected Color borderSelectionColor;

  /**
   * Icon to use when editing.
   */
  protected transient Icon editingIcon;

  /**
   * Font to paint with, null indicates font of renderer is to be used.
   */
  protected Font font;

  /**
   * Constructs a DefaultTreeCellEditor object for a JTree using the
   * specified renderer and a default editor. (Use this constructor
   * for normal editing.)
   *
   * @param tree - a JTree object
   * @param renderer - a DefaultTreeCellRenderer object
   */
  public DefaultTreeCellEditor(JTree tree, DefaultTreeCellRenderer renderer)
  {
    this(tree, renderer, null);
  }

  /**
   * Constructs a DefaultTreeCellEditor  object for a JTree using the specified
   * renderer and the specified editor. (Use this constructor
   * for specialized editing.)
   *
   * @param tree - a JTree object
   * @param renderer - a DefaultTreeCellRenderer object
   * @param editor - a TreeCellEditor object
   */
  public DefaultTreeCellEditor(JTree tree, DefaultTreeCellRenderer renderer,
                               TreeCellEditor editor)
  {
    this.renderer = renderer;
    realEditor = editor;
    if (realEditor == null)
      realEditor = createTreeCellEditor();
    editingContainer = createContainer();
    setTree(tree);
    Color c = UIManager.getColor("Tree.editorBorderSelectionColor");
    setBorderSelectionColor(c);
  }

  /**
   * writeObject
   *
   * @param value0
   *          TODO
   * @exception IOException
   *              TODO
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
   * Sets the color to use for the border.
   * @param newColor - the new border color
   */
  public void setBorderSelectionColor(Color newColor)
  {
    this.borderSelectionColor = newColor;
  }

  /**
   * Returns the color the border is drawn.
   * @return Color
   */
  public Color getBorderSelectionColor()
  {
    return borderSelectionColor;
  }

  /**
   * Sets the font to edit with. null indicates the renderers
   * font should be used. This will NOT override any font you have
   * set in the editor the receiver was instantied with. If null for
   * an editor was passed in, a default editor will be created that
   * will pick up this font.
   *
   * @param font - the editing Font
   */
  public void setFont(Font font)
  {
    if (font != null)
      this.font = font;
    else
      this.font = renderer.getFont();
  }

  /**
   * Gets the font used for editing.
   *
   * @return the editing font
   */
  public Font getFont()
  {
    return font;
  }

  /**
   * Configures the editor. Passed onto the realEditor.
   * Sets an initial value for the editor. This will cause
   * the editor to stopEditing and lose any partially edited value
   * if the editor is editing when this method is called.
   * Returns the component that should be added to the client's Component
   * hierarchy. Once installed in the client's hierarchy this component will
   * then be able to draw and receive user input.
   *
   * @param tree - the JTree that is asking the editor to edit; this parameter can be null
   * @param value - the value of the cell to be edited
   * @param isSelected - true is the cell is to be rendered with selection highlighting
   * @param expanded - true if the node is expanded
   * @param leaf - true if the node is a leaf node
   * @param row - the row index of the node being edited
   *
   * @return the component for editing
   */
  public Component getTreeCellEditorComponent(JTree tree, Object value,
                                              boolean isSelected,
                                              boolean expanded,
                                              boolean leaf, int row)
  {
    setTree(tree);
    lastRow = row;
    determineOffset(tree, value, isSelected, expanded, leaf, row);
    if (editingComponent != null)
      editingContainer.remove(editingComponent);

    editingComponent = realEditor.getTreeCellEditorComponent(tree, value,
                                                             isSelected,
                                                             expanded, leaf,
                                                             row);
    Font f = getFont();
    if (f == null)
      {
        if (renderer != null)
          f = renderer.getFont();
        if (f == null)
          f = tree.getFont();
      }
    editingContainer.setFont(f);
    prepareForEditing();
    return editingContainer;
  }

  /**
   * Returns the value currently being edited (requests it from the
   * {@link #realEditor}.
   *
   * @return the value currently being edited
   */
  public Object getCellEditorValue()
  {
    return realEditor.getCellEditorValue();
  }

  /**
   * If the realEditor returns true to this message, prepareForEditing
   * is messaged and true is returned.
   *
   * @param event - the event the editor should use to consider whether to
   * begin editing or not
   * @return true if editing can be started
   */
  public boolean isCellEditable(EventObject event)
  {
    boolean ret = false;
    boolean ed = false;
    if (event != null)
      {
        if (event.getSource() instanceof JTree)
          {
            setTree((JTree) event.getSource());
            if (event instanceof MouseEvent)
              {
                MouseEvent me = (MouseEvent) event;
                TreePath path = tree.getPathForLocation(me.getX(), me.getY());
                ed = lastPath != null && path != null && lastPath.equals(path);
                if (path != null)
                  {
                    lastRow = tree.getRowForPath(path);
                    Object val = path.getLastPathComponent();
                    boolean isSelected = tree.isRowSelected(lastRow);
                    boolean isExpanded = tree.isExpanded(path);
                    TreeModel m = tree.getModel();
                    boolean isLeaf = m.isLeaf(val);
                    determineOffset(tree, val, isSelected, isExpanded, isLeaf,
                                    lastRow);
                  }
              }
          }
      }
    if (! realEditor.isCellEditable(event))
      ret = false;
    else
      {
        if (canEditImmediately(event))
          ret = true;
        else if (ed && shouldStartEditingTimer(event))
          startEditingTimer();
        else if (timer != null && timer.isRunning())
          timer.stop();
      }
    if (ret)
      prepareForEditing();
    return ret;

  }

  /**
   * Messages the realEditor for the return value.
   *
   * @param event -
   *          the event the editor should use to start editing
   * @return true if the editor would like the editing cell to be selected;
   *         otherwise returns false
   */
  public boolean shouldSelectCell(EventObject event)
  {
    return true;
  }

  /**
   * If the realEditor will allow editing to stop, the realEditor
   * is removed and true is returned, otherwise false is returned.
   * @return true if editing was stopped; false otherwise
   */
  public boolean stopCellEditing()
  {
    boolean ret = false;
    if (realEditor.stopCellEditing())
      {
        finish();
        ret = true;
      }
    return ret;
  }

  /**
   * Messages cancelCellEditing to the realEditor and removes it
   * from this instance.
   */
  public void cancelCellEditing()
  {
    realEditor.cancelCellEditing();
    finish();
  }

  private void finish()
  {
    if (editingComponent != null)
      editingContainer.remove(editingComponent);
    editingComponent = null;
  }

  /**
   * Adds a <code>CellEditorListener</code> object to this editor.
   *
   * @param listener
   *          the listener to add
   */
  public void addCellEditorListener(CellEditorListener listener)
  {
    realEditor.addCellEditorListener(listener);
  }

  /**
   * Removes a <code>CellEditorListener</code> object.
   *
   * @param listener the listener to remove
   */
  public void removeCellEditorListener(CellEditorListener listener)
  {
    realEditor.removeCellEditorListener(listener);
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
   * Resets lastPath.
   *
   * @param e - the event that characterizes the change.
   */
  public void valueChanged(TreeSelectionEvent e)
  {
    if (tree != null)
      {
        if (tree.getSelectionCount() == 1)
          lastPath = tree.getSelectionPath();
        else
          lastPath = null;
      }
    // TODO: We really should do the following here, but can't due
    // to buggy DefaultTreeSelectionModel. This selection model
    // should only fire if the selection actually changes.
//    if (timer != null)
//      timer.stop();
  }

  /**
   * Messaged when the timer fires.
   *
   * @param e the event that characterizes the action.
   */
  public void actionPerformed(ActionEvent e)
  {
    if (tree != null && lastPath != null)
      tree.startEditingAtPath(lastPath);
  }

  /**
   * Sets the tree currently editing for. This is needed to add a selection
   * listener.
   *
   * @param newTree -
   *          the new tree to be edited
   */
  protected void setTree(JTree newTree)
  {
    if (tree != newTree)
      {
        if (tree != null)
          tree.removeTreeSelectionListener(this);
        tree = newTree;
        if (tree != null)
          tree.addTreeSelectionListener(this);

        if (timer != null)
          timer.stop();
      }
  }

  /**
   * Returns true if event is a MouseEvent and the click count is 1.
   *
   * @param event - the event being studied
   * @return true if editing should start
   */
  protected boolean shouldStartEditingTimer(EventObject event)
  {
    boolean ret = false;
    if (event instanceof MouseEvent)
      {
        MouseEvent me = (MouseEvent) event;
        ret = SwingUtilities.isLeftMouseButton(me) && me.getClickCount() == 1
              && inHitRegion(me.getX(), me.getY());
      }
    return ret;
  }

  /**
   * Starts the editing timer (if one installed).
   */
  protected void startEditingTimer()
  {
    if (timer == null)
      {
        timer = new Timer(1200, this);
        timer.setRepeats(false);
      }
    timer.start();
  }

  /**
   * Returns true if event is null, or it is a MouseEvent with
   * a click count > 2 and inHitRegion returns true.
   *
   * @param event - the event being studied
   * @return true if event is null, or it is a MouseEvent with
   * a click count > 2 and inHitRegion returns true
   */
  protected boolean canEditImmediately(EventObject event)
  {
    if (event == null || !(event instanceof MouseEvent) || (((MouseEvent) event).
        getClickCount() > 2 && inHitRegion(((MouseEvent) event).getX(),
                                         ((MouseEvent) event).getY())))
      return true;
    return false;
  }

  /**
   * Returns true if the passed in location is a valid mouse location
   * to start editing from. This is implemented to return false if x is
   * less than or equal to the width of the icon and icon
   * gap displayed by the renderer. In other words this returns true if
   * the user clicks over the text part displayed by the renderer, and
   * false otherwise.
   *
   * @param x - the x-coordinate of the point
   * @param y - the y-coordinate of the point
   *
   * @return true if the passed in location is a valid mouse location
   */
  protected boolean inHitRegion(int x, int y)
  {
    Rectangle bounds = tree.getPathBounds(lastPath);
    return bounds.contains(x, y);
  }

  /**
   * determineOffset
   * @param tree -
   * @param value -
   * @param isSelected -
   * @param expanded -
   * @param leaf -
   * @param row -
   */
  protected void determineOffset(JTree tree, Object value, boolean isSelected,
                                 boolean expanded, boolean leaf, int row)
  {
    if (renderer != null)
      {
        if (leaf)
          editingIcon = renderer.getLeafIcon();
        else if (expanded)
          editingIcon = renderer.getOpenIcon();
        else
          editingIcon = renderer.getClosedIcon();
        if (editingIcon != null)
          offset = renderer.getIconTextGap() + editingIcon.getIconWidth();
        else
          offset = renderer.getIconTextGap();
      }
    else
      {
        editingIcon = null;
        offset = 0;
      }
  }

  /**
   * Invoked just before editing is to start. Will add the
   * editingComponent to the editingContainer.
   */
  protected void prepareForEditing()
  {
    if (editingComponent != null)
      editingContainer.add(editingComponent);
  }

  /**
   * Creates the container to manage placement of editingComponent.
   *
   * @return the container to manage the placement of the editingComponent.
   */
  protected Container createContainer()
  {
    return new DefaultTreeCellEditor.EditorContainer();
  }

  /**
   * This is invoked if a TreeCellEditor is not supplied in the constructor.
   * It returns a TextField editor.
   *
   * @return a new TextField editor
   */
  protected TreeCellEditor createTreeCellEditor()
  {
    Border border = UIManager.getBorder("Tree.editorBorder");
    JTextField tf = new DefaultTreeCellEditor.DefaultTextField(border);
    DefaultCellEditor editor = new DefaultCellEditor(tf);
    editor.setClickCountToStart(1);
    realEditor = editor;
    return editor;
  }
}
