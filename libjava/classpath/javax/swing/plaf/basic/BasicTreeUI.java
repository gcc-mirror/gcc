/* BasicTreeUI.java --
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


package javax.swing.plaf.basic;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Enumeration;
import java.util.Hashtable;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.CellRendererPane;
import javax.swing.Icon;
import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.KeyStroke;
import javax.swing.LookAndFeel;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.UIManager;
import javax.swing.event.CellEditorListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.MouseInputListener;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeExpansionListener;
import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.plaf.ActionMapUIResource;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.InputMapUIResource;
import javax.swing.plaf.TreeUI;
import javax.swing.text.Caret;
import javax.swing.tree.AbstractLayoutCache;
import javax.swing.tree.DefaultTreeCellEditor;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.FixedHeightLayoutCache;
import javax.swing.tree.TreeCellEditor;
import javax.swing.tree.TreeCellRenderer;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

/**
 * A delegate providing the user interface for <code>JTree</code> according to
 * the Basic look and feel.
 * 
 * @see javax.swing.JTree
 * @author Lillian Angel (langel@redhat.com)
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class BasicTreeUI extends TreeUI
{
  /** Collapse Icon for the tree. */
  protected transient Icon collapsedIcon;

  /** Expanded Icon for the tree. */
  protected transient Icon expandedIcon;

  /** Distance between left margin and where vertical dashes will be drawn. */
  protected int leftChildIndent;

  /**
   * Distance between leftChildIndent and where cell contents will be drawn.
   */
  protected int rightChildIndent;

  /**
   * Total fistance that will be indented. The sum of leftChildIndent and
   * rightChildIndent .
   */
  protected int totalChildIndent;

  /** Index of the row that was last selected. */
  protected int lastSelectedRow;

  /** Component that we're going to be drawing onto. */
  protected JTree tree;

  /** Renderer that is being used to do the actual cell drawing. */
  protected transient TreeCellRenderer currentCellRenderer;

  /**
   * Set to true if the renderer that is currently in the tree was created by
   * this instance.
   */
  protected boolean createdRenderer;

  /** Editor for the tree. */
  protected transient TreeCellEditor cellEditor;

  /**
   * Set to true if editor that is currently in the tree was created by this
   * instance.
   */
  protected boolean createdCellEditor;

  /**
   * Set to false when editing and shouldSelectCall() returns true meaning the
   * node should be selected before editing, used in completeEditing.
   */
  protected boolean stopEditingInCompleteEditing;

  /** Used to paint the TreeCellRenderer. */
  protected CellRendererPane rendererPane;

  /** Size needed to completely display all the nodes. */
  protected Dimension preferredSize;

  /** Minimum size needed to completely display all the nodes. */
  protected Dimension preferredMinSize;

  /** Is the preferredSize valid? */
  protected boolean validCachedPreferredSize;

  /** Object responsible for handling sizing and expanded issues. */
  protected AbstractLayoutCache treeState;

  /** Used for minimizing the drawing of vertical lines. */
  protected Hashtable drawingCache;

  /**
   * True if doing optimizations for a largeModel. Subclasses that don't support
   * this may wish to override createLayoutCache to not return a
   * FixedHeightLayoutCache instance.
   */
  protected boolean largeModel;

  /** Responsible for telling the TreeState the size needed for a node. */
  protected AbstractLayoutCache.NodeDimensions nodeDimensions;

  /** Used to determine what to display. */
  protected TreeModel treeModel;

  /** Model maintaining the selection. */
  protected TreeSelectionModel treeSelectionModel;

  /**
   * How much the depth should be offset to properly calculate x locations. This
   * is based on whether or not the root is visible, and if the root handles are
   * visible.
   */
  protected int depthOffset;

  /**
   * When editing, this will be the Component that is doing the actual editing.
   */
  protected Component editingComponent;

  /** Path that is being edited. */
  protected TreePath editingPath;

  /**
   * Row that is being edited. Should only be referenced if editingComponent is
   * null.
   */
  protected int editingRow;

  /** Set to true if the editor has a different size than the renderer. */
  protected boolean editorHasDifferentSize;

  /** The action listener for the editor's Timer. */
  Timer editorTimer = new EditorUpdateTimer();

  /** The new value of the node after editing. */
  Object newVal;

  /** The action bound to KeyStrokes. */
  TreeAction action;

  /** Boolean to keep track of editing. */
  boolean isEditing;

  /** The current path of the visible nodes in the tree. */
  TreePath currentVisiblePath;

  /** The gap between the icon and text. */
  int gap = 4;

  /** The max height of the nodes in the tree. */
  int maxHeight = 0;

  /** Listeners */
  private PropertyChangeListener propertyChangeListener;

  private FocusListener focusListener;

  private TreeSelectionListener treeSelectionListener;

  private MouseListener mouseListener;

  private KeyListener keyListener;

  private PropertyChangeListener selectionModelPropertyChangeListener;

  private ComponentListener componentListener;

  CellEditorListener cellEditorListener;

  private TreeExpansionListener treeExpansionListener;

  private TreeModelListener treeModelListener;

  /**
   * Creates a new BasicTreeUI object.
   */
  public BasicTreeUI()
  {
    validCachedPreferredSize = false;
    drawingCache = new Hashtable();
    nodeDimensions = createNodeDimensions();
    configureLayoutCache();

    propertyChangeListener = createPropertyChangeListener();
    focusListener = createFocusListener();
    treeSelectionListener = createTreeSelectionListener();
    mouseListener = createMouseListener();
    keyListener = createKeyListener();
    selectionModelPropertyChangeListener = createSelectionModelPropertyChangeListener();
    componentListener = createComponentListener();
    cellEditorListener = createCellEditorListener();
    treeExpansionListener = createTreeExpansionListener();
    treeModelListener = createTreeModelListener();

    editingRow = -1;
    lastSelectedRow = -1;
  }

  /**
   * Returns an instance of the UI delegate for the specified component.
   * 
   * @param c
   *          the <code>JComponent</code> for which we need a UI delegate for.
   * @return the <code>ComponentUI</code> for c.
   */
  public static ComponentUI createUI(JComponent c)
  {
    return new BasicTreeUI();
  }

  /**
   * Returns the Hash color.
   * 
   * @return the <code>Color</code> of the Hash.
   */
  protected Color getHashColor()
  {
    return UIManager.getColor("Tree.hash");
  }

  /**
   * Sets the Hash color.
   * 
   * @param color
   *          the <code>Color</code> to set the Hash to.
   */
  protected void setHashColor(Color color)
  {
    // FIXME: Putting something in the UIDefaults map is certainly wrong.
    UIManager.put("Tree.hash", color);
  }

  /**
   * Sets the left child's indent value.
   * 
   * @param newAmount
   *          is the new indent value for the left child.
   */
  public void setLeftChildIndent(int newAmount)
  {
    leftChildIndent = newAmount;
  }

  /**
   * Returns the indent value for the left child.
   * 
   * @return the indent value for the left child.
   */
  public int getLeftChildIndent()
  {
    return leftChildIndent;
  }

  /**
   * Sets the right child's indent value.
   * 
   * @param newAmount
   *          is the new indent value for the right child.
   */
  public void setRightChildIndent(int newAmount)
  {
    rightChildIndent = newAmount;
  }

  /**
   * Returns the indent value for the right child.
   * 
   * @return the indent value for the right child.
   */
  public int getRightChildIndent()
  {
    return rightChildIndent;
  }

  /**
   * Sets the expanded icon.
   * 
   * @param newG
   *          is the new expanded icon.
   */
  public void setExpandedIcon(Icon newG)
  {
    expandedIcon = newG;
  }

  /**
   * Returns the current expanded icon.
   * 
   * @return the current expanded icon.
   */
  public Icon getExpandedIcon()
  {
    return expandedIcon;
  }

  /**
   * Sets the collapsed icon.
   * 
   * @param newG
   *          is the new collapsed icon.
   */
  public void setCollapsedIcon(Icon newG)
  {
    collapsedIcon = newG;
  }

  /**
   * Returns the current collapsed icon.
   * 
   * @return the current collapsed icon.
   */
  public Icon getCollapsedIcon()
  {
    return collapsedIcon;
  }

  /**
   * Updates the componentListener, if necessary.
   * 
   * @param largeModel
   *          sets this.largeModel to it.
   */
  protected void setLargeModel(boolean largeModel)
  {
    if (largeModel != this.largeModel)
      {
        tree.removeComponentListener(componentListener);
        this.largeModel = largeModel;
        tree.addComponentListener(componentListener);
      }
  }

  /**
   * Returns true if largeModel is set
   * 
   * @return true if largeModel is set, otherwise false.
   */
  protected boolean isLargeModel()
  {
    return largeModel;
  }

  /**
   * Sets the row height.
   * 
   * @param rowHeight
   *          is the height to set this.rowHeight to.
   */
  protected void setRowHeight(int rowHeight)
  {
    if (rowHeight == 0)
      rowHeight = Math.max(getMaxHeight(tree), 20);
    treeState.setRowHeight(rowHeight);
  }

  /**
   * Returns the current row height.
   * 
   * @return current row height.
   */
  protected int getRowHeight()
  {
    return treeState.getRowHeight();
  }

  /**
   * Sets the TreeCellRenderer to <code>tcr</code>. This invokes
   * <code>updateRenderer</code>.
   * 
   * @param tcr
   *          is the new TreeCellRenderer.
   */
  protected void setCellRenderer(TreeCellRenderer tcr)
  {
    currentCellRenderer = tcr;
    updateRenderer();
  }

  /**
   * Return currentCellRenderer, which will either be the trees renderer, or
   * defaultCellRenderer, which ever was not null.
   * 
   * @return the current Cell Renderer
   */
  protected TreeCellRenderer getCellRenderer()
  {
    if (currentCellRenderer != null)
      return currentCellRenderer;

    return createDefaultCellRenderer();
  }

  /**
   * Sets the tree's model.
   * 
   * @param model
   *          to set the treeModel to.
   */
  protected void setModel(TreeModel model)
  {
    tree.setModel(model);
    treeModel = tree.getModel();
  }

  /**
   * Returns the tree's model
   * 
   * @return treeModel
   */
  protected TreeModel getModel()
  {
    return treeModel;
  }

  /**
   * Sets the root to being visible.
   * 
   * @param newValue
   *          sets the visibility of the root
   */
  protected void setRootVisible(boolean newValue)
  {
    tree.setRootVisible(newValue);
  }

  /**
   * Returns true if the root is visible.
   * 
   * @return true if the root is visible.
   */
  protected boolean isRootVisible()
  {
    return tree.isRootVisible();
  }

  /**
   * Determines whether the node handles are to be displayed.
   * 
   * @param newValue
   *          sets whether or not node handles should be displayed.
   */
  protected void setShowsRootHandles(boolean newValue)
  {
    tree.setShowsRootHandles(newValue);
  }

  /**
   * Returns true if the node handles are to be displayed.
   * 
   * @return true if the node handles are to be displayed.
   */
  protected boolean getShowsRootHandles()
  {
    return tree.getShowsRootHandles();
  }

  /**
   * Sets the cell editor.
   * 
   * @param editor
   *          to set the cellEditor to.
   */
  protected void setCellEditor(TreeCellEditor editor)
  {
    cellEditor = editor;
    createdCellEditor = true;
  }

  /**
   * Returns the <code>TreeCellEditor</code> for this tree.
   * 
   * @return the cellEditor for this tree.
   */
  protected TreeCellEditor getCellEditor()
  {
    return cellEditor;
  }

  /**
   * Configures the receiver to allow, or not allow, editing.
   * 
   * @param newValue
   *          sets the receiver to allow editing if true.
   */
  protected void setEditable(boolean newValue)
  {
    tree.setEditable(newValue);
  }

  /**
   * Returns true if the receiver allows editing.
   * 
   * @return true if the receiver allows editing.
   */
  protected boolean isEditable()
  {
    return tree.isEditable();
  }

  /**
   * Resets the selection model. The appropriate listeners are installed on the
   * model.
   * 
   * @param newLSM
   *          resets the selection model.
   */
  protected void setSelectionModel(TreeSelectionModel newLSM)
  {
    if (newLSM != null)
      {
        treeSelectionModel = newLSM;
        tree.setSelectionModel(treeSelectionModel);
      }
  }

  /**
   * Returns the current selection model.
   * 
   * @return the current selection model.
   */
  protected TreeSelectionModel getSelectionModel()
  {
    return treeSelectionModel;
  }

  /**
   * Returns the Rectangle enclosing the label portion that the last item in
   * path will be drawn to. Will return null if any component in path is
   * currently valid.
   * 
   * @param tree
   *          is the current tree the path will be drawn to.
   * @param path
   *          is the current path the tree to draw to.
   * @return the Rectangle enclosing the label portion that the last item in the
   *         path will be drawn to.
   */
  public Rectangle getPathBounds(JTree tree, TreePath path)
  {
    int row = -1;
    Object cell = null;
    if (path != null)
      {
        row = getRowForPath(tree, path);
        cell = path.getLastPathComponent();
      }
    return nodeDimensions.getNodeDimensions(cell, row, getLevel(cell),
                                            tree.isExpanded(path),
                                            new Rectangle());
  }

  /**
   * Returns the max height of all the nodes in the tree.
   * 
   * @param tree -
   *          the current tree
   * @return the max height.
   */
  private int getMaxHeight(JTree tree)
  {
    if (maxHeight != 0)
      return maxHeight;

    Icon e = UIManager.getIcon("Tree.openIcon");
    Icon c = UIManager.getIcon("Tree.closedIcon");
    Icon l = UIManager.getIcon("Tree.leafIcon");
    int rc = getRowCount(tree);
    int iconHeight = 0;

    for (int row = 0; row < rc; row++)
      {
        if (isLeaf(row))
          iconHeight = l.getIconHeight();
        else if (tree.isExpanded(row))
          iconHeight = e.getIconHeight();
        else
          iconHeight = c.getIconHeight();

        maxHeight = Math.max(maxHeight, iconHeight + gap);
      }

    return maxHeight;
  }

  /**
   * Returns the path for passed in row. If row is not visible null is returned.
   * 
   * @param tree
   *          is the current tree to return path for.
   * @param row
   *          is the row number of the row to return.
   * @return the path for passed in row. If row is not visible null is returned.
   */
  public TreePath getPathForRow(JTree tree, int row)
  {
    if (treeModel != null && currentVisiblePath != null)
      {
        Object[] nodes = currentVisiblePath.getPath();
        if (row < nodes.length)
          return new TreePath(getPathToRoot(nodes[row], 0));
      }
    return null;
  }

  /**
   * Returns the row that the last item identified in path is visible at. Will
   * return -1 if any of the elments in the path are not currently visible.
   * 
   * @param tree
   *          is the current tree to return the row for.
   * @param path
   *          is the path used to find the row.
   * @return the row that the last item identified in path is visible at. Will
   *         return -1 if any of the elments in the path are not currently
   *         visible.
   */
  public int getRowForPath(JTree tree, TreePath path)
  {
    int row = 0;
    Object dest = path.getLastPathComponent();
    int rowCount = getRowCount(tree);
    if (currentVisiblePath != null)
      {
        Object[] nodes = currentVisiblePath.getPath();
        while (row < rowCount)
          {
            if (dest.equals(nodes[row]))
              return row;
            row++;
          }
      }
    return -1;
  }

  /**
   * Returns the number of rows that are being displayed.
   * 
   * @param tree
   *          is the current tree to return the number of rows for.
   * @return the number of rows being displayed.
   */
  public int getRowCount(JTree tree)
  {
    if (currentVisiblePath != null)
      return currentVisiblePath.getPathCount();
    return 0;
  }

  /**
   * Returns the path to the node that is closest to x,y. If there is nothing
   * currently visible this will return null, otherwise it'll always return a
   * valid path. If you need to test if the returned object is exactly at x,y
   * you should get the bounds for the returned path and test x,y against that.
   * 
   * @param tree
   *          the tree to search for the closest path
   * @param x
   *          is the x coordinate of the location to search
   * @param y
   *          is the y coordinate of the location to search
   * @return the tree path closes to x,y.
   */
  public TreePath getClosestPathForLocation(JTree tree, int x, int y)
  {
    int row = Math.round(y / getMaxHeight(tree));
    TreePath path = getPathForRow(tree, row);

    // no row is visible at this node
    while (row > 0 && path == null)
      {
        --row;
        path = getPathForRow(tree, row);
      }

    return path;
  }

  /**
   * Returns true if the tree is being edited. The item that is being edited can
   * be returned by getEditingPath().
   * 
   * @param tree
   *          is the tree to check for editing.
   * @return true if the tree is being edited.
   */
  public boolean isEditing(JTree tree)
  {
    return isEditing;
  }

  /**
   * Stops the current editing session. This has no effect if the tree is not
   * being edited. Returns true if the editor allows the editing session to
   * stop.
   * 
   * @param tree
   *          is the tree to stop the editing on
   * @return true if the editor allows the editing session to stop.
   */
  public boolean stopEditing(JTree tree)
  {
    if (isEditing(tree))
      completeEditing(true, false, false);
    return !isEditing(tree);
  }

  /**
   * Cancels the current editing session.
   * 
   * @param tree
   *          is the tree to cancel the editing session on.
   */
  public void cancelEditing(JTree tree)
  {
    if (isEditing(tree))
      completeEditing(false, true, false);
  }

  /**
   * Selects the last item in path and tries to edit it. Editing will fail if
   * the CellEditor won't allow it for the selected item.
   * 
   * @param tree
   *          is the tree to edit on.
   * @param path
   *          is the path in tree to edit on.
   */
  public void startEditingAtPath(JTree tree, TreePath path)
  {
    startEditing(path, null);
  }

  /**
   * Returns the path to the element that is being editted.
   * 
   * @param tree
   *          is the tree to get the editing path from.
   * @return the path that is being edited.
   */
  public TreePath getEditingPath(JTree tree)
  {
    return editingPath;
  }

  /**
   * Invoked after the tree instance variable has been set, but before any
   * default/listeners have been installed.
   */
  protected void prepareForUIInstall()
  {
    // TODO: Implement this properly.
  }

  /**
   * Invoked from installUI after all the defaults/listeners have been
   * installed.
   */
  protected void completeUIInstall()
  {
    // TODO: Implement this properly.
  }

  /**
   * Invoked from uninstallUI after all the defaults/listeners have been
   * uninstalled.
   */
  protected void completeUIUninstall()
  {
    // TODO: Implement this properly.
  }

  /**
   * Installs the subcomponents of the tree, which is the renderer pane.
   */
  protected void installComponents()
  {
    currentCellRenderer = createDefaultCellRenderer();
    rendererPane = createCellRendererPane();
    createdRenderer = true;
    setCellRenderer(currentCellRenderer);
  }

  /**
   * Creates an instance of NodeDimensions that is able to determine the size of
   * a given node in the tree.
   * 
   * @return the NodeDimensions of a given node in the tree
   */
  protected AbstractLayoutCache.NodeDimensions createNodeDimensions()
  {
    return new NodeDimensionsHandler();
  }

  /**
   * Creates a listener that is reponsible for the updates the UI based on how
   * the tree changes.
   * 
   * @return the PropertyChangeListener that is reposnsible for the updates
   */
  protected PropertyChangeListener createPropertyChangeListener()
  {
    return new PropertyChangeHandler();
  }

  /**
   * Creates the listener responsible for updating the selection based on mouse
   * events.
   * 
   * @return the MouseListener responsible for updating.
   */
  protected MouseListener createMouseListener()
  {
    return new MouseHandler();
  }

  /**
   * Creates the listener that is responsible for updating the display when
   * focus is lost/grained.
   * 
   * @return the FocusListener responsible for updating.
   */
  protected FocusListener createFocusListener()
  {
    return new FocusHandler();
  }

  /**
   * Creates the listener reponsible for getting key events from the tree.
   * 
   * @return the KeyListener responsible for getting key events.
   */
  protected KeyListener createKeyListener()
  {
    return new KeyHandler();
  }

  /**
   * Creates the listener responsible for getting property change events from
   * the selection model.
   * 
   * @returns the PropertyChangeListener reponsible for getting property change
   *          events from the selection model.
   */
  protected PropertyChangeListener createSelectionModelPropertyChangeListener()
  {
    return new SelectionModelPropertyChangeHandler();
  }

  /**
   * Creates the listener that updates the display based on selection change
   * methods.
   * 
   * @return the TreeSelectionListener responsible for updating.
   */
  protected TreeSelectionListener createTreeSelectionListener()
  {
    return new TreeSelectionHandler();
  }

  /**
   * Creates a listener to handle events from the current editor
   * 
   * @return the CellEditorListener that handles events from the current editor
   */
  protected CellEditorListener createCellEditorListener()
  {
    return new CellEditorHandler();
  }

  /**
   * Creates and returns a new ComponentHandler. This is used for the large
   * model to mark the validCachedPreferredSize as invalid when the component
   * moves.
   * 
   * @return a new ComponentHandler.
   */
  protected ComponentListener createComponentListener()
  {
    return new ComponentHandler();
  }

  /**
   * Creates and returns the object responsible for updating the treestate when
   * a nodes expanded state changes.
   * 
   * @return the TreeExpansionListener responsible for updating the treestate
   */
  protected TreeExpansionListener createTreeExpansionListener()
  {
    return new TreeExpansionHandler();
  }

  /**
   * Creates the object responsible for managing what is expanded, as well as
   * the size of nodes.
   * 
   * @return the object responsible for managing what is expanded.
   */
  protected AbstractLayoutCache createLayoutCache()
  {
    return new FixedHeightLayoutCache();
  }

  /**
   * Returns the renderer pane that renderer components are placed in.
   * 
   * @return the rendererpane that render components are placed in.
   */
  protected CellRendererPane createCellRendererPane()
  {
    return new CellRendererPane();
  }

  /**
   * Creates a default cell editor.
   * 
   * @return the default cell editor.
   */
  protected TreeCellEditor createDefaultCellEditor()
  {
    if (currentCellRenderer != null)
      return new DefaultTreeCellEditor(
                                       tree,
                                       (DefaultTreeCellRenderer) currentCellRenderer,
                                       cellEditor);
    return new DefaultTreeCellEditor(
                                     tree,
                                     (DefaultTreeCellRenderer) createDefaultCellRenderer(),
                                     cellEditor);
  }

  /**
   * Returns the default cell renderer that is used to do the stamping of each
   * node.
   * 
   * @return the default cell renderer that is used to do the stamping of each
   *         node.
   */
  protected TreeCellRenderer createDefaultCellRenderer()
  {
    return new DefaultTreeCellRenderer();
  }

  /**
   * Returns a listener that can update the tree when the model changes.
   * 
   * @return a listener that can update the tree when the model changes.
   */
  protected TreeModelListener createTreeModelListener()
  {
    return new TreeModelHandler();
  }

  /**
   * Uninstall all registered listeners
   */
  protected void uninstallListeners()
  {
    tree.removePropertyChangeListener(propertyChangeListener);
    tree.removeFocusListener(focusListener);
    tree.removeTreeSelectionListener(treeSelectionListener);
    tree.removeMouseListener(mouseListener);
    tree.removeKeyListener(keyListener);
    tree.removePropertyChangeListener(selectionModelPropertyChangeListener);
    tree.removeComponentListener(componentListener);
    tree.removeTreeExpansionListener(treeExpansionListener);

    TreeCellEditor tce = tree.getCellEditor();
    if (tce != null)
      tce.removeCellEditorListener(cellEditorListener);
    if (treeModel != null)
      treeModel.removeTreeModelListener(treeModelListener);
  }

  /**
   * Uninstall all keyboard actions.
   */
  protected void uninstallKeyboardActions()
  {
    action = null;
    tree.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).setParent(
                                                                              null);
    tree.getActionMap().setParent(null);
  }

  /**
   * Uninstall the rendererPane.
   */
  protected void uninstallComponents()
  {
    currentCellRenderer = null;
    rendererPane = null;
    createdRenderer = false;
    setCellRenderer(currentCellRenderer);
  }

  /**
   * The vertical element of legs between nodes starts at the bottom of the
   * parent node by default. This method makes the leg start below that.
   * 
   * @return the vertical leg buffer
   */
  protected int getVerticalLegBuffer()
  {
    return getRowHeight() / 2;
  }

  /**
   * The horizontal element of legs between nodes starts at the right of the
   * left-hand side of the child node by default. This method makes the leg end
   * before that.
   * 
   * @return the horizontal leg buffer
   */
  protected int getHorizontalLegBuffer()
  {
    return rightChildIndent / 2;
  }

  /**
   * Make all the nodes that are expanded in JTree expanded in LayoutCache. This
   * invokes updateExpandedDescendants with the root path.
   */
  protected void updateLayoutCacheExpandedNodes()
  {
    if (treeModel != null)
      updateExpandedDescendants(new TreePath(treeModel.getRoot()));
  }

  /**
   * Updates the expanded state of all the descendants of the <code>path</code>
   * by getting the expanded descendants from the tree and forwarding to the
   * tree state.
   * 
   * @param path
   *          the path used to update the expanded states
   */
  protected void updateExpandedDescendants(TreePath path)
  {
    Enumeration expanded = tree.getExpandedDescendants(path);
    while (expanded.hasMoreElements())
      treeState.setExpandedState(((TreePath) expanded.nextElement()), true);
  }

  /**
   * Returns a path to the last child of <code>parent</code>
   * 
   * @param parent
   *          is the topmost path to specified
   * @return a path to the last child of parent
   */
  protected TreePath getLastChildPath(TreePath parent)
  {
    return ((TreePath) parent.getLastPathComponent());
  }

  /**
   * Updates how much each depth should be offset by.
   */
  protected void updateDepthOffset()
  {
    depthOffset += getVerticalLegBuffer();
  }

  /**
   * Updates the cellEditor based on editability of the JTree that we're
   * contained in. If the tree is editable but doesn't have a cellEditor, a
   * basic one will be used.
   */
  protected void updateCellEditor()
  {
    if (tree.isEditable() && cellEditor == null)
      setCellEditor(createDefaultCellEditor());
    createdCellEditor = true;
  }

  /**
   * Messaged from the tree we're in when the renderer has changed.
   */
  protected void updateRenderer()
  {
    if (tree != null)
      {
        if (tree.getCellRenderer() == null)
          {
            if (currentCellRenderer == null)
              currentCellRenderer = createDefaultCellRenderer();
            tree.setCellRenderer(currentCellRenderer);
          }
      }
  }

  /**
   * Resets the treeState instance based on the tree we're providing the look
   * and feel for.
   */
  protected void configureLayoutCache()
  {
    treeState = createLayoutCache();
  }

  /**
   * Marks the cached size as being invalid, and messages the tree with
   * <code>treeDidChange</code>.
   */
  protected void updateSize()
  {
    preferredSize = null;
    updateCachedPreferredSize();
    tree.treeDidChange();
  }

  /**
   * Updates the <code>preferredSize</code> instance variable, which is
   * returned from <code>getPreferredSize()</code>.
   */
  protected void updateCachedPreferredSize()
  {
    int maxWidth = 0;
    boolean isLeaf = false;
    if (currentVisiblePath != null)
      {
        Object[] path = currentVisiblePath.getPath();
        for (int i = 0; i < path.length; i++)
          {
            TreePath curr = new TreePath(getPathToRoot(path[i], 0));
            Rectangle bounds = getPathBounds(tree, curr);
            if (treeModel != null)
              isLeaf = treeModel.isLeaf(path[i]);
            if (!isLeaf && hasControlIcons())
              bounds.width += getCurrentControlIcon(curr).getIconWidth();
            maxWidth = Math.max(maxWidth, bounds.x + bounds.width);
          }

        maxHeight = 0;
        maxHeight = getMaxHeight(tree);
        preferredSize = new Dimension(maxWidth, (maxHeight * path.length));
      }
    else
      preferredSize = new Dimension(0, 0);
    validCachedPreferredSize = true;
  }

  /**
   * Messaged from the VisibleTreeNode after it has been expanded.
   * 
   * @param path
   *          is the path that has been expanded.
   */
  protected void pathWasExpanded(TreePath path)
  {
    validCachedPreferredSize = false;
    tree.repaint();
  }

  /**
   * Messaged from the VisibleTreeNode after it has collapsed
   */
  protected void pathWasCollapsed(TreePath path)
  {
    validCachedPreferredSize = false;
    tree.repaint();
  }

  /**
   * Install all defaults for the tree.
   */
  protected void installDefaults()
  {
    LookAndFeel.installColorsAndFont(tree, "Tree.background",
                                     "Tree.foreground", "Tree.font");
    tree.setOpaque(true);

    rightChildIndent = UIManager.getInt("Tree.rightChildIndent");
    leftChildIndent = UIManager.getInt("Tree.leftChildIndent");
    setRowHeight(UIManager.getInt("Tree.rowHeight"));
    tree.setRowHeight(getRowHeight());
    tree.requestFocusInWindow(false);
    tree.setScrollsOnExpand(UIManager.getBoolean("Tree.scrollsOnExpand"));
    setExpandedIcon(UIManager.getIcon("Tree.expandedIcon"));
    setCollapsedIcon(UIManager.getIcon("Tree.collapsedIcon"));
  }

  /**
   * Install all keyboard actions for this
   */
  protected void installKeyboardActions()
  {
    InputMap focusInputMap = (InputMap) UIManager.get("Tree.focusInputMap");
    InputMapUIResource parentInputMap = new InputMapUIResource();
    ActionMap parentActionMap = new ActionMapUIResource();
    action = new TreeAction();
    Object keys[] = focusInputMap.allKeys();

    for (int i = 0; i < keys.length; i++)
      {
        parentInputMap.put(
                           KeyStroke.getKeyStroke(
                                                  ((KeyStroke) keys[i]).getKeyCode(),
                                                  convertModifiers(((KeyStroke) keys[i]).getModifiers())),
                           (String) focusInputMap.get((KeyStroke) keys[i]));

        parentInputMap.put(
                           KeyStroke.getKeyStroke(
                                                  ((KeyStroke) keys[i]).getKeyCode(),
                                                  ((KeyStroke) keys[i]).getModifiers()),
                           (String) focusInputMap.get((KeyStroke) keys[i]));

        parentActionMap.put(
                            (String) focusInputMap.get((KeyStroke) keys[i]),
                            new ActionListenerProxy(
                                                    action,
                                                    (String) focusInputMap.get((KeyStroke) keys[i])));

      }

    parentInputMap.setParent(tree.getInputMap(
                                              JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).getParent());
    parentActionMap.setParent(tree.getActionMap().getParent());
    tree.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).setParent(
                                                                              parentInputMap);
    tree.getActionMap().setParent(parentActionMap);
  }

  /**
   * Converts the modifiers.
   * 
   * @param mod -
   *          modifier to convert
   * @returns the new modifier
   */
  private int convertModifiers(int mod)
  {
    if ((mod & KeyEvent.SHIFT_DOWN_MASK) != 0)
      {
        mod |= KeyEvent.SHIFT_MASK;
        mod &= ~KeyEvent.SHIFT_DOWN_MASK;
      }
    if ((mod & KeyEvent.CTRL_DOWN_MASK) != 0)
      {
        mod |= KeyEvent.CTRL_MASK;
        mod &= ~KeyEvent.CTRL_DOWN_MASK;
      }
    if ((mod & KeyEvent.META_DOWN_MASK) != 0)
      {
        mod |= KeyEvent.META_MASK;
        mod &= ~KeyEvent.META_DOWN_MASK;
      }
    if ((mod & KeyEvent.ALT_DOWN_MASK) != 0)
      {
        mod |= KeyEvent.ALT_MASK;
        mod &= ~KeyEvent.ALT_DOWN_MASK;
      }
    if ((mod & KeyEvent.ALT_GRAPH_DOWN_MASK) != 0)
      {
        mod |= KeyEvent.ALT_GRAPH_MASK;
        mod &= ~KeyEvent.ALT_GRAPH_DOWN_MASK;
      }
    return mod;
  }

  /**
   * Install all listeners for this
   */
  protected void installListeners()
  {
    tree.addPropertyChangeListener(propertyChangeListener);
    tree.addFocusListener(focusListener);
    tree.addTreeSelectionListener(treeSelectionListener);
    tree.addMouseListener(mouseListener);
    tree.addKeyListener(keyListener);
    tree.addPropertyChangeListener(selectionModelPropertyChangeListener);
    tree.addComponentListener(componentListener);
    tree.addTreeExpansionListener(treeExpansionListener);
    if (treeModel != null)
      treeModel.addTreeModelListener(treeModelListener);
  }

  /**
   * Install the UI for the component
   * 
   * @param c
   *          the component to install UI for
   */
  public void installUI(JComponent c)
  {
    tree = (JTree) c;
    prepareForUIInstall();
    super.installUI(c);
    installDefaults();

    installComponents();
    installKeyboardActions();
    installListeners();

    setCellEditor(createDefaultCellEditor());
    createdCellEditor = true;
    isEditing = false;

    setModel(tree.getModel());
    treeSelectionModel = tree.getSelectionModel();

    completeUIInstall();
  }

  /**
   * Uninstall the defaults for the tree
   */
  protected void uninstallDefaults()
  {
    tree.setFont(null);
    tree.setForeground(null);
    tree.setBackground(null);
  }

  /**
   * Uninstall the UI for the component
   * 
   * @param c
   *          the component to uninstall UI for
   */
  public void uninstallUI(JComponent c)
  {
    prepareForUIUninstall();
    uninstallDefaults();
    uninstallKeyboardActions();
    uninstallListeners();
    tree = null;
    uninstallComponents();
    completeUIUninstall();
  }

  /**
   * Paints the specified component appropriate for the look and feel. This
   * method is invoked from the ComponentUI.update method when the specified
   * component is being painted. Subclasses should override this method and use
   * the specified Graphics object to render the content of the component.
   * 
   * @param g
   *          the Graphics context in which to paint
   * @param c
   *          the component being painted; this argument is often ignored, but
   *          might be used if the UI object is stateless and shared by multiple
   *          components
   */
  public void paint(Graphics g, JComponent c)
  {
    JTree tree = (JTree) c;
    updateCurrentVisiblePath();

    Rectangle clip = g.getClipBounds();
    Insets insets = tree.getInsets();

    if (clip != null && treeModel != null && currentVisiblePath != null)
      {
        int startIndex = tree.getClosestRowForLocation(clip.x, clip.y);
        int endIndex = tree.getClosestRowForLocation(clip.x + clip.width,
                                                     clip.y + clip.height);

        paintVerticalPartOfLeg(g, clip, insets, currentVisiblePath);
        for (int i = startIndex; i <= endIndex; i++)
          {
            Object curr = currentVisiblePath.getPathComponent(i);
            boolean isLeaf = treeModel.isLeaf(curr);
            TreePath path = new TreePath(getPathToRoot(curr, 0));

            boolean isExpanded = tree.isExpanded(path);
            Rectangle bounds = getPathBounds(tree, path);
            paintHorizontalPartOfLeg(g, clip, insets, bounds, path, i,
                                     isExpanded, false, isLeaf);
            paintRow(g, clip, insets, bounds, path, i, isExpanded, false,
                     isLeaf);
          }
      }
  }

  /**
   * Ensures that the rows identified by beginRow through endRow are visible.
   * 
   * @param beginRow
   *          is the first row
   * @param endRow
   *          is the last row
   */
  protected void ensureRowsAreVisible(int beginRow, int endRow)
  {
    if (beginRow < endRow)
      {
        int temp = endRow;
        endRow = beginRow;
        beginRow = temp;
      }

    for (int i = beginRow; i < endRow; i++)
      {
        TreePath path = getPathForRow(tree, i);
        if (!tree.isVisible(path))
          tree.makeVisible(path);
      }
  }

  /**
   * Sets the preferred minimum size.
   * 
   * @param newSize
   *          is the new preferred minimum size.
   */
  public void setPreferredMinSize(Dimension newSize)
  {
    preferredMinSize = newSize;
  }

  /**
   * Gets the preferred minimum size.
   * 
   * @returns the preferred minimum size.
   */
  public Dimension getPreferredMinSize()
  {
    return preferredMinSize;
  }

  /**
   * Returns the preferred size to properly display the tree, this is a cover
   * method for getPreferredSize(c, false).
   * 
   * @param c
   *          the component whose preferred size is being queried; this argument
   *          is often ignored but might be used if the UI object is stateless
   *          and shared by multiple components
   * @return the preferred size
   */
  public Dimension getPreferredSize(JComponent c)
  {
    return getPreferredSize(c, false);
  }

  /**
   * Returns the preferred size to represent the tree in c. If checkConsistancy
   * is true, checkConsistancy is messaged first.
   * 
   * @param c
   *          the component whose preferred size is being queried.
   * @param checkConsistancy
   *          if true must check consistancy
   * @return the preferred size
   */
  public Dimension getPreferredSize(JComponent c, boolean checkConsistancy)
  {
    // FIXME: checkConsistancy not implemented, c not used
    if (!validCachedPreferredSize)
      updateCachedPreferredSize();
    return preferredSize;
  }

  /**
   * Returns the minimum size for this component. Which will be the min
   * preferred size or (0,0).
   * 
   * @param c
   *          the component whose min size is being queried.
   * @returns the preferred size or null
   */
  public Dimension getMinimumSize(JComponent c)
  {
    Dimension min = getPreferredMinSize();
    if (min == null)
      return new Dimension();
    return min;
  }

  /**
   * Returns the maximum size for the component, which will be the preferred
   * size if the instance is currently in JTree or (0,0).
   * 
   * @param c
   *          the component whose preferred size is being queried
   * @return the max size or null
   */
  public Dimension getMaximumSize(JComponent c)
  {
    if (c instanceof JTree)
      return ((JTree) c).getPreferredSize();
    return new Dimension();
  }

  /**
   * Messages to stop the editing session. If the UI the receiver is providing
   * the look and feel for returns true from
   * <code>getInvokesStopCellEditing</code>, stopCellEditing will be invoked
   * on the current editor. Then completeEditing will be messaged with false,
   * true, false to cancel any lingering editing.
   */
  protected void completeEditing()
  {
    completeEditing(false, true, false);
  }

  /**
   * Stops the editing session. If messageStop is true, the editor is messaged
   * with stopEditing, if messageCancel is true the editor is messaged with
   * cancelEditing. If messageTree is true, the treeModel is messaged with
   * valueForPathChanged.
   * 
   * @param messageStop
   *          message to stop editing
   * @param messageCancel
   *          message to cancel editing
   * @param messageTree
   *          message to treeModel
   */
  protected void completeEditing(boolean messageStop, boolean messageCancel,
                                 boolean messageTree)
  {
    if (messageStop)
      {
        getCellEditor().stopCellEditing();
        stopEditingInCompleteEditing = true;
      }

    if (messageCancel)
      {
        getCellEditor().cancelCellEditing();
        stopEditingInCompleteEditing = true;
      }

    if (messageTree)
      treeModel.valueForPathChanged(tree.getLeadSelectionPath(), newVal);
  }

  /**
   * Will start editing for node if there is a cellEditor and shouldSelectCall
   * returns true. This assumes that path is valid and visible.
   * 
   * @param path
   *          is the path to start editing
   * @param event
   *          is the MouseEvent performed on the path
   * @return true if successful
   */
  protected boolean startEditing(TreePath path, MouseEvent event)
  {
    int x;
    int y;
    if (event == null)
      {
        Rectangle bounds = getPathBounds(tree, path);
        x = bounds.x;
        y = bounds.y;
      }
    else
      {
        x = event.getX();
        y = event.getY();
      }

    updateCellEditor();
    TreeCellEditor ed = getCellEditor();
    if (ed != null && ed.shouldSelectCell(event) && ed.isCellEditable(event))
      {
        editingPath = path;
        editingRow = tree.getRowForPath(editingPath);

        Object val = editingPath.getLastPathComponent();
        cellEditor.addCellEditorListener(cellEditorListener);
        stopEditingInCompleteEditing = false;
        boolean expanded = tree.isExpanded(editingPath);
        isEditing = true;
        editingComponent = ed.getTreeCellEditorComponent(tree, val, true,
                                                         expanded,
                                                         isLeaf(editingRow),
                                                         editingRow);
        editingComponent.getParent().setVisible(true);
        editingComponent.getParent().validate();
        tree.add(editingComponent.getParent());
        editingComponent.getParent().validate();
        validCachedPreferredSize = false;

        ((JTextField) editingComponent).requestFocusInWindow(false);
        editorTimer.start();
        return true;
      }
    return false;
  }

  /**
   * If the <code>mouseX</code> and <code>mouseY</code> are in the expand or
   * collapse region of the row, this will toggle the row.
   * 
   * @param path
   *          the path we are concerned with
   * @param mouseX
   *          is the cursor's x position
   * @param mouseY
   *          is the cursor's y position
   */
  protected void checkForClickInExpandControl(TreePath path, int mouseX,
                                              int mouseY)
  {
    if (isLocationInExpandControl(path, mouseX, mouseY))
      toggleExpandState(path);
  }

  /**
   * Returns true if the <code>mouseX</code> and <code>mouseY</code> fall in
   * the area of row that is used to expand/collpse the node and the node at row
   * does not represent a leaf.
   * 
   * @param path
   *          the path we are concerned with
   * @param mouseX
   *          is the cursor's x position
   * @param mouseY
   *          is the cursor's y position
   * @return true if the <code>mouseX</code> and <code>mouseY</code> fall in
   *         the area of row that is used to expand/collpse the node and the
   *         node at row does not represent a leaf.
   */
  protected boolean isLocationInExpandControl(TreePath path, int mouseX,
                                              int mouseY)
  {
    boolean cntlClick = false;
    int row = getRowForPath(tree, path);

    if (!isLeaf(row))
      {
        Rectangle bounds = getPathBounds(tree, path);

        if (hasControlIcons()
            && (mouseX < bounds.x)
            && (mouseX > (bounds.x - getCurrentControlIcon(path).getIconWidth() - gap)))
          cntlClick = true;
      }
    return cntlClick;
  }

  /**
   * Messaged when the user clicks the particular row, this invokes
   * toggleExpandState.
   * 
   * @param path
   *          the path we are concerned with
   * @param mouseX
   *          is the cursor's x position
   * @param mouseY
   *          is the cursor's y position
   */
  protected void handleExpandControlClick(TreePath path, int mouseX, int mouseY)
  {
    toggleExpandState(path);
  }

  /**
   * Expands path if it is not expanded, or collapses row if it is expanded. If
   * expanding a path and JTree scroll on expand, ensureRowsAreVisible is
   * invoked to scroll as many of the children to visible as possible (tries to
   * scroll to last visible descendant of path).
   * 
   * @param path
   *          the path we are concerned with
   */
  protected void toggleExpandState(TreePath path)
  {
    if (tree.isExpanded(path))
      tree.collapsePath(path);
    else
      tree.expandPath(path);
  }

  /**
   * Returning true signifies a mouse event on the node should toggle the
   * selection of only the row under the mouse.
   * 
   * @param event
   *          is the MouseEvent performed on the row.
   * @return true signifies a mouse event on the node should toggle the
   *         selection of only the row under the mouse.
   */
  protected boolean isToggleSelectionEvent(MouseEvent event)
  {
    return (tree.getSelectionModel().getSelectionMode() == TreeSelectionModel.SINGLE_TREE_SELECTION);
  }

  /**
   * Returning true signifies a mouse event on the node should select from the
   * anchor point.
   * 
   * @param event
   *          is the MouseEvent performed on the node.
   * @return true signifies a mouse event on the node should select from the
   *         anchor point.
   */
  protected boolean isMultiSelectEvent(MouseEvent event)
  {
    return (tree.getSelectionModel().getSelectionMode() == TreeSelectionModel.CONTIGUOUS_TREE_SELECTION);
  }

  /**
   * Returning true indicates the row under the mouse should be toggled based on
   * the event. This is invoked after checkForClickInExpandControl, implying the
   * location is not in the expand (toggle) control.
   * 
   * @param event
   *          is the MouseEvent performed on the row.
   * @return true indicates the row under the mouse should be toggled based on
   *         the event.
   */
  protected boolean isToggleEvent(MouseEvent event)
  {
    return true;
  }

  /**
   * Messaged to update the selection based on a MouseEvent over a particular
   * row. If the even is a toggle selection event, the row is either selected,
   * or deselected. If the event identifies a multi selection event, the
   * selection is updated from the anchor point. Otherwise, the row is selected,
   * and if the even specified a toggle event the row is expanded/collapsed.
   * 
   * @param path
   *          is the path selected for an event
   * @param event
   *          is the MouseEvent performed on the path.
   */
  protected void selectPathForEvent(TreePath path, MouseEvent event)
  {
    if (isToggleSelectionEvent(event))
      {
        if (tree.isPathSelected(path))
          tree.removeSelectionPath(path);
        else
          {
            tree.addSelectionPath(path);
            tree.setAnchorSelectionPath(path);
          }
      }
    else if (isMultiSelectEvent(event))
      {
        TreePath anchor = tree.getAnchorSelectionPath();
        if (anchor != null)
          {
            int aRow = getRowForPath(tree, anchor);
            tree.addSelectionInterval(aRow, getRowForPath(tree, path));
          }
        else
          tree.addSelectionPath(path);
      }
    else
      tree.addSelectionPath(path);
  }

  /**
   * Returns true if the node at <code>row</code> is a leaf.
   * 
   * @param row
   *          is the row we are concerned with.
   * @return true if the node at <code>row</code> is a leaf.
   */
  protected boolean isLeaf(int row)
  {
    TreePath pathForRow = getPathForRow(tree, row);
    if (pathForRow == null)
      return true;

    Object node = pathForRow.getLastPathComponent();
    return treeModel.isLeaf(node);
  }

  /**
   * This class implements the actions that we want to happen when specific keys
   * are pressed for the JTree. The actionPerformed method is called when a key
   * that has been registered for the JTree is received.
   */
  class TreeAction extends AbstractAction
  {

    /**
     * What to do when this action is called.
     * 
     * @param e
     *          the ActionEvent that caused this action.
     */
    public void actionPerformed(ActionEvent e)
    {
      TreePath lead = tree.getLeadSelectionPath();

      if (e.getActionCommand().equals("selectPreviousChangeLead")
          || e.getActionCommand().equals("selectPreviousExtendSelection")
          || e.getActionCommand().equals("selectPrevious")
          || e.getActionCommand().equals("selectNext")
          || e.getActionCommand().equals("selectNextExtendSelection")
          || e.getActionCommand().equals("selectNextChangeLead"))
        (new TreeIncrementAction(0, "")).actionPerformed(e);
      else if (e.getActionCommand().equals("selectParent")
               || e.getActionCommand().equals("selectChild"))
        (new TreeTraverseAction(0, "")).actionPerformed(e);
      else if (e.getActionCommand().equals("selectAll"))
        {
          TreePath[] paths = new TreePath[tree.getVisibleRowCount()];

          Object curr = getNextVisibleNode(treeModel.getRoot());
          int i = 0;
          while (curr != null && i < paths.length)
            {
              paths[i] = new TreePath(getPathToRoot(curr, 0));
              i++;
            }

          tree.addSelectionPaths(paths);
        }
      else if (e.getActionCommand().equals("startEditing"))
        tree.startEditingAtPath(lead);
      else if (e.getActionCommand().equals("toggle"))
        {
          if (tree.isEditing())
            tree.stopEditing();
          else
            {
              Object last = lead.getLastPathComponent();
              TreePath path = new TreePath(getPathToRoot(last, 0));
              if (!treeModel.isLeaf(last))
                toggleExpandState(path);
            }
        }
      else if (e.getActionCommand().equals("clearSelection"))
        tree.clearSelection();

      if (tree.isEditing() && !e.getActionCommand().equals("startEditing"))
        tree.cancelEditing();

      tree.scrollPathToVisible(lead);
    }
  }

  /**
   * This class is used to mimic the behaviour of the JDK when registering
   * keyboard actions. It is the same as the private class used in JComponent
   * for the same reason. This class receives an action event and dispatches it
   * to the true receiver after altering the actionCommand property of the
   * event.
   */
  private static class ActionListenerProxy extends AbstractAction
  {
    ActionListener target;

    String bindingCommandName;

    public ActionListenerProxy(ActionListener li, String cmd)
    {
      target = li;
      bindingCommandName = cmd;
    }

    public void actionPerformed(ActionEvent e)
    {
      ActionEvent derivedEvent = new ActionEvent(e.getSource(), e.getID(),
                                                 bindingCommandName,
                                                 e.getModifiers());

      target.actionPerformed(derivedEvent);
    }
  }

  /**
   * The timer that updates the editor component.
   */
  private class EditorUpdateTimer extends Timer implements ActionListener
  {
    /**
     * Creates a new EditorUpdateTimer object with a default delay of 0.3
     * seconds.
     */
    public EditorUpdateTimer()
    {
      super(300, null);
      addActionListener(this);
    }

    /**
     * Lets the caret blink and repaints the table.
     */
    public void actionPerformed(ActionEvent ev)
    {
      Caret c = ((JTextField) editingComponent).getCaret();
      if (c != null)
        c.setVisible(!c.isVisible());
      tree.repaint();
    }

    /**
     * Updates the blink delay according to the current caret.
     */
    public void update()
    {
      stop();
      Caret c = ((JTextField) editingComponent).getCaret();
      if (c != null)
        {
          setDelay(c.getBlinkRate());
          if (((JTextField) editingComponent).isEditable())
            start();
          else
            c.setVisible(false);
        }
    }
  }

  /**
   * Updates the preferred size when scrolling, if necessary.
   */
  public class ComponentHandler extends ComponentAdapter implements
      ActionListener
  {
    /**
     * Timer used when inside a scrollpane and the scrollbar is adjusting
     */
    protected Timer timer;

    /** ScrollBar that is being adjusted */
    protected JScrollBar scrollBar;

    /**
     * Constructor
     */
    public ComponentHandler()
    {
      // Nothing to do here.
    }

    /**
     * Invoked when the component's position changes.
     * 
     * @param e
     *          the event that occurs when moving the component
     */
    public void componentMoved(ComponentEvent e)
    {
      // TODO: What should be done here, if anything?
    }

    /**
     * Creates, if necessary, and starts a Timer to check if needed to resize
     * the bounds
     */
    protected void startTimer()
    {
      // TODO: Implement this properly.
    }

    /**
     * Returns the JScrollPane housing the JTree, or null if one isn't found.
     * 
     * @return JScrollPane housing the JTree, or null if one isn't found.
     */
    protected JScrollPane getScrollPane()
    {
      return null;
    }

    /**
     * Public as a result of Timer. If the scrollBar is null, or not adjusting,
     * this stops the timer and updates the sizing.
     * 
     * @param ae
     *          is the action performed
     */
    public void actionPerformed(ActionEvent ae)
    {
      // TODO: Implement this properly.
    }
  }

  /**
   * Listener responsible for getting cell editing events and updating the tree
   * accordingly.
   */
  public class CellEditorHandler implements CellEditorListener
  {
    /**
     * Constructor
     */
    public CellEditorHandler()
    {
      // Nothing to do here.
    }

    /**
     * Messaged when editing has stopped in the tree. Tells the listeners
     * editing has stopped.
     * 
     * @param e
     *          is the notification event
     */
    public void editingStopped(ChangeEvent e)
    {
      editingPath = null;
      editingRow = -1;
      stopEditingInCompleteEditing = false;
      if (editingComponent != null)
        {
          tree.remove(editingComponent.getParent());
          editingComponent = null;
        }
      if (cellEditor != null)
        {
          newVal = ((JTextField) getCellEditor().getCellEditorValue()).getText();
          completeEditing(false, false, true);
          if (cellEditor instanceof DefaultTreeCellEditor)
            tree.removeTreeSelectionListener((DefaultTreeCellEditor) cellEditor);
          cellEditor.removeCellEditorListener(cellEditorListener);
          setCellEditor(null);
          createdCellEditor = false;
        }
      isEditing = false;
      tree.requestFocusInWindow(false);
      editorTimer.stop();
      validCachedPreferredSize = false;
      tree.repaint();
    }

    /**
     * Messaged when editing has been canceled in the tree. This tells the
     * listeners the editor has canceled editing.
     * 
     * @param e
     *          is the notification event
     */
    public void editingCanceled(ChangeEvent e)
    {
      editingPath = null;
      editingRow = -1;
      stopEditingInCompleteEditing = false;
      if (editingComponent != null)
        tree.remove(editingComponent.getParent());
      editingComponent = null;
      if (cellEditor != null)
        {
          if (cellEditor instanceof DefaultTreeCellEditor)
            tree.removeTreeSelectionListener((DefaultTreeCellEditor) cellEditor);
          cellEditor.removeCellEditorListener(cellEditorListener);
          setCellEditor(null);
          createdCellEditor = false;
        }
      tree.requestFocusInWindow(false);
      editorTimer.stop();
      isEditing = false;
      validCachedPreferredSize = false;
      tree.repaint();
    }
  }// CellEditorHandler

  /**
   * Repaints the lead selection row when focus is lost/grained.
   */
  public class FocusHandler implements FocusListener
  {
    /**
     * Constructor
     */
    public FocusHandler()
    {
      // Nothing to do here.
    }

    /**
     * Invoked when focus is activated on the tree we're in, redraws the lead
     * row. Invoked when a component gains the keyboard focus.
     * 
     * @param e
     *          is the focus event that is activated
     */
    public void focusGained(FocusEvent e)
    {
      // TODO: Implement this properly.
    }

    /**
     * Invoked when focus is deactivated on the tree we're in, redraws the lead
     * row. Invoked when a component loses the keyboard focus.
     * 
     * @param e
     *          is the focus event that is deactivated
     */
    public void focusLost(FocusEvent e)
    {
      // TODO: Implement this properly.
    }
  }

  /**
   * This is used to get multiple key down events to appropriately genereate
   * events.
   */
  public class KeyHandler extends KeyAdapter
  {
    /** Key code that is being generated for. */
    protected Action repeatKeyAction;

    /** Set to true while keyPressed is active */
    protected boolean isKeyDown;

    /**
     * Constructor
     */
    public KeyHandler()
    {
      // Nothing to do here.
    }

    /**
     * Invoked when a key has been typed. Moves the keyboard focus to the first
     * element whose first letter matches the alphanumeric key pressed by the
     * user. Subsequent same key presses move the keyboard focus to the next
     * object that starts with the same letter.
     * 
     * @param e
     *          the key typed
     */
    public void keyTyped(KeyEvent e)
    {
      // TODO: What should be done here, if anything?
    }

    /**
     * Invoked when a key has been pressed.
     * 
     * @param e
     *          the key pressed
     */
    public void keyPressed(KeyEvent e)
    {
      // TODO: What should be done here, if anything?
    }

    /**
     * Invoked when a key has been released
     * 
     * @param e
     *          the key released
     */
    public void keyReleased(KeyEvent e)
    {
      // TODO: What should be done here, if anything?
    }
  }

  /**
   * MouseListener is responsible for updating the selection based on mouse
   * events.
   */
  public class MouseHandler extends MouseAdapter implements MouseMotionListener
  {
    /**
     * Constructor
     */
    public MouseHandler()
    {
      // Nothing to do here.
    }

    /**
     * Invoked when a mouse button has been pressed on a component.
     * 
     * @param e
     *          is the mouse event that occured
     */
    public void mousePressed(MouseEvent e)
    {
      Point click = e.getPoint();
      TreePath path = getClosestPathForLocation(tree, click.x, click.y);

      if (path != null)
        {
          Rectangle bounds = getPathBounds(tree, path);
          int row = getRowForPath(tree, path);
          boolean cntlClick = isLocationInExpandControl(path, click.x, click.y);

          boolean isLeaf = isLeaf(row);

          TreeCellRenderer tcr = getCellRenderer();
          Icon icon;
          if (isLeaf)
            icon = UIManager.getIcon("Tree.leafIcon");
          else if (tree.isExpanded(path))
            icon = UIManager.getIcon("Tree.openIcon");
          else
            icon = UIManager.getIcon("Tree.closedIcon");

          if (tcr instanceof DefaultTreeCellRenderer)
            {
              Icon tmp = ((DefaultTreeCellRenderer) tcr).getIcon();
              if (tmp != null)
                icon = tmp;
            }

          // add gap*2 for the space before and after the text
          if (icon != null)
            bounds.width += icon.getIconWidth() + gap * 2;

          boolean inBounds = bounds.contains(click.x, click.y);
          if ((inBounds || cntlClick) && tree.isVisible(path))
            {
              if (inBounds)
                {
                  selectPath(tree, path);
                  if (e.getClickCount() == 2 && !isLeaf(row))
                    toggleExpandState(path);
                }

              if (cntlClick)
                {
                  handleExpandControlClick(path, click.x, click.y);
                  if (cellEditor != null)
                    cellEditor.cancelCellEditing();
                  tree.scrollPathToVisible(path);
                }
              else if (tree.isEditable())
                startEditing(path, e);
            }
        }
    }

    /**
     * Invoked when a mouse button is pressed on a component and then dragged.
     * MOUSE_DRAGGED events will continue to be delivered to the component where
     * the drag originated until the mouse button is released (regardless of
     * whether the mouse position is within the bounds of the component).
     * 
     * @param e
     *          is the mouse event that occured
     */
    public void mouseDragged(MouseEvent e)
    {
      // TODO: What should be done here, if anything?
    }

    /**
     * Invoked when the mouse button has been moved on a component (with no
     * buttons no down).
     * 
     * @param e
     *          the mouse event that occured
     */
    public void mouseMoved(MouseEvent e)
    {
      // TODO: What should be done here, if anything?
    }

    /**
     * Invoked when a mouse button has been released on a component.
     * 
     * @param e
     *          is the mouse event that occured
     */
    public void mouseReleased(MouseEvent e)
    {
      // TODO: What should be done here, if anything?
    }
  }

  /**
   * MouseInputHandler handles passing all mouse events, including mouse motion
   * events, until the mouse is released to the destination it is constructed
   * with.
   */
  public class MouseInputHandler implements MouseInputListener
  {
    /** Source that events are coming from */
    protected Component source;

    /** Destination that receives all events. */
    protected Component destination;

    /**
     * Constructor
     * 
     * @param source
     *          that events are coming from
     * @param destination
     *          that receives all events
     * @param e
     *          is the event received
     */
    public MouseInputHandler(Component source, Component destination,
                             MouseEvent e)
    {
      this.source = source;
      this.destination = destination;
    }

    /**
     * Invoked when the mouse button has been clicked (pressed and released) on
     * a component.
     * 
     * @param e
     *          mouse event that occured
     */
    public void mouseClicked(MouseEvent e)
    {
      // TODO: What should be done here, if anything?
    }

    /**
     * Invoked when a mouse button has been pressed on a component.
     * 
     * @param e
     *          mouse event that occured
     */
    public void mousePressed(MouseEvent e)
    {
      // TODO: What should be done here, if anything?
    }

    /**
     * Invoked when a mouse button has been released on a component.
     * 
     * @param e
     *          mouse event that occured
     */
    public void mouseReleased(MouseEvent e)
    {
      // TODO: What should be done here, if anything?
    }

    /**
     * Invoked when the mouse enters a component.
     * 
     * @param e
     *          mouse event that occured
     */
    public void mouseEntered(MouseEvent e)
    {
      // TODO: What should be done here, if anything?
    }

    /**
     * Invoked when the mouse exits a component.
     * 
     * @param e
     *          mouse event that occured
     */
    public void mouseExited(MouseEvent e)
    {
      // TODO: What should be done here, if anything?
    }

    /**
     * Invoked when a mouse button is pressed on a component and then dragged.
     * MOUSE_DRAGGED events will continue to be delivered to the component where
     * the drag originated until the mouse button is released (regardless of
     * whether the mouse position is within the bounds of the component).
     * 
     * @param e
     *          mouse event that occured
     */
    public void mouseDragged(MouseEvent e)
    {
      // TODO: What should be done here, if anything?
    }

    /**
     * Invoked when the mouse cursor has been moved onto a component but no
     * buttons have been pushed.
     * 
     * @param e
     *          mouse event that occured
     */
    public void mouseMoved(MouseEvent e)
    {
      // TODO: What should be done here, if anything?
    }

    /**
     * Removes event from the source
     */
    protected void removeFromSource()
    {
      // TODO: Implement this properly.
    }
  }

  /**
   * Class responsible for getting size of node, method is forwarded to
   * BasicTreeUI method. X location does not include insets, that is handled in
   * getPathBounds.
   */
  public class NodeDimensionsHandler extends AbstractLayoutCache.NodeDimensions
  {
    /**
     * Constructor
     */
    public NodeDimensionsHandler()
    {
      // Nothing to do here.
    }

    /**
     * Returns, by reference in bounds, the size and x origin to place value at.
     * The calling method is responsible for determining the Y location. If
     * bounds is null, a newly created Rectangle should be returned, otherwise
     * the value should be placed in bounds and returned.
     * 
     * @param cell
     *          the value to be represented
     * @param row
     *          row being queried
     * @param depth
     *          the depth of the row
     * @param expanded
     *          true if row is expanded
     * @param size
     *          a Rectangle containing the size needed to represent value
     * @return containing the node dimensions, or null if node has no dimension
     */
    public Rectangle getNodeDimensions(Object cell, int row, int depth,
                                       boolean expanded, Rectangle size)
    {
      if (size == null || cell == null)
        return null;

      String s = cell.toString();
      Font f = tree.getFont();
      FontMetrics fm = tree.getToolkit().getFontMetrics(f);

      if (s != null)
        {
          size.x = getRowX(row, depth);
          size.width = SwingUtilities.computeStringWidth(fm, s);
          size.height = getMaxHeight(tree);
          size.y = size.height * row;
        }

      return size;
    }

    /**
     * Returns the amount to indent the given row
     * 
     * @return amount to indent the given row.
     */
    protected int getRowX(int row, int depth)
    {
      if (row == 0)
        return 0;
      return depth * rightChildIndent;
    }
  }// NodeDimensionsHandler

  /**
   * PropertyChangeListener for the tree. Updates the appropriate varaible, or
   * TreeState, based on what changes.
   */
  public class PropertyChangeHandler implements PropertyChangeListener
  {

    /**
     * Constructor
     */
    public PropertyChangeHandler()
    {
      // Nothing to do here.
    }

    /**
     * This method gets called when a bound property is changed.
     * 
     * @param event
     *          A PropertyChangeEvent object describing the event source and the
     *          property that has changed.
     */
    public void propertyChange(PropertyChangeEvent event)
    {
      if ((event.getPropertyName()).equals("rootVisible"))
        {
          validCachedPreferredSize = false;
          tree.repaint();
        }
    }
  }

  /**
   * Listener on the TreeSelectionModel, resets the row selection if any of the
   * properties of the model change.
   */
  public class SelectionModelPropertyChangeHandler implements
      PropertyChangeListener
  {

    /**
     * Constructor
     */
    public SelectionModelPropertyChangeHandler()
    {
      // Nothing to do here.
    }

    /**
     * This method gets called when a bound property is changed.
     * 
     * @param event
     *          A PropertyChangeEvent object describing the event source and the
     *          property that has changed.
     */
    public void propertyChange(PropertyChangeEvent event)
    {
      // TODO: What should be done here, if anything?
    }
  }

  /**
   * ActionListener that invokes cancelEditing when action performed.
   */
  public class TreeCancelEditingAction extends AbstractAction
  {

    /**
     * Constructor
     */
    public TreeCancelEditingAction(String name)
    {
      // TODO: Implement this properly.
    }

    /**
     * Invoked when an action occurs.
     * 
     * @param e
     *          event that occured
     */
    public void actionPerformed(ActionEvent e)
    {
      // TODO: Implement this properly.
    }

    /**
     * Returns true if the action is enabled.
     * 
     * @return true if the action is enabled, false otherwise
     */
    public boolean isEnabled()
    {
      // TODO: Implement this properly.
      return false;
    }
  }

  /**
   * Updates the TreeState in response to nodes expanding/collapsing.
   */
  public class TreeExpansionHandler implements TreeExpansionListener
  {

    /**
     * Constructor
     */
    public TreeExpansionHandler()
    {
      // Nothing to do here.
    }

    /**
     * Called whenever an item in the tree has been expanded.
     * 
     * @param event
     *          is the event that occured
     */
    public void treeExpanded(TreeExpansionEvent event)
    {
      validCachedPreferredSize = false;
      tree.repaint();
    }

    /**
     * Called whenever an item in the tree has been collapsed.
     * 
     * @param event
     *          is the event that occured
     */
    public void treeCollapsed(TreeExpansionEvent event)
    {
      validCachedPreferredSize = false;
      tree.repaint();
    }
  }// TreeExpansionHandler

  /**
   * TreeHomeAction is used to handle end/home actions. Scrolls either the first
   * or last cell to be visible based on direction.
   */
  public class TreeHomeAction extends AbstractAction
  {

    /** direction is either home or end */
    protected int direction;

    /**
     * Constructor
     * 
     * @param direction -
     *          it is home or end
     * @param name
     *          is the name of the direction
     */
    public TreeHomeAction(int direction, String name)
    {
      // TODO: Implement this properly
    }

    /**
     * Invoked when an action occurs.
     * 
     * @param e
     *          is the event that occured
     */
    public void actionPerformed(ActionEvent e)
    {
      // TODO: Implement this properly
    }

    /**
     * Returns true if the action is enabled.
     * 
     * @return true if the action is enabled.
     */
    public boolean isEnabled()
    {
      // TODO: Implement this properly
      return false;
    }
  }

  /**
   * TreeIncrementAction is used to handle up/down actions. Selection is moved
   * up or down based on direction.
   */
  public class TreeIncrementAction extends AbstractAction
  {

    /** Specifies the direction to adjust the selection by. */
    protected int direction;

    /**
     * Constructor
     * 
     * @param direction
     *          up or down
     * @param name
     *          is the name of the direction
     */
    public TreeIncrementAction(int direction, String name)
    {
      // TODO: Implement this properly
    }

    /**
     * Invoked when an action occurs.
     * 
     * @param e
     *          is the event that occured
     */
    public void actionPerformed(ActionEvent e)
    {
      Object last = tree.getLeadSelectionPath().getLastPathComponent();

      if (e.getActionCommand().equals("selectPreviousChangeLead"))
        {
          Object prev = getPreviousVisibleNode(last);

          if (prev != null)
            {
              TreePath newPath = new TreePath(getPathToRoot(prev, 0));
              selectPath(tree, newPath);
              tree.setLeadSelectionPath(newPath);
            }
        }
      else if (e.getActionCommand().equals("selectPreviousExtendSelection"))
        {
          Object prev = getPreviousVisibleNode(last);
          if (prev != null)
            {
              TreePath newPath = new TreePath(getPathToRoot(prev, 0));
              tree.addSelectionPath(newPath);
              tree.setLeadSelectionPath(newPath);
            }
        }
      else if (e.getActionCommand().equals("selectPrevious"))
        {
          Object prev = getPreviousVisibleNode(last);

          if (prev != null)
            {
              TreePath newPath = new TreePath(getPathToRoot(prev, 0));
              selectPath(tree, newPath);
            }
        }
      else if (e.getActionCommand().equals("selectNext"))
        {
          Object next = getNextVisibleNode(last);

          if (next != null)
            {
              TreePath newPath = new TreePath(getPathToRoot(next, 0));
              selectPath(tree, newPath);
            }
        }
      else if (e.getActionCommand().equals("selectNextExtendSelection"))
        {
          Object next = getNextVisibleNode(last);
          if (next != null)
            {
              TreePath newPath = new TreePath(getPathToRoot(next, 0));
              tree.addSelectionPath(newPath);
              tree.setLeadSelectionPath(newPath);
            }
        }
      else if (e.getActionCommand().equals("selectNextChangeLead"))
        {
          Object next = getNextVisibleNode(last);
          if (next != null)
            {
              TreePath newPath = new TreePath(getPathToRoot(next, 0));
              selectPath(tree, newPath);
              tree.setLeadSelectionPath(newPath);
            }
        }
    }

    /**
     * Returns true if the action is enabled.
     * 
     * @return true if the action is enabled.
     */
    public boolean isEnabled()
    {
      // TODO: Implement this properly
      return false;
    }
  }

  /**
   * Forwards all TreeModel events to the TreeState.
   */
  public class TreeModelHandler implements TreeModelListener
  {
    /**
     * Constructor
     */
    public TreeModelHandler()
    {
      // Nothing to do here.
    }

    /**
     * Invoked after a node (or a set of siblings) has changed in some way. The
     * node(s) have not changed locations in the tree or altered their children
     * arrays, but other attributes have changed and may affect presentation.
     * Example: the name of a file has changed, but it is in the same location
     * in the file system. To indicate the root has changed, childIndices and
     * children will be null. Use e.getPath() to get the parent of the changed
     * node(s). e.getChildIndices() returns the index(es) of the changed
     * node(s).
     * 
     * @param e
     *          is the event that occured
     */
    public void treeNodesChanged(TreeModelEvent e)
    {
      validCachedPreferredSize = false;
      tree.repaint();
    }

    /**
     * Invoked after nodes have been inserted into the tree. Use e.getPath() to
     * get the parent of the new node(s). e.getChildIndices() returns the
     * index(es) of the new node(s) in ascending order.
     * 
     * @param e
     *          is the event that occured
     */
    public void treeNodesInserted(TreeModelEvent e)
    {
      validCachedPreferredSize = false;
      tree.repaint();
    }

    /**
     * Invoked after nodes have been removed from the tree. Note that if a
     * subtree is removed from the tree, this method may only be invoked once
     * for the root of the removed subtree, not once for each individual set of
     * siblings removed. Use e.getPath() to get the former parent of the deleted
     * node(s). e.getChildIndices() returns, in ascending order, the index(es)
     * the node(s) had before being deleted.
     * 
     * @param e
     *          is the event that occured
     */
    public void treeNodesRemoved(TreeModelEvent e)
    {
      validCachedPreferredSize = false;
      tree.repaint();
    }

    /**
     * Invoked after the tree has drastically changed structure from a given
     * node down. If the path returned by e.getPath() is of length one and the
     * first element does not identify the current root node the first element
     * should become the new root of the tree. Use e.getPath() to get the path
     * to the node. e.getChildIndices() returns null.
     * 
     * @param e
     *          is the event that occured
     */
    public void treeStructureChanged(TreeModelEvent e)
    {
      if (e.getPath().length == 1
          && !e.getPath()[0].equals(treeModel.getRoot()))
        tree.expandPath(new TreePath(treeModel.getRoot()));
      validCachedPreferredSize = false;
      tree.repaint();
    }
  }// TreeModelHandler

  /**
   * TreePageAction handles page up and page down events.
   */
  public class TreePageAction extends AbstractAction
  {
    /** Specifies the direction to adjust the selection by. */
    protected int direction;

    /**
     * Constructor
     * 
     * @param direction
     *          up or down
     * @param name
     *          is the name of the direction
     */
    public TreePageAction(int direction, String name)
    {
      this.direction = direction;
    }

    /**
     * Invoked when an action occurs.
     * 
     * @param e
     *          is the event that occured
     */
    public void actionPerformed(ActionEvent e)
    {
      // TODO: Implement this properly.
    }

    /**
     * Returns true if the action is enabled.
     * 
     * @return true if the action is enabled.
     */
    public boolean isEnabled()
    {
      return false;
    }
  }// TreePageAction

  /**
   * Listens for changes in the selection model and updates the display
   * accordingly.
   */
  public class TreeSelectionHandler implements TreeSelectionListener
  {
    /**
     * Constructor
     */
    public TreeSelectionHandler()
    {
      // Nothing to do here.
    }

    /**
     * Messaged when the selection changes in the tree we're displaying for.
     * Stops editing, messages super and displays the changed paths.
     * 
     * @param event
     *          the event that characterizes the change.
     */
    public void valueChanged(TreeSelectionEvent event)
    {
      if (tree.isEditing())
        tree.cancelEditing();
    }
  }// TreeSelectionHandler

  /**
   * For the first selected row expandedness will be toggled.
   */
  public class TreeToggleAction extends AbstractAction
  {
    /**
     * Constructor
     * 
     * @param name
     *          is the name of <code>Action</code> field
     */
    public TreeToggleAction(String name)
    {
      // Nothing to do here.
    }

    /**
     * Invoked when an action occurs.
     * 
     * @param e
     *          the event that occured
     */
    public void actionPerformed(ActionEvent e)
    {
      // TODO: Implement this properly.
    }

    /**
     * Returns true if the action is enabled.
     * 
     * @return true if the action is enabled, false otherwise
     */
    public boolean isEnabled()
    {
      return false;
    }
  } // TreeToggleAction

  /**
   * TreeTraverseAction is the action used for left/right keys. Will toggle the
   * expandedness of a node, as well as potentially incrementing the selection.
   */
  public class TreeTraverseAction extends AbstractAction
  {
    /**
     * Determines direction to traverse, 1 means expand, -1 means collapse.
     */
    protected int direction;

    /**
     * Constructor
     * 
     * @param direction
     *          to traverse
     * @param name
     *          is the name of the direction
     */
    public TreeTraverseAction(int direction, String name)
    {
      this.direction = direction;
    }

    /**
     * Invoked when an action occurs.
     * 
     * @param e
     *          the event that occured
     */
    public void actionPerformed(ActionEvent e)
    {
      Object last = tree.getLeadSelectionPath().getLastPathComponent();

      if (e.getActionCommand().equals("selectParent"))
        {
          TreePath path = new TreePath(getPathToRoot(last, 0));
          Object p = getParent(treeModel.getRoot(), last);

          if (!treeModel.isLeaf(last))
            toggleExpandState(path);
          else if (p != null)
            selectPath(tree, new TreePath(getPathToRoot(p, 0)));
        }
      else if (e.getActionCommand().equals("selectChild"))
        {
          TreePath path = new TreePath(getPathToRoot(last, 0));

          if (!treeModel.isLeaf(last))
            toggleExpandState(path);
          else
            {
              Object next = getNextVisibleNode(last);

              if (next != null)
                selectPath(tree, new TreePath(getPathToRoot(next, 0)));
            }
        }
    }

    /**
     * Returns true if the action is enabled.
     * 
     * @return true if the action is enabled, false otherwise
     */
    public boolean isEnabled()
    {
      // TODO: Implement this properly
      return false;
    }
  }

  /**
   * Returns true if the LookAndFeel implements the control icons. Package
   * private for use in inner classes.
   * 
   * @returns true if there are control icons
   */
  boolean hasControlIcons()
  {
    if (expandedIcon != null || collapsedIcon != null)
      return true;
    return false;
  }

  /**
   * Returns control icon. It is null if the LookAndFeel does not implements the
   * control icons. Package private for use in inner classes.
   * 
   * @return control icon if it exists.
   */
  Icon getCurrentControlIcon(TreePath path)
  {
    if (tree.isExpanded(path))
      return expandedIcon;
    return collapsedIcon;
  }

  /**
   * Returns the parent of the current node
   * 
   * @param root
   *          is the root of the tree
   * @param node
   *          is the current node
   * @return is the parent of the current node
   */
  Object getParent(Object root, Object node)
  {
    if (root == null || node == null || root.equals(node))
      return null;

    if (node instanceof TreeNode)
      return ((TreeNode) node).getParent();
    return findNode(root, node);
  }

  /**
   * Recursively checks the tree for the specified node, starting at the root.
   * 
   * @param root
   *          is starting node to start searching at.
   * @param node
   *          is the node to search for
   * @return the parent node of node
   */
  private Object findNode(Object root, Object node)
  {
    if (!treeModel.isLeaf(root) && !root.equals(node))
      {
        int size = treeModel.getChildCount(root);
        for (int j = 0; j < size; j++)
          {
            Object child = treeModel.getChild(root, j);
            if (node.equals(child))
              return root;

            Object n = findNode(child, node);
            if (n != null)
              return n;
          }
      }
    return null;
  }

  /**
   * Get previous visible node in the tree. Package private for use in inner
   * classes.
   * 
   * @param node -
   *          current node
   * @return the next visible node in the JTree. Return null if there are no
   *         more.
   */
  Object getPreviousVisibleNode(Object node)
  {
    if (currentVisiblePath != null)
      {
        Object[] nodes = currentVisiblePath.getPath();
        int i = 0;
        while (i < nodes.length && !node.equals(nodes[i]))
          i++;
        // return the next node
        if (i - 1 >= 0)
          return nodes[i - 1];
      }
    return null;
  }

  /**
   * Returns the next node in the tree Package private for use in inner classes.
   * 
   * @param curr -
   *          current node
   * @return the next node in the tree
   */
  Object getNextNode(Object curr)
  {
    if (!treeModel.isLeaf(curr) && treeModel.getChildCount(curr) > 0)
      return treeModel.getChild(curr, 0);

    Object node = curr;
    Object sibling = null;
    do
      {
        sibling = getNextSibling(node);
        node = getParent(treeModel.getRoot(), node);
      }
    while (sibling == null && node != null);

    return sibling;
  }

  /**
   * Returns the previous node in the tree Package private for use in inner
   * classes.
   * 
   * @param node
   *          current node
   * @return the previous node in the tree
   */
  Object getPreviousNode(Object node)
  {
    Object parent = getParent(treeModel.getRoot(), node);
    if (parent == null)
      return null;

    Object sibling = getPreviousSibling(node);

    if (sibling == null)
      return parent;

    int size = 0;
    if (!treeModel.isLeaf(sibling))
      size = treeModel.getChildCount(sibling);
    while (size > 0)
      {
        sibling = treeModel.getChild(sibling, size - 1);
        if (!treeModel.isLeaf(sibling))
          size = treeModel.getChildCount(sibling);
        else
          size = 0;
      }

    return sibling;
  }

  /**
   * Returns the next sibling in the tree Package private for use in inner
   * classes.
   * 
   * @param node -
   *          current node
   * @return the next sibling in the tree
   */
  Object getNextSibling(Object node)
  {
    Object parent = getParent(treeModel.getRoot(), node);
    if (parent == null)
      return null;

    int index = treeModel.getIndexOfChild(parent, node) + 1;

    int size = 0;
    if (!treeModel.isLeaf(parent))
      size = treeModel.getChildCount(parent);
    if (index == 0 || index >= size)
      return null;

    return treeModel.getChild(parent, index);
  }

  /**
   * Returns the previous sibling in the tree Package private for use in inner
   * classes.
   * 
   * @param node -
   *          current node
   * @return the previous sibling in the tree
   */
  Object getPreviousSibling(Object node)
  {
    Object parent = getParent(treeModel.getRoot(), node);
    if (parent == null)
      return null;

    int index = treeModel.getIndexOfChild(parent, node) - 1;

    int size = 0;
    if (!treeModel.isLeaf(parent))
      size = treeModel.getChildCount(parent);
    if (index < 0 || index >= size)
      return null;

    return treeModel.getChild(parent, index);
  }

  /**
   * Selects the specified path in the tree depending on modes. Package private
   * for use in inner classes.
   * 
   * @param tree
   *          is the tree we are selecting the path in
   * @param path
   *          is the path we are selecting
   */
  void selectPath(JTree tree, TreePath path)
  {
    if (path != null)
      {
        if (tree.getSelectionModel().getSelectionMode() == TreeSelectionModel.SINGLE_TREE_SELECTION)
          {
            tree.getSelectionModel().clearSelection();
            tree.addSelectionPath(path);
            tree.setLeadSelectionPath(path);
          }
        else if (tree.getSelectionModel().getSelectionMode() == TreeSelectionModel.CONTIGUOUS_TREE_SELECTION)
          {
            // TODO
          }
        else
          {
            tree.addSelectionPath(path);
            tree.setLeadSelectionPath(path);
            tree.getSelectionModel().setSelectionMode(
                                                      TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION);
          }
      }
  }

  /**
   * Returns the path from node to the root. Package private for use in inner
   * classes.
   * 
   * @param node
   *          the node to get the path to
   * @param depth
   *          the depth of the tree to return a path for
   * @return an array of tree nodes that represent the path to node.
   */
  Object[] getPathToRoot(Object node, int depth)
  {
    if (node == null)
      {
        if (depth == 0)
          return null;

        return new Object[depth];
      }

    Object[] path = getPathToRoot(getParent(treeModel.getRoot(), node),
                                  depth + 1);
    path[path.length - depth - 1] = node;
    return path;
  }

  /**
   * Returns the level of the node in the tree.
   * 
   * @param node -
   *          current node
   * @return the number of the level
   */
  int getLevel(Object node)
  {
    int count = -1;

    Object current = node;

    if (treeModel != null)
      {
        Object root = treeModel.getRoot();
        if (!tree.isRootVisible() && tree.isExpanded(new TreePath(root)))
          count--;

        do
          {
            current = getParent(root, current);
            count++;
          }
        while (current != null);
      }
    return count;
  }

  /**
   * Draws a vertical line using the given graphic context
   * 
   * @param g
   *          is the graphic context
   * @param c
   *          is the component the new line will belong to
   * @param x
   *          is the horizonal position
   * @param top
   *          specifies the top of the line
   * @param bottom
   *          specifies the bottom of the line
   */
  protected void paintVerticalLine(Graphics g, JComponent c, int x, int top,
                                   int bottom)
  {
    // FIXME: Check if drawing a dashed line or not.
    g.setColor(getHashColor());
    g.drawLine(x, top, x, bottom);
  }

  /**
   * Draws a horizontal line using the given graphic context
   * 
   * @param g
   *          is the graphic context
   * @param c
   *          is the component the new line will belong to
   * @param y
   *          is the vertical position
   * @param left
   *          specifies the left point of the line
   * @param right
   *          specifies the right point of the line
   */
  protected void paintHorizontalLine(Graphics g, JComponent c, int y, int left,
                                     int right)
  {
    // FIXME: Check if drawing a dashed line or not.
    g.setColor(getHashColor());
    g.drawLine(left, y, right, y);
  }

  /**
   * Draws an icon at around a specific position
   * 
   * @param c
   *          is the component the new line will belong to
   * @param g
   *          is the graphic context
   * @param icon
   *          is the icon which will be drawn
   * @param x
   *          is the center position in x-direction
   * @param y
   *          is the center position in y-direction
   */
  protected void drawCentered(Component c, Graphics g, Icon icon, int x, int y)
  {
    x -= icon.getIconWidth() / 2;
    y -= icon.getIconHeight() / 2;

    if (x < 0)
      x = 0;
    if (y < 0)
      y = 0;

    icon.paintIcon(c, g, x, y);
  }

  /**
   * Draws a dashed horizontal line.
   * 
   * @param g -
   *          the graphics configuration.
   * @param y -
   *          the y location to start drawing at
   * @param x1 -
   *          the x location to start drawing at
   * @param x2 -
   *          the x location to finish drawing at
   */
  protected void drawDashedHorizontalLine(Graphics g, int y, int x1, int x2)
  {
    g.setColor(getHashColor());
    for (int i = x1; i < x2; i += 2)
      g.drawLine(i, y, i + 1, y);
  }

  /**
   * Draws a dashed vertical line.
   * 
   * @param g -
   *          the graphics configuration.
   * @param x -
   *          the x location to start drawing at
   * @param y1 -
   *          the y location to start drawing at
   * @param y2 -
   *          the y location to finish drawing at
   */
  protected void drawDashedVerticalLine(Graphics g, int x, int y1, int y2)
  {
    g.setColor(getHashColor());
    for (int i = y1; i < y2; i += 2)
      g.drawLine(x, i, x, i + 1);
  }

  /**
   * Paints the expand (toggle) part of a row. The receiver should NOT modify
   * clipBounds, or insets.
   * 
   * @param g -
   *          the graphics configuration
   * @param clipBounds -
   * @param insets -
   * @param bounds -
   *          bounds of expand control
   * @param path -
   *          path to draw control for
   * @param row -
   *          row to draw control for
   * @param isExpanded -
   *          is the row expanded
   * @param hasBeenExpanded -
   *          has the row already been expanded
   * @param isLeaf -
   *          is the path a leaf
   */
  protected void paintExpandControl(Graphics g, Rectangle clipBounds,
                                    Insets insets, Rectangle bounds,
                                    TreePath path, int row, boolean isExpanded,
                                    boolean hasBeenExpanded, boolean isLeaf)
  {
    if (shouldPaintExpandControl(path, row, isExpanded, hasBeenExpanded, isLeaf))
      {
        Icon icon = getCurrentControlIcon(path);
        int iconW = icon.getIconWidth();
        int x = bounds.x - rightChildIndent + iconW / 2;
        if (x + iconW > bounds.x)
          x = bounds.x - rightChildIndent - gap;
        icon.paintIcon(tree, g, x, bounds.y + bounds.height / 2
                                   - icon.getIconHeight() / 2);
      }
  }

  /**
   * Paints the horizontal part of the leg. The receiver should NOT modify
   * clipBounds, or insets. NOTE: parentRow can be -1 if the root is not
   * visible.
   * 
   * @param g -
   *          the graphics configuration
   * @param clipBounds -
   * @param insets -
   * @param bounds -
   *          bounds of the cell
   * @param path -
   *          path to draw leg for
   * @param row -
   *          row to start drawing at
   * @param isExpanded -
   *          is the row expanded
   * @param hasBeenExpanded -
   *          has the row already been expanded
   * @param isLeaf -
   *          is the path a leaf
   */
  protected void paintHorizontalPartOfLeg(Graphics g, Rectangle clipBounds,
                                          Insets insets, Rectangle bounds,
                                          TreePath path, int row,
                                          boolean isExpanded,
                                          boolean hasBeenExpanded,
                                          boolean isLeaf)
  {
    if (row != 0)
      paintHorizontalLine(g, tree, bounds.y + bounds.height / 2, bounds.x - gap
                                                                 - 2, bounds.x);
  }

  /**
   * Paints the vertical part of the leg. The receiver should NOT modify
   * clipBounds, insets.
   * 
   * @param g -
   *          the graphics configuration.
   * @param clipBounds -
   * @param insets -
   * @param path -
   *          the path to draw the vertical part for.
   */
  protected void paintVerticalPartOfLeg(Graphics g, Rectangle clipBounds,
                                        Insets insets, TreePath path)
  {
    int max = tree.getVisibleRowCount();
    for (int i = 0; i < max; i++)
      {
        Object curr = path.getPathComponent(i);
        TreePath currPath = new TreePath(getPathToRoot(curr, 0));
        int numChild = treeModel.getChildCount(curr);
        if (numChild > 0 && tree.isExpanded(currPath))
          {
            Rectangle bounds = getPathBounds(tree, currPath);
            Rectangle lastChildBounds = getPathBounds(
                                                      tree,
                                                      new TreePath(
                                                                   getPathToRoot(
                                                                                 treeModel.getChild(
                                                                                                    curr,
                                                                                                    numChild - 1),
                                                                                 0)));
            paintVerticalLine(g, tree, bounds.x + gap + 2, bounds.y
                                                           + bounds.height - 2,
                              lastChildBounds.y + lastChildBounds.height / 2);
          }
      }
  }

  /**
   * Paints the renderer part of a row. The receiver should NOT modify
   * clipBounds, or insets.
   * 
   * @param g -
   *          the graphics configuration
   * @param clipBounds -
   * @param insets -
   * @param bounds -
   *          bounds of expand control
   * @param path -
   *          path to draw control for
   * @param row -
   *          row to draw control for
   * @param isExpanded -
   *          is the row expanded
   * @param hasBeenExpanded -
   *          has the row already been expanded
   * @param isLeaf -
   *          is the path a leaf
   */
  protected void paintRow(Graphics g, Rectangle clipBounds, Insets insets,
                          Rectangle bounds, TreePath path, int row,
                          boolean isExpanded, boolean hasBeenExpanded,
                          boolean isLeaf)
  {
    boolean selected = tree.isPathSelected(path);
    boolean hasIcons = false;
    Object node = path.getLastPathComponent();

    if (tree.isVisible(path))
      {
        if (!validCachedPreferredSize)
          updateCachedPreferredSize();

        paintExpandControl(g, clipBounds, insets, bounds, path, row,
                           isExpanded, hasBeenExpanded, isLeaf);

        if (row != 0)
          bounds.x += gap;
        bounds.width = preferredSize.width + bounds.x;
        if (editingComponent != null && editingPath != null && isEditing(tree)
            && node.equals(editingPath.getLastPathComponent()))
          {
            rendererPane.paintComponent(g, editingComponent.getParent(), null,
                                        bounds);
          }
        else
          {
            TreeCellRenderer dtcr = tree.getCellRenderer();
            if (dtcr == null)
              dtcr = createDefaultCellRenderer();

            Component c = dtcr.getTreeCellRendererComponent(tree, node,
                                                            selected,
                                                            isExpanded, isLeaf,
                                                            row,
                                                            tree.hasFocus());
            rendererPane.paintComponent(g, c, c.getParent(), bounds);
          }
      }
  }

  /**
   * Prepares for the UI to uninstall.
   */
  protected void prepareForUIUninstall()
  {
    // TODO: Implement this properly.
  }

  /**
   * Returns true if the expand (toggle) control should be drawn for the
   * specified row.
   * 
   * @param path -
   *          current path to check for.
   * @param row -
   *          current row to check for.
   * @param isExpanded -
   *          true if the path is expanded
   * @param hasBeenExpanded -
   *          true if the path has been expanded already
   * @param isLeaf -
   *          true if the row is a lead
   */
  protected boolean shouldPaintExpandControl(TreePath path, int row,
                                             boolean isExpanded,
                                             boolean hasBeenExpanded,
                                             boolean isLeaf)
  {
    Object node = path.getLastPathComponent();
    return (!isLeaf && getLevel(node) != 0 && hasControlIcons());
  }

  /**
   * Updates the cached current TreePath of all visible nodes in the tree.
   */
  void updateCurrentVisiblePath()
  {
    if (treeModel == null)
      return;

    Object next = treeModel.getRoot();
    if (next == null)
      return;

    TreePath rootPath = new TreePath(next);
    Rectangle bounds = getPathBounds(tree, rootPath);

    // If root is not a valid size to be visible, or is
    // not visible and the tree is expanded, then the next node acts
    // as the root
    if ((bounds.width == 0 && bounds.height == 0)
        || (!isRootVisible() && tree.isExpanded(new TreePath(next))))
      {
        next = getNextNode(next);
        rootPath = new TreePath(next);
      }

    Object root = next;
    TreePath current = null;
    while (next != null)
      {
        if (current == null)
          current = rootPath;
        else
          current = current.pathByAddingChild(next);

        do
          {
            TreePath path = new TreePath(getPathToRoot(next, 0));
            if ((tree.isVisible(path) && tree.isExpanded(path))
                || treeModel.isLeaf(next))
              next = getNextNode(next);
            else
              {
                Object pNext = next;
                next = getNextSibling(pNext);
                // if no next sibling, check parent's next sibling.
                if (next == null)
                  {
                    Object parent = getParent(root, pNext);
                    while (next == null && parent != null)
                      {
                        next = getNextSibling(parent);
                        if (next == null)
                          parent = getParent(root, parent);
                      }
                  }
              }
          }
        while (next != null
               && !tree.isVisible(new TreePath(getPathToRoot(next, 0))));
      }

    currentVisiblePath = current;
    tree.setVisibleRowCount(getRowCount(tree));

    if (tree.getSelectionModel() != null && tree.getSelectionCount() == 0
        && currentVisiblePath != null)
      selectPath(
                 tree,
                 new TreePath(
                              getPathToRoot(
                                            currentVisiblePath.getPathComponent(0),
                                            0)));
  }

  /**
   * Get next visible node in the currentVisiblePath. Package private for use in
   * inner classes.
   * 
   * @param node
   *          current node
   * @return the next visible node in the JTree. Return null if there are no
   *         more.
   */
  Object getNextVisibleNode(Object node)
  {
    if (currentVisiblePath != null)
      {
        Object[] nodes = currentVisiblePath.getPath();
        int i = 0;
        while (i < nodes.length && !node.equals(nodes[i]))
          i++;
        // return the next node
        if (i + 1 < nodes.length)
          return nodes[i + 1];
      }
    return null;
  }
} // BasicTreeUI
