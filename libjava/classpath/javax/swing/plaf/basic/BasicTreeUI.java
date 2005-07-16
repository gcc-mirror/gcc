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

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.CellRendererPane;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.UIDefaults;
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
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.TreeUI;
import javax.swing.tree.AbstractLayoutCache;
import javax.swing.tree.FixedHeightLayoutCache;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellEditor;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeCellEditor;
import javax.swing.tree.TreeCellRenderer;
import javax.swing.tree.TreeSelectionModel;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;

import java.util.Enumeration;
import java.util.Hashtable;

/**
 * A delegate providing the user interface for <code>JTree</code> according to
 * the Basic look and feel.
 * 
 * @see javax.swing.JTree
 * @author Sascha Brawer (brawer@dandelis.ch)
 * @author Lillian Angel (langel@redhat.com)
 */
public class BasicTreeUI
      extends TreeUI
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

   /** Minimum preferred size. */
   protected Dimension preferredMinsize;

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

   /** Is the preferredSize valid? */
   protected boolean validCachedPreferredSize;

   /** Object responsible for handling sizing and expanded issues. */
   protected AbstractLayoutCache treeState;

   /** Used for minimizing the drawing of vertical lines. */
   protected Hashtable drawingCache;

   /**
    * True if doing optimizations for a largeModel. Subclasses that don't
    * support this may wish to override createLayoutCache to not return a
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
    * How much the depth should be offset to properly calculate x locations.
    * This is based on whether or not the root is visible, and if the root
    * handles are visible.
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

   /** Listeners */
   private PropertyChangeListener propertyChangeListener;

   private FocusListener focusListener;

   private TreeSelectionListener treeSelectionListener;

   private MouseInputListener mouseInputListener;

   private KeyListener keyListener;

   private PropertyChangeListener selectionModelPropertyChangeListener;

   private ComponentListener componentListener;

   private CellEditorListener cellEditorListener;

   private TreeExpansionListener treeExpansionListener;

   private TreeModelListener treeModelListener;

   /**
    * Creates a new BasicTreeUI object.
    */
   public BasicTreeUI()
   {
      drawingCache = new Hashtable();
      cellEditor = createDefaultCellEditor();
      currentCellRenderer = createDefaultCellRenderer();
      nodeDimensions = createNodeDimensions();
      rendererPane = createCellRendererPane();
      configureLayoutCache();

      propertyChangeListener = createPropertyChangeListener();
      focusListener = createFocusListener();
      treeSelectionListener = createTreeSelectionListener();
      mouseInputListener = new MouseInputHandler(null, null, null);
      keyListener = createKeyListener();
      selectionModelPropertyChangeListener = createSelectionModelPropertyChangeListener();
      componentListener = createComponentListener();
      cellEditorListener = createCellEditorListener();
      treeExpansionListener = createTreeExpansionListener();
      treeModelListener = createTreeModelListener();

      createdRenderer = true;
      createdCellEditor = true;
      editingRow = -1;
      lastSelectedRow = -1;
   }

   /**
    * Returns an instance of the UI delegate for the specified component.
    * 
    * @param c the <code>JComponent</code> for which we need a UI delegate
    *        for.
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
      return UIManager.getLookAndFeelDefaults().getColor("Tree.hash");
   }

   /**
    * Sets the Hash color.
    * 
    * @param the <code>Color</code> to set the Hash to.
    */
   protected void setHashColor(Color color)
   {
      // FIXME: not implemented

   }

   /**
    * Sets the left child's indent value.
    * 
    * @param newAmount is the new indent value for the left child.
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
   public int getLeftChildIndent(int newAmount)
   {
      return leftChildIndent;
   }

   /**
    * Sets the right child's indent value.
    * 
    * @param newAmount is the new indent value for the right child.
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
   public int getRightChildIndent(int newAmount)
   {
      return rightChildIndent;
   }

   /**
    * Sets the expanded icon.
    * 
    * @param newG is the new expanded icon.
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
    * @param newG is the new collapsed icon.
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
    * @param largeModel sets this.largeModel to it.
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
    * @param rowHeight is the height to set this.rowHeight to.
    */
   protected void setRowHeight(int rowHeight)
   {
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
    * @param tcr is the new TreeCellRenderer.
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
    * @param model to set the treeModel to.
    */
   protected void setModel(TreeModel model)
   {
      treeState.setModel(model);
      treeModel = model;
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
    * @param newValue sets the visibility of the root
    */
   protected void setRootVisible(boolean newValue)
   {
      treeState.setRootVisible(newValue);
   }

   /**
    * Returns true if the root is visible.
    * 
    * @return true if the root is visible.
    */
   protected boolean isRootVisible()
   {
      return treeState.isRootVisible();
   }

   /**
    * Determines whether the node handles are to be displayed.
    * 
    * @param newValue sets whether or not node handles should be displayed.
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
    * @param editor to set the cellEditor to.
    */
   protected void setCellEditor(TreeCellEditor editor)
   {
      cellEditor = editor;
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
    * @param newValue sets the receiver to allow editing if true.
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
    * @param newLSM resets the selection model.
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
    * @param tree is the current tree the path will be drawn to.
    * @param path is the current path the tree to draw to.
    * @return the Rectangle enclosing the label portion that the last item in
    *         the path will be drawn to.
    */
   public Rectangle getPathBounds(JTree tree, TreePath path)
   {
      Object cell = path.getLastPathComponent();
      TreeModel mod = tree.getModel();
      Point loc = getCellLocation(0, 0, tree, mod, cell, mod.getRoot());
      int x = (int) loc.getX();
      int y = (int) loc.getY();
      return getCellBounds(x, y, cell);      
   }

   /**
    * Returns the path for passed in row. If row is not visible null is
    * returned.
    * 
    * @param tree is the current tree to return path for.
    * @param row is the row number of the row to return.
    * @return the path for passed in row. If row is not visible null is
    *         returned.
    */
   public TreePath getPathForRow(JTree tree, int row)
   {
      DefaultMutableTreeNode node = ((DefaultMutableTreeNode) (tree.getModel())
            .getRoot());

      for (int i = 0; i < row; i++)
         node = getNextVisibleNode(node);

      // in case nothing was found
      if (node == null)
         return null;

      // something was found
      return new TreePath(node.getPath());
   }

   /**
    * Get next visible node in the tree.
    * Package private for use in inner classes.
    * @param the current node
    * @return the next visible node in the JTree. Return null if there are no
    *         more.
    */
   DefaultMutableTreeNode getNextVisibleNode(DefaultMutableTreeNode node)
   {
      DefaultMutableTreeNode next = null;
      TreePath current = null;

      if (node != null)
         next = node.getNextNode();

      if (next != null)
      {
         current = new TreePath(next.getPath());
         if (tree.isVisible(current))
            return next;

         while (next != null && !tree.isVisible(current))
         {
            next = next.getNextNode();

            if (next != null)
               current = new TreePath(next.getPath());
         }
      }
      return next;
   }

   /**
    * Get previous visible node in the tree.
    * Package private for use in inner classes.
    * 
    * @param the current node
    * @return the next visible node in the JTree. Return null if there are no
    *         more.
    */
   DefaultMutableTreeNode getPreviousVisibleNode
                                             (DefaultMutableTreeNode node)
   {
      DefaultMutableTreeNode prev = null;
      TreePath current = null;

      if (node != null)
         prev = node.getPreviousNode();

      if (prev != null)
      {
         current = new TreePath(prev.getPath());
         if (tree.isVisible(current))
            return prev;

         while (prev != null && !tree.isVisible(current))
         {
            prev = prev.getPreviousNode();

            if (prev != null)
               current = new TreePath(prev.getPath());
         }
      }
      return prev;
   }
   
   /**
    * Returns the row that the last item identified in path is visible at. Will
    * return -1 if any of the elments in the path are not currently visible.
    * 
    * @param tree is the current tree to return the row for.
    * @param path is the path used to find the row.
    * @return the row that the last item identified in path is visible at. Will
    *         return -1 if any of the elments in the path are not currently
    *         visible.
    */
   public int getRowForPath(JTree tree, TreePath path)
   {
      // FIXME: check visibility
      // right now, just returns last element because
      // expand/collapse is not implemented
      return path.getPathCount() - 1;
   }

   /**
    * Returns the number of rows that are being displayed.
    * 
    * @param tree is the current tree to return the number of rows for.
    * @return the number of rows being displayed.
    */
   public int getRowCount(JTree tree)
   {
      DefaultMutableTreeNode node = ((DefaultMutableTreeNode) (tree.getModel())
            .getRoot());
      int count = 0;
      
      while (node != null)
      {
         count++;
         node = getNextVisibleNode(node);
      }
      
      return count;
   }

   /**
    * Returns the path to the node that is closest to x,y. If there is nothing
    * currently visible this will return null, otherwise it'll always return a
    * valid path. If you need to test if the returned object is exactly at x,y
    * you should get the bounds for the returned path and test x,y against that.
    * 
    * @param tree the tree to search for the closest path
    * @param x is the x coordinate of the location to search
    * @param y is the y coordinate of the location to search
    * @return the tree path closes to x,y.
    */
   public TreePath getClosestPathForLocation(JTree tree, int x, int y)
   {
      return treeState.getPathClosestTo(x, y);
   }

   /**
    * Returns true if the tree is being edited. The item that is being edited
    * can be returned by getEditingPath().
    * 
    * @param tree is the tree to check for editing.
    * @return true if the tree is being edited.
    */
   public boolean isEditing(JTree tree)
   {
      // FIXME: not implemented
      return false;
   }

   /**
    * Stops the current editing session. This has no effect if the tree is not
    * being edited. Returns true if the editor allows the editing session to
    * stop.
    * 
    * @param tree is the tree to stop the editing on
    * @return true if the editor allows the editing session to stop.
    */
   public boolean stopEditing(JTree tree)
   {
      // FIXME: not implemented
      return false;
   }

   /**
    * Cancels the current editing session.
    * 
    * @param tree is the tree to cancel the editing session on.
    */
   public void cancelEditing(JTree tree)
   {
      // FIXME: not implemented
   }

   /**
    * Selects the last item in path and tries to edit it. Editing will fail if
    * the CellEditor won't allow it for the selected item.
    * 
    * @param tree is the tree to edit on.
    * @param path is the path in tree to edit on.
    */
   public void startEditingAtPath(JTree tree, TreePath path)
   {
      // FIXME: not implemented
   }

   /**
    * Returns the path to the element that is being editted.
    * 
    * @param tree is the tree to get the editing path from.
    * @return the path that is being edited.
    */
   public TreePath getEditingPath(JTree tree)
   {
      // FIXME: not implemented
      return null;
   }

   /**
    * Invoked after the tree instance variable has been set, but before any
    * default/listeners have been installed.
    */
   protected void prepareForUIInstall()
   {
      // FIXME: not implemented
   }

   /**
    * Invoked from installUI after all the defaults/listeners have been
    * installed.
    */
   protected void completeUIInstall()
   {
      // FIXME: not implemented
   }

   /**
    * Invoked from uninstallUI after all the defaults/listeners have been
    * uninstalled.
    */
   protected void completeUIUninstall()
   {
      // FIXME: not implemented
   }

   /**
    * Installs the subcomponents of the tree, which is the renderer pane.
    */
   protected void installComponents()
   {
      // FIXME: not implemented
   }

   /**
    * Creates an instance of NodeDimensions that is able to determine the size
    * of a given node in the tree.
    * 
    * @return the NodeDimensions of a given node in the tree
    */
   protected AbstractLayoutCache.NodeDimensions createNodeDimensions()
   {
      // FIXME: not implemented
      return null;
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
      return new DefaultTreeCellEditor(tree,
            (DefaultTreeCellRenderer) createDefaultCellRenderer(), cellEditor);
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
      tree.removeMouseListener(mouseInputListener);
      tree.removeKeyListener(keyListener);
      tree.removePropertyChangeListener(selectionModelPropertyChangeListener);
      tree.removeComponentListener(componentListener);
      tree.getCellEditor().removeCellEditorListener(cellEditorListener);
      tree.removeTreeExpansionListener(treeExpansionListener);
      tree.getModel().removeTreeModelListener(treeModelListener);
   }

   /**
    * Uninstall all keyboard actions.
    */
   protected void uninstallKeyboardActions()
   {
   }

   /**
    * Uninstall the rendererPane.
    */
   protected void uninstallComponents()
   {
      // FIXME: not implemented
   }

   /**
    * The vertical element of legs between nodes starts at the bottom of the
    * parent node by default. This method makes the leg start below that.
    * 
    * @return the vertical leg buffer
    */
   protected int getVerticalLegBuffer()
   {
      // FIXME: not implemented
      return 0;
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
      // FIXME: not implemented
      return 0;
   }

   /**
    * Make all the nodes that are expanded in JTree expanded in LayoutCache.
    * This invokes update ExpandedDescendants with the root path.
    */
   protected void updateLayoutCacheExpandedNodes()
   {
      // FIXME: not implemented
   }

   /**
    * Updates the expanded state of all the descendants of the <code>path</code>
    * by getting the expanded descendants from the tree and forwarding to the
    * tree state.
    * 
    * @param path the path used to update the expanded states
    */
   protected void updateExpandedDescendants(TreePath path)
   {
      // FIXME: not implemented
   }

   /**
    * Returns a path to the last child of <code>parent</code>
    * 
    * @param parent is the topmost path to specified
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
      // FIXME: not implemented
   }

   /**
    * Updates the cellEditor based on editability of the JTree that we're
    * contained in. Ig the tree is editable but doesn't have a cellEditor, a
    * basic one will be used.
    */
   protected void updateCellEditor()
   {
      // FIXME: not implemented
   }

   /**
    * Messaged from the tree we're in when the renderer has changed.
    */
   protected void updateRenderer()
   {
      // FIXME: not implemented
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
      // FIXME: not implemented
   }

   /**
    * Updates the <code>preferredSize</code> instance variable, which is
    * returned from <code>getPreferredSize()</code>. For left to right
    * orientations, the size is determined from the current AbstractLayoutCache.
    * For RTL orientations, the preferred size becomes the width minus the
    * minimum x position.
    */
   protected void updateCachedPreferredSize()
   {
      // FIXME: not implemented
   }

   /**
    * Messaged from the VisibleTreeNode after it has been expanded.
    * 
    * @param path is the path that has been expanded.
    */
   protected void pathWasExpanded(TreePath path)
   {
      // FIXME: not implemented
   }

   /**
    * Messaged from the VisibleTreeNode after it has collapsed
    */
   protected void pathWasCollapsed(TreePath path)
   {
      // FIXME: not implemented
   }

   /**
    * Install all defaults for the tree.
    * 
    * @param tree is the JTree to install defaults for
    */
   protected void installDefaults(JTree tree)
   {
      UIDefaults defaults = UIManager.getLookAndFeelDefaults();

      tree.setFont(defaults.getFont("Tree.font"));
      tree.setForeground(defaults.getColor("Tree.foreground"));
      tree.setBackground(defaults.getColor("Tree.background"));
      tree.setOpaque(true);

      rightChildIndent = defaults.getInt("Tree.rightChildIndent");
      leftChildIndent = defaults.getInt("Tree.leftChildIndent");
      setRowHeight(defaults.getInt("Tree.rowHeight"));
   }

   /**
    * Install all keyboard actions for this
    */
   protected void installKeyboardActions()
   {
   }

   /**
    * Install all listeners for this
    */
   protected void installListeners()
   {
      tree.addPropertyChangeListener(propertyChangeListener);
      tree.addFocusListener(focusListener);
      tree.addTreeSelectionListener(treeSelectionListener);
      tree.addMouseListener(mouseInputListener);
      tree.addKeyListener(keyListener);
      tree.addPropertyChangeListener(selectionModelPropertyChangeListener);
      tree.addComponentListener(componentListener);
      cellEditor.addCellEditorListener(cellEditorListener);
      tree.addTreeExpansionListener(treeExpansionListener);
      treeModel.addTreeModelListener(treeModelListener);
   }

   /**
    * Install the UI for the component
    * 
    * @param c the component to install UI for
    */
   public void installUI(JComponent c)
   {
      super.installUI(c);
      installDefaults((JTree) c);
      tree = (JTree) c;
      setModel(tree.getModel());
      tree.setRootVisible(true);
      tree.expandPath(new TreePath(((DefaultMutableTreeNode) 
            (tree.getModel()).getRoot()).getPath()));
      treeSelectionModel = tree.getSelectionModel();
      installListeners();
      installKeyboardActions();
      completeUIInstall();
   }

   /**
    * Uninstall the defaults for the tree
    * 
    * @param tree to uninstall defaults for
    */
   protected void uninstallDefaults(JTree tree)
   {
      UIDefaults defaults = UIManager.getLookAndFeelDefaults();
      tree.setFont(null);
      tree.setForeground(null);
      tree.setBackground(null);
      tree.setCellRenderer(null);
   }

   /**
    * Uninstall the UI for the component
    * 
    * @param c the component to uninstall UI for
    */
   public void uninstallUI(JComponent c)
   {
      uninstallDefaults((JTree) c);
      uninstallKeyboardActions();
      uninstallListeners();
      tree = null;
      completeUIUninstall();
   }

   /**
    * Paints the specified component appropriate for the look and feel. This
    * method is invoked from the ComponentUI.update method when the specified
    * component is being painted. Subclasses should override this method and use
    * the specified Graphics object to render the content of the component.
    * 
    * @param g the Graphics context in which to paint
    * @param c the component being painted; this argument is often ignored, but
    *        might be used if the UI object is stateless and shared by multiple
    *        components
    */
   public void paint(Graphics g, JComponent c)
   {
      JTree tree = (JTree) c;
      TreeModel mod = tree.getModel();
      g.translate(10, 10);
      paintRecursive(g, 0, 0, 0, 0, tree, mod, mod.getRoot());
      paintControlIcons(g, 0, 0, 0, 0, tree, mod, mod.getRoot());
      g.translate(-10, -10);
   }

   /**
    * Ensures that the rows identified by beginRow through endRow are visible.
    * 
    * @param beginRow is the first row
    * @param endRow is the last row
    */
   protected void ensureRowsAreVisible(int beginRow, int endRow)
   {
      // FIXME: not implemented
   }

   /**
    * Sets the preferred minimum size.
    * 
    * @param newSize is the new preferred minimum size.
    */
   public void setPreferredMinSize(Dimension newSize)
   {
      // FIXME: not implemented
   }

   /**
    * Gets the preferred minimum size.
    * 
    * @returns the preferred minimum size.
    */
   public Dimension getPreferredMinSize()
   {
      // FIXME: not implemented
      return null;
   }

   /**
    * Returns the preferred size to properly display the tree, this is a cover
    * method for getPreferredSize(c, false).
    * 
    * @param c the component whose preferred size is being queried; this
    *        argument is often ignored but might be used if the UI object is
    *        stateless and shared by multiple components
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
    * @param c the component whose preferred size is being queried.
    * @param checkConsistancy if true must check consistancy
    * @return the preferred size
    */
   public Dimension getPreferredSize(JComponent c, boolean checkConsistancy)
   {
      // FIXME: checkConsistancy not implemented, c not used
      DefaultMutableTreeNode node = ((DefaultMutableTreeNode) (tree.getModel())
            .getRoot());
      int maxWidth = 0;
      int count = 0;
      if (node != null)
      {
         maxWidth = (int) (getCellBounds(0, 0, node).getWidth());
         while (node != null)
         {
            count++;
            DefaultMutableTreeNode nextNode = node.getNextNode();
            if (nextNode != null)
               maxWidth = Math.max(maxWidth, (int) (getCellBounds(0, 0, nextNode)
                     .getWidth()));
            node = nextNode;
         }
      }
      
      return new Dimension(maxWidth, (getRowHeight() * count));
   }

   /**
    * Returns the minimum size for this component. Which will be the min
    * preferred size or (0,0).
    * 
    * @param c the component whose min size is being queried.
    * @returns the preferred size or null
    */
   public Dimension getMinimumSize(JComponent c)
   {
      // FIXME: not implemented
      return getPreferredSize(c);
   }

   /**
    * Returns the maximum size for the component, which will be the preferred
    * size if the instance is currently in JTree or (0,0).
    * 
    * @param c the component whose preferred size is being queried
    * @return the max size or null
    */
   public Dimension getMaximumSize(JComponent c)
   {
      // FIXME: not implemented
      return getPreferredSize(c);
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
      // FIXME: not implemented
   }

   /**
    * Stops the editing session. If messageStop is true, the editor is messaged
    * with stopEditing, if messageCancel is true the editor is messaged with
    * cancelEditing. If messageTree is true, the treeModel is messaged with
    * valueForPathChanged.
    * 
    * @param messageStop message to stop editing
    * @param messageCancel message to cancel editing
    * @param messageTree message to treeModel
    */
   protected void completeEditing(boolean messageStop, boolean messageCancel,
         boolean messageTree)
   {
      // FIXME: not implemented
   }

   /**
    * Will start editing for node if there is a cellEditor and shouldSelectCall
    * returns true. This assumes that path is valid and visible.
    * 
    * @param path is the path to start editing
    * @param event is the MouseEvent performed on the path
    * @return true if successful
    */
   protected boolean startEditing(TreePath path, MouseEvent event)
   {
      // FIXME: not implemented
      return false;
   }

   /**
    * If the <code>mouseX</code> and <code>mouseY</code> are in the expand
    * or collapse region of the row, this will toggle the row.
    * 
    * @param path the path we are concerned with
    * @param mouseX is the cursor's x position
    * @param mouseY is the cursor's y position
    */
   protected void checkForClickInExpandControl(TreePath path, int mouseX,
         int mouseY)
   {
      // FIXME: not implemented
   }

   /**
    * Returns true if the <code>mouseX</code> and <code>mouseY</code> fall
    * in the area of row that is used to expand/collpse the node and the node at
    * row does not represent a leaf.
    * 
    * @param path the path we are concerned with
    * @param mouseX is the cursor's x position
    * @param mouseY is the cursor's y position
    * @return true if the <code>mouseX</code> and <code>mouseY</code> fall
    *         in the area of row that is used to expand/collpse the node and the
    *         node at row does not represent a leaf.
    */
   protected boolean isLocationInExpandControl(TreePath path, int mouseX,
         int mouseY)
   {
      // FIXME: not implemented
      return false;
   }

   /**
    * Messaged when the user clicks the particular row, this invokes
    * toggleExpandState.
    * 
    * @param path the path we are concerned with
    * @param mouseX is the cursor's x position
    * @param mouseY is the cursor's y position
    */
   protected void handleExpandControlClick(TreePath path, int mouseX, int mouseY)
   {
      // FIXME: not implemented
   }

   /**
    * Expands path if it is not expanded, or collapses row if it is expanded. If
    * expanding a path and JTree scroll on expand, ensureRowsAreVisible is
    * invoked to scroll as many of the children to visible as possible (tries to
    * scroll to last visible descendant of path).
    * 
    * @param path the path we are concerned with
    */
   protected void toggleExpandState(TreePath path)
   {
      // FIXME: not implemented
   }

   /**
    * Returning true signifies a mouse event on the node should toggle the
    * selection of only the row under the mouse.
    * 
    * @param event is the MouseEvent performed on the row.
    * @return true signifies a mouse event on the node should toggle the
    *         selection of only the row under the mouse.
    */
   protected boolean isToggleSelectionEvent(MouseEvent event)
   {
      // FIXME: not implemented
      return false;
   }

   /**
    * Returning true signifies a mouse event on the node should select from the
    * anchor point.
    * 
    * @param event is the MouseEvent performed on the node.
    * @return true signifies a mouse event on the node should select from the
    *         anchor point.
    */
   protected boolean isMultiSelectEvent(MouseEvent event)
   {
      // FIXME: not implemented
      return false;
   }

   /**
    * Returning true indicates the row under the mouse should be toggled based
    * on the event. This is invoked after checkForClickInExpandControl, implying
    * the location is not in the expand (toggle) control.
    * 
    * @param event is the MouseEvent performed on the row.
    * @return true indicates the row under the mouse should be toggled based on
    *         the event.
    */
   protected boolean isToggleEvent(MouseEvent event)
   {
      // FIXME: not implemented
      return false;
   }

   /**
    * Messaged to update the selection based on a MouseEvent over a particular
    * row. If the even is a toggle selection event, the row is either selected,
    * or deselected. If the event identifies a multi selection event, the
    * selection is updated from the anchor point. Otherwise, the row is
    * selected, and if the even specified a toggle event the row is
    * expanded/collapsed.
    * 
    * @param path is the path selected for an event
    * @param event is the MouseEvent performed on the path.
    */
   protected void selectPathForEvent(TreePath path, MouseEvent event)
   {
      // FIXME: not implemented
   }

   /**
    * Returns true if the node at <code>row</code> is a leaf.
    * 
    * @param row is the row we are concerned with.
    * @return true if the node at <code>row</code> is a leaf.
    */
   protected boolean isLeaf(int row)
   {
      TreePath pathForRow = getPathForRow(tree, row);
      if (pathForRow == null)
         return true;

      Object node = pathForRow.getLastPathComponent();

      if (node instanceof TreeNode)
         return ((TreeNode) node).isLeaf();
      else
         return true;
   }

   /**
    * Selects the specified path in the tree depending on modes.
    * Package private for use in inner classes.
    * 
    * @param tree is the tree we are selecting the path in
    * @param path is the path we are selecting
    */
   void selectPath(JTree tree, TreePath path)
   {
      if (path != null)
      {
         if (tree.isPathSelected(path))
            tree.removeSelectionPath(path);
         else if (tree.getSelectionModel().getSelectionMode() 
               == TreeSelectionModel.SINGLE_TREE_SELECTION)
         {
            tree.getSelectionModel().clearSelection();
            tree.addSelectionPath(path);
            tree.setLeadSelectionPath(path);
         }
         else if (tree.getSelectionModel().getSelectionMode() 
               == TreeSelectionModel.CONTIGUOUS_TREE_SELECTION)
         {
            // TODO
         }
         else
         {
            tree.getSelectionModel().setSelectionMode(
                  TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION);
            tree.addSelectionPath(path);
            tree.setLeadSelectionPath(path);
         }
      }
   }
   
   /* * INTERNAL CLASSES * */

   /**
    * Updates the preferred size when scrolling, if necessary.
    */
   public class ComponentHandler
         extends ComponentAdapter
         implements ActionListener
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
      }

      /**
       * Invoked when the component's position changes.
       * 
       * @param e the event that occurs when moving the component
       */
      public void componentMoved(ComponentEvent e)
      {
      }

      /**
       * Creats, if necessary, and starts a Timer to check if needed to resize
       * the bounds
       */
      protected void startTimer()
      {
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
       * Public as a result of Timer. If the scrollBar is null, or not
       * adjusting, this stops the timer and updates the sizing.
       * 
       * @param ae is the action performed
       */
      public void actionPerformed(ActionEvent ae)
      {
      }
   }// ComponentHandler

   /**
    * Listener responsible for getting cell editing events and updating the tree
    * accordingly.
    */
   public class CellEditorHandler
         implements CellEditorListener
   {
      /**
       * Constructor
       */
      public CellEditorHandler()
      {
      }

      /**
       * Messaged when editing has stopped in the tree. Tells the listeners
       * editing has stopped.
       * 
       * @param e is the notification event
       */
      public void editingStopped(ChangeEvent e)
      {
      }

      /**
       * Messaged when editing has been canceled in the tree. This tells the
       * listeners the editor has canceled editing.
       * 
       * @param e is the notification event
       */
      public void editingCanceled(ChangeEvent e)
      {
      }
   }// CellEditorHandler

   /**
    * Repaints the lead selection row when focus is lost/grained.
    */
   public class FocusHandler
         implements FocusListener
   {
      /**
       * Constructor
       */
      public FocusHandler()
      {
      }

      /**
       * Invoked when focus is activated on the tree we're in, redraws the lead
       * row. Invoked when a component gains the keyboard focus.
       * 
       * @param e is the focus event that is activated
       */
      public void focusGained(FocusEvent e)
      {
      }

      /**
       * Invoked when focus is deactivated on the tree we're in, redraws the
       * lead row. Invoked when a component loses the keyboard focus.
       * 
       * @param e is the focus event that is deactivated
       */
      public void focusLost(FocusEvent e)
      {
      }
   }// FocusHandler

   /**
    * This is used to get multiple key down events to appropriately genereate
    * events.
    */
   public class KeyHandler
         extends KeyAdapter
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
      }

      /**
       * Invoked when a key has been typed. Moves the keyboard focus to the
       * first element whose first letter matches the alphanumeric key pressed
       * by the user. Subsequent same key presses move the keyboard focus to the
       * next object that starts with the same letter.
       * 
       * @param e the key typed
       */
      public void keyTyped(KeyEvent e)
      {
      }

      /**
       * Invoked when a key has been pressed.
       * 
       * @param e the key pressed
       */
      public void keyPressed(KeyEvent e)
      {         
         TreePath start = BasicTreeUI.this.tree.getLeadSelectionPath();
         DefaultMutableTreeNode last = null;
         
         if (start != null)
            last = (DefaultMutableTreeNode) start.getLastPathComponent();
         if (last != null)
         {
            if (e.getKeyCode() == KeyEvent.VK_DOWN)
            {
               DefaultMutableTreeNode next = (DefaultMutableTreeNode) 
                  BasicTreeUI.this.getNextVisibleNode(last);
               
               if (next != null)
                  BasicTreeUI.this.selectPath(BasicTreeUI.this.tree,
                        new TreePath(next.getPath()));
            }
            else if (e.getKeyCode() == KeyEvent.VK_UP)
            {
               DefaultMutableTreeNode prev = (DefaultMutableTreeNode) 
               BasicTreeUI.this.getPreviousVisibleNode(last);
            
            if (prev != null)
               BasicTreeUI.this.selectPath(BasicTreeUI.this.tree,
                     new TreePath(prev.getPath()));
            }
            else if (e.getKeyCode() == KeyEvent.VK_LEFT)
            {
               TreePath path = new TreePath(last.getPath());
               
               if (!last.isLeaf() && BasicTreeUI.this.tree.isExpanded(path))
               {
                  BasicTreeUI.this.tree.collapsePath(path);
                  BasicTreeUI.this.tree.fireTreeCollapsed(path);
               }
            }
            else if (e.getKeyCode() == KeyEvent.VK_RIGHT)
            {
               TreePath path = new TreePath(last.getPath());
   
               if (!last.isLeaf() && BasicTreeUI.this.tree.isCollapsed(path))
               {
                  BasicTreeUI.this.tree.expandPath(path);
                  BasicTreeUI.this.tree.fireTreeExpanded(path);
               }
            }
         }
      }

      /**
       * Invoked when a key has been released
       * 
       * @param e the key released
       */
      public void keyReleased(KeyEvent e)
      {
      }
   }// KeyHandler

   /**
    * MouseListener is responsible for updating the selevtion based on mouse
    * events.
    */
   public class MouseHandler
         extends MouseAdapter
         implements MouseMotionListener
   {
      /**
       * Constructor
       */
      public MouseHandler()
      {
      }

      /**
       * Invoked when a mouse button has been pressed on a component.
       * 
       * @param e is the mouse event that occured
       */
      public void mousePressed(MouseEvent e)
      {
      }

      /**
       * Invoked when a mouse button is pressed on a component and then dragged.
       * MOUSE_DRAGGED events will continue to be delivered to the component
       * where the drag originated until the mouse button is released
       * (regardless of whether the mouse position is within the bounds of the
       * component).
       * 
       * @param e is the mouse event that occured
       */
      public void mouseDragged(MouseEvent e)
      {
      }

      /**
       * Invoked when the mouse button has been moved on a component (with no
       * buttons no down).
       * 
       * @param e the mouse event that occured
       */
      public void mouseMoved(MouseEvent e)
      {
      }

      /**
       * Invoked when a mouse button has been released on a component.
       * 
       * @param e is the mouse event that occured
       */
      public void mouseReleased(MouseEvent e)
      {
      }
   }// MouseHandler

   /**
    * MouseInputHandler handles passing all mouse events, including mouse motion
    * events, until the mouse is released to the destination it is constructed
    * with.
    */
   public class MouseInputHandler
         implements MouseInputListener
   {
      /** Source that events are coming from */
      protected Component source;

      /** Destination that receives all events. */
      protected Component destination;
      
      /** Number of mouse clicks on a non-leaf */
      private int clickCount = 0;

      /**
       * Constructor
       * 
       * @param source that events are coming from
       * @param destination that receives all events
       * @param event is the event received
       */
      public MouseInputHandler(Component source, Component destination,
            MouseEvent e)
      {
      }

      /**
       * Invoked when the mouse button has been clicked (pressed and released)
       * on a component.
       * 
       * @param e mouse event that occured
       */
      public void mouseClicked(MouseEvent e)
      {
         Point click = e.getPoint();
         int clickX = (int) click.getX();
         int clickY = (int) click.getY();
         int row = (clickY  / getRowHeight()) - 1;
         TreePath path = BasicTreeUI.this.tree.getPathForRow(row);
         
         boolean inBounds = false;
         boolean cntlClick = false;
         Rectangle bounds = BasicTreeUI.this.getPathBounds(
               BasicTreeUI.this.tree, path);
         int x = (int) bounds.getX();
         int y = (int) bounds.getY();

         if (clickY > y && clickY < (y + bounds.height + 10))
         {
            if (clickX > x && clickX < (x + bounds.width + 20))
               inBounds = true;
            else if (clickX < (x - rightChildIndent + 5) && 
                  clickX > (x - rightChildIndent - 5))
               cntlClick = true;
         }

         if ((inBounds || cntlClick) && path != null && 
               BasicTreeUI.this.tree.isVisible(path))
         {           
            if (!cntlClick && !BasicTreeUI.this.isLeaf(row))
               clickCount++;
            
            if (clickCount == 2 || cntlClick == true)
            {
               clickCount = 0;
               BasicTreeUI.this.tree.getSelectionModel().clearSelection();
               if (BasicTreeUI.this.tree.isExpanded(path))
               {
                  BasicTreeUI.this.tree.collapsePath(path);
                  BasicTreeUI.this.tree.fireTreeCollapsed(path);
               }
               else
               {
                  BasicTreeUI.this.tree.expandPath(path);
                  BasicTreeUI.this.tree.fireTreeExpanded(path);
               }
            }
            
            BasicTreeUI.this.selectPath(BasicTreeUI.this.tree, path);
         }
      }

      /**
       * Invoked when a mouse button has been pressed on a component.
       * 
       * @param e mouse event that occured
       */
      public void mousePressed(MouseEvent e)
      {
      }

      /**
       * Invoked when a mouse button has been released on a component.
       * 
       * @param e mouse event that occured
       */
      public void mouseReleased(MouseEvent e)
      {
      }

      /**
       * Invoked when the mouse enters a component.
       * 
       * @param e mouse event that occured
       */
      public void mouseEntered(MouseEvent e)
      {
      }

      /**
       * Invoked when the mouse exits a component.
       * 
       * @param e mouse event that occured
       */
      public void mouseExited(MouseEvent e)
      {
      }

      /**
       * Invoked when a mouse button is pressed on a component and then dragged.
       * MOUSE_DRAGGED events will continue to be delivered to the component
       * where the drag originated until the mouse button is released
       * (regardless of whether the mouse position is within the bounds of the
       * component).
       * 
       * @param e mouse event that occured
       */
      public void mouseDragged(MouseEvent e)
      {
      }

      /**
       * Invoked when the mouse cursor has been moved onto a component but no
       * buttons have been pushed.
       * 
       * @param e mouse event that occured
       */
      public void mouseMoved(MouseEvent e)
      {
      }

      /**
       * Removes event from the source
       */
      protected void removeFromSource()
      {
      }
   }// MouseInputHandler

   /**
    * Class responsible for getting size of node, method is forwarded to
    * BasicTreeUI method. X location does not include insets, that is handled in
    * getPathBounds.
    */
   public class NodeDimensionsHandler
         extends AbstractLayoutCache.NodeDimensions
   {
      /**
       * Constructor
       */
      public NodeDimensionsHandler()
      {
      }

      /**
       * Responsible for getting the size of a particular node.
       * 
       * @param value the value to be represented
       * @param row row being queried
       * @param depth the depth of the row
       * @param expanded true if row is expanded
       * @param size a Rectangle containing the size needed to represent value
       * @return containing the node dimensions, or null if node has no
       *         dimension
       */
      public Rectangle getNodeDimensions(Object value, int row, int depth,
            boolean expanded, Rectangle size)
      {
         return null;
      }

      /**
       * Returns the amount to indent the given row
       * 
       * @return amount to indent the given row.
       */
      protected int getRowX(int row, int depth)
      {
         return 0;
      }
   }// NodeDimensionsHandler

   /**
    * PropertyChangeListener for the tree. Updates the appropriate varaible, or
    * TreeState, based on what changes.
    */
   public class PropertyChangeHandler
         implements PropertyChangeListener
   {

      /**
       * Constructor
       */
      public PropertyChangeHandler()
      {
      }

      /**
       * This method gets called when a bound property is changed.
       * 
       * @param event A PropertyChangeEvent object describing the event source
       *        and the property that has changed.
       */
      public void propertyChange(PropertyChangeEvent event)
      {
      }
   }// PropertyChangeHandler

   /**
    * Listener on the TreeSelectionModel, resets the row selection if any of the
    * properties of the model change.
    */
   public class SelectionModelPropertyChangeHandler
         implements PropertyChangeListener
   {

      /**
       * Constructor
       */
      public SelectionModelPropertyChangeHandler()
      {
      }

      /**
       * This method gets called when a bound property is changed.
       * 
       * @param event A PropertyChangeEvent object describing the event source
       *        and the property that has changed.
       */
      public void propertyChange(PropertyChangeEvent event)
      {
      }
   }// SelectionModelPropertyChangeHandler

   /**
    * ActionListener that invokes cancelEditing when action performed.
    */
   public class TreeCancelEditingAction
         extends AbstractAction
   {

      /**
       * Constructor
       */
      public TreeCancelEditingAction()
      {
      }

      /**
       * Invoked when an action occurs.
       * 
       * @param e event that occured
       */
      public void actionPerformed(ActionEvent e)
      {
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
   }// TreeCancelEditingAction

   /**
    * Updates the TreeState in response to nodes expanding/collapsing.
    */
   public class TreeExpansionHandler
         implements TreeExpansionListener
   {

      /**
       * Constructor
       */
      public TreeExpansionHandler()
      {
      }

      /**
       * Called whenever an item in the tree has been expanded.
       * 
       * @param event is the event that occured
       */
      public void treeExpanded(TreeExpansionEvent event)
      {
         BasicTreeUI.this.tree.repaint();
      }

      /**
       * Called whenever an item in the tree has been collapsed.
       * 
       * @param event is the event that occured
       */
      public void treeCollapsed(TreeExpansionEvent event)
      {
         BasicTreeUI.this.tree.repaint();
      }
   }// TreeExpansionHandler

   /**
    * TreeHomeAction is used to handle end/home actions. Scrolls either the
    * first or last cell to be visible based on direction.
    */
   public class TreeHomeAction
         extends AbstractAction
   {

      /** direction is either home or end */
      protected int direction;

      /**
       * Constructor
       * 
       * @param direction - it is home or end
       * @param name is the name of the direction
       */
      public TreeHomeAction(int direction, String name)
      {
      }

      /**
       * Invoked when an action occurs.
       * 
       * @param e is the event that occured
       */
      public void actionPerformed(ActionEvent e)
      {
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
   }// TreeHomeAction

   /**
    * TreeIncrementAction is used to handle up/down actions. Selection is moved
    * up or down based on direction.
    */
   public class TreeIncrementAction
         extends AbstractAction
   {

      /** Specifies the direction to adjust the selection by. */
      protected int direction;

      /**
       * Constructor
       * 
       * @param direction up or down
       * @param name is the name of the direction
       */
      public TreeIncrementAction(int direction, String name)
      {
      }

      /**
       * Invoked when an action occurs.
       * 
       * @param e is the event that occured
       */
      public void actionPerformed(ActionEvent e)
      {
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
   }// TreeIncrementAction

   /**
    * Forwards all TreeModel events to the TreeState.
    */
   public class TreeModelHandler
         implements TreeModelListener
   {
      /**
       * Constructor
       */
      public TreeModelHandler()
      {
      }

      /**
       * Invoked after a node (or a set of siblings) has changed in some way.
       * The node(s) have not changed locations in the tree or altered their
       * children arrays, but other attributes have changed and may affect
       * presentation. Example: the name of a file has changed, but it is in the
       * same location in the file system. To indicate the root has changed,
       * childIndices and children will be null. Use e.getPath() to get the
       * parent of the changed node(s). e.getChildIndices() returns the
       * index(es) of the changed node(s).
       * 
       * @param e is the event that occured
       */
      public void treeNodesChanged(TreeModelEvent e)
      {
      }

      /**
       * Invoked after nodes have been inserted into the tree. Use e.getPath()
       * to get the parent of the new node(s). e.getChildIndices() returns the
       * index(es) of the new node(s) in ascending order.
       * 
       * @param e is the event that occured
       */
      public void treeNodesInserted(TreeModelEvent e)
      {
      }

      /**
       * Invoked after nodes have been removed from the tree. Note that if a
       * subtree is removed from the tree, this method may only be invoked once
       * for the root of the removed subtree, not once for each individual set
       * of siblings removed. Use e.getPath() to get the former parent of the
       * deleted node(s). e.getChildIndices() returns, in ascending order, the
       * index(es) the node(s) had before being deleted.
       * 
       * @param e is the event that occured
       */
      public void treeNodesRemoved(TreeModelEvent e)
      {
      }

      /**
       * Invoked after the tree has drastically changed structure from a given
       * node down. If the path returned by e.getPath() is of length one and the
       * first element does not identify the current root node the first element
       * should become the new root of the tree. Use e.getPath() to get the path
       * to the node. e.getChildIndices() returns null.
       * 
       * @param e is the event that occured
       */
      public void treeStructureChanged(TreeModelEvent e)
      {
      }
   }// TreeModelHandler

   /**
    * TreePageAction handles page up and page down events.
    */
   public class TreePageAction
         extends AbstractAction
   {
      /** Specifies the direction to adjust the selection by. */
      protected int direction;

      /**
       * Constructor
       * 
       * @param direction up or down
       * @param name is the name of the direction
       */
      public TreePageAction(int direction, String name)
      {
      }

      /**
       * Invoked when an action occurs.
       * 
       * @param e is the event that occured
       */
      public void actionPerformed(ActionEvent e)
      {
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
   public class TreeSelectionHandler
         implements TreeSelectionListener
   {
      /**
       * Constructor
       */
      public TreeSelectionHandler()
      {
      }

      /**
       * Messaged when the selection changes in the tree we're displaying for.
       * Stops editing, messages super and displays the changed paths.
       * 
       * @param event the event that characterizes the change.
       */
      public void valueChanged(TreeSelectionEvent event)
      {
      }
   }// TreeSelectionHandler

   /**
    * For the first selected row expandedness will be toggled.
    */
   public class TreeToggleAction
         extends AbstractAction
   {
      /**
       * Constructor
       * 
       * @param name is the name of <code>Action</code> field
       */
      public TreeToggleAction(String name)
      {
      }

      /**
       * Invoked when an action occurs.
       * 
       * @param e the event that occured
       */
      public void actionPerformed(ActionEvent e)
      {
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
    * TreeTraverseAction is the action used for left/right keys. Will toggle
    * the expandedness of a node, as well as potentially incrementing the
    * selection.
    */
   public class TreeTraverseAction
         extends AbstractAction
   {
      /**
       * Determines direction to traverse, 1 means expand, -1 means collapse.
       */
      protected int direction;

      /**
       * Constructor
       * 
       * @param direction to traverse
       * @param name is the name of the direction
       */
      public TreeTraverseAction(int direction, String name)
      {
      }

      /**
       * Invoked when an action occurs.
       * 
       * @param e the event that occured
       */
      public void actionPerformed(ActionEvent e)
      {
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
   } // TreeTraverseAction

   /**
    * Returns the cell bounds for painting selected cells
    * 
    * @param x is the x location of the cell
    * @param y is the y location of the cell
    * @param cell is the Object to get the bounds for
    * 
    * @returns Rectangle that represents the cell bounds
    */
   private Rectangle getCellBounds(int x, int y, Object cell)
   {
      if (cell != null)
      {
         String s = cell.toString();
         Font f = tree.getFont();
         FontMetrics fm = tree.getToolkit().getFontMetrics(tree.getFont());

         return new Rectangle(x, y, SwingUtilities.computeStringWidth(fm, s),
               fm.getHeight());
      }
      return null;
   }

   /**
    * Retrieves the location of some node, recursively starting at from
    * some node.
    * 
    * @param x is the starting x position, offset
    * @param y is the starting y position, offset
    * @param tree is the tree to traverse
    * @param mod is the TreeModel to use
    * @param node is the node to get the location for
    * @param startNode is the node to start searching from
    * 
    * @return Point - the location of node
    */
   private Point getCellLocation(int x, int y, JTree tree, TreeModel mod,
         Object node, Object startNode)
   {
      int rowHeight = getRowHeight();
      if (startNode == null || startNode.equals(node))
         return new Point(x + ((((DefaultMutableTreeNode) node).
               getLevel() + 1) * rightChildIndent), y);

      if (!mod.isLeaf(startNode)
            && tree.isExpanded(new TreePath(
                  ((DefaultMutableTreeNode) startNode).getPath())))
      {
         Object child = mod.getChild(startNode, 0);
         if (child != null)
            return getCellLocation(x, y + rowHeight, tree, mod,
                  node, child);
      }
      
         return getCellLocation(x, y + rowHeight, tree, mod, node,
               getNextVisibleNode((DefaultMutableTreeNode) startNode));
   }
   
   /**
    * Paints a leaf in the tree
    * 
    * @param g the Graphics context in which to paint
    * @param x the x location of the leaf
    * @param y the y location of the leaf
    * @param tree the tree to draw on
    * @param leaf the object to draw
    */
   private void paintLeaf(Graphics g, int x, int y, JTree tree, Object leaf)
   {
      TreePath curr = new TreePath(((DefaultMutableTreeNode) leaf).getPath());
      boolean selected = tree.isPathSelected(curr);

      if (tree.isVisible(curr))
      {          
         DefaultTreeCellRenderer dtcr = (DefaultTreeCellRenderer) 
                                             tree.getCellRenderer();
         boolean hasIcons = false;
         Icon li = dtcr.getLeafIcon();
         if (li != null)
            hasIcons = true;
         
         if (selected)
         {
            Component c = dtcr.getTreeCellRendererComponent(tree, leaf,
                  true, false, true, 0, false);
            
            if (hasIcons)
            {
               li.paintIcon(c, g, x, y + 2);
               x += li.getIconWidth() + 4;
            }
            rendererPane.paintComponent(g, c, tree, 
                                    getCellBounds(x, y, leaf));
         }
         else
         {            
            Component c = dtcr.getTreeCellRendererComponent(
                  tree, leaf, false, false, true, 0, false);
            
            g.translate(x, y);
            
            if (hasIcons)
            {
               Component icon = dtcr.getTreeCellRendererComponent(tree, 
                  li, false, false, true, 0, false); 
               icon.paint(g);
            }
            
            c.paint(g);
            g.translate(-x, -y);
         }
      }
   }

   /**
    * Paints a non-leaf in the tree
    * 
    * @param g the Graphics context in which to paint
    * @param x the x location of the non-leaf
    * @param y the y location of the non-leaf
    * @param tree the tree to draw on
    * @param nonLeaf the object to draw
    */
   private void paintNonLeaf(Graphics g, int x, int y, JTree tree,
         Object nonLeaf)
   {
      TreePath curr = new TreePath(((DefaultMutableTreeNode) nonLeaf).getPath());
      boolean selected = tree.isPathSelected(curr);
      boolean expanded = tree.isExpanded(curr);

      if (tree.isVisible(curr))
      {
            DefaultTreeCellRenderer dtcr = (DefaultTreeCellRenderer) 
                                                tree.getCellRenderer();
            boolean hasIcons = false;
            boolean hasOtherIcons = false;
            Icon oi = dtcr.getOpenIcon();
            Icon ci = dtcr.getClosedIcon();
            
            if (oi != null || ci != null)
               hasIcons = true;
            
            if (selected)
            {      
               Component c = dtcr.getTreeCellRendererComponent(tree, nonLeaf,
                     true, expanded, false, 0, false);

               if (hasIcons)
               {
                  if (expanded)
                  {
                     oi.paintIcon(c, g, x, y + 2);
                     x += (oi.getIconWidth() + 4);
                  }
                  else
                  {
                     ci.paintIcon(c, g, x, y + 2);
                     x += (ci.getIconWidth() + 4);
                  }
                  
               }
               rendererPane.paintComponent(g, c, tree, 
                           getCellBounds(x, y, nonLeaf));
            }
            else
            {
               Component c = dtcr.getTreeCellRendererComponent(tree, nonLeaf, 
                                          false, expanded, false, 0, false);
               g.translate(x, y);
               
               if (hasIcons)
               {
                  Component icon;
                  if (expanded)
                     icon = dtcr.getTreeCellRendererComponent(tree, 
                        oi, false, false, false, 0, false);
                  else
                     icon = dtcr.getTreeCellRendererComponent(tree, 
                        ci, false, false, false, 0, false);
                  
                  icon.paint(g);
               }
               c.paint(g);
               g.translate(-x, -y);
            }
      }
   }

   /**
    * Recursively paints all elements of the tree
    * 
    * @param g the Graphics context in which to paint
    * @param indentation of the current object
    * @param descent is the number of elements drawn
    * @param childNumber is the index of the current child in the tree
    * @param depth is the depth of the current object in the tree
    * @param tree is the tree to draw to
    * @param mod is the TreeModel we are using to draw
    * @param curr is the current object to draw
    * 
    * @return int - current descent of the tree
    */
   private int paintRecursive(Graphics g, int indentation, int descent,
         int childNumber, int depth, JTree tree, TreeModel mod, Object curr)
   {
      Rectangle clip = g.getClipBounds();
      if (indentation > clip.x + clip.width + rightChildIndent
            || descent > clip.y + clip.height + getRowHeight())
         return descent;

      int halfHeight = getRowHeight() / 2;
      int halfWidth = rightChildIndent / 2;
      int y0 = descent + halfHeight;
      int heightOfLine = descent + halfHeight;
      
      if (mod.isLeaf(curr))
      {
         paintLeaf(g, indentation + 4, descent, tree, curr);
         descent += getRowHeight();
      }
      else
      {
         if (depth > 0 || tree.isRootVisible())
         {
            paintNonLeaf(g, indentation + 4, descent, tree, curr);
            descent += getRowHeight();
            y0 += halfHeight;
         }
         
         int max = mod.getChildCount(curr);
         if (tree.isExpanded(new TreePath(((DefaultMutableTreeNode) curr)
               .getPath())))
         {
            for (int i = 0; i < max; ++i)
            {
               g.setColor(getHashColor());
               heightOfLine = descent + halfHeight;
               g.drawLine(indentation + halfWidth, heightOfLine,
                     indentation + rightChildIndent, heightOfLine);
                              
               descent = paintRecursive(g, indentation + rightChildIndent,
                     descent, i, depth + 1, tree, mod, mod.getChild(curr, i));
            }
         }
      }

      if (tree.isExpanded(new TreePath(((DefaultMutableTreeNode) curr)
            .getPath())))
         if (y0 != heightOfLine)
         {
            g.setColor(getHashColor());
            g.drawLine(indentation + halfWidth, y0, indentation + halfWidth,
                  heightOfLine);
         }
      
      return descent;
   }
   
   /**
    * Recursively paints all the control icons on the tree.
    * 
    * @param g the Graphics context in which to paint
    * @param indentation of the current object
    * @param descent is the number of elements drawn
    * @param childNumber is the index of the current child in the tree
    * @param depth is the depth of the current object in the tree
    * @param tree is the tree to draw to
    * @param mod is the TreeModel we are using to draw
    * @param curr is the current object to draw
    * 
    * @return int - current descent of the tree
    */
   private int paintControlIcons(Graphics g, int indentation, int descent,
         int childNumber, int depth, JTree tree, TreeModel mod, Object node)
   {
      int h = descent;
      int rowHeight = getRowHeight();
      Icon ei = UIManager.getLookAndFeelDefaults().
         getIcon("Tree.expandedIcon");
      Icon ci = UIManager.getLookAndFeelDefaults().
         getIcon("Tree.collapsedIcon");
      Rectangle clip = g.getClipBounds();
      if (ci == null || ei == null || indentation > clip.x + clip.width +
            rightChildIndent || descent > clip.y + clip.height + 
               getRowHeight())
         return descent;
      
      if (mod.isLeaf(node))
         descent += rowHeight;
      else 
      {
         if (depth > 0 || tree.isRootVisible())
            descent += rowHeight;
         
         int max = mod.getChildCount(node);
         if (tree.isExpanded(new TreePath(((DefaultMutableTreeNode) node)
               .getPath())))
         {
            if (!node.equals(mod.getRoot()))
               ei.paintIcon(tree, g, indentation - rightChildIndent - 3, h);
            
            for (int i = 0; i < max; ++i)
            {           
               descent = paintControlIcons(g, indentation + rightChildIndent,
                     descent, i, depth + 1, tree, mod, mod.getChild(node, i));
            }
         }
         else if (!node.equals(mod.getRoot()))
            ci.paintIcon(tree, g, indentation - rightChildIndent - 3, 
                  descent - getRowHeight());
      }
      
      return descent;
   }
} // BasicTreeUI