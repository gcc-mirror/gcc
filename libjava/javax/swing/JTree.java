/* JTree.java --
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


package javax.swing;

import java.awt.Dimension;
import java.awt.Rectangle;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Vector;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeExpansionListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.event.TreeWillExpandListener;
import javax.swing.plaf.TreeUI;
import javax.swing.tree.ExpandVetoException;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeCellEditor;
import javax.swing.tree.TreeCellRenderer;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;


public class JTree extends JComponent
  implements Scrollable, Accessible
{
  private static final long serialVersionUID = 7559816092864483649L;

  public static final String ANCHOR_SELECTION_PATH_PROPERTY = "anchorSelectionPath";
  public static final String CELL_EDITOR_PROPERTY = "cellEditor";
  public static final String CELL_RENDERER_PROPERTY = "cellRenderer";
  public static final String EDITABLE_PROPERTY = "editable";
  public static final String EXPANDS_SELECTED_PATHS_PROPERTY = "expandsSelectedPaths";
  public static final String INVOKES_STOP_CELL_EDITING_PROPERTY = "invokesStopCellEditing";
  public static final String LARGE_MODEL_PROPERTY = "largeModel";
  public static final String LEAD_SELECTION_PATH_PROPERTY = "leadSelectionPath";
  public static final String ROOT_VISIBLE_PROPERTY = "rootVisible";
  public static final String ROW_HEIGHT_PROPERTY = "rowHeight";
  public static final String SCROLLS_ON_EXPAND_PROPERTY = "scrollsOnExpand";
  public static final String SELECTION_MODEL_PROPERTY = "selectionModel";
  public static final String SHOWS_ROOT_HANDLES_PROPERTY = "showsRootHandles";
  public static final String TOGGLE_CLICK_COUNT_PROPERTY = "toggleClickCount";
  public static final String TREE_MODEL_PROPERTY = "model";
  public static final String VISIBLE_ROW_COUNT_PROPERTY = "visibleRowCount";

  protected TreeCellEditor cellEditor;
  protected TreeCellRenderer cellRenderer;
  protected boolean editable;
  protected boolean invokesStopCellEditing;
  protected boolean largeModel;
  protected boolean rootVisible;
  protected int rowHeight;
  protected boolean scrollsOnExpand;
  protected TreeSelectionModel selectionModel;
  protected boolean showsRootHandles;
  protected int toggleClickCount;
  protected TreeModel treeModel;
  protected int visibleRowCount;

  /**
   * Creates a new <code>JTree</code> object.
   */
  public JTree()
  {
    this(createTreeModel(null));
  }

  /**
   * Creates a new <code>JTree</code> object.
   *
   * @param value the initial nodes in the tree
   */
  public JTree(Hashtable value)
  {
    this(createTreeModel(value));
  }

  /**
   * Creates a new <code>JTree</code> object.
   *
   * @param value the initial nodes in the tree
   */
  public JTree(Object[] value)
  {
    this(createTreeModel(value));
  }

  /**
   * Creates a new <code>JTree</code> object.
   *
   * @param model the model to use
   */
  public JTree(TreeModel model)
  {
    treeModel = model;
    setCellRenderer(new DefaultTreeCellRenderer());
    updateUI();
  }

  /**
   * Creates a new <code>JTree</code> object.
   *
   * @param root the root node
   */
  public JTree(TreeNode root)
  {
    this(root, false);
  }

  /**
   * Creates a new <code>JTree</code> object.
   *
   * @param root the root node
   * @param asksAllowChildren if false, all nodes without children are leaf nodes.
   * If true, only nodes that do not allow children are leaf nodes.
   */
  public JTree(TreeNode root, boolean asksAllowChildren)
  {
    this(new DefaultTreeModel(root, asksAllowChildren));
  }

  /**
   * Creates a new <code>JTree</code> object.
   *
   * @param value the initial nodes in the tree
   */
  public JTree(Vector value)
  {
    this(createTreeModel(value));
  }

  public static class DynamicUtilTreeNode 
    extends DefaultMutableTreeNode
  {
    protected Object childValue;
    protected boolean loadedChildren;
    public DynamicUtilTreeNode(Object value,
                               Object children) 
    {
      super(value);
      childValue = children;
      loadedChildren = false;
    }

    public int getChildCount()
    {
      loadChildren();
      return super.getChildCount();
    }

    protected void loadChildren()
    {
      if (!loadedChildren)
        {
          createChildren(this, childValue);
          loadedChildren = true;
        }
    }
    
    public Enumeration children()
    {
      loadChildren();
      return super.children();
    }

    public boolean isLeaf() 
    {
      return (childValue == null || 
              !(childValue instanceof Hashtable
               || childValue instanceof Vector
               || childValue.getClass().isArray()));
    }

    public static void createChildren(DefaultMutableTreeNode parent,
                                      Object children)
    {
      if (children instanceof Hashtable)
        {
          Hashtable tab = (Hashtable) children;
          Enumeration e = tab.keys();
          while (e.hasMoreElements()) 
            {
              Object key = e.nextElement();
              Object val = tab.get(key);
              parent.add(new DynamicUtilTreeNode(key, val));
            }
        }
      else if (children instanceof Vector)
        {
          Iterator i = ((Vector)children).iterator();
          while (i.hasNext())
            {
              Object n = i.next();
              parent.add(new DynamicUtilTreeNode(n,n));
            }
        }
      else if (children.getClass().isArray())
        {
          Object[] arr = (Object[]) children;
          for (int i = 0; i < arr.length; ++i)
            parent.add(new DynamicUtilTreeNode(arr[i], arr[i]));
      }
    }
  }

  /**
   * Creates a new <code>TreeModel</code> object.
   *
   * @param value the values stored in the model
   */
  protected static TreeModel createTreeModel(Object value)
  {
    return new DefaultTreeModel(new DynamicUtilTreeNode(value, value));
  }

  /**
   * Return the UI associated with this <code>JTree</code> object.
   *
   * @return the associated <code>TreeUI</code> object
   */
  public TreeUI getUI()
  {
    return (TreeUI) ui;
  }

  /**
   * Sets the UI associated with this <code>JTree</code> object.
   *
   * @param ui the <code>TreeUI</code> to associate
   */
  public void setUI(TreeUI ui)
  {
    super.setUI(ui);
  }

  /**
   * This method resets the UI used to the Look and Feel defaults..
   */
  public void updateUI()
  {
    setUI((TreeUI) UIManager.getUI(this));
    revalidate();
    repaint();
  }

  /**
   * This method returns the String ID of the UI class of  Separator.
   *
   * @return The UI class' String ID.
   */
  public String getUIClassID()
  {
    return "TreeUI";
  }

  /**
   * Gets the AccessibleContext associated with this <code>JToggleButton</code>.
   *
   * @return the associated context
   */
  public AccessibleContext getAccessibleContext()
  {
    return null;
  }

  /**
   * Returns the preferred viewport size..
   *
   * @return the preferred size
   */
  public Dimension getPreferredScrollableViewportSize()
  {
    return null;
  }

  public int getScrollableUnitIncrement(Rectangle visibleRect,
                                        int orientation, int direction)
  {
    return 1;
  }

  public int getScrollableBlockIncrement(Rectangle visibleRect,
                                         int orientation, int direction)
  {
    return 1;
  }

  public boolean getScrollableTracksViewportWidth()
  {
    return false;
  }

  public boolean getScrollableTracksViewportHeight()
  {
    return false;
  }

  /**
   * Adds a <code>TreeExpansionListener</code> object to the tree.
   *
   * @param listener the listener to add
   */
  public void addTreeExpansionListener(TreeExpansionListener listener)
  {
    listenerList.add(TreeExpansionListener.class, listener);
  }

  /**
   * Removes a <code>TreeExpansionListener</code> object from the tree.
   *
   * @param listener the listener to remove
   */
  public void removeTreeExpansionListener(TreeExpansionListener listener)
  {
    listenerList.remove(TreeExpansionListener.class, listener);
  }

  /**
   * Returns all added <code>TreeExpansionListener</code> objects.
   *
   * @return an array of listeners
   */
  public TreeExpansionListener[] getTreeExpansionListeners()
  {
    return (TreeExpansionListener[]) getListeners(TreeExpansionListener.class);
  }

  /**
   * Notifies all listeners that the tree was collapsed.
   *
   * @param path the path to the node that was collapsed
   */
  public void fireTreeCollapsed(TreePath path)
  {
    TreeExpansionEvent event = new TreeExpansionEvent(this, path);
    TreeExpansionListener[] listeners = getTreeExpansionListeners();

    for (int index = 0; index < listeners.length; ++index)
      listeners[index].treeCollapsed(event);
  }
  
  /**
   * Notifies all listeners that the tree was expanded.
   *
   * @param path the path to the node that was expanded
   */
  public void fireTreeExpanded(TreePath path)
  {
    TreeExpansionEvent event = new TreeExpansionEvent(this, path);
    TreeExpansionListener[] listeners = getTreeExpansionListeners();

    for (int index = 0; index < listeners.length; ++index)
      listeners[index].treeExpanded(event);
  }

  /**
   * Adds a <code>TreeSelctionListener</code> object to the tree.
   *
   * @param listener the listener to add
   */
  public void addTreeSelectionListener(TreeSelectionListener listener)
  {
    listenerList.add(TreeSelectionListener.class, listener);
  }

  /**
   * Removes a <code>TreeSelectionListener</code> object from the tree.
   *
   * @param listener the listener to remove
   */
  public void removeTreeSelectionListener(TreeSelectionListener listener)
  {
    listenerList.remove(TreeSelectionListener.class, listener);
  }

  /**
   * Returns all added <code>TreeSelectionListener</code> objects.
   *
   * @return an array of listeners
   */
  public TreeSelectionListener[] getTreeSelectionListeners()
  {
    return (TreeSelectionListener[]) getListeners(TreeSelectionListener.class);
  }

  /**
   * Notifies all listeners when the selection of the tree changed.
   *
   * @param event the event to send
   */
  protected void fireValueChanged(TreeSelectionEvent event)
  {
    TreeSelectionListener[] listeners = getTreeSelectionListeners();

    for (int index = 0; index < listeners.length; ++index)
      listeners[index].valueChanged(event);
  }

  /**
   * Adds a <code>TreeWillExpandListener</code> object to the tree.
   *
   * @param listener the listener to add
   */
  public void addTreeWillExpandListener(TreeWillExpandListener listener)
  {
    listenerList.add(TreeWillExpandListener.class, listener);
  }

  /**
   * Removes a <code>TreeWillExpandListener</code> object from the tree.
   *
   * @param listener the listener to remove
   */
  public void removeTreeWillExpandListener(TreeWillExpandListener listener)
  {
    listenerList.remove(TreeWillExpandListener.class, listener);
  }

  /**
   * Returns all added <code>TreeWillExpandListener</code> objects.
   *
   * @return an array of listeners
   */
  public TreeWillExpandListener[] getTreeWillExpandListeners()
  {
    return (TreeWillExpandListener[]) getListeners(TreeWillExpandListener.class);
  }

  /**
   * Notifies all listeners that the tree will collapse.
   *
   * @param path the path to the node that will collapse
   */
  public void fireTreeWillCollapse(TreePath path)
    throws ExpandVetoException
  {
    TreeExpansionEvent event = new TreeExpansionEvent(this, path);
    TreeWillExpandListener[] listeners = getTreeWillExpandListeners();

    for (int index = 0; index < listeners.length; ++index)
      listeners[index].treeWillCollapse(event);
  }

  /**
   * Notifies all listeners that the tree will expand.
   *
   * @param path the path to the node that will expand
   */
  public void fireTreeWillExpand(TreePath path)
    throws ExpandVetoException
  {
    TreeExpansionEvent event = new TreeExpansionEvent(this, path);
    TreeWillExpandListener[] listeners = getTreeWillExpandListeners();

    for (int index = 0; index < listeners.length; ++index)
      listeners[index].treeWillExpand(event);
  }

  /**
   * Returns the model of this <code>JTree</code> object.
   *
   * @return the associated <code>TreeModel</code>
   */
  public TreeModel getModel()
  {
    return treeModel;
  }

  /**
   * Sets the model to use in <code>JTree</object>.
   *
   * @param model the <code>TreeModel</code> to use
   */
  public void setModel(TreeModel model)
  {
    treeModel = model;
  }

  /**
   * Checks if this <code>JTree</code> object is editable.
   *
   * @return <code>true</code> if this tree object is editable,
   * <code>false</code> otherwise
   */
  public boolean isEditable()
  {
    return editable;
  }

  /**
   * Sets the <code>editable</code> property.
   *
   * @param flag <code>true</code> to make this tree object editable,
   * <code>false</code> otherwise
   */
  public void setEditable(boolean flag)
  {
    if (editable == flag)
      return;

    boolean oldValue = editable;
    editable = flag;
    firePropertyChange("editable", oldValue, editable);
  }

  /**
   * Checks if the root element is visible.
   *
   * @return <code>true</code> if the root element is visible,
   * <code>false</code> otherwise
   */
  public boolean isRootVisible()
  {
    return rootVisible;
  }

  public void setRootVisible(boolean flag)
  {
    rootVisible = flag;
  }

  public boolean getShowsRootHandles()
  {
    return showsRootHandles;
  }

  public void setShowsRootHandles(boolean flag)
  {
    showsRootHandles = flag;
  }

  public TreeCellEditor getCellEditor()
  {
    return cellEditor;
  }

  public void setCellEditor(TreeCellEditor editor)
  {
    cellEditor = editor;
  }
  
  public TreeCellRenderer getCellRenderer()
  {
    return cellRenderer;
  }
  
  public void setCellRenderer(TreeCellRenderer newRenderer)
  {
    cellRenderer = newRenderer;
  }

  public TreeSelectionModel getSelectionModel()
  {
    return selectionModel;
  }

  public void setSelectionModel(TreeSelectionModel model)
  {
    selectionModel = model;
  }

  public int getVisibleRowCount()
  {
    return visibleRowCount;
  }

  public void setVisibleRowCount(int rows)
  {
    visibleRowCount = rows;
  }

  public boolean isLargeModel()
  {
    return largeModel;
  }

  public void setLargeModel(boolean large)
  {
    largeModel = large;
  }

  public int getRowHeight()
  {
    return rowHeight;
  }

  public void setRowHeight(int height)
  {
    rowHeight = height;
  }

  public boolean getInvokesStopCellEditing()
  {
    return invokesStopCellEditing;
  }

  public void setInvokesStopCellEditing(boolean invoke)
  {
    invokesStopCellEditing = invoke;
  }

  /**
   * @since 1.3
   */
  public int getToggleClickCount()
  {
    return toggleClickCount;
  }

  /**
   * @since 1.3
   */
  public void setToggleClickCount(int count)
  {
    toggleClickCount = count;
  }

  public boolean getScrollsOnExpand()
  {
    return scrollsOnExpand;
  }

  public void setScrollsOnExpand(boolean scroll)
  {
    scrollsOnExpand = scroll;
  }
}
