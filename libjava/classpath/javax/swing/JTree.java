/* JTree.java 
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

package javax.swing;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.FocusListener;
import java.beans.PropertyChangeListener;
import java.io.Serializable;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Locale;
import java.util.Vector;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleAction;
import javax.accessibility.AccessibleComponent;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleSelection;
import javax.accessibility.AccessibleState;
import javax.accessibility.AccessibleStateSet;
import javax.accessibility.AccessibleText;
import javax.accessibility.AccessibleValue;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeExpansionListener;
import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.event.TreeWillExpandListener;
import javax.swing.plaf.TreeUI;
import javax.swing.text.Position;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.DefaultTreeSelectionModel;
import javax.swing.tree.ExpandVetoException;
import javax.swing.tree.TreeCellEditor;
import javax.swing.tree.TreeCellRenderer;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

public class JTree extends JComponent implements Scrollable, Accessible
{

  /**
   * This class implements accessibility support for the JTree class. It 
   * provides an implementation of the Java Accessibility API appropriate 
   * to tree user-interface elements.
   */
  protected class AccessibleJTree extends JComponent.AccessibleJComponent
      implements AccessibleSelection, TreeSelectionListener, TreeModelListener,
      TreeExpansionListener
  {
    
    /**
     * This class implements accessibility support for the JTree child. It provides 
     * an implementation of the Java Accessibility API appropriate to tree nodes.
     */
    protected class AccessibleJTreeNode extends AccessibleContext 
       implements Accessible, AccessibleComponent, AccessibleSelection, 
       AccessibleAction
    {
      
      private JTree tree;
      private TreePath tp;
      private Accessible acc;
      private AccessibleStateSet states;
      private Vector selectionList;
      private Vector actionList;
      private TreeModel mod;
      private Cursor cursor;
      
      /**
       * Constructs an AccessibleJTreeNode
       * 
       * @param t - the current tree
       * @param p - the current path to be dealt with
       * @param ap - the accessible object to use
       */
      public AccessibleJTreeNode(JTree t, TreePath p, Accessible ap)
      {
        states = new AccessibleStateSet();
        selectionList = new Vector();
        actionList = new Vector();
        mod = tree.getModel();
        cursor = JTree.this.getCursor();
                
        tree = t;
        tp = p;
        acc = ap;
        
        // Add all the children of this path that may already be
        // selected to the selection list.
        TreePath[] selected = tree.getSelectionPaths();
        for (int i = 0; i < selected.length; i++)
          {
            TreePath sel = selected[i];
            if ((sel.getParentPath()).equals(tp))
              selectionList.add(sel);
          }
        
        // Add all the actions available for a node to 
        // the action list.
        actionList.add("EXPAND");
        actionList.add("COLLAPSE");
        actionList.add("EDIT");
        actionList.add("SELECT");
        actionList.add("DESELECT");
      }      
      
      /**
       * Adds the specified selected item in the object to the object's
       * selection.
       * 
       * @param i - the i-th child of this node.
       */
      public void addAccessibleSelection(int i)
      {
        if (mod != null)
          {
            Object child = mod.getChild(tp.getLastPathComponent(), i);
            if (child != null)
              {
                if (!states.contains(AccessibleState.MULTISELECTABLE))
                  clearAccessibleSelection();
                selectionList.add(child);                  
                tree.addSelectionPath(tp.pathByAddingChild(child));
              }
          }
      }
      
      /**
       * Adds the specified focus listener to receive focus events 
       * from this component.
       * 
       * @param l - the new focus listener
       */
      public void addFocusListener(FocusListener l)
      {
        tree.addFocusListener(l);
      }
      
      /**
       * Add a PropertyChangeListener to the listener list.
       * 
       * @param l - the new property change listener
       */
      public void addPropertyChangeListener(PropertyChangeListener l)
      {
        // Nothing to do here.
      }
      
      /**
       * Clears the selection in the object, so that nothing in the 
       * object is selected.
       */
      public void clearAccessibleSelection()
      {
        selectionList.clear();
      }
      
      /**
       * Checks whether the specified point is within this object's 
       * bounds, where the point's x and y coordinates are defined to be 
       * relative to the coordinate system of the object. 
       * 
       * @param p - the point to check
       * @return true if p is in the bounds
       */
      public boolean contains(Point p)
      {
        return getBounds().contains(p);
      }
      
      /**
       * Perform the specified Action on the tree node.
       * 
       * @param i - the i-th action to perform
       * @return true if the the action was performed; else false.
       */
      public boolean doAccessibleAction(int i)
      {
        if (i >= actionList.size() || i < 0)
          return false;
        
        if (actionList.get(i).equals("EXPAND"))
          tree.expandPath(tp);
        else if (actionList.get(i).equals("COLLAPSE"))
          tree.collapsePath(tp);
        else if (actionList.get(i).equals("SELECT"))
          tree.addSelectionPath(tp);
        else if (actionList.get(i).equals("DESELECT"))
          tree.removeSelectionPath(tp);
        else if (actionList.get(i).equals("EDIT"))
          tree.startEditingAtPath(tp);
        else
          return false;
        return true;
      }
      
      /**
       * Get the AccessibleAction associated with this object.
       * 
       * @return the action
       */
      public AccessibleAction getAccessibleAction()
      {
        return this;
      }
      
      /**
       * Returns the number of accessible actions available in this tree node.
       * 
       * @return the number of actions
       */
      public int getAccessibleActionCount()
      {
        return actionList.size();
      }
      
      /**
       * Return a description of the specified action of the tree node.
       * 
       * @param i - the i-th action's description
       * @return a description of the action
       */
      public String getAccessibleActionDescription(int i)
      {
        if (i < 0 || i >= actionList.size())
          return (actionList.get(i)).toString();
        return super.getAccessibleDescription();
      }
      
      /**
       * Returns the Accessible child, if one exists, contained at the 
       * local coordinate Point.
       * 
       * @param p - the point of the accessible
       * @return the accessible at point p if it exists
       */
      public Accessible getAccessibleAt(Point p)
      {
        TreePath acc = tree.getClosestPathForLocation(p.x, p.y);
        if (acc != null)
          return new AccessibleJTreeNode(tree, acc, this);
        return null;
      }
      
      /**
       * Return the specified Accessible child of the object.
       * 
       * @param i - the i-th child of the current path
       * @return the child if it exists
       */
      public Accessible getAccessibleChild(int i)
      {
        if (mod != null)
          {
            Object child = mod.getChild(tp.getLastPathComponent(), i);
            if (child != null)
              return new AccessibleJTreeNode(tree, tp.pathByAddingChild(child),
                                             acc);
          }
        return null;
      }
      
      /**
       * Returns the number of accessible children in the object.
       * 
       * @return the number of children the current node has
       */
      public int getAccessibleChildrenCount()
      {
        TreeModel mod = getModel();
        if (mod != null)
          return mod.getChildCount(tp.getLastPathComponent());
        return 0;
      }
      
      /**
       * Get the AccessibleComponent associated with this object.
       * 
       * @return the accessible component if it is supported.
       */
      public AccessibleComponent getAccessibleComponent()
      {
        return this;
      }
      
      /**
       * Get the AccessibleContext associated with this tree node.
       * 
       * @return an instance of this class
       */
      public AccessibleContext getAccessibleContext()
      {
        return this;
      }
      
      /**
       * Get the accessible description of this object.
       * 
       * @return the accessible description
       */
      public String getAccessibleDescription()
      {
        return super.getAccessibleDescription();
      }
      
      /**
       * Get the index of this object in its accessible parent.
       * 
       * @return the index of this in the parent.
       */
      public int getAccessibleIndexInParent()
      {
        AccessibleContext parent = getAccessibleParent().getAccessibleContext();
        if (parent != null)
          for (int i = 0; i < parent.getAccessibleChildrenCount(); i++)
            {
              if ((parent.getAccessibleChild(i)).equals(this))
                return i;
            }
        return -1;
      }
      
      /**
       * Get the accessible name of this object.
       * 
       * @return the accessible name
       */
      public String getAccessibleName()
      {
        return super.getAccessibleName();
      }
      
      /**
       * Get the Accessible parent of this object.
       * 
       * @return the accessible parent if it exists.
       */
      public Accessible getAccessibleParent()
      {
        return super.getAccessibleParent();
      }
      
      /**
       * Get the role of this object.
       * 
       * @return the accessible role
       */
      public AccessibleRole getAccessibleRole()
      {
        return AccessibleJTree.this.getAccessibleRole();
      }
      
      /**
       * Get the AccessibleSelection associated with this object if one exists.
       * 
       * @return the accessible selection for this.
       */
      public AccessibleSelection getAccessibleSelection()
      {
        return this;
      }
      
      /**
       * Returns an Accessible representing the specified selected item 
       * in the object.
       * 
       * @return the accessible representing a certain selected item.
       */
      public Accessible getAccessibleSelection(int i)
      {
        if (i > 0 && i < getAccessibleSelectionCount())
            return new AccessibleJTreeNode(tree, 
                  tp.pathByAddingChild(selectionList.get(i)), acc);
        return null;
      }
      
      /**
       * Returns the number of items currently selected.
       * 
       * @return the number of items selected.
       */
      public int getAccessibleSelectionCount()
      {
        return selectionList.size();
      }
      
      /**
       * Get the state set of this object.
       * 
       * @return the state set for this object
       */
      public AccessibleStateSet getAccessibleStateSet()
      {
        if (isVisible())
          states.add(AccessibleState.VISIBLE);
        if (tree.isCollapsed(tp))
          states.add(AccessibleState.COLLAPSED);
        if (tree.isEditable())
          states.add(AccessibleState.EDITABLE);
        if (mod != null && 
            !mod.isLeaf(tp.getLastPathComponent()))
          states.add(AccessibleState.EXPANDABLE);
        if (tree.isExpanded(tp))
          states.add(AccessibleState.EXPANDED);
        if (isFocusable())
          states.add(AccessibleState.FOCUSABLE);
        if (hasFocus())
          states.add(AccessibleState.FOCUSED);
        if (tree.getSelectionModel().getSelectionMode() != 
          TreeSelectionModel.SINGLE_TREE_SELECTION)
          states.add(AccessibleState.MULTISELECTABLE);
        if (tree.isOpaque())
          states.add(AccessibleState.OPAQUE);
        if (tree.isPathSelected(tp))
          states.add(AccessibleState.SELECTED);
        if (isShowing())
          states.add(AccessibleState.SHOWING);

        states.add(AccessibleState.SELECTABLE);
        return states;
      }
      
      /**
       * Get the AccessibleText associated with this object if one exists.
       * 
       * @return the accessible text
       */
      public AccessibleText getAccessibleText()
      {
        return super.getAccessibleText();
      }
      
      /**
       * Get the AccessibleValue associated with this object if one exists.
       * 
       * @return the accessible value if it exists
       */
      public AccessibleValue getAccessibleValue()
      {
        return super.getAccessibleValue();
      }
      
      /**
       * Get the background color of this object.
       * 
       * @return the color of the background.
       */
      public Color getBackground()
      {
        return tree.getBackground();
      }
      
      /**
       * Gets the bounds of this object in the form of a Rectangle object.
       * 
       * @return the bounds of the current node.
       */
      public Rectangle getBounds()
      {
        return tree.getPathBounds(tp);
      }
      
      /**
       * Gets the Cursor of this object.
       * 
       * @return the cursor for the current node
       */
      public Cursor getCursor()
      {
        return cursor;
      }
      
      /**
       * Gets the Font of this object.
       * 
       * @return the font for the current node
       */
      public Font getFont()
      {
        return tree.getFont();
      }
      
      /**
       * Gets the FontMetrics of this object.
       * 
       * @param f - the current font.
       * @return the font metrics for the given font.
       */
      public FontMetrics getFontMetrics(Font f)
      {
        return tree.getFontMetrics(f);
      }
      
      /**
       * Get the foreground color of this object.
       * 
       * @return the foreground for this object.
       */
      public Color getForeground()
      {
        return tree.getForeground();
      }
      
      /**
       * Gets the locale of the component.
       * 
       * @return the locale of the component.
       */
      public Locale getLocale()
      {
        return tree.getLocale();
      }
      
      /**
       * Gets the location of the object relative to the 
       * parent in the form of a point specifying the object's 
       * top-left corner in the screen's coordinate space. 
       * 
       * @return the location of the current node.
       */
      public Point getLocation()
      {
        return getLocationInJTree();
      }
      
      /**
       * Returns the location in the tree.
       * 
       * @return the location in the JTree.
       */
      protected Point getLocationInJTree()
      {
        Rectangle bounds = tree.getPathBounds(tp);
        return new Point(bounds.x, bounds.y);
      }
      
      /**
       * Returns the location of the object on the screen.
       * 
       * @return the location of the object on the screen.
       */
      public Point getLocationOnScreen()
      {
        Point loc = getLocation();
        SwingUtilities.convertPointToScreen(loc, tree);
        return loc;
      }
      
      /**
       * Returns the size of this object in the form of a Dimension object.
       * 
       * @return the size of the object
       */
      public Dimension getSize()
      {
        Rectangle b = getBounds();
        return b.getSize();
      }
      
      /**
       * Returns true if the current child of this object is selected.
       * 
       * @param i - the child of the current node
       * @return true if the child is selected.
       */
      public boolean isAccessibleChildSelected(int i)
      {
        Object child = mod.getChild(tp.getLastPathComponent(), i);
        if (child != null)
          return tree.isPathSelected(tp.pathByAddingChild(child));
        return false;
      }
      
      /**
       * Determines if the object is enabled.
       * 
       * @return true if the tree is enabled
       */
      public boolean isEnabled()
      {
        return tree.isEnabled();
      }
      
      /**
       * Returns whether this object can accept focus or not.
       * 
       * @return true, it is always focus traversable
       */
      public boolean isFocusTraversable()
      {
        return true;
      }
      
      /**
       * Determines if the object is showing.
       * 
       * @return true if the object is visible and the
       * parent is visible.
       */
      public boolean isShowing()
      {
        return isVisible() && tree.isShowing();
      }
      
      /**
       * Determines if the object is visible.
       * 
       * @return true if the object is visible.
       */
      public boolean isVisible()
      {
        return tree.isVisible(tp);
      }
      
      /**
       * Removes the specified selected item in the object from the
       * object's selection.
       * 
       * @param i - the specified item to remove
       */
      public void removeAccessibleSelection(int i)
      {
        if (mod != null)
          {
            Object child = mod.getChild(tp.getLastPathComponent(), i);
            if (child != null)
              {
                if (!states.contains(AccessibleState.MULTISELECTABLE))
                  clearAccessibleSelection();
                if (selectionList.contains(child))
                  {
                    selectionList.remove(child);                  
                    tree.removeSelectionPath(tp.pathByAddingChild(child));
                  }
              }
          }
      }
      
      /**
       * Removes the specified focus listener so it no longer receives focus 
       * events from this component.
       * 
       * @param l - the focus listener to remove
       */
      public void removeFocusListener(FocusListener l)
      {
        tree.removeFocusListener(l);
      }
      
      /**
       * Remove a PropertyChangeListener from the listener list.
       * 
       * @param l - the property change listener to remove.
       */
      public void removePropertyChangeListener(PropertyChangeListener l)
      {
        // Nothing to do here.
      }
      
      /**
       * Requests focus for this object.
       */
      public void requestFocus()
      {
        tree.requestFocus();
      }
      
      /**
       * Causes every selected item in the object to be selected if the object 
       * supports multiple selections.
       */
      public void selectAllAccessibleSelection()
      {
        Object parent = tp.getLastPathComponent();
        if (mod != null)
          {
            for (int i = 0; i < mod.getChildCount(parent); i++)
              {
                Object child = mod.getChild(parent, i);
                if (child != null)
                  {
                    if (!states.contains(AccessibleState.MULTISELECTABLE))
                      clearAccessibleSelection();
                    if (selectionList.contains(child))
                      {
                        selectionList.add(child);
                        tree.addSelectionPath(tp.pathByAddingChild(child));
                      }
                  }
              }
          }
      }
      
      /**
       * Set the accessible description of this object.
       * 
       * @param s - the string to set the accessible description to.
       */
      public void setAccessibleDescription(String s)
      {
        super.setAccessibleDescription(s);
      }
      
      /**
       * Set the localized accessible name of this object.
       * 
       * @param s - the string to set the accessible name to.
       */
      public void setAccessibleName(String s)
      {
        super.setAccessibleName(s);
      }
      
      /**
       * Set the background color of this object.
       * 
       * @param c - the color to set the background to.
       */
      public void setBackground(Color c)
      {
        // Nothing to do here.
      }
      
      /**
       * Sets the bounds of this object in the form of a Rectangle object.
       * 
       * @param r - the bounds to set the object o
       */
      public void setBounds(Rectangle r)
      {
        // Nothing to do here.
      }
      
      /**
       * Sets the Cursor of this object.
       * 
       * @param c - the new cursor
       */
      public void setCursor(Cursor c)
      {
        cursor = c;
      }
      
      /**
       * Sets the enabled state of the object.
       * 
       * @param b - boolean to enable or disable object
       */
      public void setEnabled(boolean b)
      {
         // Nothing to do here.
      }
      
      /**
       * Sets the Font of this object.
       * 
       * @param f - the new font.
       */
      public void setFont(Font f)
      {
         // Nothing to do here.
      }
      
      /**
       * Sets the foreground color of this object.
       * 
       * @param c - the new foreground color.
       */
      public void setForeground(Color c)
      {
        // Nothing to do here.
      }
      
      /**
       * Sets the location of the object relative to the parent.
       * 
       * @param p - the new location for the object.
       */
      public void setLocation(Point p)
      {
        // Nothing to do here.
      }
      
      /**
       * Resizes this object so that it has width and height.
       * 
       * @param d - the new size for the object.
       */
      public void setSize(Dimension d)
      {
        // Nothing to do here.
      }
      
      /**
       * Sets the visible state of the object.
       * 
       * @param b - sets the objects visibility.
       */
      public void setVisible(boolean b)
      {
        // Nothing to do here.
      }
    }
    
    /**
     * Constructor
     */
    public AccessibleJTree()
    {
      // Nothing to do here.
    }
    
    /**
     * Adds the specified selected item in the object to the object's selection.
     * 
     * @param i - the row to add to the tree's selection
     */
    public void addAccessibleSelection(int i)
    {
      addSelectionInterval(i, i);
    }
    
    /**
     * Clears the selection in the object, so that nothing in the object is selected.
     */
    public void clearAccessibleSelection()
    {
      clearSelection();
    }
    
    /**
     * Fire a visible data property change notification.
     */
    public void fireVisibleDataPropertyChange()
    {
      treeDidChange();
    }
    
    /**
     * Returns the Accessible child, if one exists, contained at the local 
     * coordinate Point.
     * 
     * @param p - the point of the accessible to get.
     * @return the accessible at point p.
     */
    public Accessible getAccessibleAt(Point p)
    {
      TreePath tp = getClosestPathForLocation(p.x, p.y);
      if (tp != null)
        return new AccessibleJTreeNode(JTree.this, tp, null);
      return null;
    }
    
    /**
     * Return the nth Accessible child of the object.
     * 
     * @param i - the accessible child to get
     * @return the i-th child
     */
    public Accessible getAccessibleChild(int i)
    {
      return null;
    }
    
    /**
     * Returns the number of top-level children nodes of this JTree.
     * 
     * @return the number of top-level children
     */
    public int getAccessibleChildrenCount()
    {
      TreeModel model = getModel();
      if (model != null)
        return model.getChildCount(model.getRoot());
      return 0;
    }
    
    /**
     * Get the index of this object in its accessible parent.
     * 
     * @return the index of this object.
     */
    public int getAccessibleIndexInParent()
    {
      return 0;
    }
    
    /**
     * Get the role of this object.
     * 
     * @return the role of this object
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.TREE;
    }
    
    /**
     * Get the AccessibleSelection associated with this object.
     * 
     * @return the accessible selection of the tree
     */
    public AccessibleSelection getAccessibleSelection()
    {
      TreeModel mod = getModel();
      if (mod != null)
        return (new AccessibleJTreeNode(JTree.this, 
                  new TreePath(mod.getRoot()), null)).getAccessibleSelection();
      return null;
    }
    
    /**
     * Returns an Accessible representing the specified selected item in the object.
     * 
     * @return the i-th accessible in the selection
     */
    public Accessible getAccessibleSelection(int i)
    {
      TreeModel mod = getModel();
      if (mod != null)
        return (new AccessibleJTreeNode(JTree.this, 
                  new TreePath(mod.getRoot()), null)).getAccessibleSelection(i);
      return null;
    }
    
    /**
     * Returns the number of items currently selected.
     * 
     * @return the number of selected accessibles.
     */
    public int getAccessibleSelectionCount()
    {
      return getSelectionCount();
    }
    
    /**
     * Returns true if the current child of this object is selected.
     * 
     * @param i - the child of this object
     * @return true if the i-th child is selected.
     */
    public boolean isAccessibleChildSelected(int i)
    {
      // Nothing to do here.
      return false;
    }
    
    /**
     * Removes the specified selected item in the object from the object's
     * selection.
     * 
     * @param i - the i-th selected item to remove
     */
    public void removeAccessibleSelection(int i)
    {
      removeSelectionInterval(i, i);
    }
    
    /**
     * Causes every selected item in the object to be selected if the object
     * supports multiple selections.
     */
    public void selectAllAccessibleSelection()
    {
      if (getSelectionModel().getSelectionMode() != 
        TreeSelectionModel.SINGLE_TREE_SELECTION)
      addSelectionInterval(0, getVisibleRowCount());
    }
    
    /**
     * Tree Collapsed notification
     * 
     * @param e - the event
     */
    public void treeCollapsed(TreeExpansionEvent e)
    {
      fireTreeCollapsed(e.getPath());
    }
   
    /**
     * Tree Model Expansion notification.
     * 
     * @param e - the event
     */
    public void treeExpanded(TreeExpansionEvent e)
    {
      fireTreeExpanded(e.getPath());
    }
    
    /**
     * Tree Model Node change notification.
     * 
     * @param e - the event
     */
    public void treeNodesChanged(TreeModelEvent e)
    {
      // Nothing to do here.
    }
    
    /**
     * Tree Model Node change notification.
     * 
     * @param e - the event
     */
    public void treeNodesInserted(TreeModelEvent e)
    {
      // Nothing to do here.
    }
    
    /**
     * Tree Model Node change notification.
     * 
     * @param e - the event
     */
    public void treeNodesRemoved(TreeModelEvent e)
    {
      // Nothing to do here.
    }
    
    /**
     * Tree Model structure change change notification.
     * 
     * @param e - the event
     */
    public void treeStructureChanged(TreeModelEvent e)
    {
      // Nothing to do here.
    }
    
    /**
     * Tree Selection Listener value change method.
     * 
     * @param e - the event
     */
    public void valueChanged(TreeSelectionEvent e)
    {
      fireValueChanged(e);
    }
  }
  
  public static class DynamicUtilTreeNode extends DefaultMutableTreeNode
  {
    protected Object childValue;

    protected boolean loadedChildren;

    /**
     * Currently not set or used by this class. It might be set and used in
     * later versions of this class.
     */
    protected boolean hasChildren;

    public DynamicUtilTreeNode(Object value, Object children)
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

    /**
     * Returns the child node at position <code>pos</code>. Subclassed
     * here to load the children if necessary.
     * 
     * @param pos the position of the child node to fetch
     * 
     * @return the childnode at the specified position
     */
    public TreeNode getChildAt(int pos)
    {
      loadChildren();
      return super.getChildAt(pos);
    }

    public boolean isLeaf()
    {
      return childValue == null || !(childValue instanceof Hashtable
          || childValue instanceof Vector 
          || childValue.getClass().isArray());
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
          Iterator i = ((Vector) children).iterator();
          while (i.hasNext())
            {
              Object n = i.next();
              parent.add(new DynamicUtilTreeNode(n, n));
            }
        }
      else if (children != null && children.getClass().isArray())
        {
          Object[] arr = (Object[]) children;
          for (int i = 0; i < arr.length; ++i)
            parent.add(new DynamicUtilTreeNode(arr[i], arr[i]));
        }
    }
  }

  /**
   * Listens to the model of the JTree and updates the property 
   * <code>expandedState</code> if nodes are removed or changed.
   */
  protected class TreeModelHandler implements TreeModelListener
  {

    /**
     * Creates a new instance of TreeModelHandler.
     */
    protected TreeModelHandler()
    {
      // Nothing to do here.
    }

    /**
     * Notifies when a node has changed in some ways. This does not include
     * that a node has changed its location or changed it's children. It
     * only means that some attributes of the node have changed that might
     * affect its presentation.
     * 
     * This method is called after the actual change occured.
     * 
     * @param ev the TreeModelEvent describing the change
     */
    public void treeNodesChanged(TreeModelEvent ev)
    {
      // Nothing to do here.
    }

    /**
     * Notifies when a node is inserted into the tree.
     * 
     * This method is called after the actual change occured.
     * 
     * @param ev the TreeModelEvent describing the change
     */
    public void treeNodesInserted(TreeModelEvent ev)
    {
      // nothing to do here
    }

    /**
     * Notifies when a node is removed from the tree.
     * 
     * This method is called after the actual change occured.
     *
     * @param ev the TreeModelEvent describing the change
	 */
    public void treeNodesRemoved(TreeModelEvent ev)
    {
      if (ev != null)
        {
          TreePath parent = ev.getTreePath();
          Object[] children = ev.getChildren();
          TreeSelectionModel sm = getSelectionModel();
          if (children != null)
            {
              TreePath path;
              Vector toRemove = new Vector();
              // Collect items that we must remove.
              for (int i = children.length - 1; i >= 0; i--)
                {
                  path = parent.pathByAddingChild(children[i]);
                  if (nodeStates.containsKey(path))
                    toRemove.add(path);
                  // Clear selection while we are at it.
                  if (sm != null)
                    removeDescendantSelectedPaths(path, true);
                }
              if (toRemove.size() > 0)
                removeDescendantToggledPaths(toRemove.elements());
              TreeModel model = getModel();
              if (model == null || model.isLeaf(parent.getLastPathComponent()))
                nodeStates.remove(parent);
            }
        }
    }

    /**
     * Notifies when the structure of the tree is changed.
     * 
     * This method is called after the actual change occured.
     * 
     * @param ev the TreeModelEvent describing the change
     */
    public void treeStructureChanged(TreeModelEvent ev)
    {
      if (ev != null)
        {
          TreePath parent = ev.getTreePath();
          if (parent != null)
            {
              if (parent.getPathCount() == 1)
                {
                  // We have a new root, clear everything.
                  clearToggledPaths();
                  Object root = treeModel.getRoot();
                  if (root != null && treeModel.isLeaf(root))
                    nodeStates.put(parent, Boolean.TRUE);
                }
              else if (nodeStates.containsKey(parent))
                {
                  Vector toRemove = new Vector();
                  boolean expanded = isExpanded(parent);
                  toRemove.add(parent);
                  removeDescendantToggledPaths(toRemove.elements());
                  if (expanded)
                    {
                      TreeModel model = getModel();
                      if (model != null
                          || model.isLeaf(parent.getLastPathComponent()))
                        collapsePath(parent);
                      else
                        nodeStates.put(parent, Boolean.TRUE);
                    }
                }
              removeDescendantSelectedPaths(parent, false);
            }
        }
    }
  }

  /**
   * This redirects TreeSelectionEvents and rewrites the source of it to be
   * this JTree. This is typically done when the tree model generates an
   * event, but the JTree object associated with that model should be listed
   * as the actual source of the event.
   */
  protected class TreeSelectionRedirector implements TreeSelectionListener,
                                                     Serializable
  {
    /** The serial version UID. */
    private static final long serialVersionUID = -3505069663646241664L;

    /**
     * Creates a new instance of TreeSelectionRedirector
     */
    protected TreeSelectionRedirector()
    {
      // Nothing to do here.
    }

    /**
     * Notifies when the tree selection changes.
     * 
     * @param ev the TreeSelectionEvent that describes the change
     */
    public void valueChanged(TreeSelectionEvent ev)
    {
      TreeSelectionEvent rewritten = 
        (TreeSelectionEvent) ev.cloneWithSource(JTree.this);
      fireValueChanged(rewritten);
    }
  }

  /**
   * A TreeModel that does not allow anything to be selected.
   */
  protected static class EmptySelectionModel extends DefaultTreeSelectionModel
  {
    /** The serial version UID. */
    private static final long serialVersionUID = -5815023306225701477L;

    /**
     * The shared instance of this model.
     */
    protected static final EmptySelectionModel sharedInstance =
      new EmptySelectionModel();

    /**
     * Creates a new instance of EmptySelectionModel.
     */
    protected EmptySelectionModel()
    {
      // Nothing to do here.
    }

    /**
     * Returns the shared instance of EmptySelectionModel.
     * 
     * @return the shared instance of EmptySelectionModel
     */
    public static EmptySelectionModel sharedInstance()
    {
      return sharedInstance;
    }

    /**
     * This catches attempts to set a selection and sets nothing instead.
     * 
     * @param paths not used here
     */
    public void setSelectionPaths(TreePath[] paths)
    {
      // We don't allow selections in this class.
    }

    /**
     * This catches attempts to add something to the selection.
     * 
     * @param paths not used here
     */
    public void addSelectionPaths(TreePath[] paths)
    {
      // We don't allow selections in this class.
    }

    /**
     * This catches attempts to remove something from the selection.
     * 
     * @param paths not used here
     */
    public void removeSelectionPaths(TreePath[] paths)
    {
      // We don't allow selections in this class.
    }
  }

  private static final long serialVersionUID = 7559816092864483649L;

  public static final String CELL_EDITOR_PROPERTY = "cellEditor";

  public static final String CELL_RENDERER_PROPERTY = "cellRenderer";

  public static final String EDITABLE_PROPERTY = "editable";

  public static final String INVOKES_STOP_CELL_EDITING_PROPERTY =
    "invokesStopCellEditing";

  public static final String LARGE_MODEL_PROPERTY = "largeModel";

  public static final String ROOT_VISIBLE_PROPERTY = "rootVisible";

  public static final String ROW_HEIGHT_PROPERTY = "rowHeight";

  public static final String SCROLLS_ON_EXPAND_PROPERTY = "scrollsOnExpand";

  public static final String SELECTION_MODEL_PROPERTY = "selectionModel";

  public static final String SHOWS_ROOT_HANDLES_PROPERTY = "showsRootHandles";

  public static final String TOGGLE_CLICK_COUNT_PROPERTY = "toggleClickCount";

  public static final String TREE_MODEL_PROPERTY = "model";

  public static final String VISIBLE_ROW_COUNT_PROPERTY = "visibleRowCount";

  /** @since 1.3 */
  public static final String ANCHOR_SELECTION_PATH_PROPERTY =
    "anchorSelectionPath";

	/** @since 1.3 */
  public static final String LEAD_SELECTION_PATH_PROPERTY = "leadSelectionPath";

  /** @since 1.3 */
  public static final String EXPANDS_SELECTED_PATHS_PROPERTY =
    "expandsSelectedPaths";

  private static final Object EXPANDED = Boolean.TRUE;

  private static final Object COLLAPSED = Boolean.FALSE;

  private boolean dragEnabled;

  private boolean expandsSelectedPaths;

  private TreePath anchorSelectionPath;

  /**
   * This contains the state of all nodes in the tree. Al/ entries map the
   * TreePath of a note to to its state. Valid states are EXPANDED and
   * COLLAPSED. Nodes not in this Hashtable are assumed state COLLAPSED.
   *
   * This is package private to avoid accessor methods.
   */
  Hashtable nodeStates = new Hashtable();

  protected transient TreeCellEditor cellEditor;

  protected transient TreeCellRenderer cellRenderer;

  protected boolean editable;

  protected boolean invokesStopCellEditing;

  protected boolean largeModel;

  protected boolean rootVisible;

  protected int rowHeight;

  protected boolean scrollsOnExpand;

  protected transient TreeSelectionModel selectionModel;

  protected boolean showsRootHandles;

  protected int toggleClickCount;

  protected transient TreeModel treeModel;

  protected int visibleRowCount;

  /**
   * Handles TreeModelEvents to update the expandedState.
   */
  protected transient TreeModelListener treeModelListener;

  /**
   * Redirects TreeSelectionEvents so that the source is this JTree.
   */
  protected TreeSelectionRedirector selectionRedirector =
    new TreeSelectionRedirector();

  /**
   * Indicates if the rowHeight property has been set by a client
   * program or by the UI.
   *
   * @see #setUIProperty(String, Object)
   * @see LookAndFeel#installProperty(JComponent, String, Object)
   */
  private boolean clientRowHeightSet = false;

  /**
   * Indicates if the scrollsOnExpand property has been set by a client
   * program or by the UI.
   *
   * @see #setUIProperty(String, Object)
   * @see LookAndFeel#installProperty(JComponent, String, Object)
   */
  private boolean clientScrollsOnExpandSet = false;

  /**
   * Indicates if the showsRootHandles property has been set by a client
   * program or by the UI.
   *
   * @see #setUIProperty(String, Object)
   * @see LookAndFeel#installProperty(JComponent, String, Object)
   */
  private boolean clientShowsRootHandlesSet = false;

  /**
   * Creates a new <code>JTree</code> object.
   */
  public JTree()
  {
    this(getDefaultTreeModel());
  }

  /**
   * Creates a new <code>JTree</code> object.
   * 
   * @param value the initial nodes in the tree
   */
  public JTree(Hashtable<?, ?> value)
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
    setRootVisible(true);
    setSelectionModel( new DefaultTreeSelectionModel() );
    
    // The root node appears expanded by default.
    nodeStates = new Hashtable();

    // The cell renderer gets set by the UI.
    cellRenderer = null;

    // Install the UI before installing the model. This way we avoid double
    // initialization of lots of UI and model stuff inside the UI and related
    // classes. The necessary UI updates are performed via property change
    // events to the UI.
    updateUI();
    setModel(model);
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
   * @param asksAllowChildren if false, all nodes without children are leaf
   *        nodes. If true, only nodes that do not allow children are leaf
   *        nodes.
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
  public JTree(Vector<?> value)
  {
    this(createTreeModel(value));
  }

  public int getRowForPath(TreePath path)
  {
    TreeUI ui = getUI();

    if (ui != null)
      return ui.getRowForPath(this, path);

    return -1;
  }

  public TreePath getPathForRow(int row)
  {
    TreeUI ui = getUI();
    return ui != null ? ui.getPathForRow(this, row) : null;
  }
  
  /**
   * Get the pathes that are displayes between the two given rows.
   * 
   * @param index0 the starting row, inclusive
   * @param index1 the ending row, inclusive
   * 
   * @return the array of the tree pathes
   */
  protected TreePath[] getPathBetweenRows(int index0, int index1)
  {
    TreeUI ui = getUI();

    if (ui == null)
      return null;

    int minIndex = Math.min(index0, index1);
    int maxIndex = Math.max(index0, index1);
    TreePath[] paths = new TreePath[maxIndex - minIndex + 1];

    for (int i = minIndex; i <= maxIndex; ++i)
      paths[i - minIndex] = ui.getPathForRow(this, i);

    return paths;
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
  }

  /**
   * This method returns the String ID of the UI class of Separator.
   * 
   * @return The UI class' String ID.
   */
  public String getUIClassID()
  {
    return "TreeUI";
  }

  /**
   * Gets the AccessibleContext associated with this
   * <code>JTree</code>.
   * 
   * @return the associated context
   */
  public AccessibleContext getAccessibleContext()
  {
    return new AccessibleJTree();
  }

  /**
   * Returns the preferred viewport size.
   * 
   * @return the preferred size
   */
  public Dimension getPreferredScrollableViewportSize()
  {
    return getPreferredSize();
  }
  
  /**
   * Return the preferred scrolling amount (in pixels) for the given scrolling
   * direction and orientation. This method handles a partially exposed row by
   * returning the distance required to completely expose the item.
   * 
   * @param visibleRect the currently visible part of the component.
   * @param orientation the scrolling orientation
   * @param direction the scrolling direction (negative - up, positive -down).
   *          The values greater than one means that more mouse wheel or similar
   *          events were generated, and hence it is better to scroll the longer
   *          distance.
   * @author Audrius Meskauskas (audriusa@bioinformatics.org)
   */
  public int getScrollableUnitIncrement(Rectangle visibleRect, int orientation,
                                        int direction)
  {
    int delta = 0;

    // Round so that the top would start from the row boundary
    if (orientation == SwingConstants.VERTICAL)
      {
        int row = getClosestRowForLocation(0, visibleRect.y);
        if (row != -1)
          {
            Rectangle b = getRowBounds(row);
            if (b.y != visibleRect.y)
              {
                if (direction < 0)
                  delta = Math.max(0, visibleRect.y - b.y);
                else
                  delta = b.y + b.height - visibleRect.y;
              }
            else
              {
                if (direction < 0)
                  {
                    if (row != 0)
                      {
                        b = getRowBounds(row - 1);
                        delta = b.height;
                      }
                  }
                else
                  delta = b.height;
              }
          }
      }
    else
      // The RI always  returns 4 for HORIZONTAL scrolling.
      delta = 4;
    return delta;
  }

  public int getScrollableBlockIncrement(Rectangle visibleRect,
                                         int orientation, int direction)
  {
    int block;
    if (orientation == SwingConstants.VERTICAL)
      block = visibleRect.height;
    else
      block = visibleRect.width;
    return block;
  }

  public boolean getScrollableTracksViewportHeight()
  {
    if (getParent() instanceof JViewport)
      return ((JViewport) getParent()).getHeight() > getPreferredSize().height;
    return false;
  }

  public boolean getScrollableTracksViewportWidth()
  {
    if (getParent() instanceof JViewport)
      return ((JViewport) getParent()).getWidth() > getPreferredSize().width;
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
    return (TreeSelectionListener[]) 
    getListeners(TreeSelectionListener.class);
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
    return (TreeWillExpandListener[]) 
    getListeners(TreeWillExpandListener.class);
  }

  /**
   * Notifies all listeners that the tree will collapse.
   * 
   * @param path the path to the node that will collapse
   */
  public void fireTreeWillCollapse(TreePath path) throws ExpandVetoException
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
  public void fireTreeWillExpand(TreePath path) throws ExpandVetoException
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
   * Sets the model to use in <code>JTree</code>.
   * 
   * @param model the <code>TreeModel</code> to use
   */
  public void setModel(TreeModel model)
  {
    if (treeModel == model)
      return;

    // Remove listeners from old model.
    if (treeModel != null && treeModelListener != null)
      treeModel.removeTreeModelListener(treeModelListener);

    // add treeModelListener to the new model
    if (treeModelListener == null)
      treeModelListener = createTreeModelListener();
    if (model != null) // as setModel(null) is allowed
      model.addTreeModelListener(treeModelListener);

    TreeModel oldValue = treeModel;
    treeModel = model;
    clearToggledPaths();

    if (treeModel != null)
      {
        if (treeModelListener == null)
          treeModelListener = createTreeModelListener();
        if (treeModelListener != null)
          treeModel.addTreeModelListener(treeModelListener);
        Object root = treeModel.getRoot();
        if (root != null && !treeModel.isLeaf(root))
          {
            nodeStates.put(new TreePath(root), Boolean.TRUE);
          }
      }

    firePropertyChange(TREE_MODEL_PROPERTY, oldValue, model);
  }

  /**
   * Checks if this <code>JTree</code> object is editable.
   * 
   * @return <code>true</code> if this tree object is editable,
   *         <code>false</code> otherwise
   */
  public boolean isEditable()
  {
    return editable;
  }

  /**
   * Sets the <code>editable</code> property.
   * 
   * @param flag <code>true</code> to make this tree object editable,
   *        <code>false</code> otherwise
   */
  public void setEditable(boolean flag)
  {
    if (editable == flag)
      return;

    boolean oldValue = editable;
    editable = flag;
    firePropertyChange(EDITABLE_PROPERTY, oldValue, editable);
  }

  /**
   * Checks if the root element is visible.
   * 
   * @return <code>true</code> if the root element is visible,
   *         <code>false</code> otherwise
   */
  public boolean isRootVisible()
  {
    return rootVisible;
  }

  public void setRootVisible(boolean flag)
  {
    if (rootVisible == flag)
      return;

    // If the root is currently selected, unselect it
    if (rootVisible && !flag)
      {
        TreeSelectionModel model = getSelectionModel();
        // The root is always shown in the first row
        TreePath rootPath = getPathForRow(0);
        model.removeSelectionPath(rootPath);
      }
    
    boolean oldValue = rootVisible;
    rootVisible = flag;
    firePropertyChange(ROOT_VISIBLE_PROPERTY, oldValue, flag);
    
  }

  public boolean getShowsRootHandles()
  {
    return showsRootHandles;
  }

  public void setShowsRootHandles(boolean flag)
  {
    clientShowsRootHandlesSet = true;

    if (showsRootHandles == flag)
      return;
    
    boolean oldValue = showsRootHandles;
    showsRootHandles = flag;
    firePropertyChange(SHOWS_ROOT_HANDLES_PROPERTY, oldValue, flag);
  }

  public TreeCellEditor getCellEditor()
  {
    return cellEditor;
  }

  public void setCellEditor(TreeCellEditor editor)
  {
    if (cellEditor == editor)
      return;

    TreeCellEditor oldValue = cellEditor;
    cellEditor = editor;
    firePropertyChange(CELL_EDITOR_PROPERTY, oldValue, editor);
  }

  public TreeCellRenderer getCellRenderer()
  {
    return cellRenderer;
  }

  public void setCellRenderer(TreeCellRenderer newRenderer)
  {
    if (cellRenderer == newRenderer)
      return;

    TreeCellRenderer oldValue = cellRenderer;
    cellRenderer = newRenderer;
    firePropertyChange(CELL_RENDERER_PROPERTY, oldValue, newRenderer);
  }

  public TreeSelectionModel getSelectionModel()
  {
    return selectionModel;
  }

  public void setSelectionModel(TreeSelectionModel model)
  {
    if (selectionModel == model)
      return;

    if( model == null )
      model = EmptySelectionModel.sharedInstance();

    if (selectionModel != null)
      selectionModel.removeTreeSelectionListener(selectionRedirector);

    TreeSelectionModel oldValue = selectionModel;
    selectionModel = model;

    selectionModel.addTreeSelectionListener(selectionRedirector);

    firePropertyChange(SELECTION_MODEL_PROPERTY, oldValue, model);
    revalidate();
    repaint();
  }

  public int getVisibleRowCount()
  {
    return visibleRowCount;
  }

  public void setVisibleRowCount(int rows)
  {
    if (visibleRowCount == rows)
      return;

    int oldValue = visibleRowCount;
    visibleRowCount = rows;
    firePropertyChange(VISIBLE_ROW_COUNT_PROPERTY, oldValue, rows);
  }

  public boolean isLargeModel()
  {
    return largeModel;
  }

  public void setLargeModel(boolean large)
  {
    if (largeModel == large)
      return;

    boolean oldValue = largeModel;
    largeModel = large;
    firePropertyChange(LARGE_MODEL_PROPERTY, oldValue, large);
  }

  public int getRowHeight()
  {
    return rowHeight;
  }

  public void setRowHeight(int height)
  {
    clientRowHeightSet = true;

    if (rowHeight == height)
      return;

    int oldValue = rowHeight;
    rowHeight = height;
    firePropertyChange(ROW_HEIGHT_PROPERTY, oldValue, height);
  }

  public boolean isFixedRowHeight()
  {
    return rowHeight > 0;
  }

  public boolean getInvokesStopCellEditing()
  {
    return invokesStopCellEditing;
  }

  public void setInvokesStopCellEditing(boolean invoke)
  {
    if (invokesStopCellEditing == invoke)
      return;

    boolean oldValue = invokesStopCellEditing;
    invokesStopCellEditing = invoke;
    firePropertyChange(INVOKES_STOP_CELL_EDITING_PROPERTY, 
                       oldValue, invoke);
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
    if (toggleClickCount == count)
      return;

    int oldValue = toggleClickCount;
    toggleClickCount = count;
    firePropertyChange(TOGGLE_CLICK_COUNT_PROPERTY, oldValue, count);
  }

  public void scrollPathToVisible(TreePath path)
  {
    if (path == null)
      return;
    Rectangle rect = getPathBounds(path);
    scrollRectToVisible(rect);
  }

  public void scrollRowToVisible(int row)
  {
    scrollPathToVisible(getPathForRow(row));
  }

  public boolean getScrollsOnExpand()
  {
    return scrollsOnExpand;
  }

  public void setScrollsOnExpand(boolean scroll)
  {
    clientScrollsOnExpandSet = true;
    if (scrollsOnExpand == scroll)
      return;

    boolean oldValue = scrollsOnExpand;
    scrollsOnExpand = scroll;
    firePropertyChange(SCROLLS_ON_EXPAND_PROPERTY, oldValue, scroll);
  }

  public void setSelectionPath(TreePath path)
  {
    clearSelectionPathStates();
    selectionModel.setSelectionPath(path);
  }

  public void setSelectionPaths(TreePath[] paths)
  {
    clearSelectionPathStates();
    selectionModel.setSelectionPaths(paths);
  }
  
  /**
   * This method, and all calls to it, should be removed once the
   * DefaultTreeModel fires events properly.  Maintenance of the nodeStates
   * table should really be done in the TreeModelHandler.  
   */
  private void clearSelectionPathStates()
  {
    TreePath[] oldPaths = selectionModel.getSelectionPaths();
    if (oldPaths != null)
      for (int i = 0; i < oldPaths.length; i++)
        nodeStates.remove(oldPaths[i]);
  }

  public void setSelectionRow(int row)
  {
    TreePath path = getPathForRow(row);

    if (path != null)
      setSelectionPath(path);
  }

  public void setSelectionRows(int[] rows)
  {
    // Make sure we have an UI so getPathForRow() does not return null.
    if (rows == null || getUI() == null)
      return;

    TreePath[] paths = new TreePath[rows.length];

    for (int i = rows.length - 1; i >= 0; --i)
      paths[i] = getPathForRow(rows[i]);

    setSelectionPaths(paths);
  }

  public void setSelectionInterval(int index0, int index1)
  {
    TreePath[] paths = getPathBetweenRows(index0, index1);

    if (paths != null)
      setSelectionPaths(paths);
  }

  public void addSelectionPath(TreePath path)
  {
    selectionModel.addSelectionPath(path);
  }

  public void addSelectionPaths(TreePath[] paths)
  {
    selectionModel.addSelectionPaths(paths);
  }

  public void addSelectionRow(int row)
  {
    TreePath path = getPathForRow(row);

    if (path != null)
      selectionModel.addSelectionPath(path);
  }

  public void addSelectionRows(int[] rows)
  {
    // Make sure we have an UI so getPathForRow() does not return null.
    if (rows == null || getUI() == null)
      return;

    TreePath[] paths = new TreePath[rows.length];

    for (int i = rows.length - 1; i >= 0; --i)
      paths[i] = getPathForRow(rows[i]);

    addSelectionPaths(paths);
  }
  
  /**
   * Select all rows between the two given indexes, inclusive. The method
   * will not select the inner leaves and braches of the currently collapsed
   * nodes in this interval.
   * 
   * @param index0 the starting row, inclusive
   * @param index1 the ending row, inclusive
   */
  public void addSelectionInterval(int index0, int index1)
  {
    TreePath[] paths = getPathBetweenRows(index0, index1);

    if (paths != null)
      addSelectionPaths(paths);
  }

  public void removeSelectionPath(TreePath path)
  {
    clearSelectionPathStates();
    selectionModel.removeSelectionPath(path);
  }

  public void removeSelectionPaths(TreePath[] paths)
  {
    clearSelectionPathStates();
    selectionModel.removeSelectionPaths(paths);
  }

  public void removeSelectionRow(int row)
  {
    TreePath path = getPathForRow(row);

    if (path != null)
      removeSelectionPath(path);
  }

  public void removeSelectionRows(int[] rows)
  {
    if (rows == null || getUI() == null)
      return;

    TreePath[] paths = new TreePath[rows.length];

    for (int i = rows.length - 1; i >= 0; --i)
      paths[i] = getPathForRow(rows[i]);

    removeSelectionPaths(paths);
  }

  public void removeSelectionInterval(int index0, int index1)
  {
    TreePath[] paths = getPathBetweenRows(index0, index1);

    if (paths != null)
      removeSelectionPaths(paths);
  }

  public void clearSelection()
  {
    selectionModel.clearSelection();
    setLeadSelectionPath(null);
  }

  public TreePath getLeadSelectionPath()
  {
    if (selectionModel == null)
      return null;
    else
      return selectionModel.getLeadSelectionPath();
  }

  /**
   * @since 1.3
   */
  public void setLeadSelectionPath(TreePath path)
  {
    if (selectionModel != null)
      {
        TreePath oldValue = selectionModel.getLeadSelectionPath();
        if (path == oldValue || path != null && path.equals(oldValue))
          return;
       
        // Repaint the previous and current rows with the lead selection path.
        if (path != null)
          {
            repaint(getPathBounds(path));
            selectionModel.addSelectionPath(path);
          }
        
        if (oldValue != null)
          repaint(getPathBounds(oldValue));
        
        firePropertyChange(LEAD_SELECTION_PATH_PROPERTY, oldValue, path);
      }
  }

  /**
   * @since 1.3
   */
  public TreePath getAnchorSelectionPath()
  {
    return anchorSelectionPath;
  }

  /**
   * @since 1.3
   */
  public void setAnchorSelectionPath(TreePath path)
  {
    if (anchorSelectionPath == path)
      return;

    TreePath oldValue = anchorSelectionPath;
    anchorSelectionPath = path;
    firePropertyChange(ANCHOR_SELECTION_PATH_PROPERTY, oldValue, path);
  }

  public int getLeadSelectionRow()
  {
    return selectionModel.getLeadSelectionRow();
  }

  public int getMaxSelectionRow()
  {
    return selectionModel.getMaxSelectionRow();
  }

  public int getMinSelectionRow()
  {
    return selectionModel.getMinSelectionRow();
  }

  public int getSelectionCount()
  {
    return selectionModel.getSelectionCount();
  }

  public TreePath getSelectionPath()
  {
    return selectionModel.getSelectionPath();
  }

  public TreePath[] getSelectionPaths()
  {
    return selectionModel.getSelectionPaths();
  }

  public int[] getSelectionRows()
  {
    return selectionModel.getSelectionRows();
  }

  public boolean isPathSelected(TreePath path)
  {
    return selectionModel.isPathSelected(path);
  }

  /**
   * Returns <code>true</code> when the specified row is selected,
   * <code>false</code> otherwise. This call is delegated to the
   * {@link TreeSelectionModel#isRowSelected(int)} method.
   *
   * @param row the row to check
   *
   * @return <code>true</code> when the specified row is selected,
   *         <code>false</code> otherwise
   */
  public boolean isRowSelected(int row)
  {
    return selectionModel.isRowSelected(row);
  }

  public boolean isSelectionEmpty()
  {
    return selectionModel.isSelectionEmpty();
  }

  /**
   * Return the value of the <code>dragEnabled</code> property.
   * 
   * @return the value
   * 
   * @since 1.4
   */
  public boolean getDragEnabled()
  {
    return dragEnabled;
  }

  /**
   * Set the <code>dragEnabled</code> property.
   * 
   * @param enabled new value
   * 
   * @since 1.4
   */
  public void setDragEnabled(boolean enabled)
  {
    dragEnabled = enabled;
  }

  public int getRowCount()
  {
    TreeUI ui = getUI();

    if (ui != null)
      return ui.getRowCount(this);

    return 0;
  }

  public void collapsePath(TreePath path)
  {
    try
      {
        fireTreeWillCollapse(path);
      }
    catch (ExpandVetoException ev)
      {
        // We do nothing if attempt has been vetoed.
      }
    setExpandedState(path, false);
    fireTreeCollapsed(path);
  }

  public void collapseRow(int row)
  {
    if (row < 0 || row >= getRowCount())
      return;

    TreePath path = getPathForRow(row);

    if (path != null)
      collapsePath(path);
  }

  public void expandPath(TreePath path)
  {
    // Don't expand if path is null
    // or is already expanded.
    if (path == null || isExpanded(path))
      return;

    try
      {
        fireTreeWillExpand(path);
      }
    catch (ExpandVetoException ev)
      {
        // We do nothing if attempt has been vetoed.
      }

    setExpandedState(path, true);
    fireTreeExpanded(path);
  }

  public void expandRow(int row)
  {
    if (row < 0 || row >= getRowCount())
      return;

    TreePath path = getPathForRow(row);

    if (path != null)
      expandPath(path);
  }

  public boolean isCollapsed(TreePath path)
  {
    return !isExpanded(path);
  }

  public boolean isCollapsed(int row)
  {
    if (row < 0 || row >= getRowCount())
      return false;

    TreePath path = getPathForRow(row);

    if (path != null)
      return isCollapsed(path);

    return false;
  }

  public boolean isExpanded(TreePath path)
  {
    if (path == null)
      return false;

    Object state = nodeStates.get(path);

    if ((state == null) || (state != EXPANDED))
      return false;

    TreePath parent = path.getParentPath();

    if (parent != null)
      return isExpanded(parent);

    return true;
  }

  public boolean isExpanded(int row)
  {
    if (row < 0 || row >= getRowCount())
      return false;

    TreePath path = getPathForRow(row);

    if (path != null)
      return isExpanded(path);

    return false;
  }

  /**
   * @since 1.3
   */
  public boolean getExpandsSelectedPaths()
  {
    return expandsSelectedPaths;
  }

  /**
   * @since 1.3
   */
  public void setExpandsSelectedPaths(boolean flag)
  {
    if (expandsSelectedPaths == flag)
      return;

    boolean oldValue = expandsSelectedPaths;
    expandsSelectedPaths = flag;
    firePropertyChange(EXPANDS_SELECTED_PATHS_PROPERTY, oldValue, flag);
  }

  public Rectangle getPathBounds(TreePath path)
  {
    TreeUI ui = getUI();

    if (ui == null)
      return null;

    return ui.getPathBounds(this, path);
  }

  public Rectangle getRowBounds(int row)
  {
    TreePath path = getPathForRow(row);

    if (path != null)
      return getPathBounds(path);

    return null;
  }

  public boolean isEditing()
  {
    TreeUI ui = getUI();

    if (ui != null)
      return ui.isEditing(this);

    return false;
  }

  public boolean stopEditing()
  {
    TreeUI ui = getUI();

    if (isEditing())
      if (ui != null)
        return ui.stopEditing(this);

    return false;
  }

  public void cancelEditing()
  {
    TreeUI ui = getUI();
      
    if (isEditing())
      if (ui != null)
        ui.cancelEditing(this);
  }

  public void startEditingAtPath(TreePath path)
  {
    TreeUI ui = getUI();

    if (ui != null)
      ui.startEditingAtPath(this, path);
  }

  public TreePath getEditingPath()
  {
    TreeUI ui = getUI();

    if (ui != null)
      return ui.getEditingPath(this);

    return null;
  }

  public TreePath getPathForLocation(int x, int y)
  {
    TreePath path = getClosestPathForLocation(x, y);

    if (path != null)
      {
        Rectangle rect = getPathBounds(path);

        if ((rect != null) && rect.contains(x, y))
          return path;
      }

    return null;
  }

  public int getRowForLocation(int x, int y)
  {
    TreePath path = getPathForLocation(x, y);

    if (path != null)
      return getRowForPath(path);

    return -1;
  }

  public TreePath getClosestPathForLocation(int x, int y)
  {
    TreeUI ui = getUI();

    if (ui != null)
      return ui.getClosestPathForLocation(this, x, y);

    return null;
  }

  public int getClosestRowForLocation(int x, int y)
  {
    TreePath path = getClosestPathForLocation(x, y);

    if (path != null)
      return getRowForPath(path);

    return -1;
  }

  public Object getLastSelectedPathComponent()
  {
    TreePath path = getSelectionPath();

    if (path != null)
      return path.getLastPathComponent();

    return null;
  }

  private void doExpandParents(TreePath path, boolean state)
  {
    TreePath parent = path.getParentPath();		

    if (!isExpanded(parent) && parent != null)
      doExpandParents(parent, false);
    
    nodeStates.put(path, state ? EXPANDED : COLLAPSED);
  }

  protected void setExpandedState(TreePath path, boolean state)
  {
    if (path == null)
      return;

    doExpandParents(path, state);
  }

  protected void clearToggledPaths()
  {
    nodeStates.clear();
  }

  protected Enumeration<TreePath> getDescendantToggledPaths(TreePath parent)
  {
    if (parent == null)
      return null;

    Enumeration nodes = nodeStates.keys();
    Vector result = new Vector();

    while (nodes.hasMoreElements())
      {
        TreePath path = (TreePath) nodes.nextElement();

        if (path.isDescendant(parent))
          result.addElement(path);
      }

    return result.elements();
  }

  public boolean hasBeenExpanded(TreePath path)
  {
    if (path == null)
      return false;

    return nodeStates.get(path) != null;
  }

  public boolean isVisible(TreePath path)
  {
    if (path == null)
      return false;

    TreePath parent = path.getParentPath();

    if (parent == null)
      return true; // Is root node.

    return isExpanded(parent);
  }

  public void makeVisible(TreePath path)
  {
    if (path == null)
      return;
    
    expandPath(path.getParentPath());
  }

  public boolean isPathEditable(TreePath path)
  {
    return isEditable();
  }

  /**
   * Creates and returns an instance of {@link TreeModelHandler}.
   * 
   * @return an instance of {@link TreeModelHandler}
   */
  protected TreeModelListener createTreeModelListener()
  {
    return new TreeModelHandler();
  }

  /**
   * Returns a sample TreeModel that can be used in a JTree. This can be used
   * in Bean- or GUI-Builders to show something interesting.
   * 
   * @return a sample TreeModel that can be used in a JTree
   */
  protected static TreeModel getDefaultTreeModel()
  {
    DefaultMutableTreeNode root = new DefaultMutableTreeNode("Root node");
    DefaultMutableTreeNode child1 = new DefaultMutableTreeNode("Child node 1");
    DefaultMutableTreeNode child11 =
      new DefaultMutableTreeNode("Child node 1.1");
    DefaultMutableTreeNode child12 =
      new DefaultMutableTreeNode("Child node 1.2");
    DefaultMutableTreeNode child13 =
      new DefaultMutableTreeNode("Child node 1.3");
    DefaultMutableTreeNode child2 = new DefaultMutableTreeNode("Child node 2");
    DefaultMutableTreeNode child21 =
      new DefaultMutableTreeNode("Child node 2.1");
    DefaultMutableTreeNode child22 =
      new DefaultMutableTreeNode("Child node 2.2");
    DefaultMutableTreeNode child23 =
      new DefaultMutableTreeNode("Child node 2.3");
    DefaultMutableTreeNode child24 =
      new DefaultMutableTreeNode("Child node 2.4");

    DefaultMutableTreeNode child3 = new DefaultMutableTreeNode("Child node 3");
    root.add(child1);
    root.add(child2);
    root.add(child3);
    child1.add(child11);
    child1.add(child12);
    child1.add(child13);
    child2.add(child21);
    child2.add(child22);
    child2.add(child23);
    child2.add(child24);
    return new DefaultTreeModel(root);
  }

  /**
   * Converts the specified value to a String. This is used by the renderers
   * of this JTree and its nodes.
   * 
   * This implementation simply returns <code>value.toString()</code> and
   * ignores all other parameters. Subclass this method to control the
   * conversion.
   * 
   * @param value the value that is converted to a String
   * @param selected indicates if that value is selected or not
   * @param expanded indicates if that value is expanded or not
   * @param leaf indicates if that value is a leaf node or not
   * @param row the row of the node
   * @param hasFocus indicates if that node has focus or not
   */
  public String convertValueToText(Object value, boolean selected,
                                   boolean expanded, boolean leaf, int row, boolean hasFocus)
  {
    return value.toString();
  }

  /**
   * A String representation of this JTree. This is intended to be used for
   * debugging. The returned string may be empty but may not be
   * <code>null</code>.
   * 
   * @return a String representation of this JTree
   */
  protected String paramString()
  {
    // TODO: this is completely legal, but it would possibly be nice
    // to return some more content, like the tree structure, some properties
    // etc ...
    return "";
  }

  /**
   * Returns all TreePath objects which are a descendants of the given path
   * and are exapanded at the moment of the execution of this method. If the
   * state of any node is beeing toggled while this method is executing this
   * change may be left unaccounted.
   * 
   * @param path The parent of this request
   *
   * @return An Enumeration containing TreePath objects
   */
  public Enumeration<TreePath> getExpandedDescendants(TreePath path)
  {
    Enumeration paths = nodeStates.keys();
    Vector relevantPaths = new Vector();
    while (paths.hasMoreElements())
      {
        TreePath nextPath = (TreePath) paths.nextElement();
        if (nodeStates.get(nextPath) == EXPANDED
            && path.isDescendant(nextPath))
          {
            relevantPaths.add(nextPath);
          }
      }
    return relevantPaths.elements();
  }

  /**
   * Returns the next table element (beginning from the row
   * <code>startingRow</code> that starts with <code>prefix</code>.
   * Searching is done in the direction specified by <code>bias</code>.
   * 
   * @param prefix the prefix to search for in the cell values
   * @param startingRow the index of the row where to start searching from
   * @param bias the search direction, either {@link Position.Bias#Forward} or
   *        {@link Position.Bias#Backward}
   * 
   * @return the path to the found element or -1 if no such element has been
   *         found
   * 
   * @throws IllegalArgumentException if prefix is <code>null</code> or
   *         startingRow is not valid
   * 
   * @since 1.4
   */
  public TreePath getNextMatch(String prefix, int startingRow,
                               Position.Bias bias)
  {
    if (prefix == null)
      throw new IllegalArgumentException("The argument 'prefix' must not be"
                                         + " null.");
    if (startingRow < 0)
      throw new IllegalArgumentException("The argument 'startingRow' must not"
                                         + " be less than zero.");

    int size = getRowCount();
    if (startingRow > size)
      throw new IllegalArgumentException("The argument 'startingRow' must not"
                                         + " be greater than the number of"
                                         + " elements in the TreeModel.");

    TreePath foundPath = null;
    if (bias == Position.Bias.Forward)
      {
        for (int i = startingRow; i < size; i++)
          {
            TreePath path = getPathForRow(i);
            Object o = path.getLastPathComponent();
            // FIXME: in the following call to convertValueToText the
            // last argument (hasFocus) should be done right.
            String item = convertValueToText(o, isRowSelected(i),
                                             isExpanded(i), treeModel.isLeaf(o),
                                             i, false);
            if (item.startsWith(prefix))
              {
                foundPath = path;
                break;
              }
          }
      }
    else
      {
        for (int i = startingRow; i >= 0; i--)
          {
            TreePath path = getPathForRow(i);
            Object o = path.getLastPathComponent();
            // FIXME: in the following call to convertValueToText the
            // last argument (hasFocus) should be done right.
            String item = convertValueToText(o, isRowSelected(i),
                                             isExpanded(i), treeModel.isLeaf(o), i, false);
            if (item.startsWith(prefix))
              {
                foundPath = path;
                break;
              }
          }
      }
    return foundPath;
  }

  /**
   * Removes any paths in the current set of selected paths that are
   * descendants of <code>path</code>. If <code>includePath</code> is set
   * to <code>true</code> and <code>path</code> itself is selected, then
   * it will be removed too.
   * 
   * @param path the path from which selected descendants are to be removed
   * @param includeSelected if <code>true</code> then <code>path</code> itself
   *        will also be remove if it's selected
   * 
   * @return <code>true</code> if something has been removed,
   *         <code>false</code> otherwise
   * 
   * @since 1.3
   */
  protected boolean removeDescendantSelectedPaths(TreePath path,
                                                  boolean includeSelected)
  {
    boolean removedSomething = false;
    TreePath[] selected = getSelectionPaths();
    for (int index = 0; index < selected.length; index++)
      {
        if ((selected[index] == path && includeSelected)
            || (selected[index].isDescendant(path)))
          {
            removeSelectionPath(selected[index]);
            removedSomething = true;
          }
      }
    return removedSomething;
  }
  
  /**
   * Removes any descendants of the TreePaths in toRemove that have been 
   * expanded.
   * 
   * @param toRemove - Enumeration of TreePaths that need to be removed from
   * cache of toggled tree paths.
   */
  protected void removeDescendantToggledPaths(Enumeration<TreePath> toRemove)
  {
    while (toRemove.hasMoreElements())
      {
        TreePath current = (TreePath) toRemove.nextElement();
        Enumeration descendants = getDescendantToggledPaths(current);
        
        while (descendants.hasMoreElements())
          {
            TreePath currentDes = (TreePath) descendants.nextElement();
            if (isExpanded(currentDes))
                nodeStates.remove(currentDes);
          }
      }
  }

  /**
   * <p>
   * Sent when the tree has changed enough that we need to resize the bounds,
   * but not enough that we need to remove the expanded node set (e.g nodes were
   * expanded or collapsed, or nodes were inserted into the tree). You should
   * never have to invoke this, the UI will invoke this as it needs to.
   * </p>
   * <p>
   * If the tree uses {@link DefaultTreeModel}, you must call
   * {@link DefaultTreeModel#reload(TreeNode)} or
   * {@link DefaultTreeModel#reload()} after adding or removing nodes. Following
   * the official Java 1.5 API standard, just calling treeDidChange, repaint()
   * or revalidate() does <i>not</i> update the tree appearance properly.
   * 
   * @see DefaultTreeModel#reload()
   */
  public void treeDidChange()
  {
    repaint();
  }

  /**
   * Helper method for
   * {@link LookAndFeel#installProperty(JComponent, String, Object)}.
   * 
   * @param propertyName the name of the property
   * @param value the value of the property
   *
   * @throws IllegalArgumentException if the specified property cannot be set
   *         by this method
   * @throws ClassCastException if the property value does not match the
   *         property type
   * @throws NullPointerException if <code>c</code> or
   *         <code>propertyValue</code> is <code>null</code>
   */
  void setUIProperty(String propertyName, Object value)
  {
    if (propertyName.equals("rowHeight"))
      {
        if (! clientRowHeightSet)
          {
            setRowHeight(((Integer) value).intValue());
            clientRowHeightSet = false;
          }
      }
    else if (propertyName.equals("scrollsOnExpand"))
      {
        if (! clientScrollsOnExpandSet)
          {
            setScrollsOnExpand(((Boolean) value).booleanValue());
            clientScrollsOnExpandSet = false;
          }
      }
    else if (propertyName.equals("showsRootHandles"))
      {
        if (! clientShowsRootHandlesSet)
          {
            setShowsRootHandles(((Boolean) value).booleanValue());
            clientShowsRootHandlesSet = false;
          }
      }
    else
      {
        super.setUIProperty(propertyName, value);
      }
  }
}
