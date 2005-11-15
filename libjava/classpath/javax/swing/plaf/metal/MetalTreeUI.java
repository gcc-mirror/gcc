/* MetalTreeUI.java
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package javax.swing.plaf.metal;

import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.event.ComponentListener;
import java.awt.event.FocusListener;
import java.awt.event.KeyListener;
import java.awt.event.MouseListener;
import java.beans.PropertyChangeListener;
import java.util.Hashtable;

import javax.swing.JComponent;
import javax.swing.JTree;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.tree.TreeCellEditor;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import javax.swing.event.CellEditorListener;
import javax.swing.event.TreeExpansionListener;
import javax.swing.event.TreeModelListener;
import javax.swing.event.TreeSelectionListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicTreeUI;

/**
 * A UI delegate for the {@link JTree} component.
 */
public class MetalTreeUI extends BasicTreeUI
{

  /** Listeners */
  private PropertyChangeListener propertyChangeListener;
  private FocusListener focusListener;
  private TreeSelectionListener treeSelectionListener;
  private MouseListener mouseListener;
  private KeyListener keyListener;
  private PropertyChangeListener selectionModelPropertyChangeListener;
  private ComponentListener componentListener;
  private CellEditorListener cellEditorListener;
  private TreeExpansionListener treeExpansionListener;
  private TreeModelListener treeModelListener;
    
  /**
   * Constructs a new instance of <code>MetalTreeUI</code>.
   */
  public MetalTreeUI()
  {
    super();
  }

  /**
   * Returns a new instance of <code>MetalTreeUI</code>.
   *
   * @param component the component for which we return an UI instance
   *
   * @return A new instance of <code>MetalTreeUI</code>.
   */
  public static ComponentUI createUI(JComponent component)
  {
    return new MetalTreeUI();
  }
  
  /**
   * The horizontal element of legs between nodes starts at the right of the
   * left-hand side of the child node by default. This method makes the
   * leg end before that.
   */
  protected int getHorizontalLegBuffer()
  {
    return super.getHorizontalLegBuffer();
  }

  /**
   * Configures the specified component appropriate for the look and feel.
   * This method is invoked when the ComponentUI instance is being installed 
   * as the UI delegate on the specified component. This method should completely 
   * configure the component for the look and feel, including the following:
   * 1. Install any default property values for color, fonts, borders, icons, 
   *    opacity, etc. on the component. Whenever possible, property values
   *    initialized by the client program should not be overridden.
   * 2. Install a LayoutManager on the component if necessary.
   * 3. Create/add any required sub-components to the component.
   * 4. Create/install event listeners on the component.
   * 5. Create/install a PropertyChangeListener on the component in order 
   *    to detect and respond to component property changes appropriately.
   * 6. Install keyboard UI (mnemonics, traversal, etc.) on the component.
   * 7. Initialize any appropriate instance data. 
   */
  public void installUI(JComponent c)
  {
    tree = (JTree) c;
    configureLayoutCache();
    
    UIDefaults defaults = UIManager.getLookAndFeelDefaults();
    tree.setFont(defaults.getFont("Tree.font"));
    tree.setForeground(defaults.getColor("Tree.foreground"));
    tree.setBackground(defaults.getColor("Tree.background"));
    tree.setOpaque(true);
    tree.setScrollsOnExpand(defaults.getBoolean("Tree.scrollsOnExpand"));
    rightChildIndent = defaults.getInt("Tree.rightChildIndent");
    leftChildIndent = defaults.getInt("Tree.leftChildIndent");
    setRowHeight(defaults.getInt("Tree.rowHeight"));
    tree.setRowHeight(defaults.getInt("Tree.rowHeight"));
    tree.requestFocusInWindow(false);
    
    setExpandedIcon(defaults.getIcon("Tree.expandedIcon"));
    setCollapsedIcon(defaults.getIcon("Tree.collapsedIcon"));
    
    currentCellRenderer = createDefaultCellRenderer();
    rendererPane = createCellRendererPane();
    createdRenderer = true;
    setCellEditor(createDefaultCellEditor());
    createdCellEditor = true;
    TreeModel mod = tree.getModel();
    setModel(mod);

    treeSelectionModel = tree.getSelectionModel();
    drawingCache = new Hashtable();
    nodeDimensions = createNodeDimensions();
    
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
    
    installKeyboardActions();
    
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
    
    if (mod != null)
      {
        TreePath path = new TreePath(mod.getRoot());
        if (!tree.isExpanded(path))
          toggleExpandState(path);
      }
    
    completeUIInstall();
  }
  
  /**
   * Reverses configuration which was done on the specified component during 
   * installUI. This method is invoked when this UIComponent instance is being 
   * removed as the UI delegate for the specified component. This method should 
   * undo the configuration performed in installUI, being careful to leave the 
   * JComponent instance in a clean state (no extraneous listeners, 
   * look-and-feel-specific property objects, etc.). This should include 
   * the following:
   * 1. Remove any UI-set borders from the component.
   * 2. Remove any UI-set layout managers on the component.
   * 3. Remove any UI-added sub-components from the component.
   * 4. Remove any UI-added event/property listeners from the component.
   * 5. Remove any UI-installed keyboard UI from the component.
   * 6. Nullify any allocated instance data objects to allow for GC. 
   */
  public void uninstallUI(JComponent c)
  {
    tree.setFont(null);
    tree.setForeground(null);
    tree.setBackground(null);
    
    uninstallKeyboardActions();
    
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
    TreeModel tm = tree.getModel();
    if (tm != null)
      tm.removeTreeModelListener(treeModelListener);
    
    tree = null;
    uninstallComponents();
    completeUIUninstall();
  }
  
  /**
   * This function converts between the string passed into the client
   * property and the internal representation (currently an int).
   * 
   * @param lineStyleFlag - String representation
   */     
  protected void decodeLineStyle(Object lineStyleFlag)
  {
    // FIXME: not implemented
  }

  /**
   * Checks if the location is in expand control.
   * 
   * @param row - current row
   * @param rowLevel - current level
   * @param mouseX - current x location of the mouse click
   * @param mouseY - current y location of the mouse click
   */
  protected boolean isLocationInExpandControl(int row, int rowLevel,
                                          int mouseX, int mouseY)
  {
    return super.isLocationInExpandControl(tree.getPathForRow(row), 
                                           mouseX, mouseY);
  }
  
  /**
   * Paints the specified component appropriate for the look and feel. 
   * This method is invoked from the ComponentUI.update method when the 
   * specified component is being painted. Subclasses should override this 
   * method and use the specified Graphics object to render the content of 
   * the component.
   * 
   * @param g - the current graphics configuration.
   * @param c - the current component to draw
   */
  public void paint(Graphics g, JComponent c)
  {
    // Calls BasicTreeUI's paint since it takes care of painting all
    // types of icons. 
    super.paint(g, c);
  }
  
  /**
   * Paints the horizontal separators.
   * 
   * @param g - the current graphics configuration.
   * @param c - the current component to draw
   */
  protected void paintHorizontalSeparators(Graphics g, JComponent c)
  {
    // FIXME: not implemented
  }

  
  /**
   * Paints the vertical part of the leg. The receiver should NOT modify 
   * clipBounds, insets.
   * 
   * @param g - the current graphics configuration.
   * @param clipBounds -
   * @param insets - 
   * @param path - the current path
   */
  protected void paintVerticalPartOfLeg(Graphics g, Rectangle clipBounds,
                                    Insets insets, TreePath path)
  {
    super.paintVerticalPartOfLeg(g, clipBounds, insets, path);
  }

  /**
   * Paints the horizontal part of the leg. The receiver should NOT \
   * modify clipBounds, or insets.
   * NOTE: parentRow can be -1 if the root is not visible.
   */
  protected void paintHorizontalPartOfLeg(Graphics g, Rectangle clipBounds,
                                        Insets insets, Rectangle bounds,
                                        TreePath path, int row,
                                        boolean isExpanded, boolean hasBeenExpanded,
                                        boolean isLeaf)
  {
    super.paintHorizontalPartOfLeg(g, clipBounds, insets, bounds, path, row, 
                                   isExpanded, hasBeenExpanded, isLeaf);
  }
}
