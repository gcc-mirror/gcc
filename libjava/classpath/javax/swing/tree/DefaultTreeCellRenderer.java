/* DefaultTreeCellRenderer.java 
 Copyright (C) 2002, 2004, 2006, Free Software Foundation, Inc.
 
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
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Rectangle;

import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JTree;
import javax.swing.LookAndFeel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.plaf.UIResource;

/**
 * A default implementation of the {@link TreeCellRenderer} interface.
 * 
 * @author Andrew Selkirk
 */
public class DefaultTreeCellRenderer
  extends JLabel
  implements TreeCellRenderer
{

  /**
   * A flag indicating the current selection status.
   */
  protected boolean selected;

  /**
   * A flag indicating the current focus status.
   */
  protected boolean hasFocus;

  /**
   * Indicates if the focus border is also drawn around the icon.
   */
  private boolean drawsFocusBorderAroundIcon;

  /**
   * The icon used to represent non-leaf nodes that are closed.
   * 
   * @see #setClosedIcon(Icon)
   */
  protected transient Icon closedIcon;

  /**
   * The icon used to represent leaf nodes.
   * 
   * @see #setLeafIcon(Icon)
   */
  protected transient Icon leafIcon;

  /**
   * The icon used to represent non-leaf nodes that are open.
   * 
   * @see #setOpenIcon(Icon)
   */
  protected transient Icon openIcon;

  /**
   * The color used for text in selected cells.
   * 
   * @see #setTextSelectionColor(Color)
   */
  protected Color textSelectionColor;

  /**
   * The color used for text in non-selected cells.
   * 
   * @see #setTextNonSelectionColor(Color)
   */
  protected Color textNonSelectionColor;

  /**
   * The background color for selected cells.
   * 
   * @see #setBackgroundSelectionColor(Color)
   */
  protected Color backgroundSelectionColor;

  /**
   * The background color for non-selected cells.
   * 
   * @see #setBackgroundNonSelectionColor(Color)
   */
  protected Color backgroundNonSelectionColor;

  /**
   * The border color for selected tree cells.
   * 
   * @see #setBorderSelectionColor(Color)
   */
  protected Color borderSelectionColor;

  /**
   * Creates a new tree cell renderer with defaults appropriate for the 
   * current {@link LookAndFeel}.
   */
  public DefaultTreeCellRenderer()
  {
    setLeafIcon(getDefaultLeafIcon());
    setOpenIcon(getDefaultOpenIcon());
    setClosedIcon(getDefaultClosedIcon());

    setTextNonSelectionColor(UIManager.getColor("Tree.textForeground"));
    setTextSelectionColor(UIManager.getColor("Tree.selectionForeground"));
    setBackgroundNonSelectionColor(UIManager.getColor("Tree.textBackground"));
    setBackgroundSelectionColor(UIManager.getColor("Tree.selectionBackground"));
    setBorderSelectionColor(UIManager.getColor("Tree.selectionBorderColor"));
    Object val = UIManager.get("Tree.drawsFocusBorderAroundIcon");
    drawsFocusBorderAroundIcon = val != null && ((Boolean) val).booleanValue();
  }

  /**
   * Returns the default icon for non-leaf tree cells that are open (expanded).
   * The icon is fetched from the defaults table for the current 
   * {@link LookAndFeel} using the key <code>Tree.openIcon</code>.
   * 
   * @return The default icon.
   */
  public Icon getDefaultOpenIcon()
  {
    return UIManager.getIcon("Tree.openIcon");
  }

  /**
   * Returns the default icon for non-leaf tree cells that are closed (not 
   * expanded).  The icon is fetched from the defaults table for the current 
   * {@link LookAndFeel} using the key <code>Tree.closedIcon</code>.
   * 
   * @return The default icon.
   */
  public Icon getDefaultClosedIcon()
  {
    return UIManager.getIcon("Tree.closedIcon");
  }

  /**
   * Returns the default icon for leaf tree cells.  The icon is fetched from 
   * the defaults table for the current {@link LookAndFeel} using the key 
   * <code>Tree.leafIcon</code>.
   * 
   * @return The default icon.
   */
  public Icon getDefaultLeafIcon()
  {
    return UIManager.getIcon("Tree.leafIcon");
  }

  /**
   * Sets the icon to be displayed for non-leaf nodes that are open (expanded).
   * Set this to <code>null</code> if no icon is required.
   * 
   * @param icon  the icon (<code>null</code> permitted).
   * 
   * @see #getOpenIcon()
   */
  public void setOpenIcon(Icon icon)
  {
    openIcon = icon;
  }

  /**
   * Returns the icon displayed for non-leaf nodes that are open (expanded).  
   * The default value is initialised from the {@link LookAndFeel}.
   * 
   * @return The open icon (possibly <code>null</code>).
   * 
   * @see #setOpenIcon(Icon)
   */
  public Icon getOpenIcon()
  {
    return openIcon;
  }

  /**
   * Sets the icon to be displayed for non-leaf nodes that are closed.  Set 
   * this to <code>null</code> if no icon is required.
   * 
   * @param icon  the icon (<code>null</code> permitted).
   * 
   * @see #getClosedIcon()
   */
  public void setClosedIcon(Icon icon)
  {
    closedIcon = icon;
  }

  /**
   * Returns the icon displayed for non-leaf nodes that are closed.  The 
   * default value is initialised from the {@link LookAndFeel}.
   * 
   * @return The closed icon (possibly <code>null</code>).
   * 
   * @see #setClosedIcon(Icon)
   */
  public Icon getClosedIcon()
  {
    return closedIcon;
  }

  /**
   * Sets the icon to be displayed for leaf nodes.  Set this to 
   * <code>null</code> if no icon is required.
   * 
   * @param icon  the icon (<code>null</code> permitted).
   * 
   * @see #getLeafIcon()
   */
  public void setLeafIcon(Icon icon)
  {
    leafIcon = icon;
  }

  /**
   * Returns the icon displayed for leaf nodes.  The default value is 
   * initialised from the {@link LookAndFeel}.
   * 
   * @return The leaf icon (possibly <code>null</code>).
   * 
   * @see #setLeafIcon(Icon)
   */
  public Icon getLeafIcon()
  {
    return leafIcon;
  }

  /**
   * Sets the text color for tree cells that are selected.
   * 
   * @param c  the color (<code>null</code> permitted).
   * 
   * @see #getTextSelectionColor()
   */
  public void setTextSelectionColor(Color c)
  {
    textSelectionColor = c;
  }

  /**
   * Returns the text color for tree cells that are selected.
   * The default value is obtained from the {@link LookAndFeel} defaults
   * table using the key <code>Tree.selectionForeground</code>.
   * 
   * @return The text color for tree cells that are selected.
   * 
   * @see #setTextSelectionColor(Color)
   */
  public Color getTextSelectionColor()
  {
    return textSelectionColor;
  }

  /**
   * Sets the text color for tree cells that are not selected.
   * 
   * @param c  the color (<code>null</code> permitted).
   * 
   * @see #getTextNonSelectionColor()
   */
  public void setTextNonSelectionColor(Color c)
  {
    textNonSelectionColor = c;
  }

  /**
   * Returns the text color for tree cells that are not selected.
   * The default value is obtained from the {@link LookAndFeel} defaults
   * table using the key <code>Tree.selectionForeground</code>.
   * 
   * @return The background color for tree cells that are not selected.
   * 
   * @see #setTextgroundNonSelectionColor(Color)
   */
  public Color getTextNonSelectionColor()
  {
    return textNonSelectionColor;
  }

  /**
   * Sets the background color for tree cells that are selected.
   * 
   * @param c  the color (<code>null</code> permitted).
   * 
   * @see #getBackgroundSelectionColor()
   */
  public void setBackgroundSelectionColor(Color c)
  {
    backgroundSelectionColor = c;
  }

  /**
   * Returns the background color for tree cells that are selected.
   * The default value is obtained from the {@link LookAndFeel} defaults
   * table using the key <code>Tree.selectionBackground</code>.
   * 
   * @return The background color for tree cells that are selected.
   * 
   * @see #setBackgroundSelectionColor(Color)
   */
  public Color getBackgroundSelectionColor()
  {
    return backgroundSelectionColor;
  }

  /**
   * Sets the background color for tree cells that are not selected.
   * 
   * @param c  the color (<code>null</code> permitted).
   * 
   * @see #getBackgroundNonSelectionColor()
   */
  public void setBackgroundNonSelectionColor(Color c)
  {
    backgroundNonSelectionColor = c;
  }

  /**
   * Returns the background color for tree cells that are not selected.
   * The default value is obtained from the {@link LookAndFeel} defaults
   * table using the key <code>Tree.textBackground</code>.
   * 
   * @return The background color for tree cells that are not selected.
   * 
   * @see #setBackgroundNonSelectionColor(Color)
   */
  public Color getBackgroundNonSelectionColor()
  {
    return backgroundNonSelectionColor;
  }

  /**
   * Sets the border color for tree cells that are selected.
   * 
   * @param c  the color (<code>null</code> permitted).
   * 
   * @see #getBorderSelectionColor()
   */
  public void setBorderSelectionColor(Color c)
  {
    borderSelectionColor = c;
  }

  /**
   * Returns the border color for tree cells that are selected.
   * The default value is obtained from the {@link LookAndFeel} defaults
   * table using the key <code>Tree.selectionBorderColor</code>.
   * 
   * @return The border color for tree cells that are selected.
   * 
   * @see #setBorderSelectionColor(Color)
   */
  public Color getBorderSelectionColor()
  {
    return borderSelectionColor;
  }

  /**
   * Sets the font.
   * 
   * @param f the font.
   * 
   * @see #getFont()
   */
  public void setFont(Font f)
  {
    if (f != null && f instanceof UIResource)
      f = null;
    super.setFont(f);
  }

  /**
   * Sets the background color.
   * 
   * @param c the color.
   */
  public void setBackground(Color c)
  {
    if (c != null && c instanceof UIResource)
      c = null;
    super.setBackground(c);
  }

  /**
   * Returns a component (in fact <code>this</code>) that can be used to
   * render a tree cell with the specified state.
   * 
   * @param tree  the tree that the cell belongs to.
   * @param val  the cell value.
   * @param selected  indicates whether or not the cell is selected.
   * @param expanded  indicates whether or not the cell is expanded.
   * @param leaf  indicates whether or not the cell is a leaf in the tree.
   * @param row  the row index.
   * @param hasFocus  indicates whether or not the cell has the focus.
   * 
   * @return <code>this</code>.
   */
  public Component getTreeCellRendererComponent(JTree tree, Object val,
                                                boolean selected,
                                                boolean expanded, boolean leaf,
                                                int row, boolean hasFocus)
  {
    if (leaf)
      setIcon(getLeafIcon());
    else if (expanded)
      setIcon(getOpenIcon());
    else
      setIcon(getClosedIcon());

    setText(val.toString());
    this.selected = selected;
    this.hasFocus = hasFocus;
    setHorizontalAlignment(LEFT);
    setOpaque(false);
    setVerticalAlignment(CENTER);
    setEnabled(true);
    super.setFont(UIManager.getFont("Tree.font"));

    if (selected)
      {
        super.setBackground(getBackgroundSelectionColor());
        setForeground(getTextSelectionColor());
        
        if (hasFocus)
          setBorderSelectionColor(UIManager.getLookAndFeelDefaults().
                                  getColor("Tree.selectionBorderColor"));
        else
          setBorderSelectionColor(null);
      }
    else
      {
        super.setBackground(getBackgroundNonSelectionColor());
        setForeground(getTextNonSelectionColor());
        setBorderSelectionColor(null);
      }

    return this;
  }

  /**
   * Returns the current font.
   * 
   * @return The current font.
   * 
   * @see #setFont(Font)
   */
  public Font getFont()
  {
    return super.getFont();
  }

  /**
   * Paints the value. The background is filled based on selected.
   * 
   * @param g the graphics device.
   */
  public void paint(Graphics g)
  {
    // Determine background color.
    Color bgColor;
    if (selected)
      bgColor = getBackgroundSelectionColor();
    else
      {
        bgColor = getBackgroundNonSelectionColor();
	if (bgColor == null)
          bgColor = getBackground();
      }
    // Paint background.
    int xOffset = -1;
    if (bgColor != null)
      {
        Icon i = getIcon();
	xOffset = getXOffset();
	g.setColor(bgColor);
	g.fillRect(xOffset, 0, getWidth() - xOffset, getHeight());
      }

    if (hasFocus)
      {
        if (drawsFocusBorderAroundIcon)
          xOffset = 0;
	else if (xOffset == -1)
          xOffset = getXOffset();
	paintFocus(g, xOffset, 0, getWidth() - xOffset, getHeight());
      }
    super.paint(g);
  }

  /**
   * Paints the focus indicator.
   */
  private void paintFocus(Graphics g, int x, int y, int w, int h)
  {
    Color col = getBorderSelectionColor();
    if (col != null)
      {
        g.setColor(col);
	g.drawRect(x, y, w - 1, h - 1);
      }
  }

  /**
   * Determines the X offset of the label that is caused by
   * the icon.
   *
   * @return the X offset of the label
   */
  private int getXOffset()
  {
    Icon i = getIcon();
    int offs = 0;
    if (i != null && getText() != null)
      offs = i.getIconWidth() + Math.max(0, getIconTextGap() - 1);
    return offs;
  }

  /**
   * Returns the preferred size of the cell.
   * 
   * @return The preferred size of the cell.
   */
  public Dimension getPreferredSize()
  {
    Dimension size = super.getPreferredSize();
    size.width += 3;
    return size;
  } 

  /**
   * For performance reasons, this method is overridden to do nothing.
   */
  public void validate()
  {
    // Overridden for performance reasons.
  } 

  /**
   * For performance reasons, this method is overridden to do nothing.
   */
  public void revalidate()
  {
    // Overridden for performance reasons.
  } 

  /**
   * For performance reasons, this method is overridden to do nothing.
   * 
   * @param tm ignored
   * @param x coordinate of the region to mark as dirty
   * @param y coordinate of the region to mark as dirty
   * @param width dimension of the region to mark as dirty
   * @param height dimension of the region to mark as dirty
   */
  public void repaint(long tm, int x, int y, int width, int height)
  {
    // Overridden for performance reasons.
  } 

  /**
   * For performance reasons, this method is overridden to do nothing.
   * 
   * @param area  the area to repaint.
   */
  public void repaint(Rectangle area)
  {
    // Overridden for performance reasons.
  } 

  /**
   * For performance reasons, this method is overridden to do nothing.
   * 
   * @param name  the property name.
   * @param oldValue  the old value.
   * @param newValue  the new value.
   */
  protected void firePropertyChange(String name, Object oldValue, 
                                    Object newValue)
  {
    // Overridden for performance reasons.
  }

  /**
   * For performance reasons, this method is overridden to do nothing.
   * 
   * @param name  the property name.
   * @param oldValue  the old value.
   * @param newValue  the new value.
   */
  public void firePropertyChange(String name, byte oldValue, byte newValue)
  {
    // Overridden for performance reasons.
  }

  /**
   * For performance reasons, this method is overridden to do nothing.
   * 
   * @param name  the property name.
   * @param oldValue  the old value.
   * @param newValue  the new value.
   */
  public void firePropertyChange(String name, char oldValue, char newValue)
  {
    // Overridden for performance reasons.
  }

  /**
   * For performance reasons, this method is overridden to do nothing.
   * 
   * @param name  the property name.
   * @param oldValue  the old value.
   * @param newValue  the new value.
   */
  public void firePropertyChange(String name, short oldValue, short newValue)
  {
    // Overridden for performance reasons.
  } 

  /**
   * For performance reasons, this method is overridden to do nothing.
   * 
   * @param name  the property name.
   * @param oldValue  the old value.
   * @param newValue  the new value.
   */
  public void firePropertyChange(String name, int oldValue, int newValue)
  {
    // Overridden for performance reasons.
  }

  /**
   * For performance reasons, this method is overridden to do nothing.
   * 
   * @param name  the property name.
   * @param oldValue  the old value.
   * @param newValue  the new value.
   */
  public void firePropertyChange(String name, long oldValue, long newValue)
  {
    // Overridden for performance reasons.
  }

  /**
   * For performance reasons, this method is overridden to do nothing.
   * 
   * @param name  the property name.
   * @param oldValue  the old value.
   * @param newValue  the new value.
   */
  public void firePropertyChange(String name, float oldValue, float newValue)
  {
    // Overridden for performance reasons.
  }

  /**
   * For performance reasons, this method is overridden to do nothing.
   * 
   * @param name  the property name.
   * @param oldValue  the old value.
   * @param newValue  the new value.
   */
  public void firePropertyChange(String name, double oldValue, double newValue)
  {
    //  Overridden for performance reasons.
  }

  /**
   * For performance reasons, this method is overridden to do nothing.
   * 
   * @param name  the property name.
   * @param oldValue  the old value.
   * @param newValue  the new value.
   */
  public void firePropertyChange(String name, boolean oldValue, 
                                 boolean newValue)
  {
    //  Overridden for performance reasons.
  } 

} 
