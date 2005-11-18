/* DefaultTreeCellRenderer.java 
 Copyright (C) 2002, 2004 Free Software Foundation, Inc.
 
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

import javax.swing.border.Border;
import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JTree;
import javax.swing.UIManager;
import javax.swing.SwingUtilities;
import javax.swing.plaf.UIResource;

/**
 * DefaultTreeCellRenderer
 * 
 * @author Andrew Selkirk
 */
public class DefaultTreeCellRenderer
  extends JLabel
  implements TreeCellRenderer
{
  // -------------------------------------------------------------
  // Variables --------------------------------------------------
  // -------------------------------------------------------------

  /**
   * selected
   */
  protected boolean selected;

  /**
   * hasFocus
   */
  protected boolean hasFocus;

  /**
   * drawsFocusBorderAroundIcon
   */
  private boolean drawsFocusBorderAroundIcon;

  /**
   * closedIcon
   */
  protected transient Icon closedIcon;

  /**
   * leafIcon
   */
  protected transient Icon leafIcon;

  /**
   * openIcon
   */
  protected transient Icon openIcon;

  /**
   * textSelectionColor
   */
  protected Color textSelectionColor;

  /**
   * textNonSelectionColor
   */
  protected Color textNonSelectionColor;

  /**
   * backgroundSelectionColor
   */
  protected Color backgroundSelectionColor;

  /**
   * backgroundNonSelectionColor
   */
  protected Color backgroundNonSelectionColor;

  /**
   * borderSelectionColor
   */
  protected Color borderSelectionColor;

  // -------------------------------------------------------------
  // Initialization ---------------------------------------------
  // -------------------------------------------------------------

  /**
   * Constructor DefaultTreeCellRenderer
   */
  public DefaultTreeCellRenderer()
  {
    setLeafIcon(getDefaultLeafIcon());
    setOpenIcon(getDefaultOpenIcon());
    setClosedIcon(getDefaultClosedIcon());

    setTextNonSelectionColor(UIManager.getColor("Tree.textForeground"));
    setTextSelectionColor(UIManager.getColor("Tree.selectionForeground"));
    setBackgroundNonSelectionColor(UIManager.getColor("Tree.nonSelectionBackground"));
    setBackgroundSelectionColor(UIManager.getColor("Tree.selectionBackground"));
    setBorderSelectionColor(UIManager.getColor("Tree.selectionBorderColor"));
  }

  // -------------------------------------------------------------
  // Methods ----------------------------------------------------
  // -------------------------------------------------------------

  /**
   * getDefaultOpenIcon
   * 
   * @returns Icon
   */
  public Icon getDefaultOpenIcon()
  {
    return UIManager.getIcon("Tree.openIcon");
  }

  /**
   * getDefaultClosedIcon
   * 
   * @returns Icon
   */
  public Icon getDefaultClosedIcon()
  {
    return UIManager.getIcon("Tree.closedIcon");
  }

  /**
   * getDefaultLeafIcon
   * 
   * @returns Icon
   */
  public Icon getDefaultLeafIcon()
  {
    return UIManager.getIcon("Tree.leafIcon");
  }

  /**
   * setOpenIcon
   * 
   * @param i
   *          the icon.
   */
  public void setOpenIcon(Icon i)
  {
    openIcon = i;
  }

  /**
   * getOpenIcon
   * 
   * @returns Icon
   */
  public Icon getOpenIcon()
  {
    return openIcon;
  }

  /**
   * setClosedIcon
   * 
   * @param i
   *          the icon.
   */
  public void setClosedIcon(Icon i)
  {
    closedIcon = i;
  }

  /**
   * getClosedIcon
   * 
   * @returns Icon
   */
  public Icon getClosedIcon()
  {
    return closedIcon;
  }

  /**
   * setLeafIcon
   * 
   * @param i
   *          the icon.
   */
  public void setLeafIcon(Icon i)
  {
    leafIcon = i;
  }

  /**
   * getLeafIcon
   * 
   * @returns Icon
   */
  public Icon getLeafIcon()
  {
    return leafIcon;
  }

  /**
   * setTextSelectionColor
   * 
   * @param c
   *          the color.
   */
  public void setTextSelectionColor(Color c)
  {
    textSelectionColor = c;
  }

  /**
   * getTextSelectionColor
   * 
   * @returns Color
   */
  public Color getTextSelectionColor()
  {
    return textSelectionColor;
  }

  /**
   * setTextNonSelectionColor
   * 
   * @param c
   *          the color.
   */
  public void setTextNonSelectionColor(Color c)
  {
    textNonSelectionColor = c;
  }

  /**
   * getTextNonSelectionColor
   * 
   * @returns Color
   */
  public Color getTextNonSelectionColor()
  {
    return textNonSelectionColor;
  }

  /**
   * setBackgroundSelectionColor
   * 
   * @param c
   *          the color.
   */
  public void setBackgroundSelectionColor(Color c)
  {
    backgroundSelectionColor = c;
  }

  /**
   * getBackgroundSelectionColor
   * 
   * @returns Color
   */
  public Color getBackgroundSelectionColor()
  {
    return backgroundSelectionColor;
  }

  /**
   * setBackgroundNonSelectionColor
   * 
   * @param c
   *          the color.
   */
  public void setBackgroundNonSelectionColor(Color c)
  {
    backgroundNonSelectionColor = c;
  }

  /**
   * getBackgroundNonSelectionColor
   * 
   * @returns Color
   */
  public Color getBackgroundNonSelectionColor()
  {
    return backgroundNonSelectionColor;
  }

  /**
   * setBorderSelectionColor
   * 
   * @param c
   *          the color.
   */
  public void setBorderSelectionColor(Color c)
  {
    borderSelectionColor = c;
  }

  /**
   * getBorderSelectionColor
   * 
   * @returns Color
   */
  public Color getBorderSelectionColor()
  {
    return borderSelectionColor;
  }

  /**
   * setFont
   * 
   * @param f
   *          the font.
   */
  public void setFont(Font f)
  {
    if (f != null && f instanceof UIResource)
      f = null;
    super.setFont(f);
  }

  /**
   * setBackground
   * 
   * @param c
   *          the color.
   */
  public void setBackground(Color c)
  {
    if (c != null && c instanceof UIResource)
      c = null;
    super.setBackground(c);
  }

  /**
   * getTreeCellRendererComponent
   * 
   * @param tree
   *          TODO
   * @param val
   *          TODO
   * @param selected
   *          TODO
   * @param expanded
   *          TODO
   * @param leaf
   *          TODO
   * @param row
   *          TODO
   * @param hasFocus
   *          TODO
   * @returns Component
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
    setVerticalAlignment(TOP);
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
   * getFont
   * 
   * @return the current Font
   */
  public Font getFont()
  {
    return super.getFont();
  }

  /**
   * Paints the value. The background is filled based on selected.
   * 
   * @param g
   *          the graphics device.
   */
  public void paint(Graphics g)
  {
    // paint background
    Rectangle vr = new Rectangle();
    Rectangle ir = new Rectangle();
    Rectangle tr = new Rectangle();

    Insets insets = new Insets(0, 0, 0, 0);
    Border border = UIManager.getBorder("Tree.selectionBorder");
    if (border != null)
      insets = border.getBorderInsets(this);

    FontMetrics fm = getToolkit().getFontMetrics(getFont());
    SwingUtilities.layoutCompoundLabel(((JLabel) this), fm, getText(),
                                       getIcon(), getVerticalAlignment(),
                                       getHorizontalAlignment(),
                                       getVerticalTextPosition(),
                                       getHorizontalTextPosition(), vr, ir, tr,
                                       getIconTextGap());

    g.setColor(super.getBackground());
    g.fillRect(tr.x, tr.y, tr.width, tr.height - insets.top - insets.bottom);

    // paint border
    Color b = getBorderSelectionColor();
    if (b != null)
      {
        g.setColor(b);
        g.drawRect(tr.x, tr.y, tr.width, tr.height - insets.top - insets.bottom);
      }
    super.paint(g);
  }

  /**
   * returns the preferred size of the cell.
   * 
   * @returns Dimension
   */
  public Dimension getPreferredSize()
  {
    Rectangle vr = new Rectangle();
    Rectangle ir = new Rectangle();
    Rectangle tr = new Rectangle();

    FontMetrics fm = getToolkit().getFontMetrics(getFont());
    SwingUtilities.layoutCompoundLabel(((JLabel) this), fm, getText(),
                                       getIcon(), getVerticalAlignment(),
                                       getHorizontalAlignment(),
                                       getVerticalTextPosition(),
                                       getHorizontalTextPosition(), vr, ir, tr,
                                       getIconTextGap());
    Rectangle cr = ir.union(tr);
    return new Dimension(cr.width, cr.height);
  } // getPreferredSize()

  /**
   * validate
   */
  public void validate()
  {
    // Overridden for performance reasons.
  } // validate()

  /**
   * revalidate
   */
  public void revalidate()
  {
    // Overridden for performance reasons.
  } // revalidate()

  /**
   * repaint
   * 
   * @param value0
   *          TODO
   * @param value1
   *          TODO
   * @param value2
   *          TODO
   * @param value3
   *          TODO
   * @param value4
   *          TODO
   */
  public void repaint(long value0, int value1, int value2, int value3,
                      int value4)
  {
    // Overridden for performance reasons.
  } // repaint()

  /**
   * repaint
   * 
   * @param value0
   *          TODO
   */
  public void repaint(Rectangle value0)
  {
    // Overridden for performance reasons.
  } // repaint()

  /**
   * firePropertyChange
   * 
   * @param value0
   *          TODO
   * @param value1
   *          TODO
   * @param value2
   *          TODO
   */
  protected void firePropertyChange(String value0, Object value1, Object value2)
  {
    // Overridden for performance reasons.
  } // firePropertyChange()

  /**
   * firePropertyChange
   * 
   * @param value0
   *          TODO
   * @param value1
   *          TODO
   * @param value2
   *          TODO
   */
  public void firePropertyChange(String value0, byte value1, byte value2)
  {
    // Overridden for performance reasons.
  } // firePropertyChange()

  /**
   * firePropertyChange
   * 
   * @param value0
   *          TODO
   * @param value1
   *          TODO
   * @param value2
   *          TODO
   */
  public void firePropertyChange(String value0, char value1, char value2)
  {
    // Overridden for performance reasons.
  } // firePropertyChange()

  /**
   * firePropertyChange
   * 
   * @param value0
   *          TODO
   * @param value1
   *          TODO
   * @param value2
   *          TODO
   */
  public void firePropertyChange(String value0, short value1, short value2)
  {
    // Overridden for performance reasons.
  } // firePropertyChange()

  /**
   * firePropertyChange
   * 
   * @param value0
   *          TODO
   * @param value1
   *          TODO
   * @param value2
   *          TODO
   */
  public void firePropertyChange(String value0, int value1, int value2)
  {
    // Overridden for performance reasons.
  } // firePropertyChange()

  /**
   * firePropertyChange
   * 
   * @param value0
   *          TODO
   * @param value1
   *          TODO
   * @param value2
   *          TODO
   */
  public void firePropertyChange(String value0, long value1, long value2)
  {
    // Overridden for performance reasons.
  } // firePropertyChange()

  /**
   * firePropertyChange
   * 
   * @param value0
   *          TODO
   * @param value1
   *          TODO
   * @param value2
   *          TODO
   */
  public void firePropertyChange(String value0, float value1, float value2)
  {
    // Overridden for performance reasons.
  } // firePropertyChange()

  /**
   * firePropertyChange
   * 
   * @param value0 TODO
   * @param value1 TODO
   * @param value2 TODO
   */
  public void firePropertyChange(String value0, double value1, double value2)
  {
    //  Overridden for performance reasons.
  } // firePropertyChange()

  /**
   * firePropertyChange
   * 
   * @param name the property name.
   * @param v1 the old value.
   * @param v2 the new value.
   */
  public void firePropertyChange(String name, boolean v1, boolean v2)
  {
    //  Overridden for performance reasons.
  } // firePropertyChange()

} // DefaultTreeCellRenderer
