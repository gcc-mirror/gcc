/* DefaultSwatchChooserPanel.java --
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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


package javax.swing.colorchooser;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.Icon;
import javax.swing.JColorChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;

/**
 * This class is the DefaultSwatchChooserPanel. This chooser panel displays a
 * set of colors that can be picked. Recently picked items will go into a
 * side panel so the user can see the history of the chosen colors.
 */
class DefaultSwatchChooserPanel extends AbstractColorChooserPanel
{
  /** The main panel that holds the set of choosable colors. */
  MainSwatchPanel mainPalette;

  /** A panel that holds the recent colors. */
  RecentSwatchPanel recentPalette;

  /** The mouse handlers for the panels. */
  MouseListener mouseHandler;

  /**
   * This the base class for all swatch panels. Swatch panels are panels that
   * hold a set of blocks where colors are displayed.
   */
  abstract static class SwatchPanel extends JPanel
  {
    /** The width of each block. */
    protected int cellWidth = 10;

    /** The height of each block. */
    protected int cellHeight = 10;

    /** The gap between blocks. */
    protected int gap = 1;

    /** The number of rows in the swatch panel. */
    protected int numRows;

    /** The number of columns in the swatch panel. */
    protected int numCols;

    /**
     * Creates a new SwatchPanel object.
     */
    SwatchPanel()
    {
      super();
      setBackground(Color.WHITE);
    }

    /**
     * This method returns the preferred size of the swatch panel based on the
     * number of rows and columns and the size of each cell.
     *
     * @return The preferred size of the swatch panel.
     */
    public Dimension getPreferredSize()
    {
      int height = numRows * cellHeight + (numRows - 1) * gap;
      int width = numCols * cellWidth + (numCols - 1) * gap;
      Insets insets = getInsets();

      return new Dimension(width + insets.left + insets.right,
                           height + insets.top + insets.bottom);
    }

    /**
     * This method returns the color for the given position.
     *
     * @param x The x coordinate of the position.
     * @param y The y coordinate of the position.
     *
     * @return The color at the given position.
     */
    public abstract Color getColorForPosition(int x, int y);

    /**
     * This method initializes the colors for the swatch panel.
     */
    protected abstract void initializeColors();
  }

  /**
   * This is the main swatch panel. This panel sits in the middle and allows a
   * set of colors to be picked which will move to the recent swatch panel.
   */
  static class MainSwatchPanel extends SwatchPanel
  {
    /** The color describing (204, 255, 255) */
    public static final Color C204255255 = new Color(204, 204, 255);

    /** The color describing (255, 204, 204) */
    public static final Color C255204204 = new Color(255, 204, 204);

    /** The color describing (204, 255, 204) */
    public static final Color C204255204 = new Color(204, 255, 204);

    /** The color describing (204, 204, 204) */
    public static final Color C204204204 = new Color(204, 204, 204);

    /** The color (153, 153, 255). */
    public static final Color C153153255 = new Color(153, 153, 255);

    /** The color (51, 51, 255). */
    public static final Color C051051255 = new Color(51, 51, 255);

    /** The color (153, 0, 153). */
    public static final Color C153000153 = new Color(153, 0, 153);

    /** The color (0, 51, 51). */
    public static final Color C000051051 = new Color(0, 51, 51);

    /** The color (51, 0, 51). */
    public static final Color C051000051 = new Color(51, 0, 51);

    /** The color (51, 51, 0). */
    public static final Color C051051000 = new Color(51, 51, 0);

    /** The color (102, 102, 0). */
    public static final Color C102102000 = new Color(102, 102, 0);

    /** The color (153, 255, 153). */
    public static final Color C153255153 = new Color(153, 255, 153);

    /** The color (102, 255, 102). */
    public static final Color C102255102 = new Color(102, 255, 102);

    /** The color (0, 102, 102). */
    public static final Color C000102102 = new Color(0, 102, 102);

    /** The color (102, 0, 102). */
    public static final Color C102000102 = new Color(102, 0, 102);

    /** The color (0, 153, 153). */
    public static final Color C000153153 = new Color(0, 153, 153);

    /** The color (153, 153, 0). */
    public static final Color C153153000 = new Color(153, 153, 0);

    /** The color (204, 204, 0). */
    public static final Color C204204000 = new Color(204, 204, 0);

    /** The color (204, 0, 204). */
    public static final Color C204000204 = new Color(204, 0, 204);

    /** The color (0, 204, 204). */
    public static final Color C000204204 = new Color(0, 204, 204);

    /** The color (51, 255, 51). */
    public static final Color C051255051 = new Color(51, 255, 51);

    /** The color (255, 51, 51). */
    public static final Color C255051051 = new Color(255, 51, 51);

    /** The color (255, 102, 102). */
    public static final Color C255102102 = new Color(255, 102, 102);

    /** The color (102, 102, 255). */
    public static final Color C102102255 = new Color(102, 102, 255);

    /** The color (255, 153, 153). */
    public static final Color C255153153 = new Color(255, 153, 153);
    static Color[] colors = 
                            {
                              // Row 1
    Color.WHITE, new Color(204, 255, 255), C204255255, C204255255, C204255255,
                              C204255255, C204255255, C204255255, C204255255,
                              C204255255, C204255255, new Color(255, 204, 255),
                              C255204204, C255204204, C255204204, C255204204,
                              C255204204, C255204204, C255204204, C255204204,
                              C255204204, new Color(255, 255, 204), C204255204,
                              C204255204, C204255204, C204255204, C204255204,
                              C204255204, C204255204, C204255204, C204255204,
                              
    // Row 2
    C204204204, new Color(153, 255, 255), new Color(153, 204, 255), C153153255,
                              C153153255, C153153255, C153153255, C153153255,
                              C153153255, C153153255, new Color(204, 153, 255),
                              new Color(255, 153, 255),
                              new Color(255, 153, 204), C255153153, C255153153,
                              C255153153, C255153153, C255153153, C255153153,
                              C255153153, new Color(255, 204, 153),
                              new Color(255, 255, 153),
                              new Color(204, 255, 153), C153255153, C153255153,
                              C153255153, C153255153, C153255153, C153255153,
                              C153255153, new Color(153, 255, 204),
                              
    // Row 3
    C204204204, new Color(102, 255, 255), new Color(102, 204, 255),
                              new Color(102, 153, 255), C102102255, C102102255,
                              C102102255, C102102255, C102102255,
                              new Color(153, 102, 255),
                              new Color(204, 102, 255),
                              new Color(255, 102, 255),
                              new Color(255, 102, 204),
                              new Color(255, 102, 153), C255102102, C255102102,
                              C255102102, C255102102, C255102102,
                              new Color(255, 153, 102),
                              new Color(255, 204, 102),
                              new Color(255, 255, 102),
                              new Color(204, 255, 102),
                              new Color(153, 255, 102), C102255102, C102255102,
                              C102255102, C102255102, C102255102,
                              new Color(102, 255, 153),
                              new Color(102, 255, 204),
                              
    // Row 4
    new Color(153, 153, 153), new Color(51, 255, 255), new Color(51, 204, 255),
                              new Color(51, 153, 255), new Color(51, 102, 255),
                              C051051255, C051051255, C051051255,
                              new Color(102, 51, 255), new Color(153, 51, 255),
                              new Color(204, 51, 255), new Color(255, 51, 255),
                              new Color(255, 51, 204), new Color(255, 51, 153),
                              new Color(255, 51, 102), C255051051, C255051051,
                              C255051051, new Color(255, 102, 51),
                              new Color(255, 153, 51), new Color(255, 204, 51),
                              new Color(255, 255, 51), new Color(204, 255, 51),
                              new Color(153, 255, 51), new Color(102, 255, 51),
                              C051255051, C051255051, C051255051,
                              new Color(51, 255, 102), new Color(51, 255, 153),
                              new Color(51, 255, 204),
                              
    // Row 5
    new Color(153, 153, 153), new Color(0, 255, 255), new Color(0, 204, 255),
                              new Color(0, 153, 255), new Color(0, 102, 255),
                              new Color(0, 51, 255), new Color(0, 0, 255),
                              new Color(51, 0, 255), new Color(102, 0, 255),
                              new Color(153, 0, 255), new Color(204, 0, 255),
                              new Color(255, 0, 255), new Color(255, 0, 204),
                              new Color(255, 0, 153), new Color(255, 0, 102),
                              new Color(255, 0, 51), new Color(255, 0, 0),
                              new Color(255, 51, 0), new Color(255, 102, 0),
                              new Color(255, 153, 0), new Color(255, 204, 0),
                              new Color(255, 255, 0), new Color(204, 255, 0),
                              new Color(153, 255, 0), new Color(102, 255, 0),
                              new Color(51, 255, 0), new Color(0, 255, 0),
                              new Color(0, 255, 51), new Color(0, 255, 102),
                              new Color(0, 255, 153), new Color(0, 255, 204),
                              
    // Row 6
    new Color(102, 102, 102), C000204204, C000204204, new Color(0, 153, 204),
                              new Color(0, 102, 204), new Color(0, 51, 204),
                              new Color(0, 0, 204), new Color(51, 0, 204),
                              new Color(102, 0, 204), new Color(153, 0, 204),
                              C204000204, C204000204, C204000204,
                              new Color(204, 0, 153), new Color(204, 0, 102),
                              new Color(204, 0, 51), new Color(204, 0, 0),
                              new Color(204, 51, 0), new Color(204, 102, 0),
                              new Color(204, 153, 0), C204204000, C204204000,
                              C204204000, new Color(153, 204, 0),
                              new Color(102, 204, 0), new Color(51, 204, 0),
                              new Color(0, 204, 0), new Color(0, 204, 51),
                              new Color(0, 204, 102), new Color(0, 204, 153),
                              new Color(0, 204, 204),
                              
    // Row 7
    new Color(102, 102, 102), C000153153, C000153153, C000153153,
                              new Color(0, 102, 153), new Color(0, 51, 153),
                              new Color(0, 0, 153), new Color(51, 0, 153),
                              new Color(102, 0, 153), C153000153, C153000153,
                              C153000153, C153000153, C153000153,
                              new Color(153, 0, 102), new Color(153, 0, 51),
                              new Color(153, 0, 0), new Color(153, 51, 0),
                              new Color(153, 102, 0), C153153000, C153153000,
                              C153153000, C153153000, C153153000,
                              new Color(102, 153, 0), new Color(51, 153, 0),
                              new Color(0, 153, 0), new Color(0, 153, 51),
                              new Color(0, 153, 102), C000153153, C000153153,
                              
    // Row 8
    new Color(51, 51, 51), C000102102, C000102102, C000102102, C000102102,
                              new Color(0, 51, 102), new Color(0, 0, 102),
                              new Color(51, 0, 102), C102000102, C102000102,
                              C102000102, C102000102, C102000102, C102000102,
                              C102000102, new Color(102, 0, 51),
                              new Color(102, 0, 0), new Color(102, 51, 0),
                              C102102000, C102102000, C102102000, C102102000,
                              C102102000, C102102000, C102102000,
                              new Color(51, 102, 0), new Color(0, 102, 0),
                              new Color(0, 102, 51), C000102102, C000102102,
                              C000102102,
                              
    // Row 9.
    Color.BLACK, C000051051, C000051051, C000051051, C000051051, C000051051,
                              new Color(0, 0, 51), C051000051, C051000051,
                              C051000051, C051000051, C051000051, C051000051,
                              C051000051, C051000051, C051000051,
                              new Color(51, 0, 0), C051051000, C051051000,
                              C051051000, C051051000, C051051000, C051051000,
                              C051051000, C051051000, new Color(0, 51, 0),
                              C000051051, C000051051, C000051051, C000051051,
                              new Color(51, 51, 51)
                            };

    /**
     * Creates a new MainSwatchPanel object.
     */
    MainSwatchPanel()
    {
      super();
      numCols = 31;
      numRows = 9;
      initializeColors();
      revalidate();
    }

    /**
     * This method returns the color for the given position.
     *
     * @param x The x location for the position.
     * @param y The y location for the position.
     *
     * @return The color for the given position.
     */
    public Color getColorForPosition(int x, int y)
    {
      if (x % (cellWidth + gap) > cellWidth
          || y % (cellHeight + gap) > cellHeight)
	// position is located in gap.
	return null;

      int row = y / (cellHeight + gap);
      int col = x / (cellWidth + gap);
      return colors[row * numCols + col];
    }

    /**
     * This method initializes the colors for the main swatch panel.
     */
    protected void initializeColors()
    {
      // Unnecessary
    }

    /**
     * This method paints the main graphics panel with the given Graphics
     * object.
     *
     * @param graphics The Graphics object to paint with.
     */
    public void paint(Graphics graphics)
    {
      int index = 0;
      Insets insets = getInsets();
      int currX = insets.left;
      int currY = insets.top;
      Color saved = graphics.getColor();

      for (int i = 0; i < numRows; i++)
        {
	  for (int j = 0; j < numCols; j++)
	    {
	      graphics.setColor(colors[index++]);
	      graphics.fill3DRect(currX, currY, cellWidth, cellHeight, true);
	      currX += gap + cellWidth;
	    }
	  currX = insets.left;
	  currY += gap + cellHeight;
        }
      graphics.setColor(saved);
    }

    /**
     * This method returns the tooltip text for the given MouseEvent.
     *
     * @param e The MouseEvent to find tooltip text for.
     *
     * @return The tooltip text.
     */
    public String getToolTipText(MouseEvent e)
    {
      Color c = getColorForPosition(e.getX(), e.getY());
      if (c == null)
	return null;
      return (c.getRed() + "," + c.getGreen() + "," + c.getBlue());
    }
  }

  /**
   * This class is the recent swatch panel. It holds recently selected colors.
   */
  static class RecentSwatchPanel extends SwatchPanel
  {
    /** The array for storing recently stored colors. */
    Color[] colors;

    /** The default color. */
    public static final Color defaultColor = Color.GRAY;

    /** The index of the array that is the start. */
    int start = 0;

    /**
     * Creates a new RecentSwatchPanel object.
     */
    RecentSwatchPanel()
    {
      super();
      numCols = 5;
      numRows = 7;
      initializeColors();
      revalidate();
    }

    /**
     * This method returns the color for the given position.
     *
     * @param x The x coordinate of the position.
     * @param y The y coordinate of the position.
     *
     * @return The color for the given position.
     */
    public Color getColorForPosition(int x, int y)
    {
      if (x % (cellWidth + gap) > cellWidth
          || y % (cellHeight + gap) > cellHeight)
	// position is located in gap.
	return null;

      int row = y / (cellHeight + gap);
      int col = x / (cellWidth + gap);

      return colors[getIndexForCell(row, col)];
    }

    /**
     * This method initializes the colors for the recent swatch panel.
     */
    protected void initializeColors()
    {
      colors = new Color[numRows * numCols];
      for (int i = 0; i < colors.length; i++)
	colors[i] = defaultColor;
    }

    /**
     * This method returns the array index for the given row and column.
     *
     * @param row The row.
     * @param col The column.
     *
     * @return The array index for the given row and column.
     */
    private int getIndexForCell(int row, int col)
    {
      return ((row * numCols) + col + start) % (numRows * numCols);
    }

    /**
     * This method adds the given color to the beginning of the swatch panel.
     *
     * @param c The color to add.
     */
    private void addColorToQueue(Color c)
    {
      if (--start == -1)
	start = numRows * numCols - 1;

      colors[start] = c;
    }

    /**
     * This method paints the panel with the given Graphics object.
     *
     * @param g The Graphics object to paint with.
     */
    public void paint(Graphics g)
    {
      Color saved = g.getColor();
      Insets insets = getInsets();
      int currX = insets.left;
      int currY = insets.top;

      for (int i = 0; i < numRows; i++)
        {
	  for (int j = 0; j < numCols; j++)
	    {
	      g.setColor(colors[getIndexForCell(i, j)]);
	      g.fill3DRect(currX, currY, cellWidth, cellHeight, true);
	      currX += cellWidth + gap;
	    }
	  currX = insets.left;
	  currY += cellWidth + gap;
        }
    }

    /**
     * This method returns the tooltip text for the given MouseEvent.
     *
     * @param e The MouseEvent.
     *
     * @return The tooltip text.
     */
    public String getToolTipText(MouseEvent e)
    {
      Color c = getColorForPosition(e.getX(), e.getY());
      if (c == null)
	return null;
      return c.getRed() + "," + c.getGreen() + "," + c.getBlue();
    }
  }

  /**
   * This class handles mouse events for the two swatch panels.
   */
  class MouseHandler extends MouseAdapter
  {
    /**
     * This method is called whenever the mouse is pressed.
     *
     * @param e The MouseEvent.
     */
    public void mousePressed(MouseEvent e)
    {
      SwatchPanel panel = (SwatchPanel) e.getSource();
      Color c = panel.getColorForPosition(e.getX(), e.getY());
      recentPalette.addColorToQueue(c);
      DefaultSwatchChooserPanel.this.getColorSelectionModel().setSelectedColor(c);
      DefaultSwatchChooserPanel.this.repaint();
    }
  }

  /**
   * This is the layout manager for the main panel.
   */
  static class MainPanelLayout implements LayoutManager
  {
    /**
     * This method is called when a new component is added to the container.
     *
     * @param name The name of the component.
     * @param comp The added component.
     */
    public void addLayoutComponent(String name, Component comp)
    {
    }

    /**
     * This method is called to set the size and position of the child
     * components for the given container.
     *
     * @param parent The container to lay out.
     */
    public void layoutContainer(Container parent)
    {
      Component[] comps = parent.getComponents();
      Insets insets = parent.getInsets();
      Dimension[] pref = new Dimension[comps.length];

      int xpos = 0;
      int ypos = 0;
      int maxHeight = 0;
      int totalWidth = 0;

      for (int i = 0; i < comps.length; i++)
        {
	  pref[i] = comps[i].getPreferredSize();
	  if (pref[i] == null)
	    return;
	  maxHeight = Math.max(maxHeight, pref[i].height);
	  totalWidth += pref[i].width;
        }

      ypos = (parent.getSize().height - maxHeight) / 2 + insets.top;
      xpos = insets.left + (parent.getSize().width - totalWidth) / 2;

      for (int i = 0; i < comps.length; i++)
        {
	  if (pref[i] == null)
	    continue;
	  comps[i].setBounds(xpos, ypos, pref[i].width, pref[i].height);
	  xpos += pref[i].width;
        }
    }

    /**
     * This method is called when a component is removed from the container.
     *
     * @param comp The component that was removed.
     */
    public void removeLayoutComponent(Component comp)
    {
    }

    /**
     * This methods calculates the minimum layout size for the container.
     *
     * @param parent The container.
     *
     * @return The minimum layout size.
     */
    public Dimension minimumLayoutSize(Container parent)
    {
      return preferredLayoutSize(parent);
    }

    /**
     * This method returns the preferred layout size for the given container.
     *
     * @param parent The container.
     *
     * @return The preferred layout size.
     */
    public Dimension preferredLayoutSize(Container parent)
    {
      int xmax = 0;
      int ymax = 0;

      Component[] comps = parent.getComponents();
      Dimension pref;

      for (int i = 0; i < comps.length; i++)
        {
	  pref = comps[i].getPreferredSize();
	  if (pref == null)
	    continue;
	  xmax += pref.width;
	  ymax = Math.max(ymax, pref.height);
        }

      Insets insets = parent.getInsets();

      return new Dimension(insets.left + insets.right + xmax,
                           insets.top + insets.bottom + ymax);
    }
  }

  /**
   * This is the layout manager for the recent swatch panel.
   */
  static class RecentPanelLayout implements LayoutManager
  {
    /**
     * This method is called when a component is added to the container.
     *
     * @param name The name of the component.
     * @param comp The added component.
     */
    public void addLayoutComponent(String name, Component comp)
    {
      // Nothing needs to be done.
    }

    /**
     * This method sets the size and position of the child components of the
     * given container.
     *
     * @param parent The container to lay out.
     */
    public void layoutContainer(Container parent)
    {
      Component[] comps = parent.getComponents();
      Dimension parentSize = parent.getSize();
      Insets insets = parent.getInsets();
      int currY = insets.top;
      Dimension pref;

      for (int i = 0; i < comps.length; i++)
        {
	  pref = comps[i].getPreferredSize();
	  if (pref == null)
	    continue;
	  comps[i].setBounds(insets.left, currY, pref.width, pref.height);
	  currY += pref.height;
        }
    }

    /**
     * This method calculates the minimum layout size for the given container.
     *
     * @param parent The container.
     *
     * @return The minimum layout size.
     */
    public Dimension minimumLayoutSize(Container parent)
    {
      return preferredLayoutSize(parent);
    }

    /**
     * This method calculates the preferred layout size for the given
     * container.
     *
     * @param parent The container.
     *
     * @return The preferred layout size.
     */
    public Dimension preferredLayoutSize(Container parent)
    {
      int width = 0;
      int height = 0;
      Insets insets = parent.getInsets();
      Component[] comps = parent.getComponents();
      Dimension pref;
      for (int i = 0; i < comps.length; i++)
        {
	  pref = comps[i].getPreferredSize();
	  if (pref != null)
	    {
	      width = Math.max(width, pref.width);
	      height += pref.height;
	    }
        }

      return new Dimension(width + insets.left + insets.right,
                           height + insets.top + insets.bottom);
    }

    /**
     * This method is called whenever a component is removed from the
     * container.
     *
     * @param comp The removed component.
     */
    public void removeLayoutComponent(Component comp)
    {
      // Nothing needs to be done.
    }
  }

  /**
   * Creates a new DefaultSwatchChooserPanel object.
   */
  DefaultSwatchChooserPanel()
  {
    super();
  }

  /**
   * This method updates the chooser panel with the new value from the
   * JColorChooser.
   */
  public void updateChooser()
  {
  }

  /**
   * This method builds the chooser panel.
   */
  protected void buildChooser()
  {
    // The structure of the swatch panel is:
    // One large panel (minus the insets).
    // Inside that panel, there are two panels, one holds the palette.
    // The other holds the label and the recent colors palette.
    // The two palettes are two custom swatch panels.
    setLayout(new MainPanelLayout());

    JPanel mainPaletteHolder = new JPanel();
    JPanel recentPaletteHolder = new JPanel();

    mainPalette = new MainSwatchPanel();
    recentPalette = new RecentSwatchPanel();
    JLabel label = new JLabel("Recent:");

    mouseHandler = new MouseHandler();
    mainPalette.addMouseListener(mouseHandler);
    recentPalette.addMouseListener(mouseHandler);

    mainPaletteHolder.setLayout(new BorderLayout());
    mainPaletteHolder.add(mainPalette, BorderLayout.CENTER);

    recentPaletteHolder.setLayout(new RecentPanelLayout());
    recentPaletteHolder.add(label);
    recentPaletteHolder.add(recentPalette);

    JPanel main = new JPanel();
    main.add(mainPaletteHolder);
    main.add(recentPaletteHolder);

    this.add(main);
  }

  /**
   * This method removes the chooser panel from the JColorChooser.
   *
   * @param chooser The JColorChooser this panel is being removed from.
   */
  public void uninstallChooserPanel(JColorChooser chooser)
  {
    recentPalette = null;
    mainPalette = null;

    removeAll();
    super.uninstallChooserPanel(chooser);
  }

  /**
   * This method returns the JTabbedPane displayed name.
   *
   * @return The name displayed in the JTabbedPane.
   */
  public String getDisplayName()
  {
    return "Swatches";
  }

  /**
   * This method returns the small display icon.
   *
   * @return The small display icon.
   */
  public Icon getSmallDisplayIcon()
  {
    return null;
  }

  /**
   * This method returns the large display icon.
   *
   * @return The large display icon.
   */
  public Icon getLargeDisplayIcon()
  {
    return null;
  }

  /**
   * This method paints the chooser panel with the given Graphics object.
   *
   * @param g The Graphics object to paint with.
   */
  public void paint(Graphics g)
  {
    super.paint(g);
  }

  /**
   * This method returns the tooltip text for the given MouseEvent.
   *
   * @param e The MouseEvent.
   *
   * @return The tooltip text.
   */
  public String getToolTipText(MouseEvent e)
  {
    return null;
  }
}
