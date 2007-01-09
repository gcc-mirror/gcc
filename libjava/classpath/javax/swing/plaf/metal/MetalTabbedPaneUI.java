/* MetalTabbedPaneUI.java
   Copyright (C) 2005, 2006 Free Software Foundation, Inc.

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

import java.awt.Color;
import java.awt.Graphics;
import java.awt.LayoutManager;
import java.awt.Rectangle;

import javax.swing.JComponent;
import javax.swing.JTabbedPane;
import javax.swing.UIManager;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicTabbedPaneUI;

/**
 * A UI delegate for the {@link JTabbedPane} component.
 */
public class MetalTabbedPaneUI extends BasicTabbedPaneUI
{

  /**
   * A {@link LayoutManager} responsible for placing all the tabs and the 
   * visible component inside the {@link JTabbedPane}. This class is only used 
   * for {@link JTabbedPane#WRAP_TAB_LAYOUT}.
   *
   * @specnote Apparently this class was intended to be protected,
   *           but was made public by a compiler bug and is now
   *           public for compatibility.
   */
  public class TabbedPaneLayout 
    extends BasicTabbedPaneUI.TabbedPaneLayout
  {
    /**
     * Creates a new instance of the layout manager.
     */
    public TabbedPaneLayout()
    {
      // Nothing to do here.
    }
    
    /**
     * Overridden to do nothing, because tab runs are not rotated in the 
     * {@link MetalLookAndFeel}.
     * 
     * @param tabPlacement  the tab placement (one of {@link #TOP}, 
     *        {@link #BOTTOM}, {@link #LEFT} or {@link #RIGHT}).
     * @param selectedRun  the index of the selected run.
     */
    protected void rotateTabRuns(int tabPlacement, int selectedRun)
    {
      // do nothing, because tab runs are not rotated in the MetalLookAndFeel
    }
    
    /**
     * Overridden to do nothing, because the selected tab does not have extra
     * padding in the {@link MetalLookAndFeel}.
     * 
     * @param tabPlacement  the tab placement (one of {@link #TOP}, 
     *        {@link #BOTTOM}, {@link #LEFT} or {@link #RIGHT}).
     * @param selectedIndex  the index of the selected tab.
     */
    protected void padSelectedTab(int tabPlacement, int selectedIndex)
    {
      // do nothing, because the selected tab does not have extra padding in 
      // the MetalLookAndFeel
    }

    /**
     * Overridden because tab runs are only normalized for TOP and BOTTOM
     * tab placement in the Metal L&F.
     */
    protected void normalizeTabRuns(int tabPlacement, int tabCount, int start,
                                    int max)
    {
      if (tabPlacement == TOP || tabPlacement == BOTTOM)
        super.normalizeTabRuns(tabPlacement, tabCount, start, max);
    }
  }

  /**
   * The minimum tab width.
   */
  protected int minTabWidth;

  /**
   * The color for the selected tab.
   */
  protected Color selectColor;

  /**
   * The color for a highlighted selected tab.
   */
  protected Color selectHighlight;

  /**
   * The background color used for the tab area.
   */
  protected Color tabAreaBackground;

  /** The graphics to draw the highlight below the tab. */
  private Graphics hg;

  /**
   * Indicates if the tabs are having their background filled.
   */
  private boolean tabsOpaque;

  /**
   * Constructs a new instance of MetalTabbedPaneUI.
   */
  public MetalTabbedPaneUI()
  {
    super();
  }

  /**
   * Returns an instance of MetalTabbedPaneUI.
   *
   * @param component the component for which we return an UI instance
   *
   * @return an instance of MetalTabbedPaneUI
   */
  public static ComponentUI createUI(JComponent component)
  {
    return new MetalTabbedPaneUI();
  }
  
  /**
   * Creates and returns an instance of {@link TabbedPaneLayout}.
   * 
   * @return A layout manager used by this UI delegate.
   */
  protected LayoutManager createLayoutManager()
  {
    return (tabPane.getTabLayoutPolicy() == JTabbedPane.WRAP_TAB_LAYOUT)
           ? new MetalTabbedPaneUI.TabbedPaneLayout()
           : super.createLayoutManager();
  }
  
  /**
   * Paints the border for a single tab.
   * 
   * @param g  the graphics device.
   * @param tabPlacement  the tab placement ({@link #TOP}, {@link #LEFT}, 
   *        {@link #BOTTOM} or {@link #RIGHT}).
   * @param tabIndex  the index of the tab to draw the border for.
   * @param x  the x-coordinate for the tab's bounding rectangle.
   * @param y  the y-coordinate for the tab's bounding rectangle.
   * @param w  the width for the tab's bounding rectangle.
   * @param h  the height for the tab's bounding rectangle.
   * @param isSelected  indicates whether or not the tab is selected.
   */
  protected void paintTabBorder(Graphics g, int tabPlacement, int tabIndex, 
          int x, int y, int w, int h, boolean isSelected) 
  {
    int bottom = y + h - 1;
    int right = x + w - 1;

    switch (tabPlacement)
    {
    case LEFT: 
      paintLeftTabBorder(tabIndex, g, x, y, w, h, bottom, right, isSelected);
      break;
    case BOTTOM:
      paintBottomTabBorder(tabIndex, g, x, y, w, h, bottom, right, isSelected);
      break;
    case  RIGHT:
      paintRightTabBorder(tabIndex, g, x, y, w, h, bottom, right, isSelected);
      break;
    case TOP:
    default: 
      paintTopTabBorder(tabIndex, g, x, y, w, h, bottom, right, isSelected);
    }
  }

  /**
   * Paints the border for a tab assuming that the tab position is at the top
   * ({@link #TOP}).
   * 
   * @param tabIndex  the tab index.
   * @param g  the graphics device.
   * @param x  the x-coordinate for the tab's bounding rectangle.
   * @param y  the y-coordinate for the tab's bounding rectangle.
   * @param w  the width for the tab's bounding rectangle.
   * @param h  the height for the tab's bounding rectangle.
   * @param btm  the y coordinate of the bottom border
   * @param rght the x coordinate of the right border
   * @param isSelected  indicates whether the tab is selected.
   */
  protected void paintTopTabBorder(int tabIndex, Graphics g, int x, int y,
      int w, int h, int btm, int rght, boolean isSelected)
  {
    int tabCount = tabPane.getTabCount();
    int currentRun = getRunForTab(tabCount, tabIndex);
    int right = w - 1;
    int bottom = h - 1;

    // Paint gap.
    if (shouldFillGap(currentRun, tabIndex, x, y))
      {
        g.translate(x, y);
        g.setColor(getColorForGap(currentRun, x, y + 1));
        g.fillRect(1, 0, 5, 3);
        g.fillRect(1, 3, 2, 2);
        g.translate(-x, -y);
      }

    g.translate(x, y);

    boolean isOcean = MetalLookAndFeel.getCurrentTheme() instanceof OceanTheme;
    Color oceanSelectedBorder =
      UIManager.getColor("TabbedPane.borderHightlightColor");
    if (isOcean && isSelected)
      g.setColor(oceanSelectedBorder);
    else
      g.setColor(darkShadow);

    // Slant
    g.drawLine(1, 5, 6, 0);
    // Top.
    g.drawLine(6, 0, right, 0);
    // Right.
    int lastIndex = lastTabInRun(tabCount, currentRun);
    if (tabIndex == lastIndex)
      g.drawLine(right, 1, right, bottom);
    // Left.
    int selectedIndex = tabPane.getSelectedIndex();
    if (isOcean && tabIndex - 1 == selectedIndex
        && currentRun == getRunForTab(tabCount, selectedIndex))
      {
        g.setColor(oceanSelectedBorder);
      }
    if (tabIndex != tabRuns[runCount - 1])
      {
        if (isOcean && isSelected)
          {
            g.drawLine(0, 6, 0, bottom);
            g.setColor(darkShadow);
            g.drawLine(0, 0, 0, 5);
          }
        else
          {
            g.drawLine(0, 0, 0, bottom);
          }
      }
    else
      {
        g.drawLine(0, 6, 0, bottom);
      }

    // Paint the highlight.
    g.setColor(isSelected ? selectHighlight : highlight);
    // Slant.
    g.drawLine(1, 6, 6, 1);
    // Top.
    g.drawLine(6, 1, right, 1);
    // Left.
    g.drawLine(1, 6, 1, bottom);
    int firstIndex = tabRuns[currentRun];
    if (tabIndex == firstIndex && tabIndex != tabRuns[runCount - 1])
      {
        if (tabPane.getSelectedIndex() == tabRuns[currentRun + 1])
          g.setColor(selectHighlight);
        else
          g.setColor(highlight);
        g.drawLine(1, 0, 1, 4);
      }

    g.translate(-x, -y);
  }
  
  /**
   * Paints the border for a tab assuming that the tab position is at the left
   * ({@link #LEFT}).
   * 
   * @param tabIndex  the tab index.
   * @param g  the graphics device.
   * @param x  the x-coordinate for the tab's bounding rectangle.
   * @param y  the y-coordinate for the tab's bounding rectangle.
   * @param w  the width for the tab's bounding rectangle.
   * @param h  the height for the tab's bounding rectangle.
   * @param btm  ???
   * @param rght  ???
   * @param isSelected  indicates whether the tab is selected.
   */
  protected void paintLeftTabBorder(int tabIndex, Graphics g, int x, int y,
      int w, int h, int btm, int rght, boolean isSelected)
  {
    g.translate(x, y);
    int bottom = h - 1;
    int right = w - 1;

    int tabCount = tabPane.getTabCount();
    int currentRun = getRunForTab(tabCount, tabIndex);
    int firstIndex = tabRuns[currentRun];

    // Paint the part of the above tab.
    if (tabIndex != firstIndex && tabIndex > 0 && tabsOpaque)
      {
        Color c;
        if (tabPane.getSelectedIndex() == tabIndex - 1)
          c = selectColor;
        else
          c = getUnselectedBackground(tabIndex - 1);
        g.setColor(c);
        g.fillRect(2, 0, 4, 3);
        g.drawLine(2, 3, 2, 3);
      }

    // Paint the highlight.
    boolean isOcean = MetalLookAndFeel.getCurrentTheme() instanceof OceanTheme;
    if (isOcean)
      {
        g.setColor(isSelected ? selectHighlight : MetalLookAndFeel.getWhite());
      }
    else
      {
        g.setColor(isSelected ? selectHighlight : highlight);
      }
    // Slant.
    g.drawLine(1, 6, 6, 1);
    // Left.
    g.drawLine(1, 6, 1, bottom);
    // Top.
    g.drawLine(6, 1, right, 1);
    if (tabIndex != firstIndex)
      {
        if (isOcean)
          {
            g.setColor(MetalLookAndFeel.getWhite());
          }
        g.drawLine(1, 0, 1, 4);
      }

    // Paint border.
    Color oceanSelectedBorder =
      UIManager.getColor("TabbedPane.borderHightlightColor");
    if (isOcean && isSelected)
      {
        g.setColor(oceanSelectedBorder);
      }
    else
      {
        g.setColor(darkShadow);
      }

    // Slant.
    g.drawLine(1, 5, 6, 0);
    // Top.
    g.drawLine(6, 0, right, 0);
    // Bottom.
    int lastIndex = lastTabInRun(tabCount, currentRun);
    if (tabIndex == lastIndex)
      {
        g.drawLine(0, bottom, right, bottom);
      }
    // Left.
    if (isOcean)
      {
        if (tabPane.getSelectedIndex() == tabIndex - 1)
          {
            g.drawLine(0, 6, 0, bottom);
            if (tabIndex != firstIndex)
              {
                g.setColor(oceanSelectedBorder);
                g.drawLine(0, 0, 0, 5);
              }
          }
        else if (isSelected)
          {
            g.drawLine(0, 5, 0, bottom);
            if (tabIndex != firstIndex)
              {
                g.setColor(darkShadow);
                g.drawLine(0, 0, 0, 5);
              }
          }
        else if (tabIndex != firstIndex)
          {
            g.drawLine(0, 0, 0, bottom);
          }
        else
          {
            g.drawLine(0, 6, 0, bottom);
          }
      }
    else
      {
        if (tabIndex != firstIndex)
          {
            g.drawLine(0, 0, 0, bottom);
          }
        else
          {
            g.drawLine(0, 6, 0, bottom);
          }
      }

    g.translate(-x, -y);
  }
  
  /**
   * Paints the border for a tab assuming that the tab position is at the right
   * ({@link #RIGHT}).
   * 
   * @param tabIndex  the tab index.
   * @param g  the graphics device.
   * @param x  the x-coordinate for the tab's bounding rectangle.
   * @param y  the y-coordinate for the tab's bounding rectangle.
   * @param w  the width for the tab's bounding rectangle.
   * @param h  the height for the tab's bounding rectangle.
   * @param btm  ???
   * @param rght  ???
   * @param isSelected  indicates whether the tab is selected.
   */
  protected void paintRightTabBorder(int tabIndex, Graphics g, int x, int y,
      int w, int h, int btm, int rght, boolean isSelected)
  {
    g.translate(x, y);
    int bottom = h - 1;
    int right = w - 1;

    int tabCount = tabPane.getTabCount();
    int currentRun = getRunForTab(tabCount, tabIndex);
    int firstIndex = tabRuns[currentRun];

    // Paint part of the above tab.
    if (tabIndex != firstIndex && tabIndex > 0 && tabsOpaque)
      {
        Color c;
        if (tabPane.getSelectedIndex() == tabIndex - 1)
          c = selectColor;
        else
          c = getUnselectedBackground(tabIndex - 1);
        g.setColor(c);
        g.fillRect(right - 5, 0, 5, 3);
        g.fillRect(right - 2, 3, 2, 2);
      }

    // Paint highlight.
    g.setColor(isSelected ? selectHighlight : highlight);

    // Slant.
    g.drawLine(right - 6, 1, right - 1, 6);
    // Top.
    g.drawLine(0, 1, right - 6, 1);
    // Left.
    if (! isSelected)
      {
        g.drawLine(0, 1, 0, bottom);
      }

    // Paint border.
    boolean isOcean = MetalLookAndFeel.getCurrentTheme() instanceof OceanTheme;
    Color oceanSelectedBorder =
      UIManager.getColor("TabbedPane.borderHightlightColor");
    if (isOcean && isSelected)
      {
        g.setColor(oceanSelectedBorder);
      }
    else
      {
        g.setColor(darkShadow);
      }

    // Bottom.
    int lastIndex = lastTabInRun(tabCount, currentRun);
    if (tabIndex == lastIndex)
      {
        g.drawLine(0, bottom, right, bottom);
      }
    // Slant.
    if (isOcean && tabPane.getSelectedIndex() == tabIndex - 1)
      {
        g.setColor(oceanSelectedBorder);
      }
    g.drawLine(right - 6, 0, right, 6);
    // Top.
    g.drawLine(0, 0, right - 6, 0);
    // Right.
    if (isOcean && isSelected)
      {
        g.drawLine(right, 6, right, bottom);
        if (tabIndex != firstIndex)
          {
            g.setColor(darkShadow);
            g.drawLine(right, 0, right, 5);
          }
      }
    else if (isOcean && tabPane.getSelectedIndex() == tabIndex - 1)
      {
        if (tabIndex != firstIndex)
          {
            g.setColor(oceanSelectedBorder);
            g.drawLine(right, 0, right, 6);
          }
        g.setColor(darkShadow);
        g.drawLine(right, 7, right, bottom);
      }
    else if (tabIndex != firstIndex)
      {
        g.drawLine(right, 0, right, bottom);
      }
    else
      {
        g.drawLine(right, 6, right, bottom);
      }
    g.translate(-x, -y);
  }
  
  /**
   * Paints the border for a tab assuming that the tab position is at the bottom
   * ({@link #BOTTOM}).
   * 
   * @param tabIndex  the tab index.
   * @param g  the graphics device.
   * @param x  the x-coordinate for the tab's bounding rectangle.
   * @param y  the y-coordinate for the tab's bounding rectangle.
   * @param w  the width for the tab's bounding rectangle.
   * @param h  the height for the tab's bounding rectangle.
   * @param btm  ???
   * @param rght  ???
   * @param isSelected  indicates whether the tab is selected.
   */
  protected void paintBottomTabBorder(int tabIndex, Graphics g, int x, int y,
      int w, int h, int btm, int rght, boolean isSelected)
  {
    int bottom = h - 1;
    int right = w - 1;

    int tabCount = tabPane.getTabCount();
    int currentRun = getRunForTab(tabCount, tabIndex);
    // Paint gap if necessary.
    if (shouldFillGap(currentRun, tabIndex, x, y))
      {
        g.translate(x, y);
        g.setColor(getColorForGap(currentRun, x, y));
        g.fillRect(1, bottom - 4, 3, 5);
        g.fillRect(4, bottom - 1, 2, 2);
        g.translate(-x, -y);
      }

    g.translate(x, y);

    // Paint border.
    boolean isOcean = MetalLookAndFeel.getCurrentTheme() instanceof OceanTheme;
    Color oceanSelectedBorder =
      UIManager.getColor("TabbedPane.borderHightlightColor");
    if (isOcean && isSelected)
      {
        g.setColor(oceanSelectedBorder);
      }
    else
      {
        g.setColor(darkShadow);
      }
    // Slant.
    g.drawLine(1, bottom - 5, 6, bottom);
    // Bottom.
    g.drawLine(6, bottom, right, bottom);
    // Right.
    int lastIndex = lastTabInRun(tabCount, currentRun);
    if (tabIndex == lastIndex)
      {
        g.drawLine(right, 0, right, bottom);
      }
    // Left.
    if (isOcean && isSelected)
      {
        g.drawLine(0, 0, 0, bottom - 5);
        
        // Paint a connecting line to the tab below for all
        // but the first tab in the last run.
        if (tabIndex != tabRuns[runCount-1])
          {
            g.setColor(darkShadow);
            g.drawLine(0, bottom - 5, 0, bottom);
          }
      }
    else
      {
        if (isOcean && tabIndex == tabPane.getSelectedIndex() + 1)
          {
            g.setColor(oceanSelectedBorder);
          }
        if (tabIndex != tabRuns[runCount - 1])
          {
            g.drawLine(0, 0, 0, bottom);
          }
        else
          {
            g.drawLine(0, 0, 0, bottom - 6);
          }
      }

    // Paint highlight.
    g.setColor(isSelected ? selectHighlight : highlight);
    // Slant.
    g.drawLine(1, bottom - 6, 6, bottom - 1);
    // Left.
    g.drawLine(1, 0, 1, bottom - 6);

    int firstIndex = tabRuns[currentRun];
    if (tabIndex == firstIndex && tabIndex != tabRuns[runCount - 1])
      {
        if (tabPane.getSelectedIndex() == tabRuns[currentRun + 1])
          {
            g.setColor(selectHighlight);
          }
        else
          {
            g.setColor(highlight);
          }
        g.drawLine(1, bottom - 4, 1, bottom);
      }

    g.translate(-x, -y);
  }

  /**
   * Paints the background for a tab.
   * 
   * @param g  the graphics device.
   * @param tabPlacement  the tab placement ({@link #TOP}, {@link #LEFT}, 
   *        {@link #BOTTOM} or {@link #RIGHT}).
   * @param tabIndex  the index of the tab to draw the border for.
   * @param x  the x-coordinate for the tab's bounding rectangle.
   * @param y  the y-coordinate for the tab's bounding rectangle.
   * @param w  the width for the tab's bounding rectangle.
   * @param h  the height for the tab's bounding rectangle.
   * @param isSelected  indicates whether or not the tab is selected.
   */
  protected void paintTabBackground(Graphics g, int tabPlacement,
      int tabIndex, int x, int y, int w, int h, boolean isSelected)
  {
    if (isSelected)
      g.setColor(selectColor);
    else
      g.setColor(getUnselectedBackground(tabIndex));

    switch (tabPlacement)
    {
      case LEFT:
        g.fillRect(x + 5, y + 1, w - 5, h - 1);
        g.fillRect(x + 2, y + 4, 3, h - 4);
        break;
      case BOTTOM:
        g.fillRect(x + 2, y, w - 2, h - 3);
        g.fillRect(x + 5, y + h - 4, w - 5, 3);
        break;
      case RIGHT:
        g.fillRect(x, y + 1, w - 4, h - 1);
        g.fillRect(x + w - 4, y + 5, 3, h - 5);
        break;
      case TOP:
      default:
        g.fillRect(x + 4, y + 2, w - 4, h - 2);
        g.fillRect(x + 2, y + 5, 2, h - 5);
    }
  }
  
  /**
   * This method paints the focus rectangle around the selected tab.
   *
   * @param g The Graphics object to paint with.
   * @param tabPlacement The JTabbedPane's tab placement.
   * @param rects The array of rectangles keeping track of size and position.
   * @param tabIndex The tab index.
   * @param iconRect The icon bounds.
   * @param textRect The text bounds.
   * @param isSelected Whether this tab is selected.
   */
  protected void paintFocusIndicator(Graphics g, int tabPlacement,
                                     Rectangle[] rects, int tabIndex,
                                     Rectangle iconRect, Rectangle textRect,
                                     boolean isSelected)
  {
    if (tabPane.hasFocus() && isSelected)
      {
        Rectangle rect = rects[tabIndex];

        g.setColor(focus);
        g.translate(rect.x, rect.y);
        
        switch (tabPlacement)
          {
          case LEFT:
            // Top line
            g.drawLine(7, 2, rect.width-2, 2);
            
            // Right line
            g.drawLine(rect.width-1, 2, rect.width-1, rect.height-3);
            
            // Bottom line
            g.drawLine(rect.width-2, rect.height-2, 3, rect.height-2);
            
            // Left line
            g.drawLine(2, rect.height-3, 2, 7);

            // Slant
            g.drawLine(2, 6, 6, 2);
            break;
          case RIGHT:
            // Top line
            g.drawLine(1, 2, rect.width-8, 2);
            
            // Slant
            g.drawLine(rect.width-7, 2, rect.width-3, 6);
            
            // Right line
            g.drawLine(rect.width-3, 7, rect.width-3, rect.height-3);
            
            // Bottom line
            g.drawLine(rect.width-3, rect.height-2, 2, rect.height-2);

            // Left line
            g.drawLine(1, rect.height-2, 1, 2);
            break;
          case BOTTOM:
            // Top line
            g.drawLine(2, 1, rect.width-2, 1);
            
            // Right line
            g.drawLine(rect.width-1, 2, rect.width-1, rect.height-3);
            
            // Bottom line
            g.drawLine(7, rect.height-3, rect.width-2, rect.height-3);
            
            // Slant
            g.drawLine(6, rect.height-3, 2, rect.height-7);
            
            // Left line
            g.drawLine(2, rect.height-8, 2, 2);
            
            break;
          case TOP:
          default:
            // Top line
            g.drawLine(6, 2, rect.width-2, 2);
            
            // Right line
            g.drawLine(rect.width-1, 2, rect.width-1, rect.height-3);
            
            // Bottom line
            g.drawLine(3, rect.height-3, rect.width-2, rect.height-3);
            
            // Left line
            g.drawLine(2, rect.height-3, 2, 7);
            
            // Slant
            g.drawLine(2, 6, 6, 2);
            
          }
        
        g.translate(-rect.x, -rect.y);
      }
  }
  
  /**
   * Returns <code>true</code> if the tabs in the specified run should be 
   * padded to make the run fill the width/height of the {@link JTabbedPane}.
   * 
   * @param tabPlacement  the tab placement for the {@link JTabbedPane} (one of
   *        {@link #TOP}, {@link #BOTTOM}, {@link #LEFT} and {@link #RIGHT}).
   * @param run  the run index.
   * 
   * @return A boolean.
   */
  protected boolean shouldPadTabRun(int tabPlacement, int run)
  {
    // as far as I can tell, all runs should be padded except the last run
    // (which is drawn at the very top for tabPlacement == TOP)
    return run < this.runCount - 1;
  }

  /**
   * Installs the defaults for this UI. This method calls super.installDefaults
   * and then loads the Metal specific defaults for TabbedPane.
   */
  protected void installDefaults()
  {
    super.installDefaults();
    selectColor = UIManager.getColor("TabbedPane.selected");
    selectHighlight = UIManager.getColor("TabbedPane.selectHighlight");
    tabAreaBackground = UIManager.getColor("TabbedPane.tabAreaBackground");
    tabsOpaque = UIManager.getBoolean("TabbedPane.tabsOpaque");
    minTabWidth = 0;
  }
  
  /**
   * Returns the color for the gap.
   * 
   * @param currentRun - The current run to return the color for
   * @param x - The x position of the current run
   * @param y - The y position of the current run
   * 
   * @return the color for the gap in the current run.
   */
  protected Color getColorForGap(int currentRun, int x, int y)
  {
    int index = tabForCoordinate(tabPane, x, y);
    int selected = tabPane.getSelectedIndex();
    if (selected == index)
      return selectColor;
    return tabAreaBackground;
  }
  
  /**
   * Returns true if the gap should be filled in.
   * 
   * @param currentRun - The current run
   * @param tabIndex - The current tab
   * @param x - The x position of the tab
   * @param y - The y position of the tab
   * 
   * @return true if the gap at the current run should be filled 
   */
  protected boolean shouldFillGap(int currentRun, int tabIndex, int x, int y)
  {
    // As far as I can tell, the gap is never filled in.
    return false;
  }
  
  /**
   * Paints the highlight below the tab, if there is one.
   */
  protected void paintHighlightBelowTab()
  {
    int selected = tabPane.getSelectedIndex();
    int tabPlacement = tabPane.getTabPlacement();
    Rectangle bounds = getTabBounds(tabPane, selected);
    
    hg.setColor(selectColor);
    int x = bounds.x;
    int y = bounds.y;
    int w = bounds.width;
    int h = bounds.height;

    if (tabPlacement == TOP) 
        hg.fillRect(x, y + h - 2, w, 30);
    else if (tabPlacement == LEFT)
        hg.fillRect(x + w - 1, y, 20, h);
    else if (tabPlacement == BOTTOM)
        hg.fillRect(x, y - h + 2, w, 30);
    else if (tabPlacement == RIGHT)
        hg.fillRect(x - 18, y, 20, h);
    else 
      throw new AssertionError("Unrecognised 'tabPlacement' argument.");
    hg = null;
  }
  
  /**
   * Returns true if we should rotate the tab runs. 
   * 
   * @param tabPlacement - The current tab placement.
   * @param selectedRun - The selected run.
   * 
   * @return true if the tab runs should be rotated.
   */
  protected boolean shouldRotateTabRuns(int tabPlacement,
                                        int selectedRun)
  {
    // false because tab runs are not rotated in the MetalLookAndFeel
    return false;
  }

  protected int calculateMaxTabHeight(int tabPlacement)
  {
    // FIXME: Why is this overridden?
    return super.calculateMaxTabHeight(tabPlacement);
  }

  /**
   * Returns the amount of overlay among the tabs. In
   * the Metal L&F the overlay for LEFT and RIGHT placement
   * is half of the maxTabHeight. For TOP and BOTTOM placement
   * the tabs do not overlay.
   *
   * @param tabPlacement the placement
   *
   * @return the amount of overlay among the tabs
   */
  protected int getTabRunOverlay(int tabPlacement)
  {
    int overlay = 0;
    if (tabPlacement == LEFT || tabPlacement == RIGHT)
      {
        int maxHeight = calculateMaxTabHeight(tabPlacement);
        overlay = maxTabHeight / 2;
      }
    return overlay;
  }

  /**
   * Paints the upper edge of the content border.
   *
   * @param g the graphics to use for painting
   * @param tabPlacement the tab placement
   * @param selectedIndex the index of the selected tab
   * @param x the upper left coordinate of the content area
   * @param y the upper left coordinate of the content area
   * @param w the width of the content area
   * @param h the height of the content area
   */
  protected void paintContentBorderTopEdge(Graphics g, int tabPlacement,
                                           int selectedIndex, int x, int y,
                                           int w, int h)
  {
    Color oceanSelectedBorder =
      UIManager.getColor("TabbedPane.borderHightlightColor");
    boolean isOcean = MetalLookAndFeel.getCurrentTheme() instanceof OceanTheme;
    if (isOcean)
      {
        g.setColor(oceanSelectedBorder);
      }
    else
      {
        g.setColor(selectHighlight);
      }

    Rectangle rect = selectedIndex < 0 ? null :
                                         getTabBounds(selectedIndex, calcRect);

    // If tabs are not placed on TOP, or if the selected tab is not in the
    // run directly above the content or the selected tab is not visible,
    // then we draw an unbroken line.
    if (tabPlacement != TOP || selectedIndex < 0
        || rect.y  + rect.height + 1 < y || rect.x < x || rect.x > x + w)
      {
        g.drawLine(x, y, x + w - 2, y);
        if (isOcean && tabPlacement == TOP)
          {
            g.setColor(MetalLookAndFeel.getWhite());
            g.drawLine(x, y + 1, x + w - 2, y + 1);
          }
      }
    else
      {
        boolean isLast = isLastTabInRun(selectedIndex);
        if (isLast)
          {
            g.drawLine(x, y, rect.x + 1, y);
          }
        else
          {
            g.drawLine(x, y, rect.x, y);
          }

        int right = x + w - 1;
        if (rect.x + rect.width < right - 1)
          {
            if (isLast)
              {
                g.drawLine(rect.x + rect.width - 1, y, right - 1, y);
              }
            else
              {
                g.drawLine(rect.x + rect.width, y, right - 1, y);
              }
          }
        else
          {
            g.setColor(shadow);
            g.drawLine(x + w - 2, y, x + w - 2, y);
          }

        // When in OceanTheme, draw another white line.
        if (isOcean)
          {
            g.setColor(MetalLookAndFeel.getWhite());
            if (isLast)
              {
                g.drawLine(x, y + 1, rect.x + 1, y + 1);
              }
            else
              {
                g.drawLine(x, y + 1, rect.x, y + 1);
              }

            if (rect.x + rect.width < right - 1)
              {
                if (isLast)
                  {
                    g.drawLine(rect.x + rect.width - 1, y + 1, right - 1,
                               y + 1);
                  }
                else
                  {
                    g.drawLine(rect.x + rect.width, y + 1, right - 1, y + 1);
                  }
              }
            else
              {
                g.setColor(shadow);
                g.drawLine(x + w - 2, y + 1, x + w - 2, y + 1);
              }
          }
      }
  }

  /**
   * Paints the lower edge of the content border.
   *
   * @param g the graphics to use for painting
   * @param tabPlacement the tab placement
   * @param selectedIndex the index of the selected tab
   * @param x the upper left coordinate of the content area
   * @param y the upper left coordinate of the content area
   * @param w the width of the content area
   * @param h the height of the content area
   */
  protected void paintContentBorderBottomEdge(Graphics g, int tabPlacement,
                                              int selectedIndex, int x, int y,
                                              int w, int h)
  {
    g.setColor(darkShadow);
    
    // If tabs are not placed on BOTTOM, or if the selected tab is not in the
    // run directly below the content or the selected tab is not visible,
    // then we draw an unbroken line.
    Rectangle rect = selectedIndex < 0 ? null :
                                         getTabBounds(selectedIndex, calcRect);
    boolean isOcean = MetalLookAndFeel.getCurrentTheme() instanceof OceanTheme;
    Color oceanSelectedBorder =
      UIManager.getColor("TabbedPane.borderHightlightColor");
    if (tabPlacement != BOTTOM || selectedIndex < 0 || rect.y - 1 > h
        || rect.x < x || rect.x > x + w)
      {
        if (isOcean && tabPlacement == BOTTOM)
          {
            g.setColor(oceanSelectedBorder);
          }
        g.drawLine(x, y + h - 1, x + w - 1, y + h - 1);
      }
    else
      {
        boolean isLast = isLastTabInRun(selectedIndex);
        if (isOcean)
          {
            g.setColor(oceanSelectedBorder);
          }

        int bottom = y + h - 1;
        int right = x + w - 1;
        if (isLast)
          {
            g.drawLine(x, bottom, rect.x, bottom);
          }
        else
          {
            g.drawLine(x, bottom, rect.x - 1, bottom);
          }

        if (rect.x + rect.width < x + w - 2)
          {
            if (isLast)
              {
                g.drawLine(rect.x + rect.width - 1, bottom, right, bottom);
              }
            else
              {
                g.drawLine(rect.x + rect.width, bottom, right, bottom);
              }
          }
      }
  }

  /**
   * Paints the left edge of the content border.
   *
   * @param g the graphics to use for painting
   * @param tabPlacement the tab placement
   * @param selectedIndex the index of the selected tab
   * @param x the upper left coordinate of the content area
   * @param y the upper left coordinate of the content area
   * @param w the width of the content area
   * @param h the height of the content area
   */
  protected void paintContentBorderLeftEdge(Graphics g, int tabPlacement,
                                            int selectedIndex, int x, int y,
                                            int w, int h)
  {
    boolean isOcean = MetalLookAndFeel.getCurrentTheme() instanceof OceanTheme;
    Color oceanSelectedBorder =
      UIManager.getColor("TabbedPane.borderHightlightColor");
    Rectangle rect = selectedIndex < 0 ? null :
      getTabBounds(selectedIndex, calcRect);

    if (isOcean)
      {
        g.setColor(oceanSelectedBorder);
      }
    else
      {
        g.setColor(selectHighlight);
      }

    // If tabs are not placed on LEFT, or if the selected tab is not in the
    // run directly left to the content or the selected tab is not visible,
    // then we draw an unbroken line.
    if (tabPlacement != LEFT || selectedIndex < 0
        || rect.x + rect.width + 1 < x || rect.y < y || rect.y > y + h)
      {
        g.drawLine(x, y + 1, x, y + h - 2);
        if (isOcean && tabPlacement == LEFT)
          {
            g.setColor(MetalLookAndFeel.getWhite());
            g.drawLine(x, y + 1, x, y + h - 2);
          }       
      }
    else
      {
        g.drawLine(x, y, x, rect.y + 1);
        if (rect.y + rect.height < y + h - 2)
          {
            g.drawLine(x, rect.y + rect.height + 1, x, y + h + 2);
          }
        if (isOcean)
          {
            g.setColor(MetalLookAndFeel.getWhite());
            g.drawLine(x + 1, y + 1, x + 1, rect.y + 1);
            if (rect.y + rect.height < y + h - 2)
              {
                g.drawLine(x + 1, rect.y + rect.height + 1, x + 1, y + h + 2);
              }
          }
      }
    
  }

  /**
   * Paints the right edge of the content border.
   *
   * @param g the graphics to use for painting
   * @param tabPlacement the tab placement
   * @param selectedIndex the index of the selected tab
   * @param x the upper left coordinate of the content area
   * @param y the upper left coordinate of the content area
   * @param w the width of the content area
   * @param h the height of the content area
   */
  protected void paintContentBorderRightEdge(Graphics g, int tabPlacement,
                                             int selectedIndex, int x, int y,
                                             int w, int h)
  {
    g.setColor(darkShadow);
    Rectangle rect = selectedIndex < 0 ? null :
      getTabBounds(selectedIndex, calcRect);
    boolean isOcean = MetalLookAndFeel.getCurrentTheme() instanceof OceanTheme;
    Color oceanSelectedBorder =
      UIManager.getColor("TabbedPane.borderHightlightColor");

    // If tabs are not placed on RIGHT, or if the selected tab is not in the
    // run directly right to the content or the selected tab is not visible,
    // then we draw an unbroken line.
    if (tabPlacement != RIGHT || selectedIndex < 0 || rect.x - 1 > w
        || rect.y < y || rect.y > y + h)
      {
        if (isOcean && tabPlacement == RIGHT)
          {
            g.setColor(oceanSelectedBorder);
          }
        g.drawLine(x + w - 1, y, x + w - 1, y + h - 1);
      }
    else
      {
        if (isOcean)
          {
            g.setColor(oceanSelectedBorder);
          }
        g.drawLine(x + w - 1, y, x + w - 1, rect.y);

        if (rect.y + rect.height < y + h - 2)
          {
            g.drawLine(x + w - 1, rect.y + rect.height, x + w - 1, y + h - 2);
          }
      }
  }

  /**
   * Determines if the specified tab is the last tab in its tab run.
   *
   * @param tabIndex the index of the tab
   *
   * @return if the specified tab is the last tab in its tab run
   */
  private boolean isLastTabInRun(int tabIndex)
  {
    int count = tabPane.getTabCount();
    int run = getRunForTab(count, tabIndex);
    int lastIndex = lastTabInRun(count, run);
    return tabIndex == lastIndex;
  }

  /**
   * Returns the background for an unselected tab. This first asks the
   * JTabbedPane for the background at the specified tab index, if this
   * is an UIResource (that means, it is inherited from the JTabbedPane)
   * and the TabbedPane.unselectedBackground UI property is not null,
   * this returns the value of the TabbedPane.unselectedBackground property,
   * otherwise the value returned by the JTabbedPane.
   *
   * @param tabIndex the index of the tab for which we query the background
   *
   * @return the background for an unselected tab
   */
  private Color getUnselectedBackground(int tabIndex)
  {
    Color bg = tabPane.getBackgroundAt(tabIndex);
    Color unselectedBackground =
      UIManager.getColor("TabbedPane.unselectedBackground");
    if (bg instanceof UIResource && unselectedBackground != null)
      bg = unselectedBackground;
    return bg;
  }
  
  protected int getTabLabelShiftX(int tabPlacement,
                                  int index,
                                  boolean isSelected)
  {
    return 0;
  }

  protected int getTabLabelShiftY(int tabPlacement,
                                  int index,
                                  boolean isSelected)
  {
    return 0;
  }
  
}
