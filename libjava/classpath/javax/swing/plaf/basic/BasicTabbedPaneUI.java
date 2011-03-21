/* BasicTabbedPaneUI.java --
   Copyright (C) 2002, 2004, 2005, 2006  Free Software Foundation, Inc.

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
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.AbstractAction;
import javax.swing.ActionMap;
import javax.swing.Icon;
import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JViewport;
import javax.swing.KeyStroke;
import javax.swing.LookAndFeel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.ActionMapUIResource;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.TabbedPaneUI;
import javax.swing.plaf.UIResource;
import javax.swing.text.View;

/**
 * This is the Basic Look and Feel's UI delegate for JTabbedPane.
 *
 * @author Lillian Angel (langel@redhat.com)
 * @author Kim Ho (kho@redhat.com)
 * @author Roman Kennke (kennke@aicas.com)
 * @author Robert Schuster (robertschuster@fsfe.org)
 */
public class BasicTabbedPaneUI extends TabbedPaneUI implements SwingConstants
{

  static class NavigateAction extends AbstractAction
  {
    int direction;

    NavigateAction(String name, int dir)
    {
      super(name);
      direction = dir;
    }

    public void actionPerformed(ActionEvent event)
    {
      JTabbedPane tp = (JTabbedPane) event.getSource();
      BasicTabbedPaneUI ui = (BasicTabbedPaneUI) tp.getUI();

      ui.navigateSelectedTab(direction);
    }

  }

  static class NavigatePageDownAction extends AbstractAction
  {

    public NavigatePageDownAction()
    {
      super("navigatePageDown");
    }

    public void actionPerformed(ActionEvent event)
    {
      JTabbedPane tp = (JTabbedPane) event.getSource();
      BasicTabbedPaneUI ui = (BasicTabbedPaneUI) tp.getUI();

      int i = tp.getSelectedIndex();

      if (i < 0)
        i = 0;

      ui.selectNextTabInRun(i);
    }

  }

  static class NavigatePageUpAction extends AbstractAction
  {

    public NavigatePageUpAction()
    {
      super("navigatePageUp");
    }

    public void actionPerformed(ActionEvent event)
    {
      JTabbedPane tp = (JTabbedPane) event.getSource();
      BasicTabbedPaneUI ui = (BasicTabbedPaneUI) tp.getUI();

      int i = tp.getSelectedIndex();

      if (i < 0)
        i = 0;

      ui.selectPreviousTabInRun(i);

    }
  }

  static class RequestFocusAction extends AbstractAction
  {

    public RequestFocusAction()
    {
      super("requestFocus");
    }

    public void actionPerformed(ActionEvent event)
    {
      ((JTabbedPane) event.getSource()).requestFocus();
    }

  }

  static class RequestFocusForVisibleComponentAction extends AbstractAction
  {

    public RequestFocusForVisibleComponentAction()
    {
      super("requestFocusForVisibleComponent");
    }

    public void actionPerformed(ActionEvent event)
    {
      JTabbedPane tp = (JTabbedPane) event.getSource();

      // FIXME: This should select a suitable component within
      // the tab content. However I dont know whether we have
      // to search for this component or wether the called is
      // supposed to do that.
      tp.getSelectedComponent().requestFocus();
    }

  }

  /**
   * A helper class that handles focus.
   * <p>The purpose of this class is to implement a more flexible focus
   * handling for the tabbed pane, which is used to determine whether the
   * focus indicator should be painted or not. When in scrolling layout
   * mode the area containing the tabs is a scrollpane, so simply testing
   * whether the tabbed pane has the focus does not work.</p>
   * <p>The <code>FocusHandler</code> is installed on the scrollpane and
   * the tabbed pane and sets the variable <code>hasFocus</code> to
   * <code>false</code> only when both components do not hold the focus.</p>
   *
   * @specnote Apparently this class was intended to be protected,
   *           but was made public by a compiler bug and is now
   *           public for compatibility.
   */
  public class FocusHandler extends FocusAdapter
  {
    /**
     * This method is called when the component gains focus.
     *
     * @param e The FocusEvent.
     */
    public void focusGained(FocusEvent e)
    {
      Object source = e.getSource();
      if (source == panel )
        tabPane.requestFocus();
      else if (source == tabPane)
        tabPane.repaint();
    }

    /**
     * This method is called when the component loses focus.
     *
     * @param e The FocusEvent.
     */
    public void focusLost(FocusEvent e)
    {
      if (e.getOppositeComponent() == tabPane.getSelectedComponent())
        tabPane.requestFocus();
      else if (e.getSource() == tabPane)
        tabPane.repaint();
    }
  }

  /**
   * A helper class for determining if mouse presses occur inside tabs and
   * sets the index appropriately. In SCROLL_TAB_MODE, this class also
   * handles the mouse clicks on the scrolling buttons.
   *
   * @specnote Apparently this class was intended to be protected,
   *           but was made public by a compiler bug and is now
   *           public for compatibility.
   */
  public class MouseHandler extends MouseAdapter
  {
    public void mouseReleased(MouseEvent e)
    {
      Object s = e.getSource();

      // Event may originate from the viewport in
      // SCROLL_TAB_LAYOUT mode. It is redisptached
      // through the tabbed pane then.
      if (tabPane != e.getSource())
        {
          redispatchEvent(e);
          e.setSource(s);
        }
    }

    /**
     * This method is called when the mouse is pressed. The index cannot
     * change to a tab that is  not enabled.
     *
     * @param e The MouseEvent.
     */
    public void mousePressed(MouseEvent e)
    {
      Object s = e.getSource();

      // Event may originate from the viewport in
      // SCROLL_TAB_LAYOUT mode. It is redisptached
      // through the tabbed pane then.
      if (tabPane != e.getSource())
        {
          redispatchEvent(e);
          e.setSource(s);
        }

      int placement = tabPane.getTabPlacement();

      if (s == incrButton)
        {
          if(!incrButton.isEnabled())
            return;

          currentScrollLocation++;

          switch (placement)
            {
              case JTabbedPane.TOP:
              case JTabbedPane.BOTTOM:
                currentScrollOffset = getTabAreaInsets(placement).left;
                for (int i = 0; i < currentScrollLocation; i++)
                  currentScrollOffset += rects[i].width;
                break;
              default:
                currentScrollOffset = getTabAreaInsets(placement).top;
                for (int i = 0; i < currentScrollLocation; i++)
                  currentScrollOffset += rects[i].height;
                break;
            }

          updateViewPosition();
          updateButtons();

          tabPane.repaint();
        }
      else if (s == decrButton)
        {
          if(!decrButton.isEnabled())
            return;

           // The scroll location may be zero but the offset
           // greater than zero because of an adjustement to
           // make a partially visible tab completely visible.
           if (currentScrollLocation > 0)
             currentScrollLocation--;

           // Set the offset back to 0 and recompute it.
           currentScrollOffset = 0;

           switch (placement)
             {
               case JTabbedPane.TOP:
               case JTabbedPane.BOTTOM:
                 // Take the tab area inset into account.
                 if (currentScrollLocation > 0)
                   currentScrollOffset = getTabAreaInsets(placement).left;
                 // Recompute scroll offset.
                 for (int i = 0; i < currentScrollLocation; i++)
                   currentScrollOffset += rects[i].width;
                 break;
               default:
                 // Take the tab area inset into account.
                 if (currentScrollLocation > 0)
                   currentScrollOffset = getTabAreaInsets(placement).top;

                 for (int i = 0; i < currentScrollLocation; i++)
                   currentScrollOffset += rects[i].height;
             }

           updateViewPosition();
           updateButtons();

           tabPane.repaint();
        }
      else if (tabPane.isEnabled())
        {
          int index = tabForCoordinate(tabPane, e.getX(), e.getY());
          if (!tabPane.isEnabledAt(index))
            return;

          if (tabPane.getTabLayoutPolicy() == JTabbedPane.SCROLL_TAB_LAYOUT
              && s == panel)
            {
              scrollTab(index, placement);

              tabPane.setSelectedIndex(index);
              tabPane.repaint();
            }
          else
            {
              tabPane.setSelectedIndex(index);
              tabPane.revalidate();
              tabPane.repaint();
            }

        }

    }

    /**
     * Receives notification when the mouse pointer has entered the tabbed
     * pane.
     *
     * @param e the mouse event
     */
    public void mouseEntered(MouseEvent e)
    {
      Object s = e.getSource();

      // Event may originate from the viewport in
      // SCROLL_TAB_LAYOUT mode. It is redisptached
      // through the tabbed pane then.
      if (tabPane != e.getSource())
        {
          redispatchEvent(e);
          e.setSource(s);
        }

      int tabIndex = tabForCoordinate(tabPane, e.getX(), e.getY());
      setRolloverTab(tabIndex);
    }

    /**
     * Receives notification when the mouse pointer has exited the tabbed
     * pane.
     *
     * @param e the mouse event
     */
    public void mouseExited(MouseEvent e)
    {
      Object s = e.getSource();

      // Event may originate from the viewport in
      // SCROLL_TAB_LAYOUT mode. It is redisptached
      // through the tabbed pane then.
      if (tabPane != e.getSource())
        {
          redispatchEvent(e);
          e.setSource(s);
        }

      setRolloverTab(-1);
    }

    /**
     * Receives notification when the mouse pointer has moved over the tabbed
     * pane.
     *
     * @param ev the mouse event
     */
    public void mouseMoved(MouseEvent ev)
    {
      Object s = ev.getSource();

      if (tabPane != ev.getSource())
        {
          ev.setSource(tabPane);
          tabPane.dispatchEvent(ev);

          ev.setSource(s);
        }

      int tabIndex = tabForCoordinate(tabPane, ev.getX(), ev.getY());
      setRolloverTab(tabIndex);
    }

    /** Modifies the mouse event to originate from
     * the tabbed pane and redispatches it.
     *
     * @param me
     */
    void redispatchEvent(MouseEvent me)
    {
      me.setSource(tabPane);
      Point viewPos = viewport.getViewPosition();
      viewPos.x -= viewport.getX();
      viewPos.y -= viewport.getY();
      me.translatePoint(-viewPos.x, -viewPos.y);
      tabPane.dispatchEvent(me);

      me.translatePoint(viewPos.x, viewPos.y);
    }

  }

  /**
   * This class handles PropertyChangeEvents fired from the JTabbedPane.
   *
   * @specnote Apparently this class was intended to be protected,
   *           but was made public by a compiler bug and is now
   *           public for compatibility.
   */
  public class PropertyChangeHandler implements PropertyChangeListener
  {
    /**
     * This method is called whenever one of the properties of the JTabbedPane
     * changes.
     *
     * @param e The PropertyChangeEvent.
     */
    public void propertyChange(PropertyChangeEvent e)
    {
      out:
        {
          if (e.getPropertyName().equals("tabLayoutPolicy"))
            {
              currentScrollLocation = currentScrollOffset = 0;

              layoutManager = createLayoutManager();

              tabPane.setLayout(layoutManager);
            }
          else if (e.getPropertyName().equals("tabPlacement")
                   && tabPane.getTabLayoutPolicy()
                   == JTabbedPane.SCROLL_TAB_LAYOUT)
            {
              incrButton = createIncreaseButton();
              decrButton = createDecreaseButton();

              // If the tab placement value was changed of a tabbed pane
              // in SCROLL_TAB_LAYOUT mode we investigate the change to
              // implement the following behavior which was observed in
              // the RI:
              // The scrolling offset will be reset if we change to
              // a direction which is orthogonal to the current
              // direction and stays the same if it is parallel.

              int oldPlacement = ((Integer) e.getOldValue()).intValue();
              int newPlacement = ((Integer) e.getNewValue()).intValue();
              switch (newPlacement)
                {
                  case JTabbedPane.TOP:
                  case JTabbedPane.BOTTOM:
                    if (oldPlacement == JTabbedPane.TOP
                        || oldPlacement == JTabbedPane.BOTTOM)
                      break out;

                    currentScrollOffset = getTabAreaInsets(newPlacement).left;
                    break;
                  default:
                    if (oldPlacement == JTabbedPane.LEFT
                       || oldPlacement == JTabbedPane.RIGHT)
                      break out;

                    currentScrollOffset = getTabAreaInsets(newPlacement).top;
                }

              updateViewPosition();
              updateButtons();
            }
        }

      tabPane.revalidate();
      tabPane.repaint();
    }
  }

  /**
   * A LayoutManager responsible for placing all the tabs and the visible
   * component inside the JTabbedPane. This class is only used for
   * WRAP_TAB_LAYOUT.
   *
   * @specnote Apparently this class was intended to be protected,
   *           but was made public by a compiler bug and is now
   *           public for compatibility.
   */
  public class TabbedPaneLayout implements LayoutManager
  {
    /**
     * This method is called when a component is added to the JTabbedPane.
     *
     * @param name The name of the component.
     * @param comp The component being added.
     */
    public void addLayoutComponent(String name, Component comp)
    {
      // Do nothing.
    }

    /**
     * This method is called when the rectangles need to be calculated. It
     * also fixes the size of the visible component.
     */
    public void calculateLayoutInfo()
    {
      int count = tabPane.getTabCount();
      assureRectsCreated(count);
      calculateTabRects(tabPane.getTabPlacement(), count);
      tabRunsDirty = false;
    }

    /**
     * This method calculates the size of the the JTabbedPane.
     *
     * @param minimum Whether the JTabbedPane will try to be as small as it
     *        can.
     *
     * @return The desired size of the JTabbedPane.
     */
    protected Dimension calculateSize(boolean minimum)
    {
      int tabPlacement = tabPane.getTabPlacement();

      int width = 0;
      int height = 0;
      Component c;
      Dimension dims;

      // Find out the minimum/preferred size to display the largest child
      // of the tabbed pane.
      int count = tabPane.getTabCount();
      for (int i = 0; i < count; i++)
        {
          c = tabPane.getComponentAt(i);
          if (c == null)
            continue;
          dims = minimum ? c.getMinimumSize() : c.getPreferredSize();
          if (dims != null)
            {
              height = Math.max(height, dims.height);
              width = Math.max(width, dims.width);
            }
        }

      Insets tabAreaInsets = getTabAreaInsets(tabPlacement);
      if (tabPlacement == SwingConstants.TOP
          || tabPlacement == SwingConstants.BOTTOM)
        {
          width = Math.max(calculateMaxTabWidth(tabPlacement), width);

          height += preferredTabAreaHeight(tabPlacement,
                                           width - tabAreaInsets.left
                                           - tabAreaInsets.right);
        }
      else
        {
          height = Math.max(calculateMaxTabHeight(tabPlacement), height);

          width += preferredTabAreaWidth(tabPlacement,
                                         height - tabAreaInsets.top
                                         - tabAreaInsets.bottom);
        }

      Insets tabPaneInsets = tabPane.getInsets();
      return new Dimension(width + tabPaneInsets.left + tabPaneInsets.right,
                           height + tabPaneInsets.top + tabPaneInsets.bottom);
    }

    // if tab placement is LEFT OR RIGHT, they share width.
    // if tab placement is TOP OR BOTTOM, they share height
    // PRE STEP: finds the default sizes for the labels as well as their
    // locations.
    // AND where they will be placed within the run system.
    // 1. calls normalizeTab Runs.
    // 2. calls rotate tab runs.
    // 3. pads the tab runs.
    // 4. pads the selected tab.

    /**
     * This method is called to calculate the tab rectangles.  This method
     * will calculate the size and position of all  rectangles (taking into
     * account which ones should be in which tab run). It will pad them and
     * normalize them  as necessary.
     *
     * @param tabPlacement The JTabbedPane's tab placement.
     * @param tabCount The run the current selection is in.
     */
    protected void calculateTabRects(int tabPlacement, int tabCount)
    {
      Insets insets = tabPane.getInsets();
      Insets tabAreaInsets = getTabAreaInsets(tabPlacement);
      Dimension size = tabPane.getSize();

      // The coordinates of the upper left corner of the tab area.
      int x;
      int y;
      // The location at which the runs must be broken.
      int breakAt;

      // Calculate the bounds for the tab area.
      switch (tabPlacement)
      {
        case LEFT:
          maxTabWidth = calculateMaxTabWidth(tabPlacement);
          x = insets.left + tabAreaInsets.left;
          y = insets.top + tabAreaInsets.top;
          breakAt = size.height - (insets.bottom + tabAreaInsets.bottom);
          break;
        case RIGHT:
          maxTabWidth = calculateMaxTabWidth(tabPlacement);
          x = size.width - (insets.right + tabAreaInsets.right)
              - maxTabWidth - 1;
          y = insets.top + tabAreaInsets.top;
          breakAt = size.height - (insets.bottom + tabAreaInsets.bottom);
          break;
        case BOTTOM:
          maxTabHeight = calculateMaxTabHeight(tabPlacement);
          x = insets.left + tabAreaInsets.left;
          y = size.height - (insets.bottom + tabAreaInsets.bottom)
              - maxTabHeight - 1;
          breakAt = size.width - (insets.right + tabAreaInsets.right);
          break;
        case TOP:
        default:
          maxTabHeight = calculateMaxTabHeight(tabPlacement);
          x = insets.left + tabAreaInsets.left;
          y = insets.top + tabAreaInsets.top;
          breakAt = size.width - (insets.right + tabAreaInsets.right);
          break;
      }

      if (tabCount == 0)
        return;

      FontMetrics fm = getFontMetrics();
      runCount = 0;
      selectedRun = -1;
      int selectedIndex = tabPane.getSelectedIndex();
      if (selectedIndex < 0)
          selectedIndex = 0;

      Rectangle rect;

      // Go through all the tabs and build the tab runs.
      if (tabPlacement == SwingConstants.TOP
          || tabPlacement == SwingConstants.BOTTOM)
        {
          for (int i = 0; i < tabCount; i++)
            {
              rect = rects[i];
              if (i > 0)
                {
                  rect.x = rects[i - 1].x + rects[i - 1].width;
                }
              else
                {
                  tabRuns[0] = 0;
                  runCount = 1;
                  maxTabWidth = 0;
                  rect.x = x;
                }
              rect.width = calculateTabWidth(tabPlacement, i, fm);
              maxTabWidth = Math.max(maxTabWidth, rect.width);

              if (rect.x != 2 + insets.left && rect.x + rect.width > breakAt)
                {
                  if (runCount > tabRuns.length - 1)
                    expandTabRunsArray();
                  tabRuns[runCount] = i;
                  runCount++;
                  rect.x = x;
                }

              rect.y = y;
              rect.height = maxTabHeight;
              if (i == selectedIndex)
                selectedRun = runCount - 1;
            }
        }
      else
        {
          for (int i = 0; i < tabCount; i++)
            {
              rect = rects[i];
              if (i > 0)
                {
                  rect.y = rects[i - 1].y + rects[i - 1].height;
                }
              else
                {
                  tabRuns[0] = 0;
                  runCount = 1;
                  maxTabHeight = 0;
                  rect.y = y;
                }
              rect.height = calculateTabHeight(tabPlacement, i,
                                               fm.getHeight());
              maxTabHeight = Math.max(maxTabHeight, rect.height);

              if (rect.y != 2 + insets.top && rect.y + rect.height > breakAt)
                {
                  if (runCount > tabRuns.length - 1)
                    expandTabRunsArray();
                  tabRuns[runCount] = i;
                  runCount++;
                  rect.y = y;
                }

              rect.x = x;
              rect.width = maxTabWidth;

              if (i == selectedIndex)
                selectedRun = runCount - 1;
            }
        }

      if (runCount > 1)
        {
          int start;
          if  (tabPlacement == SwingConstants.TOP
              || tabPlacement == SwingConstants.BOTTOM)
            start = x;
          else
            start = y;
          normalizeTabRuns(tabPlacement, tabCount, start, breakAt);
          selectedRun = getRunForTab(tabCount, selectedIndex);
          if (shouldRotateTabRuns(tabPlacement))
            {
              rotateTabRuns(tabPlacement, selectedRun);
            }
        }

      // Suppress padding if we have only one tab run.
      if (runCount == 1)
        return;

      // Pad the runs.
      int tabRunOverlay = getTabRunOverlay(tabPlacement);
      for (int i = runCount - 1; i >= 0; --i)
        {
          int start = tabRuns[i];
          int nextIndex;
          if (i == runCount - 1)
            nextIndex = 0;
          else
            nextIndex = i + 1;
          int next = tabRuns[nextIndex];
          int end = next != 0 ? next - 1 : tabCount - 1;
          if (tabPlacement == SwingConstants.TOP
              || tabPlacement == SwingConstants.BOTTOM)
            {
              for (int j = start; j <= end; ++j)
                {
                  rect = rects[j];
                  rect.y = y;
                  rect.x += getTabRunIndent(tabPlacement, i);
                }
              if (shouldPadTabRun(tabPlacement, i))
                {
                  padTabRun(tabPlacement, start, end, breakAt);
                }
              if (tabPlacement == BOTTOM)
                y -= maxTabHeight - tabRunOverlay;
              else
                y += maxTabHeight - tabRunOverlay;
            }
          else
            {
              for (int j = start; j <= end; ++j)
                {
                  rect = rects[j];
                  rect.x = x;
                  rect.y += getTabRunIndent(tabPlacement, i);
                }
              if (shouldPadTabRun(tabPlacement, i))
                {
                  padTabRun(tabPlacement, start, end, breakAt);
                }
              if (tabPlacement == RIGHT)
                x -= maxTabWidth - tabRunOverlay;
              else
                x += maxTabWidth - tabRunOverlay;

            }
        }
      padSelectedTab(tabPlacement, selectedIndex);
    }

    /**
     * This method is called when the JTabbedPane is laid out in
     * WRAP_TAB_LAYOUT. It calls calculateLayoutInfo to  find the positions
     * of all its components.
     *
     * @param parent The Container to lay out.
     */
    public void layoutContainer(Container parent)
    {
      calculateLayoutInfo();

      int tabPlacement = tabPane.getTabPlacement();
      Insets insets = tabPane.getInsets();

      int selectedIndex = tabPane.getSelectedIndex();

      Component selectedComponent = null;
      if (selectedIndex >= 0)
        selectedComponent = tabPane.getComponentAt(selectedIndex);
      // The RI doesn't seem to change the component if the new selected
      // component == null. This is probably so that applications can add
      // one single component for every tab.
      if (selectedComponent != null)
        {
          setVisibleComponent(selectedComponent);
        }

      int childCount = tabPane.getComponentCount();
      if (childCount > 0)
        {
          int compX;
          int compY;
          int tabAreaWidth = 0;
          int tabAreaHeight = 0;
          switch (tabPlacement)
          {
            case LEFT:
              tabAreaWidth = calculateTabAreaWidth(tabPlacement, runCount,
                                                   maxTabWidth);
              compX = tabAreaWidth + insets.left + contentBorderInsets.left;
              compY = insets.top + contentBorderInsets.top;
              break;
            case RIGHT:
              tabAreaWidth = calculateTabAreaWidth(tabPlacement, runCount,
                                                   maxTabWidth);
              compX = insets.left + contentBorderInsets.left;
              compY = insets.top + contentBorderInsets.top;
              break;
            case BOTTOM:
              tabAreaHeight = calculateTabAreaHeight(tabPlacement, runCount,
                                                     maxTabHeight);
              compX = insets.left + contentBorderInsets.left;
              compY = insets.top + contentBorderInsets.top;
              break;
            case TOP:
            default:
              tabAreaHeight = calculateTabAreaHeight(tabPlacement, runCount,
                                                     maxTabHeight);

              compX = insets.left + contentBorderInsets.left;
              compY = tabAreaHeight + insets.top + contentBorderInsets.top;
          }
          Rectangle bounds = tabPane.getBounds();
          int compWidth = bounds.width - tabAreaWidth - insets.left
                          - insets.right - contentBorderInsets.left
                          - contentBorderInsets.right;
          int compHeight = bounds.height - tabAreaHeight - insets.top
                           - insets.bottom - contentBorderInsets.top
                           - contentBorderInsets.bottom;


          for (int i = 0; i < childCount; ++i)
            {
              Component c = tabPane.getComponent(i);
              c.setBounds(compX, compY, compWidth, compHeight);
            }
        }
    }

    /**
     * This method returns the minimum layout size for the given container.
     *
     * @param parent The container that is being sized.
     *
     * @return The minimum size.
     */
    public Dimension minimumLayoutSize(Container parent)
    {
      return calculateSize(true);
    }

    // If there is more free space in an adjacent run AND the tab
    // in the run can fit in the adjacent run, move it. This method
    // is not perfect, it is merely an approximation.
    // If you play around with Sun's JTabbedPane, you'll see that
    // it does do some pretty strange things with regards to not moving tabs
    // that should be moved.
    // start = the x position where the tabs will begin
    // max = the maximum position of where the tabs can go to
    // (tabAreaInsets.left + the width of the tab area)

    /**
     * This method tries to "even out" the number of tabs in each run based on
     * their widths.
     *
     * @param tabPlacement The JTabbedPane's tab placement.
     * @param tabCount The number of tabs.
     * @param start The x position where the tabs will begin.
     * @param max The maximum x position where the tab can run to.
     */
    protected void normalizeTabRuns(int tabPlacement, int tabCount, int start,
                                    int max)
    {
      boolean horizontal = tabPlacement == TOP || tabPlacement == BOTTOM;
      int currentRun = runCount - 1;
      double weight = 1.25;
      for (boolean adjust = true; adjust == true;)
        {
          int last = lastTabInRun(tabCount, currentRun);
          int prevLast = lastTabInRun(tabCount, currentRun - 1);
          int end;
          int prevLength;
          if (horizontal)
            {
              end = rects[last].x + rects[last].width;
              prevLength = (int) (maxTabWidth * weight);
            }
          else
            {
              end = rects[last].y + rects[last].height;
              prevLength = (int) (maxTabWidth * weight * 2);
            }
          if (max - end > prevLength)
            {
              tabRuns[currentRun] = prevLast;
              if (horizontal)
                rects[prevLast].x = start;
              else
                rects[prevLast].y = start;
              for (int i = prevLast + 1; i <= last; i++)
                {
                  if (horizontal)
                    rects[i].x = rects[i - 1].x + rects[i - 1].width;
                  else
                    rects[i].y = rects[i - 1].y + rects[i - 1].height;
                }
            }
          else if (currentRun == runCount - 1)
            adjust = false;
          if (currentRun - 1 > 0)
            currentRun -= 1;
          else
            {
              // Check again, but with higher ratio to avoid
              // clogging up the last run.
              currentRun = runCount - 1;
              weight += 0.25;
            }
        }
    }

    /**
     * This method pads the tab at the selected index by the  selected tab pad
     * insets (so that it looks larger).
     *
     * @param tabPlacement The placement of the tabs.
     * @param selectedIndex The selected index.
     */
    protected void padSelectedTab(int tabPlacement, int selectedIndex)
    {
      Insets insets = getSelectedTabPadInsets(tabPlacement);
      rects[selectedIndex].x -= insets.left;
      rects[selectedIndex].y -= insets.top;
      rects[selectedIndex].width += insets.left + insets.right;
      rects[selectedIndex].height += insets.top + insets.bottom;
    }

    // If the tabs on the run don't fill the width of the window, make it
    // fit now.
    // start = starting index of the run
    // end = last index of the run
    // max = tabAreaInsets.left + width (or equivalent)
    // assert start <= end.

    /**
     * This method makes each tab in the run larger so that the  tabs expand
     * to fill the runs width/height (depending on tabPlacement).
     *
     * @param tabPlacement The placement of the tabs.
     * @param start The index of the first tab.
     * @param end The last index of the tab
     * @param max The amount of space in the run (width for TOP and BOTTOM
     *        tabPlacement).
     */
    protected void padTabRun(int tabPlacement, int start, int end, int max)
    {
      if (tabPlacement == SwingConstants.TOP
          || tabPlacement == SwingConstants.BOTTOM)
        {
          int runWidth = rects[end].x + rects[end].width;
          int spaceRemaining = max - runWidth;
          int numTabs = end - start + 1;

          // now divvy up the space.
          int spaceAllocated = spaceRemaining / numTabs;
          int currX = rects[start].x;
          for (int i = start; i <= end; i++)
            {
              rects[i].x = currX;
              rects[i].width += spaceAllocated;

              currX += rects[i].width;
              // This is used because since the spaceAllocated
              // variable is an int, it rounds down. Sometimes,
              // we don't fill an entire row, so we make it do
              // so now.

              if (i == end && rects[i].x + rects[i].width != max)
                rects[i].width = max - rects[i].x;
            }
        }
      else
        {
          int runHeight = rects[end].y + rects[end].height;
          int spaceRemaining = max - runHeight;
          int numTabs = end - start + 1;

          int spaceAllocated = spaceRemaining / numTabs;
          int currY = rects[start].y;
          for (int i = start; i <= end; i++)
            {
              rects[i].y = currY;
              rects[i].height += spaceAllocated;
              currY += rects[i].height;
              if (i == end && rects[i].y + rects[i].height != max)
                rects[i].height = max - rects[i].y;
            }
        }
    }

    /**
     * This method returns the preferred layout size for the given container.
     *
     * @param parent The container to size.
     *
     * @return The preferred layout size.
     */
    public Dimension preferredLayoutSize(Container parent)
    {
      return calculateSize(false);
    }

    /**
     * This method returns the preferred tab height given a tabPlacement and
     * width.
     *
     * @param tabPlacement The JTabbedPane's tab placement.
     * @param width The expected width.
     *
     * @return The preferred tab area height.
     */
    protected int preferredTabAreaHeight(int tabPlacement, int width)
    {
      if (tabPane.getTabCount() == 0)
        return calculateTabAreaHeight(tabPlacement, 0, 0);

      int runs = 0;
      int runWidth = 0;
      int tabWidth = 0;

      FontMetrics fm = getFontMetrics();

      Insets tabAreaInsets = getTabAreaInsets(tabPlacement);
      Insets insets = tabPane.getInsets();

      // Only interested in width, this is a messed up rectangle now.
      width -= tabAreaInsets.left + tabAreaInsets.right + insets.left
      + insets.right;

      // The reason why we can't use runCount:
      // This method is only called to calculate the size request
      // for the tabbedPane. However, this size request is dependent on
      // our desired width. We need to find out what the height would
      // be IF we got our desired width.
      for (int i = 0; i < tabPane.getTabCount(); i++)
        {
          tabWidth = calculateTabWidth(tabPlacement, i, fm);
          if (runWidth + tabWidth > width)
            {
              runWidth = tabWidth;
              runs++;
            }
          else
            runWidth += tabWidth;
        }
      runs++;

      int maxTabHeight = calculateMaxTabHeight(tabPlacement);
      int tabAreaHeight = calculateTabAreaHeight(tabPlacement, runs,
                                                 maxTabHeight);
      return tabAreaHeight;
    }

    /**
     * This method calculates the preferred tab area width given a tab
     * placement and height.
     *
     * @param tabPlacement The JTabbedPane's tab placement.
     * @param height The expected height.
     *
     * @return The preferred tab area width.
     */
    protected int preferredTabAreaWidth(int tabPlacement, int height)
    {
      if (tabPane.getTabCount() == 0)
        return calculateTabAreaHeight(tabPlacement, 0, 0);

      int runs = 0;
      int runHeight = 0;
      int tabHeight = 0;

      FontMetrics fm = getFontMetrics();

      Insets tabAreaInsets = getTabAreaInsets(tabPlacement);
      Insets insets = tabPane.getInsets();

      height -= tabAreaInsets.top + tabAreaInsets.bottom + insets.top
      + insets.bottom;
      int fontHeight = fm.getHeight();

      for (int i = 0; i < tabPane.getTabCount(); i++)
        {
          tabHeight = calculateTabHeight(tabPlacement, i, fontHeight);
          if (runHeight + tabHeight > height)
            {
              runHeight = tabHeight;
              runs++;
            }
          else
            runHeight += tabHeight;
        }
      runs++;

      int maxTabWidth = calculateMaxTabWidth(tabPlacement);
      int tabAreaWidth = calculateTabAreaWidth(tabPlacement, runs,
                                               maxTabWidth);
      return tabAreaWidth;
    }

    /**
     * This method rotates the places each run in the correct place  the
     * tabRuns array. See the comment for tabRuns for how the runs are placed
     * in the array.
     *
     * @param tabPlacement The JTabbedPane's tab placement.
     * @param selectedRun The run the current selection is in.
     */
    protected void rotateTabRuns(int tabPlacement, int selectedRun)
    {
      if (runCount == 1 || selectedRun == 0 || selectedRun == -1)
        return;
      int[] newTabRuns = new int[tabRuns.length];
      int currentRun = selectedRun;
      int i = 0;
      do
        {
          newTabRuns[i] = tabRuns[currentRun];
          currentRun = getNextTabRun(currentRun);
          i++;
        }
      while (i < runCount);

      tabRuns = newTabRuns;
      BasicTabbedPaneUI.this.selectedRun = 1;
    }

    /**
     * This method is called when a component is removed  from the
     * JTabbedPane.
     *
     * @param comp The component removed.
     */
    public void removeLayoutComponent(Component comp)
    {
      // Do nothing.
    }
  }

  /**
   * This class acts as the LayoutManager for the JTabbedPane in
   * SCROLL_TAB_MODE.
   */
  private class TabbedPaneScrollLayout extends TabbedPaneLayout
  {
    /**
     * This method returns the preferred layout size for the given container.
     *
     * @param parent The container to calculate a size for.
     *
     * @return The preferred layout size.
     */
    public Dimension preferredLayoutSize(Container parent)
    {
      return super.calculateSize(false);
    }

    /**
     * This method returns the minimum layout size for the given container.
     *
     * @param parent The container to calculate a size for.
     *
     * @return The minimum layout size.
     */
    public Dimension minimumLayoutSize(Container parent)
    {
      return super.calculateSize(true);
    }

    /**
     * This method calculates the tab area height given  a desired width.
     *
     * @param tabPlacement The JTabbedPane's tab placement.
     * @param width The expected width.
     *
     * @return The tab area height given the width.
     */
    protected int preferredTabAreaHeight(int tabPlacement, int width)
    {
      if (tabPane.getTabCount() == 0)
        return calculateTabAreaHeight(tabPlacement, 0, 0);

      int runs = 1;

      int maxTabHeight = calculateMaxTabHeight(tabPlacement);
      int tabAreaHeight = calculateTabAreaHeight(tabPlacement, runs,
                                                 maxTabHeight);
      return tabAreaHeight;
    }

    /**
     * This method calculates the tab area width given a desired height.
     *
     * @param tabPlacement The JTabbedPane's tab placement.
     * @param height The expected height.
     *
     * @return The tab area width given the height.
     */
    protected int preferredTabAreaWidth(int tabPlacement, int height)
    {
      if (tabPane.getTabCount() == 0)
        return calculateTabAreaHeight(tabPlacement, 0, 0);

      int runs = 1;

      int maxTabWidth = calculateMaxTabWidth(tabPlacement);
      int tabAreaWidth = calculateTabAreaWidth(tabPlacement, runs, maxTabWidth);
      return tabAreaWidth;
    }

    /**
     * This method is called to calculate the tab rectangles.  This method
     * will calculate the size and position of all  rectangles (taking into
     * account which ones should be in which tab run). It will pad them and
     * normalize them  as necessary.
     *
     * @param tabPlacement The JTabbedPane's tab placement.
     * @param tabCount The number of tabs.
     */
    protected void calculateTabRects(int tabPlacement, int tabCount)
    {
      if (tabCount == 0)
        return;

      FontMetrics fm = getFontMetrics();
      SwingUtilities.calculateInnerArea(tabPane, calcRect);
      Insets tabAreaInsets = getTabAreaInsets(tabPlacement);
      Insets insets = tabPane.getInsets();
      if (tabPlacement == SwingConstants.TOP
          || tabPlacement == SwingConstants.BOTTOM)
        {
          int maxHeight = calculateMaxTabHeight(tabPlacement);
          calcRect.width -= tabAreaInsets.left + tabAreaInsets.right;
          int width = 0;
          int runWidth = tabAreaInsets.left + insets.left;
          int top = insets.top + tabAreaInsets.top;
          for (int i = 0; i < tabCount; i++)
            {
              width = calculateTabWidth(tabPlacement, i, fm);

              // The proper instances should exists because
              //  assureRectsCreated() was being run already.
              rects[i].setBounds(runWidth, top, width, maxHeight);

              runWidth += width;
            }
          tabAreaRect.width = tabPane.getWidth() - insets.left - insets.right;
          tabAreaRect.height = maxTabHeight + tabAreaInsets.top
                               + tabAreaInsets.bottom;
          contentRect.width = tabAreaRect.width;
          contentRect.height = tabPane.getHeight() - insets.top
          - insets.bottom - tabAreaRect.height;
          contentRect.x = insets.left;
          tabAreaRect.x = insets.left;
          if (tabPlacement == SwingConstants.BOTTOM)
            {
              contentRect.y = insets.top;
              tabAreaRect.y = contentRect.y + contentRect.height;
            }
          else
            {
              tabAreaRect.y = insets.top;
              contentRect.y = tabAreaRect.y + tabAreaRect.height;
            }
        }
      else
        {
          int maxWidth = calculateMaxTabWidth(tabPlacement);

          calcRect.height -= tabAreaInsets.top + tabAreaInsets.bottom;
          int height = 0;
          int runHeight = tabAreaInsets.top + insets.top;
          int fontHeight = fm.getHeight();
          int left = insets.left + tabAreaInsets.left;
          for (int i = 0; i < tabCount; i++)
            {
              height = calculateTabHeight(tabPlacement, i, fontHeight);

              // The proper instances should exists because
              //  assureRectsCreated() was being run already.
              rects[i].setBounds(left, runHeight, maxWidth, height);
              runHeight += height;
            }
          tabAreaRect.width = maxTabWidth + tabAreaInsets.left
                              + tabAreaInsets.right;
          tabAreaRect.height = tabPane.getHeight() - insets.top
                               - insets.bottom;
          tabAreaRect.y = insets.top;
          contentRect.width = tabPane.getWidth() - insets.left - insets.right
                              - tabAreaRect.width;
          contentRect.height = tabAreaRect.height;
          contentRect.y = insets.top;
          if (tabPlacement == SwingConstants.LEFT)
            {
              tabAreaRect.x = insets.left;
              contentRect.x = tabAreaRect.x + tabAreaRect.width;
            }
          else
            {
              contentRect.x = insets.left;
              tabAreaRect.x = contentRect.x + contentRect.width;
            }
        }

      // Unlike the behavior in the WRAP_TAB_LAYOUT the selected
      // tab is not padded specially.
    }

    /**
     * This method is called when the JTabbedPane is laid out in
     * SCROLL_TAB_LAYOUT. It finds the position for all components in the
     * JTabbedPane.
     *
     * @param pane The JTabbedPane to be laid out.
     */
    public void layoutContainer(Container pane)
    {
      super.layoutContainer(pane);
      int tabCount = tabPane.getTabCount();
      if (tabCount == 0)
        return;
      int tabPlacement = tabPane.getTabPlacement();

      if (tabPlacement == SwingConstants.TOP
          || tabPlacement == SwingConstants.BOTTOM)
        {
          if (tabAreaRect.x + tabAreaRect.width < rects[tabCount - 1].x
              + rects[tabCount - 1].width)
            {
              Dimension incrDims = incrButton.getPreferredSize();
              Dimension decrDims = decrButton.getPreferredSize();

              if (tabPlacement == SwingConstants.BOTTOM)
                {
                  // Align scroll buttons with the bottom border of the tabbed
                  // pane's content area.
                  decrButton.setBounds(tabAreaRect.x + tabAreaRect.width
                                       - incrDims.width - decrDims.width,
                                       tabAreaRect.y, decrDims.width,
                                       decrDims.height);
                  incrButton.setBounds(tabAreaRect.x + tabAreaRect.width
                                       - incrDims.width, tabAreaRect.y,
                                       incrDims.width, incrDims.height);
                }
              else
                {
                  // Align scroll buttons with the top border of the tabbed
                  // pane's content area.
                  decrButton.setBounds(tabAreaRect.x + tabAreaRect.width
                                       - incrDims.width - decrDims.width,
                                       tabAreaRect.y + tabAreaRect.height
                                       - decrDims.height, decrDims.width,
                                       decrDims.height);
                  incrButton.setBounds(tabAreaRect.x + tabAreaRect.width
                                       - incrDims.width,
                                       tabAreaRect.y + tabAreaRect.height
                                       - incrDims.height,
                                       incrDims.width, incrDims.height);
                }

              tabAreaRect.width -= decrDims.width + incrDims.width;

              updateButtons();

              incrButton.setVisible(true);
              decrButton.setVisible(true);
            }
          else
            {
              incrButton.setVisible(false);
              decrButton.setVisible(false);

              currentScrollOffset = 0;
              currentScrollLocation = 0;
            }
        }

      if (tabPlacement == SwingConstants.LEFT
          || tabPlacement == SwingConstants.RIGHT)
        {
          if (tabAreaRect.y + tabAreaRect.height < rects[tabCount - 1].y
              + rects[tabCount - 1].height)
            {
              Dimension incrDims = incrButton.getPreferredSize();
              Dimension decrDims = decrButton.getPreferredSize();

              if (tabPlacement == SwingConstants.RIGHT)
                {
                  // Align scroll buttons with the right border of the tabbed
                  // pane's content area.
                  decrButton.setBounds(tabAreaRect.x,
                                       tabAreaRect.y + tabAreaRect.height
                                       - incrDims.height - decrDims.height,
                                       decrDims.width, decrDims.height);
                  incrButton.setBounds(tabAreaRect.x,
                                       tabAreaRect.y + tabAreaRect.height
                                       - incrDims.height, incrDims.width,
                                       incrDims.height);
                }
              else
                {
                  // Align scroll buttons with the left border of the tabbed
                  // pane's content area.
                  decrButton.setBounds(tabAreaRect.x + tabAreaRect.width
                                       - decrDims.width,
                                       tabAreaRect.y + tabAreaRect.height
                                       - incrDims.height - decrDims.height,
                                       decrDims.width, decrDims.height);
                  incrButton.setBounds(tabAreaRect.x + tabAreaRect.width
                                       - incrDims.width,
                                       tabAreaRect.y + tabAreaRect.height
                                       - incrDims.height, incrDims.width,
                                       incrDims.height);
                }

              tabAreaRect.height -= decrDims.height + incrDims.height;

              incrButton.setVisible(true);
              decrButton.setVisible(true);
            }
          else
            {
              incrButton.setVisible(false);
              decrButton.setVisible(false);

              currentScrollOffset = 0;
              currentScrollLocation = 0;
            }
        }
      viewport.setBounds(tabAreaRect.x, tabAreaRect.y, tabAreaRect.width,
                         tabAreaRect.height);

      updateViewPosition();

      viewport.repaint();
    }
  }

  /**
   * This class handles ChangeEvents from the JTabbedPane.
   *
   * @specnote Apparently this class was intended to be protected,
   *           but was made public by a compiler bug and is now
   *           public for compatibility.
   */
  public class TabSelectionHandler implements ChangeListener
  {
    /**
     * This method is called whenever a ChangeEvent is fired from the
     * JTabbedPane.
     *
     * @param e The ChangeEvent fired.
     */
    public void stateChanged(ChangeEvent e)
    {
      selectedRun = getRunForTab(tabPane.getTabCount(),
                                 tabPane.getSelectedIndex());

      if (tabPane.getTabLayoutPolicy() == JTabbedPane.WRAP_TAB_LAYOUT)
        tabPane.revalidate();
      tabPane.repaint();
    }
  }

  /**
   * This helper class is a JPanel that fits inside the ScrollViewport. This
   * panel's sole job is to paint the tab rectangles inside the  viewport so
   * that it's clipped correctly.
   */
  private class ScrollingPanel extends JPanel
  {
    /**
     * This is a private UI class for our panel.
     */
    private class ScrollingPanelUI extends BasicPanelUI
    {
      /**
       * This method overrides the default paint method. It paints the tab
       * rectangles for the JTabbedPane in the panel.
       *
       * @param g The Graphics object to paint with.
       * @param c The JComponent to paint.
       */
      public void paint(Graphics g, JComponent c)
      {
        int placement = tabPane.getTabPlacement();
        g.setColor(highlight);
        if (placement == SwingUtilities.TOP
            || placement == SwingUtilities.BOTTOM)
          g.fillRect(currentScrollOffset, 0,
                     tabAreaRect.width, tabAreaRect.height);
        else
          g.fillRect(0, currentScrollOffset,
                     tabAreaRect.width, tabAreaRect.height);

        paintTabArea(g, placement, tabPane.getSelectedIndex());
      }
    }

    /**
     * This method overrides the updateUI method. It makes the default UI for
     * this ScrollingPanel to be  a ScrollingPanelUI.
     */
    public void updateUI()
    {
      setUI(new ScrollingPanelUI());
    }
  }

  /**
   * This is a helper class that paints the panel that paints tabs. This
   * custom JViewport is used so that the tabs painted in the panel will be
   * clipped. This class implements UIResource so tabs are not added when
   * this objects of this class are added to the  JTabbedPane.
   */
  private class ScrollingViewport extends JViewport implements UIResource
  {
    // TODO: Maybe remove this inner class.
  }

  /**
   * This is a helper class that implements UIResource so it is not added as a
   * tab when an object of this class is added to the JTabbedPane.
   */
  private class ScrollingButton extends BasicArrowButton implements UIResource
  {
    /**
     * Creates a ScrollingButton given the direction.
     *
     * @param dir The direction to point in.
     */
    public ScrollingButton(int dir)
    {
      super(dir);
    }
  }

  /** The button that increments the current scroll location.
   * This is package-private to avoid an accessor method.  */
  transient ScrollingButton incrButton;

  /** The button that decrements the current scroll location.
   * This is package-private to avoid an accessor method.  */
  transient ScrollingButton decrButton;

  /** The viewport used to display the tabs.
   * This is package-private to avoid an accessor method.  */
  transient ScrollingViewport viewport;

  /** The panel inside the viewport that paints the tabs.
   * This is package-private to avoid an accessor method.  */
  transient ScrollingPanel panel;

  /** The starting visible tab in the run in SCROLL_TAB_MODE.
   * This is package-private to avoid an accessor method.  */
  transient int currentScrollLocation;

  transient int currentScrollOffset;

  /** A reusable rectangle. */
  protected Rectangle calcRect;

  /** An array of Rectangles keeping track of the tabs' area and position. */
  protected Rectangle[] rects;

  /** The insets around the content area. */
  protected Insets contentBorderInsets;

  /** The extra insets around the selected tab. */
  protected Insets selectedTabPadInsets;

  /** The insets around the tab area. */
  protected Insets tabAreaInsets;

  /** The insets around each and every tab. */
  protected Insets tabInsets;

  /**
   * The outer bottom and right edge color for both the tab and content
   * border.
   */
  protected Color darkShadow;

  /** The color of the focus outline on the selected tab. */
  protected Color focus;

  /** FIXME: find a use for this. */
  protected Color highlight;

  /** The top and left edge color for both the tab and content border. */
  protected Color lightHighlight;

  /** The inner bottom and right edge color for the tab and content border. */
  protected Color shadow;

  /** The maximum tab height. */
  protected int maxTabHeight;

  /** The maximum tab width. */
  protected int maxTabWidth;

  /** The number of runs in the JTabbedPane. */
  protected int runCount;

  /** The index of the run that the selected index is in. */
  protected int selectedRun;

  /** The amount of space each run overlaps the previous by. */
  protected int tabRunOverlay;

  /** The gap between text and label */
  protected int textIconGap;

  /** This array keeps track of which tabs are in which run.
   * <p>The value at index i denotes the index of the first tab in run i.</p>
   * <p>If the value for any index (i > 0) is 0 then (i - 1) is the last
   * run.</p>
   */
  protected int[] tabRuns;

  /**
   * Indicates if the layout of the tab runs is ok or not. This is package
   * private to avoid a synthetic accessor method.
   */
  boolean tabRunsDirty;

  /**
   * This is the keystroke for moving down.
   *
   * @deprecated 1.3
   */
  protected KeyStroke downKey;

  /**
   * This is the keystroke for moving left.
   *
   * @deprecated 1.3
   */
  protected KeyStroke leftKey;

  /**
   * This is the keystroke for moving right.
   *
   * @deprecated 1.3
   */
  protected KeyStroke rightKey;

  /**
   * This is the keystroke for moving up.
   *
   * @deprecated 1.3
   */
  protected KeyStroke upKey;

  /** The listener that listens for focus events. */
  protected FocusListener focusListener;

  /** The listener that listens for mouse events. */
  protected MouseListener mouseListener;

  /** The listener that listens for property change events. */
  protected PropertyChangeListener propertyChangeListener;

  /** The listener that listens for change events. */
  protected ChangeListener tabChangeListener;

  /** The tab pane that this UI paints. */
  protected JTabbedPane tabPane;

  /** The current layout manager for the tabPane.
   * This is package-private to avoid an accessor method.  */
  transient LayoutManager layoutManager;

  /** The rectangle that describes the tab area's position and size.
   * This is package-private to avoid an accessor method.  */
  transient Rectangle tabAreaRect;

  /** The rectangle that describes the content area's position and
   * size.  This is package-private to avoid an accessor method.  */
  transient Rectangle contentRect;

  /**
   * The index over which the mouse is currently moving.
   */
  private int rolloverTab;

  /**
   * Determines if tabs are painted opaque or not. This can be adjusted using
   * the UIManager property 'TabbedPane.tabsOpaque'.
   */
  private boolean tabsOpaque;

  /**
   * The currently visible component.
   */
  private Component visibleComponent;

  private Color selectedColor;

  private Rectangle tempTextRect = new Rectangle();

  private Rectangle tempIconRect = new Rectangle();

  /**
   * Creates a new BasicTabbedPaneUI object.
   */
  public BasicTabbedPaneUI()
  {
    super();
    rects = new Rectangle[0];
    tabRuns = new int[10];
  }

  /**
   * This method creates a ScrollingButton that  points in the appropriate
   * direction for an increasing button.
   * This is package-private to avoid an accessor method.
   *
   * @return The increase ScrollingButton.
   */
  ScrollingButton createIncreaseButton()
  {
    if (incrButton == null)
      incrButton = new ScrollingButton(SwingConstants.NORTH);
    if (tabPane.getTabPlacement() == SwingConstants.TOP
        || tabPane.getTabPlacement() == SwingConstants.BOTTOM)
      incrButton.setDirection(SwingConstants.EAST);
    else
      incrButton.setDirection(SwingConstants.SOUTH);
    return incrButton;
  }

  /**
   * This method creates a ScrollingButton that points in the appropriate
   * direction for a decreasing button.
   * This is package-private to avoid an accessor method.
   *
   * @return The decrease ScrollingButton.
   */
  ScrollingButton createDecreaseButton()
  {
    if (decrButton == null)
      decrButton = new ScrollingButton(SwingConstants.SOUTH);
    if (tabPane.getTabPlacement() == SwingConstants.TOP
        || tabPane.getTabPlacement() == SwingConstants.BOTTOM)
      decrButton.setDirection(SwingConstants.WEST);
    else
      decrButton.setDirection(SwingConstants.NORTH);
    return decrButton;
  }

  /**
   * This method finds the point to set the view  position at given the index
   * of a tab. The tab will be the first visible tab in the run.
   * This is package-private to avoid an accessor method.
   *
   * @param index The index of the first visible tab.
   *
   * @return The position of the first visible tab.
   */
  Point findPointForIndex(int index)
  {
    int tabPlacement = tabPane.getTabPlacement();
    int selectedIndex = tabPane.getSelectedIndex();
    Insets insets = getSelectedTabPadInsets(tabPlacement);
    int w = 0;
    int h = 0;

    if (tabPlacement == TOP || tabPlacement == BOTTOM)
      {
        if (index > 0)
          {
            w += rects[index - 1].x + rects[index - 1].width;
            if (index > selectedIndex)
              w -= insets.left + insets.right;
          }
      }

    else
      {
        if (index > 0)
          {
            h += rects[index - 1].y + rects[index - 1].height;
            if (index > selectedIndex)
              h -= insets.top + insets.bottom;
          }
      }

    Point p = new Point(w, h);
    return p;
  }

  /** TabbedPanes in scrolling mode should use this method to
   * scroll properly to the tab given by the index argument.
   *
   * @param index The tab to scroll to.
   * @param placement The tab's placement.
   */
  final void scrollTab(int index, int placement)
  {
    int diff;
    if (index >= 0 && tabPane.isEnabledAt(index))
      {
        // If the user clicked on the last tab and that one was
        // only partially visible shift the scroll offset to make
        // it completely visible.
        switch (placement)
          {
            case JTabbedPane.TOP:
            case JTabbedPane.BOTTOM:
              if ((diff = rects[index].x
                  + rects[index].width
                  - decrButton.getX() - currentScrollOffset) > 0)
                currentScrollOffset += diff;
              else if ((diff = rects[index].x - currentScrollOffset) < 0)
                {
                  if (index == 0)
                    currentScrollOffset = 0;
                  else
                    currentScrollOffset += diff;
                }

              currentScrollLocation = tabForCoordinate(tabPane,
                                                       currentScrollOffset,
                                                       rects[index].y);
              break;
            default:
              if ((diff = rects[index].y + rects[index].height
                  - decrButton.getY() - currentScrollOffset) > 0)
                currentScrollOffset += diff;
              else if ((diff = rects[index].y - currentScrollOffset) < 0)
                {
                  if (index == 0)
                    currentScrollOffset = 0;
                  else
                    currentScrollOffset += diff;
                }

              currentScrollLocation = tabForCoordinate(tabPane,
                                                       rects[index].x,
                                                       currentScrollOffset);
          }

        updateViewPosition();
        updateButtons();
      }
  }

  /** Sets the enabled state of the increase and decrease button
   * according to the current scrolling offset and tab pane width
   * (or height in TOP/BOTTOM placement).
   */
  final void updateButtons()
  {
    int tc = tabPane.getTabCount();

    // The increase button should be enabled as long as the
    // right/bottom border of the last tab is under the left/top
    // border of the decrease button.
    switch (tabPane.getTabPlacement())
    {
      case JTabbedPane.BOTTOM:
      case JTabbedPane.TOP:
        incrButton.setEnabled(currentScrollLocation + 1 < tc
                              && rects[tc-1].x + rects[tc-1].width
                              - currentScrollOffset > decrButton.getX());
        break;
      default:
        incrButton.setEnabled(currentScrollLocation + 1 < tc
                              && rects[tc-1].y + rects[tc-1].height
                              - currentScrollOffset > decrButton.getY());
    }

    // The decrease button is enabled when the tab pane is scrolled in any way.
    decrButton.setEnabled(currentScrollOffset > 0);

  }

  /**
   * Updates the position of the scrolling viewport's view
   * according to the current scroll offset.
   */
  final void updateViewPosition()
  {
    Point p = viewport.getViewPosition();

    // The unneeded coordinate must be set to zero
    // in order to correctly handle placement changes.
    switch (tabPane.getTabPlacement())
    {
      case JTabbedPane.LEFT:
      case JTabbedPane.RIGHT:
        p.x = 0;
        p.y = currentScrollOffset;
        break;
      default:
        p.x = currentScrollOffset;
        p.y = 0;
    }

    viewport.setViewPosition(p);
  }

  /**
   * This method creates a new BasicTabbedPaneUI.
   *
   * @param c The JComponent to create a UI for.
   *
   * @return A new BasicTabbedPaneUI.
   */
  public static ComponentUI createUI(JComponent c)
  {
    return new BasicTabbedPaneUI();
  }

  /**
   * This method installs the UI for the given JComponent.
   *
   * @param c The JComponent to install the UI for.
   */
  public void installUI(JComponent c)
  {
    super.installUI(c);
    if (c instanceof JTabbedPane)
      {
        tabPane = (JTabbedPane) c;

        installComponents();
        installDefaults();
        installListeners();
        installKeyboardActions();

        layoutManager = createLayoutManager();
        tabPane.setLayout(layoutManager);
      }
  }

  /**
   * This method uninstalls the UI for the  given JComponent.
   *
   * @param c The JComponent to uninstall the UI for.
   */
  public void uninstallUI(JComponent c)
  {
    layoutManager = null;

    uninstallKeyboardActions();
    uninstallListeners();
    uninstallDefaults();
    uninstallComponents();

    tabPane = null;
  }

  /**
   * This method creates the appropriate layout manager for the JTabbedPane's
   * current tab layout policy. If the tab layout policy is
   * SCROLL_TAB_LAYOUT, then all the associated components that need to be
   * created will be done so now.
   *
   * @return A layout manager given the tab layout policy.
   */
  protected LayoutManager createLayoutManager()
  {
    if (tabPane.getTabLayoutPolicy() == JTabbedPane.WRAP_TAB_LAYOUT)
      return new TabbedPaneLayout();
    else
      {
        runCount = 1;
        tabRuns[0] = 0;

        incrButton = createIncreaseButton();
        incrButton.addMouseListener(mouseListener);

        decrButton = createDecreaseButton();
        decrButton.addMouseListener(mouseListener);
        decrButton.setEnabled(false);

        panel = new ScrollingPanel();
        panel.setSize(Integer.MAX_VALUE, Integer.MAX_VALUE);
        panel.addMouseListener(mouseListener);
        panel.addFocusListener(focusListener);

        viewport = new ScrollingViewport();
        viewport.setBackground(Color.LIGHT_GRAY);
        viewport.setView(panel);
        viewport.setLayout(null);

        tabPane.add(incrButton);
        tabPane.add(decrButton);
        tabPane.add(viewport);

        return new TabbedPaneScrollLayout();
      }
  }

  /**
   * This method installs components for this JTabbedPane.
   */
  protected void installComponents()
  {
    // Nothing to be done.
  }

  /**
   * This method uninstalls components for this JTabbedPane.
   */
  protected void uninstallComponents()
  {
    if (incrButton != null)
      tabPane.remove(incrButton);

    if (decrButton != null)
      tabPane.remove(decrButton);

    if (viewport != null)
      tabPane.remove(viewport);
  }

  /**
   * This method installs defaults for the Look and Feel.
   */
  protected void installDefaults()
  {
    LookAndFeel.installColorsAndFont(tabPane, "TabbedPane.background",
                                     "TabbedPane.foreground",
                                     "TabbedPane.font");
    tabPane.setOpaque(false);

    lightHighlight = UIManager.getColor("TabbedPane.highlight");
    highlight = UIManager.getColor("TabbedPane.light");

    shadow = UIManager.getColor("TabbedPane.shadow");
    darkShadow = UIManager.getColor("TabbedPane.darkShadow");

    focus = UIManager.getColor("TabbedPane.focus");

    textIconGap = UIManager.getInt("TabbedPane.textIconGap");
    tabRunOverlay = UIManager.getInt("TabbedPane.tabRunOverlay");

    tabInsets = UIManager.getInsets("TabbedPane.tabInsets");
    selectedTabPadInsets
      = UIManager.getInsets("TabbedPane.selectedTabPadInsets");
    tabAreaInsets = UIManager.getInsets("TabbedPane.tabAreaInsets");
    contentBorderInsets
      = UIManager.getInsets("TabbedPane.contentBorderInsets");
    tabsOpaque = UIManager.getBoolean("TabbedPane.tabsOpaque");

    // Although 'TabbedPane.contentAreaColor' is not defined in the defaults
    // of BasicLookAndFeel it is used by this class.
    selectedColor = UIManager.getColor("TabbedPane.contentAreaColor");
    if (selectedColor == null)
      selectedColor = UIManager.getColor("control");

    calcRect = new Rectangle();
    tabRuns = new int[10];
    tabAreaRect = new Rectangle();
    contentRect = new Rectangle();
  }

  /**
   * This method uninstalls defaults for the Look and Feel.
   */
  protected void uninstallDefaults()
  {
    calcRect = null;
    tabAreaRect = null;
    contentRect = null;
    tabRuns = null;

    tempIconRect = null;
    tempTextRect = null;

    contentBorderInsets = null;
    tabAreaInsets = null;
    selectedTabPadInsets = null;
    tabInsets = null;

    focus = null;
    darkShadow = null;
    shadow = null;
    lightHighlight = null;
    highlight = null;

    selectedColor = null;
  }

  /**
   * This method creates and installs the listeners for this UI.
   */
  protected void installListeners()
  {
    mouseListener = createMouseListener();
    tabChangeListener = createChangeListener();
    propertyChangeListener = createPropertyChangeListener();
    focusListener = createFocusListener();

    tabPane.addMouseListener(mouseListener);
    tabPane.addChangeListener(tabChangeListener);
    tabPane.addPropertyChangeListener(propertyChangeListener);
    tabPane.addFocusListener(focusListener);
  }

  /**
   * This method removes and nulls the listeners for this UI.
   */
  protected void uninstallListeners()
  {
    tabPane.removeFocusListener(focusListener);
    tabPane.removePropertyChangeListener(propertyChangeListener);
    tabPane.removeChangeListener(tabChangeListener);
    tabPane.removeMouseListener(mouseListener);

    if (incrButton != null)
      incrButton.removeMouseListener(mouseListener);

    if (decrButton != null)
      decrButton.removeMouseListener(mouseListener);

    if (panel != null)
      {
        panel.removeMouseListener(mouseListener);
        panel.removeFocusListener(focusListener);
      }

    focusListener = null;
    propertyChangeListener = null;
    tabChangeListener = null;
    mouseListener = null;
  }

  /**
   * This method creates a new MouseListener.
   *
   * @return A new MouseListener.
   */
  protected MouseListener createMouseListener()
  {
    return new MouseHandler();
  }

  /**
   * This method creates a new FocusListener.
   *
   * @return A new FocusListener.
   */
  protected FocusListener createFocusListener()
  {
    return new FocusHandler();
  }

  /**
   * This method creates a new ChangeListener.
   *
   * @return A new ChangeListener.
   */
  protected ChangeListener createChangeListener()
  {
    return new TabSelectionHandler();
  }

  /**
   * This method creates a new PropertyChangeListener.
   *
   * @return A new PropertyChangeListener.
   */
  protected PropertyChangeListener createPropertyChangeListener()
  {
    return new PropertyChangeHandler();
  }

  /**
   * This method installs keyboard actions for the JTabbedPane.
   */
  protected void installKeyboardActions()
  {
    InputMap keyMap = (InputMap) UIManager.get("TabbedPane.focusInputMap");
    SwingUtilities.replaceUIInputMap(tabPane, JComponent.WHEN_FOCUSED, keyMap);

    keyMap = (InputMap) UIManager.get("TabbedPane.ancestorInputMap");
    SwingUtilities
      .replaceUIInputMap(tabPane,
                         JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT,
                         keyMap);

    ActionMap map = getActionMap();
    SwingUtilities.replaceUIActionMap(tabPane, map);
  }

  /**
   * This method uninstalls keyboard actions for the JTabbedPane.
   */
  protected void uninstallKeyboardActions()
  {
    SwingUtilities.replaceUIActionMap(tabPane, null);
    SwingUtilities.replaceUIInputMap(tabPane, JComponent.WHEN_FOCUSED, null);
    SwingUtilities
      .replaceUIInputMap(tabPane,
                         JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT,
                         null);
  }

  /**
   * This method returns the minimum size of the JTabbedPane.
   *
   * @param c The JComponent to find a size for.
   *
   * @return The minimum size.
   */
  public Dimension getMinimumSize(JComponent c)
  {
    return layoutManager.minimumLayoutSize(tabPane);
  }

  /**
   * This method returns the maximum size of the JTabbedPane.
   *
   * @param c The JComponent to find a size for.
   *
   * @return The maximum size.
   */
  public Dimension getMaximumSize(JComponent c)
  {
    return new Dimension(Short.MAX_VALUE, Short.MAX_VALUE);
  }

  /**
   * This method paints the JTabbedPane.
   *
   * @param g The Graphics object to paint with.
   * @param c The JComponent to paint.
   */
  public void paint(Graphics g, JComponent c)
  {
    if (!tabPane.isValid())
      tabPane.validate();

    if (tabPane.getTabCount() == 0)
      return;

    int index = tabPane.getSelectedIndex();
    if (index < 0)
      index = 0;

    int tabPlacement = tabPane.getTabPlacement();

    // Paint the tab area only in WRAP_TAB_LAYOUT Mode from this method
    // because it is done through the ScrollingViewport.paint() method
    // for the SCROLL_TAB_LAYOUT mode.
    if (tabPane.getTabLayoutPolicy() == JTabbedPane.WRAP_TAB_LAYOUT)
      {
        g.setColor(highlight);
        g.fillRect(tabAreaRect.x, tabAreaRect.y,
                   tabAreaRect.width, tabAreaRect.height);
        paintTabArea(g, tabPlacement, index);
      }

    paintContentBorder(g, tabPlacement, index);
  }

  /**
   * This method paints the tab area. This includes painting the rectangles
   * that make up the tabs.
   *
   * @param g The Graphics object to paint with.
   * @param tabPlacement The JTabbedPane's tab placement.
   * @param selectedIndex The selected index.
   */
  protected void paintTabArea(Graphics g, int tabPlacement, int selectedIndex)
  {
    // Please note: the ordering of the painting is important.
    // we WANT to paint the outermost run first and then work our way in.

    // The following drawing code works for both tab layouts.
    int tabCount = tabPane.getTabCount();

    for (int i = runCount - 1; i >= 0; --i)
      {
        int start = tabRuns[i];
        int next;
        if (i == runCount - 1)
          next = tabRuns[0];
        else
          next = tabRuns[i + 1];
        int end = next != 0 ? next - 1 : tabCount - 1;
        for (int j = start; j <= end; ++j)
          {
            if (j != selectedIndex)
              {
                paintTab(g, tabPlacement, rects, j,
                         tempIconRect, tempTextRect);
              }
          }
      }

    // Paint selected tab in front of every other tab.
    if (selectedIndex >= 0)
      paintTab(g, tabPlacement, rects, selectedIndex,
               tempIconRect, tempTextRect);
  }

  /**
   * This method paints an individual tab.
   *
   * @param g The Graphics object to paint with.
   * @param tabPlacement The JTabbedPane's tab placement.
   * @param rects The array of rectangles that keep the size and position of
   *        the tabs.
   * @param tabIndex The tab index to paint.
   * @param iconRect The rectangle to use for the icon.
   * @param textRect The rectangle to use for the text.
   */
  protected void paintTab(Graphics g, int tabPlacement, Rectangle[] rects,
                          int tabIndex, Rectangle iconRect, Rectangle textRect)
  {
    Rectangle rect = rects[tabIndex];
    boolean isSelected = tabIndex == tabPane.getSelectedIndex();
    // Paint background if necessary.
    if (tabsOpaque || tabPane.isOpaque())
      {
        paintTabBackground(g, tabPlacement, tabIndex, rect.x, rect.y,
                           rect.width, rect.height, isSelected);
      }

    // Paint border.
    paintTabBorder(g, tabPlacement, tabIndex, rect.x, rect.y, rect.width,
                   rect.height, isSelected);

    // Layout label.
    FontMetrics fm = getFontMetrics();
    Icon icon = getIconForTab(tabIndex);
    String title = tabPane.getTitleAt(tabIndex);
    layoutLabel(tabPlacement, fm, tabIndex, title, icon, rect, iconRect,
                textRect, isSelected);
    // Paint the text.
    paintText(g, tabPlacement, tabPane.getFont(), fm, tabIndex, title,
              textRect, isSelected);

    // Paint icon if necessary.
    paintIcon(g, tabPlacement, tabIndex, icon, iconRect, isSelected);

    // Paint focus indicator.
    paintFocusIndicator(g, tabPlacement, rects, tabIndex, iconRect, textRect,
                        isSelected);
  }

  /**
   * This method lays out the tab and finds the location to paint the  icon
   * and text.
   *
   * @param tabPlacement The JTabbedPane's tab placement.
   * @param metrics The font metrics for the font to paint with.
   * @param tabIndex The tab index to paint.
   * @param title The string painted.
   * @param icon The icon painted.
   * @param tabRect The tab bounds.
   * @param iconRect The calculated icon bounds.
   * @param textRect The calculated text bounds.
   * @param isSelected Whether this tab is selected.
   */
  protected void layoutLabel(int tabPlacement, FontMetrics metrics,
                             int tabIndex, String title, Icon icon,
                             Rectangle tabRect, Rectangle iconRect,
                             Rectangle textRect, boolean isSelected)
  {
    // Reset the icon and text rectangles, as the result is not specified
    // when the locations are not (0,0).
    textRect.x = 0;
    textRect.y = 0;
    textRect.width = 0;
    textRect.height = 0;
    iconRect.x = 0;
    iconRect.y = 0;
    iconRect.width = 0;
    iconRect.height = 0;
    SwingUtilities.layoutCompoundLabel(tabPane, metrics, title, icon,
                                       SwingConstants.CENTER,
                                       SwingConstants.CENTER,
                                       SwingConstants.CENTER,
                                       SwingConstants.RIGHT, tabRect,
                                       iconRect, textRect, textIconGap);

    int shiftX = getTabLabelShiftX(tabPlacement, tabIndex, isSelected);
    int shiftY = getTabLabelShiftY(tabPlacement, tabIndex, isSelected);

    iconRect.x += shiftX;
    iconRect.y += shiftY;

    textRect.x += shiftX;
    textRect.y += shiftY;
  }

  /**
   * This method paints the icon.
   *
   * @param g The Graphics object to paint.
   * @param tabPlacement The JTabbedPane's tab placement.
   * @param tabIndex The tab index to paint.
   * @param icon The icon to paint.
   * @param iconRect The bounds of the icon.
   * @param isSelected Whether this tab is selected.
   */
  protected void paintIcon(Graphics g, int tabPlacement, int tabIndex,
                           Icon icon, Rectangle iconRect, boolean isSelected)
  {
    if (icon != null)
      icon.paintIcon(tabPane, g, iconRect.x, iconRect.y);
  }

  /**
   * This method paints the text for the given tab.
   *
   * @param g The Graphics object to paint with.
   * @param tabPlacement The JTabbedPane's tab placement.
   * @param font The font to paint with.
   * @param metrics The fontmetrics of the given font.
   * @param tabIndex The tab index.
   * @param title The string to paint.
   * @param textRect The bounds of the string.
   * @param isSelected Whether this tab is selected.
   */
  protected void paintText(Graphics g, int tabPlacement, Font font,
                           FontMetrics metrics, int tabIndex, String title,
                           Rectangle textRect, boolean isSelected)
  {
    g.setFont(font);
    View textView = getTextViewForTab(tabIndex);
    if (textView != null)
      {
        textView.paint(g, textRect);
        return;
      }

    int ascent = metrics.getAscent();

    int mnemIndex = tabPane.getDisplayedMnemonicIndexAt(tabIndex);
    if (tabPane.isEnabled() && tabPane.isEnabledAt(tabIndex))
      {
        Color fg = tabPane.getForegroundAt(tabIndex);
        if (isSelected && (fg instanceof UIResource))
          {
            Color selectionForeground =
              UIManager.getColor("TabbedPane.selectionForeground");
            if (selectionForeground != null)
              fg = selectionForeground;
          }
        g.setColor(fg);

        if (mnemIndex != -1)
          BasicGraphicsUtils.drawStringUnderlineCharAt(g, title, mnemIndex,
                                                       textRect.x,
                                                       textRect.y + ascent);
        else
          g.drawString(title, textRect.x, textRect.y + ascent);
      }
    else
      {
        Color bg = tabPane.getBackgroundAt(tabIndex);
        g.setColor(bg.brighter());
        if (mnemIndex != -1)
          BasicGraphicsUtils.drawStringUnderlineCharAt(g, title, mnemIndex,
                                                       textRect.x, textRect.y
                                                       + ascent);
        else
          g.drawString(title, textRect.x, textRect.y + ascent);

        g.setColor(bg.darker());
        if (mnemIndex != -1)
          BasicGraphicsUtils.drawStringUnderlineCharAt(g, title, mnemIndex,
                                                       textRect.x + 1,
                                                       textRect.y + 1
                                                       + ascent);
        else
          g.drawString(title, textRect.x + 1, textRect.y + 1 + ascent);
      }
  }

  /**
   * This method returns how much the label for the tab should shift in the X
   * direction.
   *
   * @param tabPlacement The JTabbedPane's tab placement.
   * @param tabIndex The tab index being painted.
   * @param isSelected Whether this tab is selected.
   *
   * @return The amount the label should shift by in the X direction.
   */
  protected int getTabLabelShiftX(int tabPlacement, int tabIndex,
                                  boolean isSelected)
  {
    switch (tabPlacement)
    {
      default:
      case SwingUtilities.TOP:
      case SwingUtilities.BOTTOM:
        return 1;
      case SwingUtilities.LEFT:
        return (isSelected) ? -1 : 1;
      case SwingUtilities.RIGHT:
        return (isSelected) ? 1 : -1;
    }
  }

  /**
   * This method returns how much the label for the tab should shift in the Y
   * direction.
   *
   * @param tabPlacement The JTabbedPane's tab placement.
   * @param tabIndex The tab index being painted.
   * @param isSelected Whether this tab is selected.
   *
   * @return The amount the label should shift by in the Y direction.
   */
  protected int getTabLabelShiftY(int tabPlacement, int tabIndex,
                                  boolean isSelected)
  {
    switch (tabPlacement)
    {
      default:
      case SwingUtilities.TOP:
        return (isSelected) ? -1 : 1;
      case SwingUtilities.BOTTOM:
        return (isSelected) ? 1 : -1;
      case SwingUtilities.LEFT:
      case SwingUtilities.RIGHT:
        return 0;
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
        // The focus rectangle.
        int x;
        int y;
        int w;
        int h;

        g.setColor(focus);
        switch (tabPlacement)
          {
          case LEFT:
            x = rect.x + 3;
            y = rect.y + 3;
            w = rect.width - 5;
            h = rect.height - 6;
            break;
          case RIGHT:
            x = rect.x + 2;
            y = rect.y + 3;
            w = rect.width - 6;
            h = rect.height - 5;
            break;
          case BOTTOM:
            x = rect.x + 3;
            y = rect.y + 2;
            w = rect.width - 6;
            h = rect.height - 5;
            break;
          case TOP:
          default:
            x = rect.x + 3;
            y = rect.y + 3;
            w = rect.width - 6;
            h = rect.height - 5;
          }

        BasicGraphicsUtils.drawDashedRect(g, x, y, w, h);
      }
  }

  /**
   * This method paints the border for an individual tab.
   *
   * @param g The Graphics object to paint with.
   * @param tabPlacement The JTabbedPane's tab placement.
   * @param tabIndex The tab index.
   * @param x The x position of the tab.
   * @param y The y position of the tab.
   * @param w The width of the tab.
   * @param h The height of the tab.
   * @param isSelected Whether the tab is selected.
   */
  protected void paintTabBorder(Graphics g, int tabPlacement, int tabIndex,
                                int x, int y, int w, int h, boolean isSelected)
  {
    Color saved = g.getColor();

    switch (tabPlacement)
    {
      case SwingConstants.TOP:
        g.setColor(shadow);
        // Inner right line.
        g.drawLine(x + w - 2, y + 2, x + w - 2, y + h);

        g.setColor(darkShadow);
        // Outer right line.
        g.drawLine(x + w - 1, y + 2, x + w - 1, y + h);

        // Upper right corner.
        g.drawLine(x + w - 2, y + 1, x + w - 1, y + 2);

        g.setColor(lightHighlight);

        // Left line.
        g.drawLine(x, y + 3, x, y + h);

        // Upper line.
        g.drawLine(x + 3, y, x + w - 3, y);

        // Upper left corner.
        g.drawLine(x, y + 2, x + 2, y);

        break;
      case SwingConstants.LEFT:
        g.setColor(lightHighlight);
        // Top line.
        g.drawLine(x + 3, y, x + w - 1, y);

        // Top left border.
        g.drawLine(x + 2, y, x, y + 2);

        // Left line.
        g.drawLine(x, y + 3, x, y + h - 4);

        // Bottom left corner.
        g.drawLine(x, y + h - 3, x + 1, y + h - 2);

        g.setColor(darkShadow);
        // Outer bottom line.
        g.drawLine(x + 2, y + h - 1, x + w - 1, y + h - 1);

        g.setColor(shadow);
        // Inner bottom line.
        g.drawLine(x + 2, y + h - 2,  x + w - 1, y + h - 2);

        break;
      case SwingConstants.BOTTOM:
        g.setColor(shadow);
        // Inner right line.
        g.drawLine(x + w - 2, y, x + w - 2, y + h - 2);

        // Inner bottom line.
        g.drawLine(x + 2, y + h - 1, x + w - 3, y + h - 1);

        g.setColor(darkShadow);
        // Outer right line.
        g.drawLine(x + w - 1, y, x + w - 1, y + h - 3);

        // Bottom right corner.
        g.drawLine(x + w - 1, y + h - 2, x + w - 3, y + h);

        // Bottom line.
        g.drawLine(x + 2, y + h, x + w - 4, y + h);

        g.setColor(lightHighlight);
        // Left line.
        g.drawLine(x, y, x, y + h - 3);

        // Bottom left corner.
        g.drawLine(x, y + h - 2, x + 1, y + h - 1);
        break;
      case SwingConstants.RIGHT:
        g.setColor(lightHighlight);
        // Top line.
        g.drawLine(x, y, x + w - 3, y);

        g.setColor(darkShadow);
        // Top right corner.
        g.drawLine(x + w - 2, y + 1, x + w - 1, y + 2);

        // Outer right line.
        g.drawLine(x + w - 1, y + 3, x + w - 1, y + h - 3);

        // Bottom right corner.
        g.drawLine(x + w - 2, y + h - 2, x + w - 3, y + h - 1);

        // Bottom line.
        g.drawLine(x, y + h - 1, x + w - 4, y + h - 1);

        g.setColor(shadow);

        // Inner right line.
        g.drawLine(x + w - 2, y + 2, x + w - 2, y + h - 3);

        // Inner bottom line.
        g.drawLine(x, y + h - 2, x + w - 3, y + h - 2);

        break;
    }

    g.setColor(saved);
  }

  /**
   * This method paints the background for an individual tab.
   *
   * @param g The Graphics object to paint with.
   * @param tabPlacement The JTabbedPane's tab placement.
   * @param tabIndex The tab index.
   * @param x The x position of the tab.
   * @param y The y position of the tab.
   * @param w The width of the tab.
   * @param h The height of the tab.
   * @param isSelected Whether the tab is selected.
   */
  protected void paintTabBackground(Graphics g, int tabPlacement,
                                    int tabIndex, int x, int y, int w, int h,
                                    boolean isSelected)
  {
    Color saved = g.getColor();

    if (isSelected)
      g.setColor(selectedColor);
    else
      {
        Color bg = tabPane.getBackgroundAt(tabIndex);
        if (bg == null)
          bg = Color.LIGHT_GRAY;
        g.setColor(bg);
      }

    switch (tabPlacement)
      {
        case SwingConstants.TOP:
          g.fillRect(x + 1, y + 1, w - 1, h - 1);
          break;
        case SwingConstants.BOTTOM:
          g.fillRect(x, y, w - 1, h - 1);
          break;
        case SwingConstants.LEFT:
          g.fillRect(x + 1, y + 1, w - 1, h - 2);
          break;
        case SwingConstants.RIGHT:
          g.fillRect(x, y + 1, w - 1, h - 2);
          break;
      }

    g.setColor(saved);
  }

  /**
   * This method paints the border around the content area.
   *
   * @param g The Graphics object to paint with.
   * @param tabPlacement The JTabbedPane's tab placement.
   * @param selectedIndex The index of the selected tab.
   */
  protected void paintContentBorder(Graphics g, int tabPlacement,
                                    int selectedIndex)
  {
    int width = tabPane.getWidth();
    int height = tabPane.getHeight();
    Insets insets = tabPane.getInsets();

    // Calculate coordinates of content area.
    int x = insets.left;
    int y = insets.top;
    int w = width - insets.left - insets.right;
    int h = height - insets.top - insets.bottom;

    switch (tabPlacement)
    {
    case LEFT:
      x += calculateTabAreaWidth(tabPlacement, runCount, maxTabWidth);
      w -= x - insets.left;
      break;
    case RIGHT:
      w -= calculateTabAreaWidth(tabPlacement, runCount, maxTabWidth);
      break;
    case BOTTOM:
      h -= calculateTabAreaHeight(tabPlacement, runCount, maxTabHeight);
      break;
    case TOP:
    default:
      y += calculateTabAreaHeight(tabPlacement, runCount, maxTabHeight);
      h -= y - insets.top;
    }

    // Fill background if necessary.
    if (tabPane.isOpaque())
      {
        Color bg = UIManager.getColor("TabbedPane.contentAreaColor");
        g.setColor(bg);
        g.fillRect(x, y, w, h);
      }

    // Paint border.
    paintContentBorderTopEdge(g, tabPlacement, selectedIndex, x, y, w, h);
    paintContentBorderLeftEdge(g, tabPlacement, selectedIndex, x, y, w, h);
    paintContentBorderBottomEdge(g, tabPlacement, selectedIndex, x, y, w, h);
    paintContentBorderRightEdge(g, tabPlacement, selectedIndex, x, y, w, h);
  }

  /**
   * This method paints the top edge of the content border.
   *
   * @param g The Graphics object to paint with.
   * @param tabPlacement The JTabbedPane's tab placement.
   * @param selectedIndex The selected tab index.
   * @param x The x coordinate for the content area.
   * @param y The y coordinate for the content area.
   * @param w The width of the content area.
   * @param h The height of the content area.
   */
  protected void paintContentBorderTopEdge(Graphics g, int tabPlacement,
                                           int selectedIndex, int x, int y,
                                           int w, int h)
  {
    Color saved = g.getColor();
    g.setColor(lightHighlight);

    int startgap = rects[selectedIndex].x - currentScrollOffset;
    int endgap = rects[selectedIndex].x + rects[selectedIndex].width
                 - currentScrollOffset;

    // Paint the highlight line with a gap if the tabs are at the top
    // and the selected tab is inside the visible area.
    if (tabPlacement == SwingConstants.TOP && startgap >= 0)
      {
        g.drawLine(x, y, startgap, y);
        g.drawLine(endgap, y, x + w - 1, y);

        g.setColor(selectedColor);
        g.drawLine(startgap, y, endgap - 1, y);
      }
    else
      g.drawLine(x, y, x + w, y);

    g.setColor(selectedColor);
    g.drawLine(x, y + 1, x + w - 1, y + 1);
    g.drawLine(x, y + 2, x + w - 1, y + 2);

    g.setColor(saved);
  }

  /**
   * This method paints the left edge of the content border.
   *
   * @param g The Graphics object to paint with.
   * @param tabPlacement The JTabbedPane's tab placement.
   * @param selectedIndex The selected tab index.
   * @param x The x coordinate for the content area.
   * @param y The y coordinate for the content area.
   * @param w The width of the content area.
   * @param h The height of the content area.
   */
  protected void paintContentBorderLeftEdge(Graphics g, int tabPlacement,
                                            int selectedIndex, int x, int y,
                                            int w, int h)
  {
    Color saved = g.getColor();
    g.setColor(lightHighlight);

    int startgap = rects[selectedIndex].y - currentScrollOffset;
    int endgap = rects[selectedIndex].y + rects[selectedIndex].height
                 - currentScrollOffset;

    if (tabPlacement == SwingConstants.LEFT && startgap >= 0)
      {
        g.drawLine(x, y, x, startgap);
        g.drawLine(x, endgap, x, y + h - 1);

        g.setColor(selectedColor);
        g.drawLine(x, startgap, x, endgap - 1);
      }
    else
      g.drawLine(x, y, x, y + h - 1);

    g.setColor(selectedColor);
    g.drawLine(x + 1, y + 1, x + 1, y + h - 4);

    g.setColor(saved);
  }

  /**
   * This method paints the bottom edge of the content border.
   *
   * @param g The Graphics object to paint with.
   * @param tabPlacement The JTabbedPane's tab placement.
   * @param selectedIndex The selected tab index.
   * @param x The x coordinate for the content area.
   * @param y The y coordinate for the content area.
   * @param w The width of the content area.
   * @param h The height of the content area.
   */
  protected void paintContentBorderBottomEdge(Graphics g, int tabPlacement,
                                              int selectedIndex, int x, int y,
                                              int w, int h)
  {
    Color saved = g.getColor();

    int startgap = rects[selectedIndex].x - currentScrollOffset;
    int endgap = rects[selectedIndex].x + rects[selectedIndex].width
                 - currentScrollOffset;

    if (tabPlacement == SwingConstants.BOTTOM && startgap >= 0)
      {
        g.setColor(shadow);
        g.drawLine(x + 1, y + h - 2, startgap, y + h - 2);
        g.drawLine(endgap, y + h - 2, x + w - 2, y + h - 2);

        g.setColor(darkShadow);
        g.drawLine(x, y + h - 1, startgap , y + h - 1);
        g.drawLine(endgap, y + h - 1, x + w - 1, y + h - 1);

        g.setColor(selectedColor);
        g.drawLine(startgap, y + h - 1, endgap - 1, y + h - 1);
        g.drawLine(startgap, y + h - 2, endgap - 1, y + h - 2);
      }
    else
      {
        g.setColor(shadow);
        g.drawLine(x + 1, y + h - 2, x + w - 1, y + h - 2);
        g.setColor(darkShadow);
        g.drawLine(x, y + h - 1, x + w - 1, y + h - 1);
      }

    g.setColor(selectedColor);
    g.drawLine(x + 1, y + h - 3, x + w - 2, y + h - 3);

    g.setColor(saved);
  }

  /**
   * This method paints the right edge of the content border.
   *
   * @param g The Graphics object to paint with.
   * @param tabPlacement The JTabbedPane's tab placement.
   * @param selectedIndex The selected tab index.
   * @param x The x coordinate for the content area.
   * @param y The y coordinate for the content area.
   * @param w The width of the content area.
   * @param h The height of the content area.
   */
  protected void paintContentBorderRightEdge(Graphics g, int tabPlacement,
                                             int selectedIndex, int x, int y,
                                             int w, int h)
  {
    Color saved = g.getColor();
    int startgap = rects[selectedIndex].y - currentScrollOffset;
    int endgap = rects[selectedIndex].y + rects[selectedIndex].height
                 - currentScrollOffset;

    if (tabPlacement == SwingConstants.RIGHT && startgap >= 0)
      {
        g.setColor(shadow);
        g.drawLine(x + w - 2, y + 1, x + w - 2, startgap);
        g.drawLine(x + w - 2, endgap, x + w - 2, y + h - 2);

        g.setColor(darkShadow);
        g.drawLine(x + w - 1, y, x + w - 1, startgap);
        g.drawLine(x + w - 1, endgap, x + w - 1, y + h - 2);

        g.setColor(selectedColor);
        g.drawLine(x + w - 2, startgap, x + w - 2, endgap - 1);
        g.drawLine(x + w - 1, startgap, x + w - 1, endgap - 1);
      }
    else
      {
        g.setColor(shadow);
        g.drawLine(x + w - 2, y + 1, x + w - 2, y + h - 2);
        g.setColor(darkShadow);
        g.drawLine(x + w - 1, y, x + w - 1, y + h - 2);
      }

    g.setColor(selectedColor);
    g.drawLine(x + w - 3, y + 1, x + w - 3, y + h - 4);

    g.setColor(saved);
  }

  /**
   * <p>This method returns the bounds of a tab for the given index
   * and shifts it by the current scrolling offset if the tabbed
   * pane is in scrolling tab layout mode.</p>
   *
   * <p>Subclassses should retrievs a tab's bounds by this method
   * if they want to find out whether the tab is currently visible.</p>
   *
   * @param pane The JTabbedPane.
   * @param i The index to look for.
   *
   * @return The bounds of the tab with the given index.
   */
  public Rectangle getTabBounds(JTabbedPane pane, int i)
  {
    // Need to re-layout container if tab does not exist.
    if (i >= rects.length)
      layoutManager.layoutContainer(pane);

    // Properly shift coordinates if scrolling has taken
    // place.
    if (pane.getTabLayoutPolicy() == JTabbedPane.SCROLL_TAB_LAYOUT)
      {
        Rectangle r = new Rectangle(rects[i]);

        switch(pane.getTabPlacement())
        {
          case SwingConstants.TOP:
          case SwingConstants.BOTTOM:
            r.x -= currentScrollOffset;
            break;
          default:
            r.y -= currentScrollOffset;
        }

        return r;
      }

    return rects[i];
  }

  /**
   * This method returns the number of runs.
   *
   * @param pane The JTabbedPane.
   *
   * @return The number of runs.
   */
  public int getTabRunCount(JTabbedPane pane)
  {
    return runCount;
  }

  /**
   * This method returns the tab index given a coordinate.
   *
   * @param pane The JTabbedPane.
   * @param x The x coordinate.
   * @param y The y coordinate.
   *
   * @return The tab index that the coordinate lands in.
   */
  public int tabForCoordinate(JTabbedPane pane, int x, int y)
  {
    // Note: This code is tab layout mode agnostic.
    if (! tabPane.isValid())
      tabPane.validate();

    int tabCount = tabPane.getTabCount();

    // If the user clicked outside of any tab rect the
    // selection should not change.
    int index = tabPane.getSelectedIndex();
    for (int i = 0; i < tabCount; ++i)
      {
        if (rects[i].contains(x, y))
          {
            index = i;
            break;
          }
      }

    return index;
  }

  /**
   * <p>This method returns the tab bounds in the given rectangle.</p>
   *
   * <p>The returned rectangle will be shifted by the current scroll
   * offset if the tabbed pane is in scrolling tab layout mode.</p>.
   *
   * @param tabIndex The index to get bounds for.
   * @param dest The rectangle to store bounds in.
   *
   * @return The rectangle passed in.
   */
  protected Rectangle getTabBounds(int tabIndex, Rectangle dest)
  {
    dest.setBounds(getTabBounds(tabPane, tabIndex));
    return dest;
  }

  /**
   * This method returns the component that is shown in  the content area.
   *
   * @return The component that is shown in the content area.
   */
  protected Component getVisibleComponent()
  {
    return visibleComponent;
  }

  /**
   * This method sets the visible component.
   *
   * @param component The component to be set visible.
   */
  protected void setVisibleComponent(Component component)
  {
    // Make old component invisible.
    if (visibleComponent != null && visibleComponent != component
        && visibleComponent.getParent() == tabPane)
      {
        visibleComponent.setVisible(false);
      }

    // Make new component visible.
    if (component != null && ! component.isVisible())
      {
        component.setVisible(true);
      }
    visibleComponent = component;
  }

  /**
   * This method assures that enough rectangles are created given the
   * tabCount. The old array is copied to the  new one.
   *
   * @param tabCount The number of tabs.
   */
  protected void assureRectsCreated(int tabCount)
  {
    if (rects.length < tabCount)
      {
        Rectangle[] old = rects;
        rects = new Rectangle[tabCount];
        System.arraycopy(old, 0, rects, 0, old.length);
        for (int i = old.length; i < rects.length; i++)
          rects[i] = new Rectangle();
      }
  }

  /**
   * This method expands the tabRuns array to give it more room. The old array
   * is copied to the new one.
   */
  protected void expandTabRunsArray()
  {
    // This method adds another 10 index positions to the tabRuns array.
    if (tabRuns == null)
      tabRuns = new int[10];
    else
      {
        int[] newRuns = new int[tabRuns.length + 10];
        System.arraycopy(tabRuns, 0, newRuns, 0, tabRuns.length);
        tabRuns = newRuns;
      }
  }

  /**
   * This method returns which run a particular tab belongs to.
   *
   * @param tabCount The number of tabs.
   * @param tabIndex The tab to find.
   *
   * @return The tabRuns index that it belongs to.
   */
  protected int getRunForTab(int tabCount, int tabIndex)
  {
    if (runCount == 1 && tabIndex < tabCount && tabIndex >= 0)
      return 0;
    for (int i = 0; i < runCount; i++)
      {
        int first = lastTabInRun(tabCount, getPreviousTabRun(i)) + 1;
        if (first == tabCount)
          first = 0;
        int last = lastTabInRun(tabCount, i);
        if (last >= tabIndex && first <= tabIndex)
          return i;
      }
    return -1;
  }

  /**
   * This method returns the index of the last tab in  a run.
   *
   * @param tabCount The number of tabs.
   * @param run The run to check.
   *
   * @return The last tab in the given run.
   */
  protected int lastTabInRun(int tabCount, int run)
  {
    int lastTab;
    if (runCount == 1)
      lastTab = tabCount - 1;
    else
      {
        int nextRun;
        if (run == runCount - 1)
          nextRun = 0;
        else
          nextRun = run + 1;

        if (tabRuns[nextRun] == 0)
          lastTab = tabCount - 1;
        else
          lastTab = tabRuns[nextRun] - 1;
      }
    return lastTab;
  }

  /**
   * This method returns the tab run overlay.
   *
   * @param tabPlacement The JTabbedPane's tab placement.
   *
   * @return The tab run overlay.
   */
  protected int getTabRunOverlay(int tabPlacement)
  {
    return tabRunOverlay;
  }

  /**
   * This method returns the tab run indent. It is used in WRAP_TAB_LAYOUT and
   * makes each tab run start indented by a certain amount.
   *
   * @param tabPlacement The JTabbedPane's tab placement.
   * @param run The run to get indent for.
   *
   * @return The amount a run should be indented.
   */
  protected int getTabRunIndent(int tabPlacement, int run)
  {
    return 0;
  }

  /**
   * This method returns whether a tab run should be padded.
   *
   * @param tabPlacement The JTabbedPane's tab placement.
   * @param run The run to check.
   *
   * @return Whether the given run should be padded.
   */
  protected boolean shouldPadTabRun(int tabPlacement, int run)
  {
    return true;
  }

  /**
   * This method returns whether the tab runs should be rotated.
   *
   * @param tabPlacement The JTabbedPane's tab placement.
   *
   * @return Whether runs should be rotated.
   */
  protected boolean shouldRotateTabRuns(int tabPlacement)
  {
    return true;
  }

  /**
   * This method returns an icon for the tab. If the tab is disabled, it
   * should return the disabledIcon. If it is enabled, then it should return
   * the default icon.
   *
   * @param tabIndex The tab index to get an icon for.
   *
   * @return The icon for the tab index.
   */
  protected Icon getIconForTab(int tabIndex)
  {
    if (tabPane.isEnabledAt(tabIndex))
      return tabPane.getIconAt(tabIndex);
    else
      return tabPane.getDisabledIconAt(tabIndex);
  }

  /**
   * This method returns a view that can paint the text for the label.
   *
   * @param tabIndex The tab index to get a view for.
   *
   * @return The view for the tab index.
   */
  protected View getTextViewForTab(int tabIndex)
  {
    // FIXME: When the label contains HTML this should return something
    // non-null.
    return null;
  }

  /**
   * This method returns the tab height, including insets, for the given index
   * and fontheight.
   *
   * @param tabPlacement The JTabbedPane's tab placement.
   * @param tabIndex The index of the tab to calculate.
   * @param fontHeight The font height.
   *
   * @return This tab's height.
   */
  protected int calculateTabHeight(int tabPlacement, int tabIndex,
                                   int fontHeight)
  {
    // FIXME: Handle HTML by using the view (see getTextViewForTab).

    int height = fontHeight;
    Icon icon = getIconForTab(tabIndex);
    Insets tabInsets = getTabInsets(tabPlacement, tabIndex);
    if (icon != null)
      height = Math.max(height, icon.getIconHeight());
    height += tabInsets.top + tabInsets.bottom + 2;
    return height;
  }

  /**
   * This method returns the max tab height.
   *
   * @param tabPlacement The JTabbedPane's tab placement.
   *
   * @return The maximum tab height.
   */
  protected int calculateMaxTabHeight(int tabPlacement)
  {
    maxTabHeight = 0;

    FontMetrics fm = getFontMetrics();
    int fontHeight = fm.getHeight();

    for (int i = 0; i < tabPane.getTabCount(); i++)
      maxTabHeight = Math.max(calculateTabHeight(tabPlacement, i, fontHeight),
                              maxTabHeight);

    return maxTabHeight;
  }

  /**
   * This method calculates the tab width, including insets, for the given tab
   * index and font metrics.
   *
   * @param tabPlacement The JTabbedPane's tab placement.
   * @param tabIndex The tab index to calculate for.
   * @param metrics The font's metrics.
   *
   * @return The tab width for the given index.
   */
  protected int calculateTabWidth(int tabPlacement, int tabIndex,
                                  FontMetrics metrics)
  {
    Icon icon = getIconForTab(tabIndex);
    Insets insets = getTabInsets(tabPlacement, tabIndex);

    int width = insets.bottom + insets.right + 3;
    if (icon != null)
      {
        width += icon.getIconWidth() + textIconGap;
      }

    View v = getTextViewForTab(tabIndex);
    if (v != null)
      width += v.getPreferredSpan(View.X_AXIS);
    else
      {
        String label = tabPane.getTitleAt(tabIndex);
        width += metrics.stringWidth(label);
      }
    return width;
  }

  /**
   * This method calculates the max tab width.
   *
   * @param tabPlacement The JTabbedPane's tab placement.
   *
   * @return The maximum tab width.
   */
  protected int calculateMaxTabWidth(int tabPlacement)
  {
    maxTabWidth = 0;

    FontMetrics fm = getFontMetrics();

    for (int i = 0; i < tabPane.getTabCount(); i++)
      maxTabWidth = Math.max(calculateTabWidth(tabPlacement, i, fm),
                             maxTabWidth);

    return maxTabWidth;
  }

  /**
   * This method calculates the tab area height, including insets, for the
   * given amount of runs and tab height.
   *
   * @param tabPlacement The JTabbedPane's tab placement.
   * @param horizRunCount The number of runs.
   * @param maxTabHeight The max tab height.
   *
   * @return The tab area height.
   */
  protected int calculateTabAreaHeight(int tabPlacement, int horizRunCount,
                                       int maxTabHeight)
  {
    Insets insets = getTabAreaInsets(tabPlacement);
    int tabAreaHeight = horizRunCount * maxTabHeight
                        - (horizRunCount - 1)
                        * getTabRunOverlay(tabPlacement);

    tabAreaHeight += insets.top + insets.bottom;

    return tabAreaHeight;
  }

  /**
   * This method calculates the tab area width, including insets, for the
   * given amount of runs and tab width.
   *
   * @param tabPlacement The JTabbedPane's tab placement.
   * @param vertRunCount The number of runs.
   * @param maxTabWidth The max tab width.
   *
   * @return The tab area width.
   */
  protected int calculateTabAreaWidth(int tabPlacement, int vertRunCount,
                                      int maxTabWidth)
  {
    Insets insets = getTabAreaInsets(tabPlacement);
    int tabAreaWidth = vertRunCount * maxTabWidth
                       - (vertRunCount - 1)
                       * getTabRunOverlay(tabPlacement);

    tabAreaWidth += insets.left + insets.right;

    return tabAreaWidth;
  }

  /**
   * This method returns the tab insets appropriately rotated.
   *
   * @param tabPlacement The JTabbedPane's tab placement.
   * @param tabIndex The tab index.
   *
   * @return The tab insets for the given index.
   */
  protected Insets getTabInsets(int tabPlacement, int tabIndex)
  {
    return tabInsets;
  }

  /**
   * This method returns the selected tab pad insets appropriately rotated.
   *
   * @param tabPlacement The JTabbedPane's tab placement.
   *
   * @return The selected tab pad insets.
   */
  protected Insets getSelectedTabPadInsets(int tabPlacement)
  {
    Insets target = new Insets(0, 0, 0, 0);
    rotateInsets(selectedTabPadInsets, target, tabPlacement);
    return target;
  }

  /**
   * This method returns the tab area insets appropriately rotated.
   *
   * @param tabPlacement The JTabbedPane's tab placement.
   *
   * @return The tab area insets.
   */
  protected Insets getTabAreaInsets(int tabPlacement)
  {
    Insets target = new Insets(0, 0, 0, 0);
    rotateInsets(tabAreaInsets, target, tabPlacement);
    return target;
  }

  /**
   * This method returns the content border insets appropriately rotated.
   *
   * @param tabPlacement The JTabbedPane's tab placement.
   *
   * @return The content border insets.
   */
  protected Insets getContentBorderInsets(int tabPlacement)
  {
    Insets target = new Insets(0, 0, 0, 0);
    rotateInsets(contentBorderInsets, target, tabPlacement);
    return target;
  }

  /**
   * This method returns the fontmetrics for the font of the JTabbedPane.
   *
   * @return The font metrics for the JTabbedPane.
   */
  protected FontMetrics getFontMetrics()
  {
    FontMetrics fm = tabPane.getFontMetrics(tabPane.getFont());
    return fm;
  }

  /**
   * This method navigates from the selected tab into the given direction. As
   * a result, a new tab will be selected (if possible).
   *
   * @param direction The direction to navigate in.
   */
  protected void navigateSelectedTab(int direction)
  {
    int tabPlacement = tabPane.getTabPlacement();
    if (tabPlacement == SwingConstants.TOP
        || tabPlacement == SwingConstants.BOTTOM)
      {
        if (direction == SwingConstants.WEST)
          selectPreviousTabInRun(tabPane.getSelectedIndex());
        else if (direction == SwingConstants.EAST)
          selectNextTabInRun(tabPane.getSelectedIndex());

        else
          {
            int offset = getTabRunOffset(tabPlacement, tabPane.getTabCount(),
                                         tabPane.getSelectedIndex(),
                                         (tabPlacement == SwingConstants.TOP)
                                         ? direction == SwingConstants.NORTH
                                         : direction == SwingConstants.SOUTH);
            selectAdjacentRunTab(tabPlacement, tabPane.getSelectedIndex(),
                                 offset);
          }
      }
    if (tabPlacement == SwingConstants.LEFT
        || tabPlacement == SwingConstants.RIGHT)
      {
        if (direction == SwingConstants.NORTH)
          selectPreviousTabInRun(tabPane.getSelectedIndex());
        else if (direction == SwingConstants.SOUTH)
          selectNextTabInRun(tabPane.getSelectedIndex());
        else
          {
            int offset = getTabRunOffset(tabPlacement, tabPane.getTabCount(),
                                         tabPane.getSelectedIndex(),
                                         (tabPlacement == SwingConstants.LEFT)
                                         ? direction == SwingConstants.WEST
                                         : direction == SwingConstants.EAST);
            selectAdjacentRunTab(tabPlacement, tabPane.getSelectedIndex(),
                                 offset);
          }
      }
  }

  /**
   * This method selects the next tab in the run.
   *
   * @param current The current selected index.
   */
  protected void selectNextTabInRun(int current)
  {
    current = getNextTabIndexInRun(tabPane.getTabCount(),
                                   current);

    if (tabPane.getTabLayoutPolicy() == JTabbedPane.SCROLL_TAB_LAYOUT)
      scrollTab(current, tabPane.getTabPlacement());

    tabPane.setSelectedIndex(current);
  }

  /**
   * This method selects the previous tab in the run.
   *
   * @param current The current selected index.
   */
  protected void selectPreviousTabInRun(int current)
  {
    current = getPreviousTabIndexInRun(tabPane.getTabCount(),
                                       current);

    if (tabPane.getTabLayoutPolicy() == JTabbedPane.SCROLL_TAB_LAYOUT)
      scrollTab(current, tabPane.getTabPlacement());

    tabPane.setSelectedIndex(current);
  }

  /**
   * This method selects the next tab (regardless of runs).
   *
   * @param current The current selected index.
   */
  protected void selectNextTab(int current)
  {
    current = getNextTabIndex(current);

    if (tabPane.getTabLayoutPolicy() == JTabbedPane.SCROLL_TAB_LAYOUT)
      scrollTab(current, tabPane.getTabPlacement());

    tabPane.setSelectedIndex(current);
  }

  /**
   * This method selects the previous tab (regardless of runs).
   *
   * @param current The current selected index.
   */
  protected void selectPreviousTab(int current)
  {
    current = getPreviousTabIndex(current);

    if (tabPane.getTabLayoutPolicy() == JTabbedPane.SCROLL_TAB_LAYOUT)
      scrollTab(current, tabPane.getTabPlacement());

    tabPane.setSelectedIndex(current);
  }

  /**
   * This method selects the correct tab given an offset from the current tab
   * index. If the tab placement is TOP or BOTTOM, the offset will be in the
   * y direction, otherwise, it will be in the x direction. A new coordinate
   * will be found by adding the offset to the current location of the tab.
   * The tab that the new location will be selected.
   *
   * @param tabPlacement The JTabbedPane's tab placement.
   * @param tabIndex The tab to start from.
   * @param offset The coordinate offset.
   */
  protected void selectAdjacentRunTab(int tabPlacement, int tabIndex,
                                      int offset)
  {
    int x = rects[tabIndex].x + rects[tabIndex].width / 2;
    int y = rects[tabIndex].y + rects[tabIndex].height / 2;

    switch (tabPlacement)
    {
    case SwingConstants.TOP:
    case SwingConstants.BOTTOM:
      y += offset;
      break;
    case SwingConstants.RIGHT:
    case SwingConstants.LEFT:
      x += offset;
      break;
    }

    int index = tabForCoordinate(tabPane, x, y);
    if (index != -1)
      {
        if (tabPane.getTabLayoutPolicy() == JTabbedPane.SCROLL_TAB_LAYOUT)
          scrollTab(index, tabPlacement);
        tabPane.setSelectedIndex(index);
      }
  }

  // This method is called when you press up/down to cycle through tab runs.
  // it returns the distance (between the two runs' x/y position.
  // where one run is the current selected run and the other run is the run in the
  // direction of the scroll (dictated by the forward flag)
  // the offset is an absolute value of the difference

  /**
   * This method calculates the offset distance for use in
   * selectAdjacentRunTab. The offset returned will be a difference in the y
   * coordinate between the run in  the desired direction and the current run
   * (for tabPlacement in TOP or BOTTOM). Use x coordinate for LEFT and
   * RIGHT.
   *
   * @param tabPlacement The JTabbedPane's tab placement.
   * @param tabCount The number of tabs.
   * @param tabIndex The starting index.
   * @param forward If forward, the run in the desired direction will be the
   *        next run.
   *
   * @return The offset between the two runs.
   */
  protected int getTabRunOffset(int tabPlacement, int tabCount, int tabIndex,
                                boolean forward)
  {
    int currRun = getRunForTab(tabCount, tabIndex);
    int offset;
    int nextRun = forward ? getNextTabRun(currRun) : getPreviousTabRun(currRun);
    if (tabPlacement == SwingConstants.TOP
        || tabPlacement == SwingConstants.BOTTOM)
      offset = rects[lastTabInRun(tabCount, nextRun)].y
               - rects[lastTabInRun(tabCount, currRun)].y;
    else
      offset = rects[lastTabInRun(tabCount, nextRun)].x
               - rects[lastTabInRun(tabCount, currRun)].x;

    return offset;
  }

  /**
   * This method returns the previous tab index.
   *
   * @param base The index to start from.
   *
   * @return The previous tab index.
   */
  protected int getPreviousTabIndex(int base)
  {
    base--;
    if (base < 0)
      return tabPane.getTabCount() - 1;
    return base;
  }

  /**
   * This method returns the next tab index.
   *
   * @param base The index to start from.
   *
   * @return The next tab index.
   */
  protected int getNextTabIndex(int base)
  {
    base++;
    if (base == tabPane.getTabCount())
      return 0;
    return base;
  }

  /**
   * This method returns the next tab index in the run. If the next index is
   * out of this run, it will return the starting tab index for the run.
   *
   * @param tabCount The number of tabs.
   * @param base The index to start from.
   *
   * @return The next tab index in the run.
   */
  protected int getNextTabIndexInRun(int tabCount, int base)
  {
    int index = getNextTabIndex(base);
    int run = getRunForTab(tabCount, base);
    if (base == lastTabInRun(tabCount, run))
      index = (run > 0)
              ? lastTabInRun(tabCount, getPreviousTabRun(run)) + 1
              : 0;

    return index;
  }

  /**
   * This method returns the previous tab index in the run. If the previous
   * index is out of this run, it will return the last index for the run.
   *
   * @param tabCount The number of tabs.
   * @param base The index to start from.
   *
   * @return The previous tab index in the run.
   */
  protected int getPreviousTabIndexInRun(int tabCount, int base)
  {
    int index = getPreviousTabIndex(base);
    int run = getRunForTab(tabCount, base);
    if (index == lastTabInRun(tabCount, getPreviousTabRun(run)))
      index = lastTabInRun(tabCount, run);

    return index;
  }

  /**
   * This method returns the index of the previous run.
   *
   * @param baseRun The run to start from.
   *
   * @return The index of the previous run.
   */
  protected int getPreviousTabRun(int baseRun)
  {
    if (getTabRunCount(tabPane) == 1)
      return 1;

    int prevRun = --baseRun;
    if (prevRun < 0)
      prevRun = getTabRunCount(tabPane) - 1;
    return prevRun;
  }

  /**
   * This method returns the index of the next run.
   *
   * @param baseRun The run to start from.
   *
   * @return The index of the next run.
   */
  protected int getNextTabRun(int baseRun)
  {
    if (getTabRunCount(tabPane) == 1)
      return 1;

    int nextRun = ++baseRun;
    if (nextRun == getTabRunCount(tabPane))
      nextRun = 0;
    return nextRun;
  }

  /**
   * This method rotates the insets given a direction to rotate them in.
   * Target placement should be one of TOP, LEFT, BOTTOM, RIGHT. The  rotated
   * insets will be stored in targetInsets. Passing in TOP as  the direction
   * does nothing. Passing in LEFT switches top and left, right and bottom.
   * Passing in BOTTOM switches top and bottom. Passing in RIGHT switches top
   * for left, left for bottom, bottom for right, and right for top.
   *
   * @param topInsets The reference insets.
   * @param targetInsets An Insets object to store the new insets.
   * @param targetPlacement The rotation direction.
   */
  protected static void rotateInsets(Insets topInsets, Insets targetInsets,
                                     int targetPlacement)
  {
    // Sun's version will happily throw an NPE if params are null,
    // so I won't check it either.
    switch (targetPlacement)
    {
    default:
    case SwingConstants.TOP:
      targetInsets.top = topInsets.top;
      targetInsets.left = topInsets.left;
      targetInsets.right = topInsets.right;
      targetInsets.bottom = topInsets.bottom;
      break;
    case SwingConstants.LEFT:
      targetInsets.left = topInsets.top;
      targetInsets.top = topInsets.left;
      targetInsets.right = topInsets.bottom;
      targetInsets.bottom = topInsets.right;
      break;
    case SwingConstants.BOTTOM:
      targetInsets.top = topInsets.bottom;
      targetInsets.bottom = topInsets.top;
      targetInsets.left = topInsets.left;
      targetInsets.right = topInsets.right;
      break;
    case SwingConstants.RIGHT:
      targetInsets.top = topInsets.left;
      targetInsets.left = topInsets.bottom;
      targetInsets.bottom = topInsets.right;
      targetInsets.right = topInsets.top;
      break;
    }
  }

  ActionMap getActionMap()
  {
    ActionMap map = (ActionMap) UIManager.get("TabbedPane.actionMap");

    if (map == null) // first time here
      {
        map = createActionMap();
        if (map != null)
          UIManager.put("TabbedPane.actionMap", map);
      }
    return map;
  }

  ActionMap createActionMap()
  {
    ActionMap map = new ActionMapUIResource();

    map.put("navigatePageDown", new NavigatePageDownAction());
    map.put("navigatePageUp", new NavigatePageUpAction());
    map.put("navigateDown",
            new NavigateAction("navigateDown", SwingConstants.SOUTH));

    map.put("navigateUp",
            new NavigateAction("navigateUp", SwingConstants.NORTH));

    map.put("navigateLeft",
            new NavigateAction("navigateLeft", SwingConstants.WEST));

    map.put("navigateRight",
            new NavigateAction("navigateRight", SwingConstants.EAST));

    map.put("requestFocusForVisibleComponent",
            new RequestFocusForVisibleComponentAction());
    map.put("requestFocus", new RequestFocusAction());

    return map;
  }

  /**
   * Sets the tab which should be highlighted when in rollover mode. And
   * <code>index</code> of <code>-1</code> means that the rollover tab
   * is deselected (i.e. the mouse is outside of the tabarea).
   *
   * @param index the index of the tab that is under the mouse, <code>-1</code>
   *        for no tab
   *
   * @since 1.5
   */
  protected void setRolloverTab(int index)
  {
    rolloverTab = index;
  }

  /**
   * Retunrs the index of the tab over which the mouse is currently moving,
   * or <code>-1</code> for no tab.
   *
   * @return the index of the tab over which the mouse is currently moving,
   *         or <code>-1</code> for no tab
   *
   * @since 1.5
   */
  protected int getRolloverTab()
  {
    return rolloverTab;
  }
}
