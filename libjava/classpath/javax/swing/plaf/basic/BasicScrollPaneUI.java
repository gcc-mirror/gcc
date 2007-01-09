/* BasicScrollPaneUI.java
   Copyright (C) 2002, 2004, 2005, 2006, Free Software Foundation, Inc.

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

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ContainerEvent;
import java.awt.event.ContainerListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.AbstractAction;
import javax.swing.ActionMap;
import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JViewport;
import javax.swing.LookAndFeel;
import javax.swing.ScrollPaneConstants;
import javax.swing.ScrollPaneLayout;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.ActionMapUIResource;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.ScrollPaneUI;
import javax.swing.plaf.UIResource;

/**
 * A UI delegate for the {@link JScrollPane} component.
 */
public class BasicScrollPaneUI extends ScrollPaneUI
  implements ScrollPaneConstants
{

  /**
   * Listens for changes in the state of the horizontal scrollbar's model and
   * updates the scrollpane accordingly.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  public class HSBChangeListener implements ChangeListener
  {

    /**
     * Receives notification when the state of the horizontal scrollbar
     * model has changed.
     *
     * @param event the change event
     */
    public void stateChanged(ChangeEvent event)
    {
      JScrollBar hsb = scrollpane.getHorizontalScrollBar();
      JViewport vp = scrollpane.getViewport();
      Point viewPosition = vp.getViewPosition();
      viewPosition.x = hsb.getValue();
      vp.setViewPosition(viewPosition);
    }

  }

  /**
   * Listens for changes in the state of the vertical scrollbar's model and
   * updates the scrollpane accordingly.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  public class VSBChangeListener implements ChangeListener
  {

    /**
     * Receives notification when the state of the vertical scrollbar
     * model has changed.
     *
     * @param event the change event
     */
    public void stateChanged(ChangeEvent event)
    {
      JScrollBar vsb = scrollpane.getVerticalScrollBar();
      JViewport vp = scrollpane.getViewport();
      Point viewPosition = vp.getViewPosition();
      viewPosition.y = vsb.getValue();
      vp.setViewPosition(viewPosition);
    }
 
  }

  /**
   * Listens for changes of the viewport's extent size and updates the
   * scrollpane accordingly.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  public class ViewportChangeHandler implements ChangeListener
  {

    /**
     * Receives notification when the view's size, position or extent size
     * changes. When the extents size has changed, this method calls
     * {@link BasicScrollPaneUI#syncScrollPaneWithViewport()} to adjust the
     * scrollbars extents as well.
     * 
     * @param event the change event
     */
    public void stateChanged(ChangeEvent event)
    {
      syncScrollPaneWithViewport();
    }

  }

  /**
   * Listens for property changes on the scrollpane and update the view
   * accordingly.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  public class PropertyChangeHandler implements PropertyChangeListener
  {

    /**
     * Receives notification when any of the scrollpane's bound property
     * changes. This method calls the appropriate update method on the
     * <code>ScrollBarUI</code>.
     *
     * @param e the property change event
     *
     * @see BasicScrollPaneUI#updateColumnHeader(PropertyChangeEvent)
     * @see BasicScrollPaneUI#updateRowHeader(PropertyChangeEvent)
     * @see BasicScrollPaneUI#updateScrollBarDisplayPolicy(PropertyChangeEvent)
     * @see BasicScrollPaneUI#updateViewport(PropertyChangeEvent)
     */
    public void propertyChange(PropertyChangeEvent e)
    {
      String propName = e.getPropertyName();
      if (propName.equals("viewport"))
        updateViewport(e);
      else if (propName.equals("rowHeader"))
        updateRowHeader(e);
      else if (propName.equals("columnHeader"))
        updateColumnHeader(e);
      else if (propName.equals("horizontalScrollBarPolicy")
          || e.getPropertyName().equals("verticalScrollBarPolicy"))
        updateScrollBarDisplayPolicy(e);
      else if (propName.equals("verticalScrollBar"))
        {
          JScrollBar oldSb = (JScrollBar) e.getOldValue();
          oldSb.getModel().removeChangeListener(vsbChangeListener);
          JScrollBar newSb = (JScrollBar) e.getNewValue();
          newSb.getModel().addChangeListener(vsbChangeListener);
        }
      else if (propName.equals("horizontalScrollBar"))
        {
          JScrollBar oldSb = (JScrollBar) e.getOldValue();
          oldSb.getModel().removeChangeListener(hsbChangeListener);
          JScrollBar newSb = (JScrollBar) e.getNewValue();
          newSb.getModel().addChangeListener(hsbChangeListener);
        }
    }

  }

  /**
   * Listens for mouse wheel events and update the scrollpane accordingly.
   *
   * @author Roman Kennke (kennke@aicas.com)
   *
   * @since 1.4
   */
  protected class MouseWheelHandler implements MouseWheelListener
  {
    /**
     * Use to compute the visible rectangle.
     */
    final Rectangle rect = new Rectangle();

    /**
     * Scroll with the mouse wheel.
     * 
     * @author Audrius Meskauskas (audriusa@Bioinformatics.org)
     */
    public void mouseWheelMoved(MouseWheelEvent e)
    {
      if (scrollpane.isWheelScrollingEnabled() && e.getScrollAmount() != 0)
        {
          // Try to scroll vertically first.
          JScrollBar scrollBar = scrollpane.getVerticalScrollBar();
          if (scrollBar == null || ! scrollBar.isVisible())
            scrollBar = scrollpane.getHorizontalScrollBar();
          if (scrollBar != null && scrollBar.isVisible())
            {
              int direction = e.getWheelRotation() < 0 ? -1 : 1;
              int scrollType = e.getScrollType();
              if (scrollType == MouseWheelEvent.WHEEL_UNIT_SCROLL)
                BasicScrollBarUI.scrollByUnits(scrollBar, direction,
                                               e.getScrollAmount());
              else if (scrollType == MouseWheelEvent.WHEEL_BLOCK_SCROLL)
                BasicScrollBarUI.scrollByBlock(scrollBar, direction);
            }
        }
    }
  }
  
  /**
   * Adds/removes the mouse wheel listener when the component is added/removed
   * to/from the scroll pane view port.
   * 
   * @author Audrius Meskauskas (audriusa@bioinformatics.org)
   */
  class ViewportContainerListener implements ContainerListener
  {
    /**
     * Add the mouse wheel listener, allowing to scroll with the mouse.
     */
    public void componentAdded(ContainerEvent e)
    {
      e.getChild().addMouseWheelListener(mouseWheelListener);
    }
    
    /**
     * Remove the mouse wheel listener.
     */
    public void componentRemoved(ContainerEvent e)
    {
      e.getChild().removeMouseWheelListener(mouseWheelListener);
    }
  }
  
  /**
   * The number of pixels by that we should scroll the content that does
   * not implement Scrollable.
   */
  static int SCROLL_NON_SCROLLABLES = 10;
  
  /**
   * The number of rows to scroll per mouse wheel click. From impression,
   * Sun seems using the value 3.
   */
  static int ROWS_PER_WHEEL_CLICK = 3;     

  /** The Scrollpane for which the UI is provided by this class. */
  protected JScrollPane scrollpane;

  /**
   * The horizontal scrollbar listener.
   */
  protected ChangeListener hsbChangeListener;

  /**
   * The vertical scrollbar listener.
   */
  protected ChangeListener vsbChangeListener;

  /**
   * The viewport listener.
   */
  protected ChangeListener viewportChangeListener;

  /**
   * The scrollpane property change listener.
   */
  protected PropertyChangeListener spPropertyChangeListener;

  /**
   * The mousewheel listener for the scrollpane.
   */
  MouseWheelListener mouseWheelListener;
  
  /**
   * The listener to add and remove the mouse wheel listener to/from
   * the component container.
   */
  ContainerListener containerListener;

  public static ComponentUI createUI(final JComponent c) 
  {
    return new BasicScrollPaneUI();
  }

  protected void installDefaults(JScrollPane p)
  {
    scrollpane = p;
    LookAndFeel.installColorsAndFont(p, "ScrollPane.background",
                                     "ScrollPane.foreground",
                                     "ScrollPane.font");
    LookAndFeel.installBorder(p, "ScrollPane.border");

    // Install Viewport border.
    Border vpBorder = p.getViewportBorder();
    if (vpBorder == null || vpBorder instanceof UIResource)
      {
        vpBorder = UIManager.getBorder("ScrollPane.viewportBorder");
        p.setViewportBorder(vpBorder);
      }

    p.setOpaque(true);
  }

  protected void uninstallDefaults(JScrollPane p)
  {
    LookAndFeel.uninstallBorder(p);
    Border vpBorder = p.getViewportBorder();
    if (vpBorder != null && vpBorder instanceof UIResource)
      p.setViewportBorder(null);
  }
    
  public void installUI(final JComponent c) 
  {
    super.installUI(c);
    installDefaults((JScrollPane) c);
    installListeners((JScrollPane) c);
    installKeyboardActions((JScrollPane) c);
  }

  /**
   * Installs the listeners on the scrollbars, the viewport and the scrollpane.
   *
   * @param sp the scrollpane on which to install the listeners
   */
  protected void installListeners(JScrollPane sp)
  {
    if (spPropertyChangeListener == null)
      spPropertyChangeListener = createPropertyChangeListener();
    sp.addPropertyChangeListener(spPropertyChangeListener);

    if (hsbChangeListener == null)
      hsbChangeListener = createHSBChangeListener();
    sp.getHorizontalScrollBar().getModel().addChangeListener(hsbChangeListener);
    
    if (vsbChangeListener == null)
      vsbChangeListener = createVSBChangeListener();
    sp.getVerticalScrollBar().getModel().addChangeListener(vsbChangeListener);

    if (viewportChangeListener == null)
      viewportChangeListener = createViewportChangeListener();
    
    if (mouseWheelListener == null)
      mouseWheelListener = createMouseWheelListener();
    
    if (containerListener == null)
      containerListener = new ViewportContainerListener();
    
    JViewport v = sp.getViewport();
    v.addChangeListener(viewportChangeListener);
    v.addContainerListener(containerListener);
    
    // Add mouse wheel listeners to the componets that are probably already
    // in the view port.
    for (int i = 0; i < v.getComponentCount(); i++)
      v.getComponent(i).addMouseWheelListener(mouseWheelListener);
  }

  InputMap getInputMap(int condition) 
  {
    if (condition == JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT)
      return (InputMap) UIManager.get("ScrollPane.ancestorInputMap");
    return null;
  }

  /**
   * Returns the action map for the {@link JScrollPane}.  All scroll panes 
   * share a single action map which is created the first time this method is 
   * called, then stored in the UIDefaults table for subsequent access.
   * 
   * @return The shared action map.
   */
  ActionMap getActionMap() 
  {
    ActionMap map = (ActionMap) UIManager.get("ScrollPane.actionMap");

    if (map == null) // first time here
      {
        map = createActionMap();
        if (map != null)
          UIManager.put("ScrollPane.actionMap", map);
      }
    return map;
  }

  /**
   * Creates the action map shared by all {@link JSlider} instances.
   * This method is called once by {@link #getActionMap()} when it 
   * finds no action map in the UIDefaults table...after the map is 
   * created, it gets added to the defaults table so that subsequent 
   * calls to {@link #getActionMap()} will return the same shared 
   * instance.
   * 
   * @return The action map.
   */
  ActionMap createActionMap()
  {
    ActionMap map = new ActionMapUIResource();
    map.put("scrollLeft", 
            new AbstractAction("scrollLeft") {
              public void actionPerformed(ActionEvent event)
              {
                JScrollPane sp = (JScrollPane) event.getSource();
                JScrollBar sb = sp.getHorizontalScrollBar();
                if (sb.isVisible()) 
                  {
                    int delta = sb.getBlockIncrement(-1);
                    sb.setValue(sb.getValue() + delta);
                  }
              }
            }
    );
    map.put("scrollEnd", 
            new AbstractAction("scrollEnd") {
              public void actionPerformed(ActionEvent event)
              {
                JScrollPane sp = (JScrollPane) event.getSource();
                JScrollBar sb1 = sp.getHorizontalScrollBar();
                if (sb1.isVisible()) 
                  {
                    sb1.setValue(sb1.getMaximum());
                  }
                JScrollBar sb2 = sp.getVerticalScrollBar();
                if (sb2.isVisible()) 
                  {
                    sb2.setValue(sb2.getMaximum());
                  }
              }
            }
    );
    map.put("unitScrollUp", 
            new AbstractAction("unitScrollUp") {
              public void actionPerformed(ActionEvent event)
              {
                JScrollPane sp = (JScrollPane) event.getSource();
                JScrollBar sb = sp.getVerticalScrollBar();
                if (sb.isVisible()) 
                  {
                    int delta = sb.getUnitIncrement(-1);
                    sb.setValue(sb.getValue() + delta);
                  }
              }
            }
    );
    map.put("unitScrollLeft", 
            new AbstractAction("unitScrollLeft") {
              public void actionPerformed(ActionEvent event)
              {
                JScrollPane sp = (JScrollPane) event.getSource();
                JScrollBar sb = sp.getHorizontalScrollBar();
                if (sb.isVisible()) 
                  {
                    int delta = sb.getUnitIncrement(-1);
                    sb.setValue(sb.getValue() + delta);
                  }
              }
            }
    );
    map.put("scrollUp", 
            new AbstractAction("scrollUp") {
              public void actionPerformed(ActionEvent event)
              {
                JScrollPane sp = (JScrollPane) event.getSource();
                JScrollBar sb = sp.getVerticalScrollBar();
                if (sb.isVisible()) 
                  {
                    int delta = sb.getBlockIncrement(-1);
                    sb.setValue(sb.getValue() + delta);
                  }
              }
            }
    );
    map.put("scrollRight", 
            new AbstractAction("scrollRight") {
              public void actionPerformed(ActionEvent event)
              {
                JScrollPane sp = (JScrollPane) event.getSource();
                JScrollBar sb = sp.getHorizontalScrollBar();
                if (sb.isVisible()) 
                  {
                    int delta = sb.getBlockIncrement(1);
                    sb.setValue(sb.getValue() + delta);
                  }
              }
            }
    );
    map.put("scrollHome", 
            new AbstractAction("scrollHome") {
              public void actionPerformed(ActionEvent event)
              {
                JScrollPane sp = (JScrollPane) event.getSource();
                JScrollBar sb1 = sp.getHorizontalScrollBar();
                if (sb1.isVisible()) 
                  {
                    sb1.setValue(sb1.getMinimum());
                  }
                JScrollBar sb2 = sp.getVerticalScrollBar();
                if (sb2.isVisible()) 
                  {
                    sb2.setValue(sb2.getMinimum());
                  }
              }
            }
    );
    map.put("scrollDown", 
            new AbstractAction("scrollDown") {
              public void actionPerformed(ActionEvent event)
              {
                JScrollPane sp = (JScrollPane) event.getSource();
                JScrollBar sb = sp.getVerticalScrollBar();
                if (sb.isVisible()) 
                  {
                    int delta = sb.getBlockIncrement(1);
                    sb.setValue(sb.getValue() + delta);
                  }
              }
            }
    );
    map.put("unitScrollDown", 
            new AbstractAction("unitScrollDown") {
              public void actionPerformed(ActionEvent event)
              {
                JScrollPane sp = (JScrollPane) event.getSource();
                JScrollBar sb = sp.getVerticalScrollBar();
                if (sb.isVisible()) 
                  {
                    int delta = sb.getUnitIncrement(1);
                    sb.setValue(sb.getValue() + delta);
                  }
              }
            }
    );
    map.put("unitScrollRight", 
            new AbstractAction("unitScrollRight") {
              public void actionPerformed(ActionEvent event)
              {
                JScrollPane sp = (JScrollPane) event.getSource();
                JScrollBar sb = sp.getHorizontalScrollBar();
                if (sb.isVisible()) 
                  {
                    int delta = sb.getUnitIncrement(1);
                    sb.setValue(sb.getValue() + delta);
                  }
              }
            }
    );
    return map;
  }
  
  /**
   * Installs additional keyboard actions on the scrollpane. This is a hook
   * method provided to subclasses in order to install their own keyboard
   * actions.
   *
   * @param sp the scrollpane to install keyboard actions on
   */
  protected void installKeyboardActions(JScrollPane sp)
  {
    InputMap keyMap = getInputMap(
        JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
    SwingUtilities.replaceUIInputMap(sp, 
        JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT, keyMap);
    ActionMap map = getActionMap();
    SwingUtilities.replaceUIActionMap(sp, map);
  }

  /**
   * Uninstalls all keyboard actions from the JScrollPane that have been
   * installed by {@link #installKeyboardActions}. This is a hook method
   * provided to subclasses to add their own keyboard actions.
   *
   * @param sp the scrollpane to uninstall keyboard actions from
   */
  protected void uninstallKeyboardActions(JScrollPane sp)
  {
    SwingUtilities.replaceUIActionMap(sp, null);
    SwingUtilities.replaceUIInputMap(sp, 
        JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT, null);
  }
  
  /**
   * Creates and returns the change listener for the horizontal scrollbar.
   *
   * @return the change listener for the horizontal scrollbar
   */
  protected ChangeListener createHSBChangeListener()
  {
    return new HSBChangeListener();
  }

  /**
   * Creates and returns the change listener for the vertical scrollbar.
   *
   * @return the change listener for the vertical scrollbar
   */
  protected ChangeListener createVSBChangeListener()
  {
    return new VSBChangeListener();
  }

  /**
   * Creates and returns the change listener for the viewport.
   *
   * @return the change listener for the viewport
   */
  protected ChangeListener createViewportChangeListener()
  {
    return new ViewportChangeHandler();
  }

  /**
   * Creates and returns the property change listener for the scrollpane.
   *
   * @return the property change listener for the scrollpane
   */
  protected PropertyChangeListener createPropertyChangeListener()
  {
    return new PropertyChangeHandler();
  }

  /**
   * Creates and returns the mouse wheel listener for the scrollpane.
   *
   * @return the mouse wheel listener for the scrollpane
   * 
   * @since 1.4
   */
  protected MouseWheelListener createMouseWheelListener()
  {
    return new MouseWheelHandler();
  }

  public void uninstallUI(final JComponent c) 
  {
    uninstallDefaults((JScrollPane) c);
    uninstallListeners(c);
    installKeyboardActions((JScrollPane) c);
  }

  /**
   * Uninstalls all the listeners that have been installed in
   * {@link #installListeners(JScrollPane)}.
   *
   * @param c the scrollpane from which to uninstall the listeners 
   */
  protected void uninstallListeners(JComponent c)
  {
    JScrollPane sp = (JScrollPane) c;
    sp.removePropertyChangeListener(spPropertyChangeListener);
    sp.getHorizontalScrollBar().getModel()
                               .removeChangeListener(hsbChangeListener);
    sp.getVerticalScrollBar().getModel()
                             .removeChangeListener(vsbChangeListener);
    
    JViewport v = sp.getViewport();
    v.removeChangeListener(viewportChangeListener);
    v.removeContainerListener(containerListener);
 
    for (int i = 0; i < v.getComponentCount(); i++)
      v.getComponent(i).removeMouseWheelListener(mouseWheelListener);

  }

  public Dimension getMinimumSize(JComponent c) 
  {
    JScrollPane p = (JScrollPane) c;
    ScrollPaneLayout sl = (ScrollPaneLayout) p.getLayout();
    return sl.minimumLayoutSize(c);
  }

  public void paint(Graphics g, JComponent c)
  {
    Border vpBorder = scrollpane.getViewportBorder();
    if (vpBorder != null)
      {
        Rectangle r = scrollpane.getViewportBorderBounds();
        vpBorder.paintBorder(scrollpane, g, r.x, r.y, r.width, r.height);
      }
  }

  /**
   * Synchronizes the scrollbar and header settings positions and extent
   * with the viewport's view position and extent.
   */
  protected void syncScrollPaneWithViewport()
  {
    JViewport vp = scrollpane.getViewport();

    if (vp != null)
      {
        Dimension extentSize = vp.getExtentSize();
        Point viewPos = vp.getViewPosition();
        Dimension viewSize = vp.getViewSize();

        // Update the vertical scrollbar.
        JScrollBar vsb = scrollpane.getVerticalScrollBar();
        if (vsb != null)
          {
            int extent = extentSize.height;
            int max = viewSize.height;
            int val = Math.max(0, Math.min(viewPos.y, max - extent));
            vsb.setValues(val, extent, 0, max);
          }

        // Update the horizontal scrollbar.
        JScrollBar hsb = scrollpane.getHorizontalScrollBar();
        if (hsb != null)
          {
            int extent = extentSize.width;
            int max = viewSize.width;
            int val = Math.max(0, Math.min(viewPos.x, max - extent));
            hsb.setValues(val, extent, 0, max);
          }

        // Update the row header.
        JViewport rowHeader = scrollpane.getRowHeader();
        if (rowHeader != null)
          {
            Point p = new Point(0, viewPos.y);
            rowHeader.setViewPosition(p);
          }

        // Update the column header.
        JViewport colHeader = scrollpane.getColumnHeader();
        if (colHeader != null)
          {
            Point p = new Point(viewPos.x, 0);
            colHeader.setViewPosition(p);
          }
      }
  }

  /**
   * Receives notification when the <code>columnHeader</code> property has
   * changed on the scrollpane.
   *
   * @param ev the property change event
   */
  protected void updateColumnHeader(PropertyChangeEvent ev)
  {
    // TODO: Find out what should be done here. Or is this only a hook?
  }

  /**
   * Receives notification when the <code>rowHeader</code> property has changed
   * on the scrollpane.
   *
   * @param ev the property change event
   */
  protected void updateRowHeader(PropertyChangeEvent ev)
  {
    // TODO: Find out what should be done here. Or is this only a hook?
  }

  /**
   * Receives notification when the <code>scrollBarDisplayPolicy</code>
   * property has changed on the scrollpane.
   *
   * @param ev the property change event
   */
  protected void updateScrollBarDisplayPolicy(PropertyChangeEvent ev)
  {
    scrollpane.revalidate();
    scrollpane.repaint();
  }

  /**
   * Receives notification when the <code>viewport</code> property has changed
   * on the scrollpane.
   *
   * This method sets removes the viewportChangeListener from the old viewport
   * and adds it to the new viewport.
   *
   * @param ev the property change event
   */
  protected void updateViewport(PropertyChangeEvent ev)
  {
    JViewport oldViewport = (JViewport) ev.getOldValue();
    oldViewport.removeChangeListener(viewportChangeListener);
    JViewport newViewport = (JViewport) ev.getNewValue();
    newViewport.addChangeListener(viewportChangeListener);
    syncScrollPaneWithViewport();
  }
}












