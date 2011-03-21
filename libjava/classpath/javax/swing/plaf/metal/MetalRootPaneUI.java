/* MetalRootPaneUI.java
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
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.LayoutManager2;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JLayeredPane;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JRootPane;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.border.AbstractBorder;
import javax.swing.event.MouseInputAdapter;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicRootPaneUI;

/**
 * A UI delegate for the {@link JRootPane} component. This implementation
 * supports the JRootPane <code>windowDecorationStyle</code> property.
 *
 * @author Roman Kennke (kennke@aicas.com)
 *
 * @since 1.4
 */
public class MetalRootPaneUI
  extends BasicRootPaneUI
{

  /**
   * The border that is used on JRootPane when the windowDecorationStyle
   * property of the JRootPane is set to a different value than NONE.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  private static class MetalFrameBorder
    extends AbstractBorder
  {
    /**
     * Returns the border insets.
     *
     * @param c the component
     * @param newInsets the insets to be filled with the return value, may be
     *        <code>null</code> in which case a new object is created
     *
     * @return the border insets
     */
    public Insets getBorderInsets(Component c, Insets newInsets)
    {
      if (newInsets == null)
        newInsets = new Insets(5, 5, 5, 5);
      else
        {
          newInsets.top = 5;
          newInsets.left = 5;
          newInsets.bottom = 5;
          newInsets.right = 5;
        }
      return newInsets;
    }

    /**
     * Returns the border insets.
     *
     * @param c the component
     *
     * @return the border insets
     */
    public Insets getBorderInsets(Component c)
    {
      return getBorderInsets(c, null);
    }

    /**
     * Paints the border for the specified component.
     *
     * @param c  the component
     * @param g  the graphics device
     * @param x  the x-coordinate
     * @param y  the y-coordinate
     * @param w  the width
     * @param h  the height
     */
    public void paintBorder(Component c, Graphics g, int x, int y, int w,
                            int h)
    {
      JRootPane f = (JRootPane) c;
      Window frame = SwingUtilities.getWindowAncestor(f);
      if (frame.isActive())
        g.setColor(MetalLookAndFeel.getPrimaryControlDarkShadow());
      else
        g.setColor(MetalLookAndFeel.getControlDarkShadow());

      // Fill the border background.
      g.fillRect(x, y, w, 5);
      g.fillRect(x, y, 5, h);
      g.fillRect(x + w - 5, y, 5, h);
      g.fillRect(x, y + h - 5, w, 5);

      // Draw a dot in each corner.
      g.setColor(MetalLookAndFeel.getControl());
      g.fillRect(x, y, 1, 1);
      g.fillRect(x + w - 1, y, 1, 1);
      g.fillRect(x + w - 1, y + h - 1, 1, 1);
      g.fillRect(x, y + h - 1, 1, 1);

      // Draw the lines.
      g.setColor(MetalLookAndFeel.getBlack());
      g.drawLine(x + 14, y + 2, x + w - 15, y + 2);
      g.drawLine(x + 14, y + h - 3, x + w - 15, y + h - 3);
      g.drawLine(x + 2, y + 14, x + 2, y + h - 15);
      g.drawLine(x + w - 3, y + 14, x + w - 3, y + h - 15);

      // Draw the line highlights.
      if (frame.isActive())
        g.setColor(MetalLookAndFeel.getPrimaryControlShadow());
      else
        g.setColor(MetalLookAndFeel.getControlShadow());
      g.drawLine(x + 15, y + 3, x + w - 14, y + 3);
      g.drawLine(x + 15, y + h - 2, x + w - 14, y + h - 2);
      g.drawLine(x + 3, y + 15, x + 3, y + h - 14);
      g.drawLine(x + w - 2, y + 15, x + w - 2, y + h - 14);
    }
  }

  /**
   * The component that renders the title bar for frames. This duplicates
   * most of {@link MetalInternalFrameTitlePane}. It is not reasonably possible
   * to reuse that class because that is bound to the JInternalFrame and we
   * need to handle JFrames/JRootPanes here.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  private static class MetalTitlePane extends JComponent
  {

    /**
     * Handles dragging of the title pane and moves the window accordingly.
     */
    private class MouseHandler
      extends MouseInputAdapter
    {
      /**
       * The point where the dragging started.
       */
      Point lastDragLocation;

      /**
       * Receives notification when the mouse gets pressed on the title pane.
       * This updates the lastDragLocation.
       *
       * @param ev the mouse event
       */
      public void mousePressed(MouseEvent ev)
      {
        lastDragLocation = ev.getPoint();
      }

      /**
       * Receives notification when the mouse is dragged on the title pane.
       * This will move the nearest window accordingly.
       *
       * @param ev the mouse event
       */
      public void mouseDragged(MouseEvent ev)
      {
        Point dragLocation = ev.getPoint();
        int deltaX = dragLocation.x - lastDragLocation.x;
        int deltaY = dragLocation.y - lastDragLocation.y;
        Window window = SwingUtilities.getWindowAncestor(rootPane);
        Point loc = window.getLocation();
        window.setLocation(loc.x + deltaX, loc.y + deltaY);
        // Note that we do not update the lastDragLocation. This is because
        // we move the underlying window while dragging the component, which
        // results in having the same lastDragLocation under the mouse while
        // dragging.
      }
    }

    /**
     * The Action responsible for closing the JInternalFrame.
     */
    private class CloseAction extends AbstractAction
    {
      /**
       * Creates a new action.
       */
      public CloseAction()
      {
        super("Close");
      }

      /**
       * This method is called when something closes the frame.
       *
       * @param e the ActionEvent
       */
      public void actionPerformed(ActionEvent e)
      {
        Window frame = SwingUtilities.getWindowAncestor(rootPane);
        if (frame instanceof JFrame)
          {
            JFrame jframe = (JFrame) frame;
            switch (jframe.getDefaultCloseOperation())
            {
              case JFrame.EXIT_ON_CLOSE:
                jframe.setVisible(false);
                jframe.dispose();
                System.exit(0);
                break;
              case JFrame.DISPOSE_ON_CLOSE:
                jframe.setVisible(false);
                jframe.dispose();
                break;
              case JFrame.HIDE_ON_CLOSE:
                jframe.setVisible(false);
                break;
              case JFrame.DO_NOTHING_ON_CLOSE:
              default:
                  break;
            }
          }
        else if (frame instanceof JDialog)
          {
            JDialog jdialog = (JDialog) frame;
            switch (jdialog.getDefaultCloseOperation())
            {
              case JFrame.DISPOSE_ON_CLOSE:
                jdialog.setVisible(false);
                jdialog.dispose();
                break;
              case JFrame.HIDE_ON_CLOSE:
                jdialog.setVisible(false);
                break;
              case JFrame.DO_NOTHING_ON_CLOSE:
              default:
                  break;
            }
          }
      }
    }

    /**
     * This action is performed when the iconify button is pressed.
     */
    private class IconifyAction
      extends AbstractAction
    {

      public void actionPerformed(ActionEvent event)
      {
        Window w = SwingUtilities.getWindowAncestor(rootPane);
        if (w instanceof Frame)
          {
            Frame f = (Frame) w;
            int state = f.getExtendedState();
            f.setExtendedState(Frame.ICONIFIED);
          }
      }

    }

    /**
     * This action is performed when the maximize button is pressed.
     */
    private class MaximizeAction
      extends AbstractAction
    {

      public void actionPerformed(ActionEvent event)
      {
        Window w = SwingUtilities.getWindowAncestor(rootPane);
        if (w instanceof Frame)
          {
            Frame f = (Frame) w;
            int state = f.getExtendedState();
            f.setExtendedState(Frame.MAXIMIZED_BOTH);
          }
      }
    }

    /**
     * This helper class is used to create the minimize, maximize and close
     * buttons in the top right corner of the Title Pane. These buttons are
     * special since they cannot be given focus and have no border.
     */
    private class PaneButton extends JButton
    {
      /**
       * Creates a new PaneButton object with the given Action.
       *
       * @param a The Action that the button uses.
       */
      public PaneButton(Action a)
      {
        super(a);
        setMargin(new Insets(0, 0, 0, 0));
      }

      /**
       * This method returns true if the Component can be focused.
       *
       * @return false.
       */
      public boolean isFocusable()
      {
        // These buttons cannot be given focus.
        return false;
      }

    }

    /**
     * The layout for the JRootPane when the <code>windowDecorationStyle</code>
     * property is set. In addition to the usual JRootPane.RootLayout behaviour
     * this lays out the titlePane.
     *
     * @author Roman Kennke (kennke@aicas.com)
     */
    private class MetalTitlePaneLayout implements LayoutManager
    {
      /**
       * Creates a new <code>TitlePaneLayout</code> object.
       */
      public MetalTitlePaneLayout()
      {
        // Do nothing.
      }

      /**
       * Adds a Component to the Container.
       *
       * @param name The name to reference the added Component by.
       * @param c The Component to add.
       */
      public void addLayoutComponent(String name, Component c)
      {
        // Do nothing.
      }

      /**
       * This method is called to lay out the children of the Title Pane.
       *
       * @param c The Container to lay out.
       */
      public void layoutContainer(Container c)
      {

        Dimension size = c.getSize();
        Insets insets = c.getInsets();
        int width = size.width - insets.left - insets.right;
        int height = size.height - insets.top - insets.bottom;

        int loc = width - insets.right - 1;
        int top = insets.top + 2;
        int buttonHeight = height - 4;
        if (closeButton.isVisible())
          {
            int buttonWidth = closeIcon.getIconWidth();
            loc -= buttonWidth + 2;
            closeButton.setBounds(loc, top, buttonWidth, buttonHeight);
            loc -= 6;
          }

        if (maxButton.isVisible())
          {
            int buttonWidth = maxIcon.getIconWidth();
            loc -= buttonWidth + 4;
            maxButton.setBounds(loc, top, buttonWidth, buttonHeight);
          }

        if (iconButton.isVisible())
          {
            int buttonWidth = minIcon.getIconWidth();
            loc -= buttonWidth + 4;
            iconButton.setBounds(loc, top, buttonWidth, buttonHeight);
            loc -= 2;
          }

        Dimension titlePreferredSize = title.getPreferredSize();
        title.setBounds(insets.left + 5, insets.top,
                Math.min(titlePreferredSize.width, loc - insets.left - 10),
                height);

      }

      /**
       * This method returns the minimum size of the given Container given the
       * children that it has.
       *
       * @param c The Container to get a minimum size for.
       *
       * @return The minimum size of the Container.
       */
      public Dimension minimumLayoutSize(Container c)
      {
        return preferredLayoutSize(c);
      }

      /**
       * Returns the preferred size of the given Container taking
       * into account the children that it has.
       *
       * @param c The Container to lay out.
       *
       * @return The preferred size of the Container.
       */
      public Dimension preferredLayoutSize(Container c)
      {
        return new Dimension(22, 22);
      }

      /**
       * Removes a Component from the Container.
       *
       * @param c The Component to remove.
       */
      public void removeLayoutComponent(Component c)
      {
        // Nothing to do here.
      }
    }

    JRootPane rootPane;

    /** The button that closes the JInternalFrame. */
    JButton closeButton;

    /** The button that iconifies the JInternalFrame. */
    JButton iconButton;

    /** The button that maximizes the JInternalFrame. */
    JButton maxButton;

    Icon minIcon;

    /** The icon displayed in the maximize button. */
    Icon maxIcon;

    /** The icon displayed in the iconify button. */
    private Icon iconIcon;

    /** The icon displayed in the close button. */
    Icon closeIcon;

    /**
     * The background color of the TitlePane when the JInternalFrame is not
     * selected.
     */
    private Color notSelectedTitleColor;

    /**
     * The background color of the TitlePane when the JInternalFrame is
     * selected.
     */
    private Color selectedTitleColor;

    /**
     * The label used to display the title. This label is not added to the
     * TitlePane.
     */
    JLabel title;

    /** The action associated with closing the JInternalFrame. */
    private Action closeAction;

    /** The action associated with iconifying the JInternalFrame. */
    private Action iconifyAction;

    /** The action associated with maximizing the JInternalFrame. */
    private Action maximizeAction;

    /** The JMenuBar that is located at the top left of the Title Pane. */
    private JMenuBar menuBar;

    /** The JMenu inside the menuBar. */
    protected JMenu windowMenu;

    MetalTitlePane(JRootPane rp)
    {
      rootPane = rp;
      setLayout(createLayout());
      title = new JLabel();
      title.setHorizontalAlignment(SwingConstants.LEFT);
      title.setHorizontalTextPosition(SwingConstants.LEFT);
      title.setOpaque(false);
      installTitlePane();
    }

    protected LayoutManager createLayout()
    {
      return new MetalTitlePaneLayout();
    }

    /**
     * This method installs the TitlePane onto the JInternalFrameTitlePane. It
     * also creates any children components that need to be created and adds
     * listeners to the appropriate components.
     */
    protected void installTitlePane()
    {
      installDefaults();
      installListeners();
      createActions();
      assembleSystemMenu();
      createButtons();
      setButtonIcons();
      addSubComponents();
      enableActions();
    }

    private void enableActions()
    {
      // TODO: Implement this.
    }

    private void addSubComponents()
    {
      add(menuBar);
      add(closeButton);
      add(iconButton);
      add(maxButton);
    }

    private void installListeners()
    {
      MouseInputAdapter mouseHandler = new MouseHandler();
      addMouseListener(mouseHandler);
      addMouseMotionListener(mouseHandler);
    }

    private void createActions()
    {
      closeAction = new CloseAction();
      iconifyAction = new IconifyAction();
      maximizeAction = new MaximizeAction();
    }

    private void assembleSystemMenu()
    {
      menuBar = createSystemMenuBar();
      windowMenu = createSystemMenu();
      menuBar.add(windowMenu);
      addSystemMenuItems(windowMenu);
      enableActions();
    }

    protected JMenuBar createSystemMenuBar()
    {
      if (menuBar == null)
        menuBar = new JMenuBar();
      menuBar.removeAll();
      return menuBar;
    }

    protected JMenu createSystemMenu()
    {
      if (windowMenu == null)
        windowMenu = new JMenu();
      windowMenu.removeAll();
      return windowMenu;
    }

    private void addSystemMenuItems(JMenu menu)
    {
      // TODO: Implement this.
    }

    protected void createButtons()
    {
      closeButton = new PaneButton(closeAction);
      closeButton.setText(null);
      iconButton = new PaneButton(iconifyAction);
      iconButton.setText(null);
      maxButton = new PaneButton(maximizeAction);
      maxButton.setText(null);
      closeButton.setBorderPainted(false);
      closeButton.setContentAreaFilled(false);
      iconButton.setBorderPainted(false);
      iconButton.setContentAreaFilled(false);
      maxButton.setBorderPainted(false);
      maxButton.setContentAreaFilled(false);
    }

    protected void setButtonIcons()
    {
      if (closeIcon != null && closeButton != null)
        closeButton.setIcon(closeIcon);
      if (iconIcon != null && iconButton != null)
        iconButton.setIcon(iconIcon);
      if (maxIcon != null && maxButton != null)
        maxButton.setIcon(maxIcon);
    }

    /**
     * Paints a representation of the current state of the internal frame.
     *
     * @param g  the graphics device.
     */
    public void paintComponent(Graphics g)
    {
      Window frame = SwingUtilities.getWindowAncestor(rootPane);
      Color savedColor = g.getColor();
      paintTitleBackground(g);
      paintChildren(g);
      Dimension d = getSize();
      if (frame.isActive())
        g.setColor(MetalLookAndFeel.getPrimaryControlDarkShadow());
      else
        g.setColor(MetalLookAndFeel.getControlDarkShadow());

      // put a dot in each of the top corners
      g.drawLine(0, 0, 0, 0);
      g.drawLine(d.width - 1, 0, d.width - 1, 0);

      g.drawLine(0, d.height - 1, d.width - 1, d.height - 1);

      // draw the metal pattern
      if (UIManager.get("InternalFrame.activeTitleGradient") != null
          && frame.isActive())
        {
          MetalUtils.paintGradient(g, 0, 0, getWidth(), getHeight(),
                                   SwingConstants.VERTICAL,
          "InternalFrame.activeTitleGradient");
        }

      Rectangle b = title.getBounds();
      int startX = b.x + b.width + 5;
      int endX = startX;
      if (iconButton.isVisible())
        endX = Math.max(iconButton.getX(), endX);
      else if (maxButton.isVisible())
        endX = Math.max(maxButton.getX(), endX);
      else if (closeButton.isVisible())
        endX = Math.max(closeButton.getX(), endX);
      endX -= 7;
      if (endX > startX)
        MetalUtils.fillMetalPattern(this, g, startX, 3, endX - startX, getHeight() - 6, Color.white, Color.gray);
      g.setColor(savedColor);
    }

    /**
     * This method paints the TitlePane's background.
     *
     * @param g The Graphics object to paint with.
     */
    protected void paintTitleBackground(Graphics g)
    {
      Window frame = SwingUtilities.getWindowAncestor(rootPane);

      if (!isOpaque())
        return;

      Color saved = g.getColor();
      Dimension dims = getSize();

      Color bg = getBackground();
      if (frame.isActive())
        bg = selectedTitleColor;
      else
        bg = notSelectedTitleColor;
      g.setColor(bg);
      g.fillRect(0, 0, dims.width, dims.height);
      g.setColor(saved);
    }

    /**
     * This method installs the defaults determined by the look and feel.
     */
    private void installDefaults()
    {
      title.setFont(UIManager.getFont("InternalFrame.titleFont"));
      selectedTitleColor = UIManager.getColor("InternalFrame.activeTitleBackground");
      notSelectedTitleColor = UIManager.getColor("InternalFrame.inactiveTitleBackground");
      closeIcon = UIManager.getIcon("InternalFrame.closeIcon");
      iconIcon = UIManager.getIcon("InternalFrame.iconifyIcon");
      maxIcon = UIManager.getIcon("InternalFrame.maximizeIcon");
      minIcon = MetalIconFactory.getInternalFrameAltMaximizeIcon(16);
      Frame frame = (Frame) SwingUtilities.getWindowAncestor(rootPane);
      title = new JLabel(frame.getTitle(),
              MetalIconFactory.getInternalFrameDefaultMenuIcon(),
              SwingConstants.LEFT);
    }
  }

  private static class MetalRootLayout
    implements LayoutManager2
  {

    /**
     * The cached layout info for the glass pane.
     */
    private Rectangle glassPaneBounds;

    /**
     * The cached layout info for the layered pane.
     */
    private Rectangle layeredPaneBounds;

    /**
     * The cached layout info for the content pane.
     */
    private Rectangle contentPaneBounds;

    /**
     * The cached layout info for the menu bar.
     */
    private Rectangle menuBarBounds;

    /**
     * The cached layout info for the title pane.
     */
    private Rectangle titlePaneBounds;

    /**
     * The cached preferred size.
     */
    private Dimension prefSize;

    /**
     * The title pane for l&f decorated frames.
     */
    private MetalTitlePane titlePane;

    /**
     * Creates a new MetalRootLayout.
     *
     * @param tp the title pane
     */
    MetalRootLayout(MetalTitlePane tp)
    {
      titlePane = tp;
    }

    public void addLayoutComponent(Component component, Object constraints)
    {
      // Nothing to do here.
    }

    public Dimension maximumLayoutSize(Container target)
    {
      return preferredLayoutSize(target);
    }

    public float getLayoutAlignmentX(Container target)
    {
      return 0.0F;
    }

    public float getLayoutAlignmentY(Container target)
    {
      return 0.0F;
    }

    public void invalidateLayout(Container target)
    {
      synchronized (this)
      {
        glassPaneBounds = null;
        layeredPaneBounds = null;
        contentPaneBounds = null;
        menuBarBounds = null;
        titlePaneBounds = null;
        prefSize = null;
      }
    }

    public void addLayoutComponent(String name, Component component)
    {
      // Nothing to do here.
    }

    public void removeLayoutComponent(Component component)
    {
      // TODO Auto-generated method stub

    }

    public Dimension preferredLayoutSize(Container parent)
    {
      JRootPane rp = (JRootPane) parent;
      JLayeredPane layeredPane = rp.getLayeredPane();
      Component contentPane = rp.getContentPane();
      Component menuBar = rp.getJMenuBar();

      // We must synchronize here, otherwise we cannot guarantee that the
      // prefSize is still non-null when returning.
      synchronized (this)
        {
          if (prefSize == null)
            {
              Insets i = parent.getInsets();
              prefSize = new Dimension(i.left + i.right, i.top + i.bottom);
              Dimension contentPrefSize = contentPane.getPreferredSize();
              prefSize.width += contentPrefSize.width;
              prefSize.height += contentPrefSize.height
                                 + titlePane.getPreferredSize().height;
              if (menuBar != null)
                {
                  Dimension menuBarSize = menuBar.getPreferredSize();
                  if (menuBarSize.width > contentPrefSize.width)
                    prefSize.width += menuBarSize.width - contentPrefSize.width;
                  prefSize.height += menuBarSize.height;
                }
            }
          // Return a copy here so the cached value won't get trashed by some
          // other component.
          return new Dimension(prefSize);
      }
    }

    public Dimension minimumLayoutSize(Container parent)
    {
      return preferredLayoutSize(parent);
    }

    public void layoutContainer(Container parent)
    {
      JRootPane rp = (JRootPane) parent;
      JLayeredPane layeredPane = rp.getLayeredPane();
      Component contentPane = rp.getContentPane();
      Component menuBar = rp.getJMenuBar();
      Component glassPane = rp.getGlassPane();

      if (glassPaneBounds == null || layeredPaneBounds == null
          || contentPaneBounds == null || menuBarBounds == null)
        {
          Insets i = rp.getInsets();
          int containerWidth = parent.getBounds().width - i.left - i.right;
          int containerHeight = parent.getBounds().height - i.top - i.bottom;

          // 1. The glassPane fills entire viewable region (bounds - insets).
          // 2. The layeredPane filles entire viewable region.
          // 3. The titlePane is placed at the upper edge of the layeredPane.
          // 4. The menuBar is positioned at the upper edge of layeredPane.
          // 5. The contentPane fills viewable region minus menuBar minus
          //    titlePane, if present.

          // +-------------------------------+
          // |  JLayeredPane                 |
          // |  +--------------------------+ |
          // |  | titlePane                + |
          // |  +--------------------------+ |
          // |  +--------------------------+ |
          // |  | menuBar                  | |
          // |  +--------------------------+ |
          // |  +--------------------------+ |
          // |  |contentPane               | |
          // |  |                          | |
          // |  |                          | |
          // |  |                          | |
          // |  +--------------------------+ |
          // +-------------------------------+

          // Setup titlePaneBounds.
          if (titlePaneBounds == null)
            titlePaneBounds = new Rectangle();
          titlePaneBounds.width = containerWidth;
          titlePaneBounds.height = titlePane.getPreferredSize().height;

          // Setup menuBarBounds.
          if (menuBarBounds == null)
            menuBarBounds = new Rectangle();
          menuBarBounds.setBounds(0,
                                  titlePaneBounds.y + titlePaneBounds.height,
                                  containerWidth, 0);
          if (menuBar != null)
            {
              Dimension menuBarSize = menuBar.getPreferredSize();
              if (menuBarSize.height > containerHeight)
                menuBarBounds.height = containerHeight;
              else
                menuBarBounds.height = menuBarSize.height;
            }

          // Setup contentPaneBounds.
          if (contentPaneBounds == null)
            contentPaneBounds = new Rectangle();
          contentPaneBounds.setBounds(0,
                                      menuBarBounds.y + menuBarBounds.height,
                                      containerWidth,
                                      containerHeight - menuBarBounds.y
                                      - menuBarBounds.height);
          glassPaneBounds = new Rectangle(i.left, i.top, containerWidth, containerHeight);
          layeredPaneBounds = new Rectangle(i.left, i.top, containerWidth, containerHeight);
        }

      // Layout components.
      glassPane.setBounds(glassPaneBounds);
      layeredPane.setBounds(layeredPaneBounds);
      if (menuBar != null)
        menuBar.setBounds(menuBarBounds);
      contentPane.setBounds(contentPaneBounds);
      titlePane.setBounds(titlePaneBounds);
    }

  }

  /**
   * The shared UI instance for MetalRootPaneUIs.
   */
  private static MetalRootPaneUI instance;

  /**
   * Constructs a shared instance of <code>MetalRootPaneUI</code>.
   */
  public MetalRootPaneUI()
  {
    super();
  }

  /**
   * Returns a shared instance of <code>MetalRootPaneUI</code>.
   *
   * @param component the component for which we return an UI instance
   *
   * @return A shared instance of <code>MetalRootPaneUI</code>.
   */
  public static ComponentUI createUI(JComponent component)
  {
    if (instance == null)
      instance = new MetalRootPaneUI();
    return instance;
  }

  /**
   * Installs this UI to the root pane. If the
   * <code>windowDecorationsStyle</code> property is set on the root pane,
   * the Metal window decorations are installed on the root pane.
   *
   * @param c
   */
  public void installUI(JComponent c)
  {
    super.installUI(c);
    JRootPane rp = (JRootPane) c;
    if (rp.getWindowDecorationStyle() != JRootPane.NONE)
      installWindowDecorations(rp);
  }

  /**
   * Uninstalls the UI from the root pane. This performs the superclass
   * behaviour and uninstalls the window decorations that have possibly been
   * installed by {@link #installUI}.
   *
   * @param c the root pane
   */
  public void uninstallUI(JComponent c)
  {
    JRootPane rp = (JRootPane) c;
    if (rp.getWindowDecorationStyle() != JRootPane.NONE)
      uninstallWindowDecorations(rp);
    super.uninstallUI(c);
  }

  /**
   * Receives notification if any of the JRootPane's property changes. In
   * particular this catches changes to the <code>windowDecorationStyle</code>
   * property and installs the window decorations accordingly.
   *
   * @param ev the property change event
   */
  public void propertyChange(PropertyChangeEvent ev)
  {
    super.propertyChange(ev);
    String propertyName = ev.getPropertyName();
    if (propertyName.equals("windowDecorationStyle"))
      {
        JRootPane rp = (JRootPane) ev.getSource();
        if (rp.getWindowDecorationStyle() != JRootPane.NONE)
          installWindowDecorations(rp);
        else
          uninstallWindowDecorations(rp);
      }
  }

  /**
   * Installs the window decorations to the root pane. This sets up a border,
   * a title pane and a layout manager that can layout the root pane with that
   * title pane.
   *
   * @param rp the root pane.
   */
  private void installWindowDecorations(JRootPane rp)
  {
    rp.setBorder(new MetalFrameBorder());
    MetalTitlePane titlePane = new MetalTitlePane(rp);
    rp.setLayout(new MetalRootLayout(titlePane));
    // We should have a contentPane already.
    assert rp.getLayeredPane().getComponentCount() > 0
           : "We should have a contentPane already";

    rp.getLayeredPane().add(titlePane,
                            JLayeredPane.FRAME_CONTENT_LAYER, 1);
  }

  /**
   * Uninstalls the window decorations from the root pane. This should rarely
   * be necessary, but we do it anyway.
   *
   * @param rp the root pane
   */
  private void uninstallWindowDecorations(JRootPane rp)
  {
    rp.setBorder(null);
    JLayeredPane lp = rp.getLayeredPane();
    for (int i = lp.getComponentCount() - 1; i >= 0; --i)
      {
        if (lp.getComponent(i) instanceof MetalTitlePane)
          {
            lp.remove(i);
            break;
          }
      }
  }
}
