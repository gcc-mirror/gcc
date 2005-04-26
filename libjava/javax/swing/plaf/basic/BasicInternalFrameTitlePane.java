/* BasicInternalFrameTitlePane.java --
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyVetoException;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIDefaults;
import javax.swing.UIManager;

/**
 * This class acts as a titlebar for JInternalFrames.
 */
public class BasicInternalFrameTitlePane extends JComponent
{
  /**
   * The Action responsible for closing the JInternalFrame.
   */
  public class CloseAction extends AbstractAction
  {
    /**
     * This method is called when something closes the JInternalFrame.
     *
     * @param e The ActionEvent.
     */
    public void actionPerformed(ActionEvent e)
    {
      if (frame.isClosable())
        {
	  try
	    {
	      frame.setClosed(true);
	    }
	  catch (PropertyVetoException pve)
	    {
	    }
        }
    }
  }

  /**
   * This Action is responsible for iconifying the JInternalFrame.
   */
  public class IconifyAction extends AbstractAction
  {
    /**
     * This method is called when the user wants to iconify the
     * JInternalFrame.
     *
     * @param e The ActionEvent.
     */
    public void actionPerformed(ActionEvent e)
    {
      if (frame.isIconifiable() && ! frame.isIcon())
        {
	  try
	    {
	      frame.setIcon(true);
	    }
	  catch (PropertyVetoException pve)
	    {
	    }
        }
    }
  }

  /**
   * This Action is responsible for maximizing the JInternalFrame.
   */
  public class MaximizeAction extends AbstractAction
  {
    /**
     * This method is called when the user wants to maximize the
     * JInternalFrame.
     *
     * @param e The ActionEvent.
     */
    public void actionPerformed(ActionEvent e)
    {
      try
        {
	  if (frame.isMaximizable() && ! frame.isMaximum())
	    frame.setMaximum(true);
	  else if (frame.isMaximum())
	    frame.setMaximum(false);
        }
      catch (PropertyVetoException pve)
        {
        }
    }
  }

  /**
   * This Action is responsible for dragging the JInternalFrame.
   */
  public class MoveAction extends AbstractAction
  {
    /**
     * This method is called when the user wants to drag the JInternalFrame.
     *
     * @param e The ActionEvent.
     */
    public void actionPerformed(ActionEvent e)
    {
      // FIXME: Implement keyboard driven? move actions.
    }
  }

  /**
   * This Action is responsible for restoring the JInternalFrame. Restoring
   * the JInternalFrame is the same as setting the maximum property to false.
   */
  public class RestoreAction extends AbstractAction
  {
    /**
     * This method is called when the user wants to restore the
     * JInternalFrame.
     *
     * @param e The ActionEvent.
     */
    public void actionPerformed(ActionEvent e)
    {
      if (frame.isMaximum())
        {
	  try
	    {
	      frame.setMaximum(false);
	    }
	  catch (PropertyVetoException pve)
	    {
	    }
        }
    }
  }

  /**
   * This action is responsible for sizing the JInternalFrame.
   */
  public class SizeAction extends AbstractAction
  {
    /**
     * This method is called when the user wants to resize the JInternalFrame.
     *
     * @param e The ActionEvent.
     */
    public void actionPerformed(ActionEvent e)
    {
      // FIXME: Not sure how size actions should be handled.
    }
  }

  /**
   * This class is responsible for handling property change events from the
   * JInternalFrame and adjusting the Title Pane as necessary.
   */
  protected class PropertyChangeHandler implements PropertyChangeListener
  {
    /**
     * This method is called when a PropertyChangeEvent is received by the
     * Title Pane.
     *
     * @param evt The PropertyChangeEvent.
     */
    public void propertyChange(PropertyChangeEvent evt)
    {
      // The title and frameIcon are taken care of during painting time.
      // The only other thing this will care about are the "is----izable"
      // properties. So we call enable actions to properly handle the 
      // buttons and menu items for us.
      enableActions();
    }
  }

  /**
   * This class acts as the MenuBar for the TitlePane. Clicking on the Frame
   * Icon in the top left corner will activate it.
   */
  public class SystemMenuBar extends JMenuBar
  {
    /**
     * This method returns true if it can receive focus.
     *
     * @return True if this Component can receive focus.
     */
    public boolean isFocusTransversable()
    {
      return true;
    }

    /**
     * This method returns true if this Component is expected to paint all of
     * itself.
     *
     * @return True if this Component is expect to paint all of itself.
     */
    public boolean isOpaque()
    {
      return true;
    }

    /**
     * This method paints this Component.
     *
     * @param g The Graphics object to paint with.
     */
    public void paint(Graphics g)
    {
      Icon frameIcon = frame.getFrameIcon();
      if (frameIcon == null)
	frameIcon = BasicDesktopIconUI.defaultIcon;
      frameIcon.paintIcon(this, g, 0, 0);
    }

    /**
     * This method requests that focus be given to this Component.
     */
    public void requestFocus()
    {
      super.requestFocus();
    }
  }

  /**
   * This class acts as the Layout Manager for the TitlePane.
   */
  protected class TitlePaneLayout implements LayoutManager
  {
    /**
     * Creates a new <code>TitlePaneLayout</code> object.
     */
    public TitlePaneLayout()
    {
      // Do nothing.
    }

    /**
     * This method is called when adding a Component to the Container.
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
      enableActions();

      Insets insets = c.getInsets();
      int width = c.getBounds().width - insets.left - insets.right;
      int height = c.getBounds().height - insets.top - insets.bottom;

      // MenuBar is always present and located at the top left corner.
      Dimension menupref = menuBar.getPreferredSize();
      menuBar.setBounds(insets.left, insets.top, menupref.width, height);

      int loc = width + insets.left;

      Insets i = closeButton.getInsets();
      Dimension prefs = new Dimension(iconSize + i.left + i.right,
                                      iconSize + i.top + i.bottom);
      int top = insets.top + (height - prefs.height) / 2;
      if (closeAction.isEnabled())
        {
	  loc -= prefs.width;
	  closeButton.setVisible(true);
	  closeButton.setBounds(loc, top, prefs.width, prefs.height);
        }
      else
	closeButton.setVisible(false);

      if (maximizeAction.isEnabled())
        {
	  loc -= prefs.width;
	  maxButton.setVisible(true);
	  maxButton.setBounds(loc, top, prefs.width, prefs.height);
        }
      else
	maxButton.setVisible(false);

      if (iconifyAction.isEnabled())
        {
	  loc -= prefs.width;
	  iconButton.setVisible(true);
	  iconButton.setBounds(loc, top, prefs.width, prefs.height);
        }
      else
	iconButton.setVisible(false);

      if (title != null)
	title.setBounds(insets.left + menupref.width, insets.top,
	                loc - menupref.width - insets.left, height);
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
     * This method returns the preferred size of the given Container taking
     * into account the children that it has.
     *
     * @param c The Container to lay out.
     *
     * @return The preferred size of the Container.
     */
    public Dimension preferredLayoutSize(Container c)
    {
      Insets frameInsets = frame.getInsets();

      // Height is the max of the preferredHeights of all components
      // inside the pane
      int height = 0;
      int width = 0;
      Dimension d;

      Component[] components = BasicInternalFrameTitlePane.this.getComponents();
      for (int i = 0; i < components.length; i++)
        {
	  d = components[i].getPreferredSize();
	  height = Math.max(height, d.height);
	  width += d.width;
        }

      Insets insets = BasicInternalFrameTitlePane.this.getInsets();
      height += insets.top + insets.bottom;

      return new Dimension(width, height);
    }

    /**
     * This method is called when removing a Component from the Container.
     *
     * @param c The Component to remove.
     */
    public void removeLayoutComponent(Component c)
    {
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
      setBorder(null);
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

  /** The action command for the Close action. */
  protected static final String CLOSE_CMD = "Close";

  /** The action command for the Minimize action. */
  protected static final String ICONIFY_CMD = "Minimize";

  /** The action command for the Maximize action. */
  protected static final String MAXIMIZE_CMD = "Maximize";

  /** The action command for the Move action. */
  protected static final String MOVE_CMD = "Move";

  /** The action command for the Restore action. */
  protected static final String RESTORE_CMD = "Restore";

  /** The action command for the Size action. */
  protected static final String SIZE_CMD = "Size";

  /** The action associated with closing the JInternalFrame. */
  protected Action closeAction;

  /** The action associated with iconifying the JInternalFrame. */
  protected Action iconifyAction;

  /** The action associated with maximizing the JInternalFrame. */
  protected Action maximizeAction;

  /** The action associated with moving the JInternalFrame. */
  protected Action moveAction;

  /** The action associated with restoring the JInternalFrame. */
  protected Action restoreAction;

  /** The action associated with resizing the JInternalFrame. */
  protected Action sizeAction;

  /** The button that closes the JInternalFrame. */
  protected JButton closeButton;

  /** The button that iconifies the JInternalFrame. */
  protected JButton iconButton;

  /** The button that maximizes the JInternalFrame. */
  protected JButton maxButton;

  /** Active background color. */
  protected Color activeBGColor;

  /** Active foreground color. */
  protected Color activeFGColor;

  /** Inactive background color. */
  protected Color inactiveBGColor;

  /** Inactive foreground color. */
  protected Color inactiveFGColor;

  // FIXME: These icons need to be moved to MetalIconFactory.

  /** The size of the icons in the buttons. */
  private static final int iconSize = 16;

  /** The icon displayed in the close button. */
  protected Icon closeIcon = new Icon()
    {
      public int getIconHeight()
      {
	return iconSize;
      }

      public int getIconWidth()
      {
	return iconSize;
      }

      public void paintIcon(Component c, Graphics g, int x, int y)
      {
	g.translate(x, y);
	Color saved = g.getColor();
	g.setColor(Color.BLACK);

	int four = iconSize / 4;
	int six = iconSize * 6 / 16;
	int ten = iconSize * 10 / 16;
	int twelve = iconSize * 12 / 16;

	Polygon a = new Polygon(new int[] { four, six, ten, twelve },
	                        new int[] { six, four, twelve, ten }, 4);
	Polygon b = new Polygon(new int[] { four, six, ten, twelve },
	                        new int[] { ten, twelve, four, six }, 4);

	g.fillPolygon(a);
	g.fillPolygon(b);

	g.setColor(saved);
	g.translate(-x, -y);
      }
    };

  // FIXME: Create new icon.

  /** The icon displayed in the restore button. */
  protected Icon minIcon;

  /** The icon displayed in the maximize button. */
  protected Icon maxIcon = new Icon()
    {
      public int getIconHeight()
      {
	return iconSize;
      }

      public int getIconWidth()
      {
	return iconSize;
      }

      public void paintIcon(Component c, Graphics g, int x, int y)
      {
	g.translate(x, y);
	Color saved = g.getColor();
	g.setColor(Color.BLACK);

	int four = iconSize / 4;
	int two = four / 2;
	int six = iconSize * 6 / 16;
	int eight = four * 2;

	g.fillRect(four, four, eight, two);
	g.drawRect(four, six, eight, six);

	g.setColor(saved);
	g.translate(-x, -y);
      }
    };

  /** The icon displayed in the iconify button. */
  protected Icon iconIcon = new Icon()
    {
      public int getIconHeight()
      {
	return iconSize;
      }

      public int getIconWidth()
      {
	return iconSize;
      }

      public void paintIcon(Component c, Graphics g, int x, int y)
      {
	g.translate(x, y);
	Color saved = g.getColor();
	g.setColor(Color.BLACK);

	g.fillRect(iconSize / 4, iconSize * 10 / 16, iconSize / 2, iconSize / 8);

	g.setColor(saved);
	g.translate(-x, -y);
      }
    };

  /** The JInternalFrame that this TitlePane is used in. */
  protected JInternalFrame frame;

  /** The JMenuBar that is located at the top left of the Title Pane. */
  protected JMenuBar menuBar;

  /** The JMenu inside the menuBar. */
  protected JMenu windowMenu;

  /**
   * The text color of the TitlePane when the JInternalFrame is not selected.
   */
  protected Color notSelectedTextColor;

  /**
   * The background color of the TitlePane when the JInternalFrame is not
   * selected.
   */
  protected Color notSelectedTitleColor;

  /** The text color of the titlePane when the JInternalFrame is selected. */
  protected Color selectedTextColor;

  /**
   * The background color of the TitlePane when the JInternalFrame is
   * selected.
   */
  protected Color selectedTitleColor;

  /** The Property Change listener that listens to the JInternalFrame. */
  protected PropertyChangeListener propertyChangeListener;

  /**
   * The label used to display the title. This label is not added to the
   * TitlePane.
   * This is package-private to avoid an accessor method.
   */
  transient JLabel title;

  /**
   * Creates a new BasicInternalFrameTitlePane object that is used in the
   * given JInternalFrame.
   *
   * @param f The JInternalFrame this BasicInternalFrameTitlePane will be used
   *        in.
   */
  public BasicInternalFrameTitlePane(JInternalFrame f)
  {
    frame = f;
    setLayout(createLayout());
    title = new JLabel();
    title.setHorizontalAlignment(SwingConstants.LEFT);
    title.setHorizontalTextPosition(SwingConstants.LEFT);
    title.setOpaque(false);
    setOpaque(true);

    setBackground(Color.LIGHT_GRAY);

    installTitlePane();
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

  /**
   * This method adds the sub components to the TitlePane.
   */
  protected void addSubComponents()
  {
    add(menuBar);

    add(closeButton);
    add(iconButton);
    add(maxButton);
  }

  /**
   * This method creates the actions that are used to manipulate the
   * JInternalFrame.
   */
  protected void createActions()
  {
    closeAction = new CloseAction();
    closeAction.putValue(AbstractAction.ACTION_COMMAND_KEY, CLOSE_CMD);

    iconifyAction = new IconifyAction();
    iconifyAction.putValue(AbstractAction.ACTION_COMMAND_KEY, ICONIFY_CMD);

    maximizeAction = new MaximizeAction();
    maximizeAction.putValue(AbstractAction.ACTION_COMMAND_KEY, MAXIMIZE_CMD);

    sizeAction = new SizeAction();
    sizeAction.putValue(AbstractAction.ACTION_COMMAND_KEY, SIZE_CMD);

    restoreAction = new RestoreAction();
    restoreAction.putValue(AbstractAction.ACTION_COMMAND_KEY, RESTORE_CMD);

    moveAction = new MoveAction();
    moveAction.putValue(AbstractAction.ACTION_COMMAND_KEY, MOVE_CMD);
  }

  /**
   * This method is used to install the listeners.
   */
  protected void installListeners()
  {
    propertyChangeListener = new PropertyChangeHandler();
    frame.addPropertyChangeListener(propertyChangeListener);
  }

  /**
   * This method is used to uninstall the listeners.
   */
  protected void uninstallListeners()
  {
    frame.removePropertyChangeListener(propertyChangeListener);
    propertyChangeListener = null;
  }

  /**
   * This method installs the defaults determined by the look and feel.
   */
  protected void installDefaults()
  {
    // FIXME: move icons to defaults.
    UIDefaults defaults = UIManager.getLookAndFeelDefaults();

    setFont(defaults.getFont("InternalFrame.titleFont"));
    activeFGColor = defaults.getColor("InternalFrame.activeTitleForeground");
    activeBGColor = defaults.getColor("InternalFrame.activeTitleBackground");
    inactiveFGColor = defaults.getColor("InternalFrame.inactiveTitleForeground");
    inactiveBGColor = defaults.getColor("InternalFrame.inactiveTitleBackground");
  }

  /**
   * This method uninstalls the defaults.
   */
  protected void uninstallDefaults()
  {
    setFont(null);
    activeFGColor = null;
    activeBGColor = null;
    inactiveFGColor = null;
    inactiveBGColor = null;
  }

  /**
   * This method creates the buttons used in the TitlePane.
   */
  protected void createButtons()
  {
    closeButton = new PaneButton(closeAction);
    closeButton.setOpaque(false);

    iconButton = new PaneButton(iconifyAction);
    iconButton.setOpaque(false);

    maxButton = new PaneButton(maximizeAction);
    maxButton.setOpaque(false);
  }

  /**
   * This method sets the icons in the buttons.
   */
  protected void setButtonIcons()
  {
    closeButton.setIcon(closeIcon);
    iconButton.setIcon(iconIcon);
    maxButton.setIcon(maxIcon);
  }

  /**
   * This method creates the MenuBar used in the TitlePane.
   */
  protected void assembleSystemMenu()
  {
    menuBar = createSystemMenuBar();
    windowMenu = createSystemMenu();

    menuBar.add(windowMenu);

    addSystemMenuItems(windowMenu);
    enableActions();
  }

  /**
   * This method adds the MenuItems to the given JMenu.
   *
   * @param systemMenu The JMenu to add MenuItems to.
   */
  protected void addSystemMenuItems(JMenu systemMenu)
  {
    JMenuItem tmp;

    tmp = new JMenuItem(RESTORE_CMD);
    tmp.addActionListener(restoreAction);
    tmp.setMnemonic(KeyEvent.VK_R);
    systemMenu.add(tmp);

    tmp = new JMenuItem(MOVE_CMD);
    tmp.addActionListener(moveAction);
    tmp.setMnemonic(KeyEvent.VK_M);
    systemMenu.add(tmp);

    tmp = new JMenuItem(SIZE_CMD);
    tmp.addActionListener(sizeAction);
    tmp.setMnemonic(KeyEvent.VK_S);
    systemMenu.add(tmp);

    tmp = new JMenuItem(ICONIFY_CMD);
    tmp.addActionListener(iconifyAction);
    tmp.setMnemonic(KeyEvent.VK_N);
    systemMenu.add(tmp);

    tmp = new JMenuItem(MAXIMIZE_CMD);
    tmp.addActionListener(maximizeAction);
    tmp.setMnemonic(KeyEvent.VK_X);
    systemMenu.add(tmp);

    systemMenu.addSeparator();

    tmp = new JMenuItem(CLOSE_CMD);
    tmp.addActionListener(closeAction);
    tmp.setMnemonic(KeyEvent.VK_C);
    systemMenu.add(tmp);
  }

  /**
   * This method creates a new JMenubar.
   *
   * @return A new JMenuBar.
   */
  protected JMenuBar createSystemMenuBar()
  {
    if (menuBar == null)
      menuBar = new SystemMenuBar();
    menuBar.removeAll();
    return menuBar;
  }

  /**
   * This method creates a new JMenu.
   *
   * @return A new JMenu.
   */
  protected JMenu createSystemMenu()
  {
    if (windowMenu == null)
      windowMenu = new JMenu();
    windowMenu.removeAll();
    return windowMenu;
  }

  /**
   * This method programmatically shows the JMenu.
   */
  protected void showSystemMenu()
  {
    // FIXME: Untested as KeyEvents are not hooked up.
    menuBar.getMenu(1).getPopupMenu().show();
  }

  /**
   * This method paints the TitlePane.
   *
   * @param g The Graphics object to paint with.
   */
  public void paintComponent(Graphics g)
  {
    paintTitleBackground(g);
    Font f = g.getFont();
    FontMetrics fm = g.getFontMetrics(f);
    if (frame.getTitle() != null && title != null)
      {
	Color saved = g.getColor();
	if (frame.isSelected())
	  g.setColor(activeFGColor);
	else
	  g.setColor(inactiveFGColor);
	title.setText(getTitle(frame.getTitle(), fm, title.getBounds().width));
	SwingUtilities.paintComponent(g, title, null, title.getBounds());
	g.setColor(saved);
      }
  }

  /**
   * This method paints the TitlePane's background.
   *
   * @param g The Graphics object to paint with.
   */
  protected void paintTitleBackground(Graphics g)
  {
    Color saved = g.getColor();
    Dimension dims = getSize();

    Color bg = getBackground();
    if (frame.isSelected())
      bg = activeBGColor;
    else
      bg = inactiveBGColor;
    g.setColor(bg);
    g.fillRect(0, 0, dims.width, dims.height);
    g.setColor(saved);
  }

  /**
   * This method returns the title string based on the available width and the
   * font metrics.
   *
   * @param text The desired title.
   * @param fm The FontMetrics of the font used.
   * @param availableWidth The available width.
   *
   * @return The allowable string.
   */
  protected String getTitle(String text, FontMetrics fm, int availableWidth)
  {
    Rectangle vr = new Rectangle(0, 0, availableWidth, fm.getHeight());
    Rectangle ir = new Rectangle();
    Rectangle tr = new Rectangle();
    String value = SwingUtilities.layoutCompoundLabel(this, fm, text, null,
                                                      SwingConstants.CENTER,
                                                      SwingConstants.LEFT,
                                                      SwingConstants.CENTER,
                                                      SwingConstants.LEFT, vr,
                                                      ir, tr, 0);
    return value;
  }

  /**
   * This method fires something similar to a WINDOW_CLOSING event.
   *
   * @param frame The JInternalFrame that is being closed.
   */
  protected void postClosingEvent(JInternalFrame frame)
  {
    // FIXME: Implement postClosingEvent when I figure out what
    // it's supposed to do.
    // It says that this fires an WINDOW_CLOSING like event. 
    // So the closest thing is some kind of InternalFrameEvent.
    // But none is fired.
    // Can't see it called or anything.
  }

  /**
   * This method enables the actions for the TitlePane given the frame's
   * properties.
   */
  protected void enableActions()
  {
    closeAction.setEnabled(frame.isClosable());

    iconifyAction.setEnabled(frame.isIconifiable());
    // The maximize action is responsible for restoring it
    // as well, if clicked from the button
    maximizeAction.setEnabled(frame.isMaximizable());

    // The restoring action is only active when selected
    // from the menu.
    restoreAction.setEnabled(frame.isMaximum());

    sizeAction.setEnabled(frame.isResizable());

    // FIXME: Tie MoveAction enabled status to a variable.
    moveAction.setEnabled(false);
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
   * This method creates a new LayoutManager for the TitlePane.
   *
   * @return A new LayoutManager.
   */
  protected LayoutManager createLayout()
  {
    return new TitlePaneLayout();
  }
}
