/* BasicMenuItemUI.java --
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;

import javax.swing.AbstractAction;
import javax.swing.ActionMap;
import javax.swing.ButtonModel;
import javax.swing.Icon;
import javax.swing.InputMap;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;
import javax.swing.LookAndFeel;
import javax.swing.MenuElement;
import javax.swing.MenuSelectionManager;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.event.MenuDragMouseEvent;
import javax.swing.event.MenuDragMouseListener;
import javax.swing.event.MenuKeyEvent;
import javax.swing.event.MenuKeyListener;
import javax.swing.event.MouseInputListener;
import javax.swing.plaf.ActionMapUIResource;
import javax.swing.plaf.ComponentInputMapUIResource;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.MenuItemUI;

/**
 * UI Delegate for JMenuItem.
 */
public class BasicMenuItemUI extends MenuItemUI
{
  /**
   * Font to be used when displaying menu item's accelerator.
   */
  protected Font acceleratorFont;

  /**
   * Color to be used when displaying menu item's accelerator.
   */
  protected Color acceleratorForeground;

  /**
   * Color to be used when displaying menu item's accelerator when menu item is
   * selected.
   */
  protected Color acceleratorSelectionForeground;

  /**
   * Icon that is displayed after the text to indicated that this menu contains
   * submenu.
   */
  protected Icon arrowIcon;

  /**
   * Icon that is displayed before the text. This icon is only used in
   * JCheckBoxMenuItem or JRadioBoxMenuItem.
   */
  protected Icon checkIcon;

  /**
   * Number of spaces between icon and text.
   */
  protected int defaultTextIconGap = 4;
  
  /**
   * Color of the text when menu item is disabled
   */
  protected Color disabledForeground;

  /**
   * The menu Drag mouse listener listening to the menu item.
   */
  protected MenuDragMouseListener menuDragMouseListener;

  /**
   * The menu item itself
   */
  protected JMenuItem menuItem;

  /**
   * Menu Key listener listening to the menu item.
   */
  protected MenuKeyListener menuKeyListener;

  /**
   * mouse input listener listening to menu item.
   */
  protected MouseInputListener mouseInputListener;

  /**
   * Indicates if border should be painted
   */
  protected boolean oldBorderPainted;

  /**
   * Color of text that is used when menu item is selected
   */
  protected Color selectionBackground;

  /**
   * Color of the text that is used when menu item is selected.
   */
  protected Color selectionForeground;

  /**
   * String that separates description of the modifiers and the key
   */
  private String acceleratorDelimiter;

  /**
   * ItemListener to listen for item changes in the menu item
   */
  private ItemListener itemListener;

  /**
   * Number of spaces between accelerator and menu item's label.
   */
  private int defaultAcceleratorLabelGap = 10;

  /**
   * The gap between different menus on the MenuBar.
   */
  private int MenuGap = 10;
  
  /** A PropertyChangeListener to make UI updates after property changes **/
  PropertyChangeHandler propertyChangeListener;
  
  /**
   * A class to handle PropertChangeEvents for the JMenuItem
   * @author Anthony Balkissoon abalkiss at redhat dot com.   
   */
  class PropertyChangeHandler implements PropertyChangeListener
  {
    /**
     * This method is called when a property of the menuItem is changed.
     * Currently it is only used to update the accelerator key bindings.
     * 
     * @param e
     *          the PropertyChangeEvent
     */
    public void propertyChange(PropertyChangeEvent e)
    {
      if (e.getPropertyName() == "accelerator")
        {
          InputMap map = SwingUtilities.getUIInputMap(menuItem, JComponent.WHEN_IN_FOCUSED_WINDOW);
          if (map != null)
            map.remove((KeyStroke)e.getOldValue());
          else
            map = new ComponentInputMapUIResource(menuItem);
          map.put((KeyStroke)e.getNewValue(), "doClick");
        }
    }
  }
  
  /**
   * A class to handle accelerator keys.  This is the Action we will
   * perform when the accelerator key for this JMenuItem is pressed.
   * @author Anthony Balkissoon abalkiss at redhat dot com
   *
   */
  class ClickAction extends AbstractAction
  {
    /**
     * This is what is done when the accelerator key for the JMenuItem is
     * pressed.
     */
    public void actionPerformed(ActionEvent event)
    {
      doClick(MenuSelectionManager.defaultManager());
    }    
  }
  
  /**
   * Creates a new BasicMenuItemUI object.
   */
  public BasicMenuItemUI()
  {
    mouseInputListener = createMouseInputListener(menuItem);
    menuDragMouseListener = createMenuDragMouseListener(menuItem);
    menuKeyListener = createMenuKeyListener(menuItem);
    itemListener = new ItemHandler();
    propertyChangeListener = new PropertyChangeHandler();
  }

  /**
   * Create MenuDragMouseListener to listen for mouse dragged events.
   * 
   * @param c
   *          menu item to listen to
   * @return The MenuDragMouseListener
   */
  protected MenuDragMouseListener createMenuDragMouseListener(JComponent c)
  {
    return new MenuDragMouseHandler();
  }

  /**
   * Creates MenuKeyListener to listen to key events occuring when menu item is
   * visible on the screen.
   * 
   * @param c
   *          menu item to listen to
   * @return The MenuKeyListener
   */
  protected MenuKeyListener createMenuKeyListener(JComponent c)
  {
    return new MenuKeyHandler();
  }

  /**
   * Handles mouse input events occuring for this menu item
   * 
   * @param c
   *          menu item to listen to
   * @return The MouseInputListener
   */
  protected MouseInputListener createMouseInputListener(JComponent c)
  {
    return new MouseInputHandler();
  }

  /**
   * Factory method to create a BasicMenuItemUI for the given {@link
   * JComponent}, which should be a {@link JMenuItem}.
   * 
   * @param c
   *          The {@link JComponent} a UI is being created for.
   * @return A BasicMenuItemUI for the {@link JComponent}.
   */
  public static ComponentUI createUI(JComponent c)
  {
    return new BasicMenuItemUI();
  }

  /**
   * Programatically clicks menu item.
   * 
   * @param msm
   *          MenuSelectionManager for the menu hierarchy
   */
  protected void doClick(MenuSelectionManager msm)
  {
    menuItem.doClick();
    msm.clearSelectedPath();
  }

  /**
   * Returns maximum size for the specified menu item
   * 
   * @param c
   *          component for which to get maximum size
   * @return Maximum size for the specified menu item.
   */
  public Dimension getMaximumSize(JComponent c)
  {
    return null;
  }

  /**
   * Returns minimum size for the specified menu item
   * 
   * @param c
   *          component for which to get minimum size
   * @return Minimum size for the specified menu item.
   */
  public Dimension getMinimumSize(JComponent c)
  {
    return null;
  }

  /**
   * Returns path to this menu item.
   * 
   * @return $MenuElement[]$ Returns array of menu elements that constitute a
   *         path to this menu item.
   */
  public MenuElement[] getPath()
  {
    ArrayList path = new ArrayList();

    // Path to menu should also include its popup menu.
    if (menuItem instanceof JMenu)
      path.add(((JMenu) menuItem).getPopupMenu());

    Component c = menuItem;
    while (c instanceof MenuElement)
      {
        path.add(0, (MenuElement) c);

        if (c instanceof JPopupMenu)
          c = ((JPopupMenu) c).getInvoker();
        else
          c = c.getParent();
      }

    MenuElement[] pathArray = new MenuElement[path.size()];
    path.toArray(pathArray);
    return pathArray;
  }

  /**
   * Returns preferred size for the given menu item.
   * 
   * @param c
   *          menu item for which to get preferred size
   * @param checkIcon
   *          check icon displayed in the given menu item
   * @param arrowIcon
   *          arrow icon displayed in the given menu item
   * @param defaultTextIconGap
   *          space between icon and text in the given menuItem
   * @return $Dimension$ preferred size for the given menu item
   */
  protected Dimension getPreferredMenuItemSize(JComponent c, Icon checkIcon,
                                               Icon arrowIcon,
                                               int defaultTextIconGap)
  {
    JMenuItem m = (JMenuItem) c;
    Dimension d = BasicGraphicsUtils.getPreferredButtonSize(m,
                                                            defaultTextIconGap);
    
    // if menu item has accelerator then take accelerator's size into account
    // when calculating preferred size.
    KeyStroke accelerator = m.getAccelerator();
    Rectangle rect;

    if (accelerator != null)
      {
        rect = getAcceleratorRect(
                                  accelerator,
                                  m.getToolkit().getFontMetrics(acceleratorFont));

        // add width of accelerator's text
        d.width += rect.width + defaultAcceleratorLabelGap;

        // adjust the heigth of the preferred size if necessary
        if (d.height < rect.height)
          d.height = rect.height;
      }

    if (checkIcon != null)
      {
        d.width += checkIcon.getIconWidth() + defaultTextIconGap;

        if (checkIcon.getIconHeight() > d.height)
          d.height = checkIcon.getIconHeight();
      }

    if (arrowIcon != null && (c instanceof JMenu))
      {
        int pWidth = m.getParent().getWidth();
        if (!((JMenu)c).isTopLevelMenu() && d.width < pWidth)
          d.width = pWidth
          - m.getInsets().left - m.getInsets().right;
        else
          d.width += arrowIcon.getIconWidth() + MenuGap;
        
        if (arrowIcon.getIconHeight() > d.height)
          d.height = arrowIcon.getIconHeight();
      }
    
    return d;
  }

  /**
   * Returns preferred size of the given component
   * 
   * @param c
   *          component for which to return preferred size
   * @return $Dimension$ preferred size for the given component
   */
  public Dimension getPreferredSize(JComponent c)
  {
    return getPreferredMenuItemSize(c, checkIcon, arrowIcon, defaultTextIconGap);
  }

  /**
   * Returns the prefix for entries in the {@link UIDefaults} table.
   * 
   * @return "MenuItem"
   */
  protected String getPropertyPrefix()
  {
    return "MenuItem";
  }

  /**
   * This method installs the components for this {@link JMenuItem}.
   * 
   * @param menuItem
   *          The {@link JMenuItem} to install components for.
   */
  protected void installComponents(JMenuItem menuItem)
  {
    // FIXME: Need to implement
  }

  /**
   * This method installs the defaults that are defined in the Basic look and
   * feel for this {@link JMenuItem}.
   */
  protected void installDefaults()
  {
    String prefix = getPropertyPrefix();
    LookAndFeel.installBorder(menuItem, prefix + ".border");
    LookAndFeel.installColorsAndFont(menuItem, prefix + ".background",
                                     prefix + ".foreground", prefix + ".font");
    menuItem.setMargin(UIManager.getInsets(prefix + ".margin"));
    acceleratorFont = UIManager.getFont(prefix + ".acceleratorFont");
    acceleratorForeground = UIManager.getColor(prefix + ".acceleratorForeground");
    acceleratorSelectionForeground = UIManager.getColor(prefix + ".acceleratorSelectionForeground");
    selectionBackground = UIManager.getColor(prefix + ".selectionBackground");
    selectionForeground = UIManager.getColor(prefix + ".selectionForeground");
    acceleratorDelimiter = UIManager.getString(prefix + ".acceleratorDelimiter");
    checkIcon = UIManager.getIcon(prefix + ".checkIcon");
    
    menuItem.setHorizontalTextPosition(SwingConstants.TRAILING);
    menuItem.setHorizontalAlignment(SwingConstants.LEADING);
  }

  /**
   * This method installs the keyboard actions for this {@link JMenuItem}.
   */
  protected void installKeyboardActions()
  {
    InputMap focusedWindowMap = SwingUtilities.getUIInputMap(menuItem, JComponent.WHEN_IN_FOCUSED_WINDOW);
    if (focusedWindowMap == null)
      focusedWindowMap = new ComponentInputMapUIResource(menuItem);
    focusedWindowMap.put(menuItem.getAccelerator(), "doClick");
    SwingUtilities.replaceUIInputMap(menuItem, JComponent.WHEN_IN_FOCUSED_WINDOW, focusedWindowMap);
    
    ActionMap UIActionMap = SwingUtilities.getUIActionMap(menuItem);
    if (UIActionMap == null)
      UIActionMap = new ActionMapUIResource();
    UIActionMap.put("doClick", new ClickAction());
    SwingUtilities.replaceUIActionMap(menuItem, UIActionMap);
  }

  /**
   * This method installs the listeners for the {@link JMenuItem}.
   */
  protected void installListeners()
  {
    menuItem.addMouseListener(mouseInputListener);
    menuItem.addMouseMotionListener(mouseInputListener);
    menuItem.addMenuDragMouseListener(menuDragMouseListener);
    menuItem.addMenuKeyListener(menuKeyListener);
    menuItem.addItemListener(itemListener);
    menuItem.addPropertyChangeListener(propertyChangeListener);
  }

  /**
   * Installs and initializes all fields for this UI delegate. Any properties of
   * the UI that need to be initialized and/or set to defaults will be done now.
   * It will also install any listeners necessary.
   * 
   * @param c
   *          The {@link JComponent} that is having this UI installed.
   */
  public void installUI(JComponent c)
  {
    super.installUI(c);
    menuItem = (JMenuItem) c;
    installDefaults();
    installComponents(menuItem);
    installListeners();
    installKeyboardActions();
  }

  /**
   * Paints given menu item using specified graphics context
   * 
   * @param g
   *          The graphics context used to paint this menu item
   * @param c
   *          Menu Item to paint
   */
  public void paint(Graphics g, JComponent c)
  {
    paintMenuItem(g, c, checkIcon, arrowIcon, c.getBackground(),
                  c.getForeground(), defaultTextIconGap);
  }

  /**
   * Paints background of the menu item
   * 
   * @param g
   *          The graphics context used to paint this menu item
   * @param menuItem
   *          menu item to paint
   * @param bgColor
   *          Background color to use when painting menu item
   */
  protected void paintBackground(Graphics g, JMenuItem menuItem, Color bgColor)
  {
    // Menu item is considered to be highlighted when it is selected.
    // But we don't want to paint the background of JCheckBoxMenuItems
    ButtonModel mod = menuItem.getModel();
    if ((menuItem.isSelected() && checkIcon == null) || (mod != null && 
        mod.isArmed())
        && (menuItem.getParent() instanceof MenuElement))
      {
        if (menuItem.isContentAreaFilled())
          {
            g.setColor(selectionBackground);
            g.fillRect(0, 0, menuItem.getWidth(), menuItem.getHeight());
          }
      }

  }

  /**
   * Paints specified menu item
   * 
   * @param g
   *          The graphics context used to paint this menu item
   * @param c
   *          menu item to paint
   * @param checkIcon
   *          check icon to use when painting menu item
   * @param arrowIcon
   *          arrow icon to use when painting menu item
   * @param background
   *          Background color of the menu item
   * @param foreground
   *          Foreground color of the menu item
   * @param defaultTextIconGap
   *          space to use between icon and text when painting menu item
   */
  protected void paintMenuItem(Graphics g, JComponent c, Icon checkIcon,
                               Icon arrowIcon, Color background,
                               Color foreground, int defaultTextIconGap)
  {
    JMenuItem m = (JMenuItem) c;
    Rectangle tr = new Rectangle(); // text rectangle
    Rectangle ir = new Rectangle(); // icon rectangle
    Rectangle vr = new Rectangle(); // view rectangle
    Rectangle br = new Rectangle(); // border rectangle
    Rectangle ar = new Rectangle(); // accelerator rectangle
    Rectangle cr = new Rectangle(); // checkIcon rectangle

    int vertAlign = m.getVerticalAlignment();
    int horAlign = m.getHorizontalAlignment();
    int vertTextPos = m.getVerticalTextPosition();
    int horTextPos = m.getHorizontalTextPosition();
    
    Font f = m.getFont();
    g.setFont(f);
    FontMetrics fm = g.getFontMetrics(f);
    SwingUtilities.calculateInnerArea(m, br);
    SwingUtilities.calculateInsetArea(br, m.getInsets(), vr);
    paintBackground(g, m, m.getBackground());

    /*
     * MenuItems insets are equal to menuItems margin, space between text and
     * menuItems border. We need to paint insets region as well.
     */
    Insets insets = m.getInsets();
    br.x -= insets.left;
    br.y -= insets.top;
    br.width += insets.right + insets.left;
    br.height += insets.top + insets.bottom;

    // If this menu item is a JCheckBoxMenuItem then paint check icon
    if (checkIcon != null)
      {
        SwingUtilities.layoutCompoundLabel(m, fm, null, checkIcon, vertAlign,
                                           horAlign, vertTextPos, horTextPos,
                                           vr, cr, tr, defaultTextIconGap);
        checkIcon.paintIcon(m, g, cr.x, cr.y);
        // We need to calculate position of the menu text and position of
        // user menu icon if there exists one relative to the check icon.
        // So we need to adjust view rectangle s.t. its starting point is at
        // checkIcon.width + defaultTextIconGap.
        vr.x = cr.x + cr.width + defaultTextIconGap;
      }

    // if this is a submenu, then paint arrow icon to indicate it.
    if (arrowIcon != null && (c instanceof JMenu))
      {
        if (!((JMenu) c).isTopLevelMenu())
          {
            int width = arrowIcon.getIconWidth();
            int height = arrowIcon.getIconHeight();
            int offset = (vr.height - height) / 2;
            arrowIcon.paintIcon(m, g, vr.width - width, vr.y + offset);
          }
      }

    // paint text and user menu icon if it exists
    Icon i = m.getIcon();
    SwingUtilities.layoutCompoundLabel(c, fm, m.getText(), i, vertAlign,
                                       horAlign, vertTextPos, horTextPos, vr,
                                       ir, tr, defaultTextIconGap);
    if (i != null)
      i.paintIcon(c, g, ir.x, ir.y);
    paintText(g, m, tr, m.getText());

    // paint accelerator
    String acceleratorText = "";

    if (m.getAccelerator() != null)
      {
        acceleratorText = getAcceleratorText(m.getAccelerator());
        fm = g.getFontMetrics(acceleratorFont);
        ar.width = fm.stringWidth(acceleratorText);
        ar.x = br.width - ar.width;
        vr.x = br.width - ar.width - defaultTextIconGap;

        SwingUtilities.layoutCompoundLabel(m, fm, acceleratorText, null,
                                           vertAlign, horAlign, vertTextPos,
                                           horTextPos, vr, ir, ar,
                                           defaultTextIconGap);

        paintAccelerator(g, m, ar, acceleratorText);
      }
  }

  /**
   * Paints label for the given menu item
   * 
   * @param g
   *          The graphics context used to paint this menu item
   * @param menuItem
   *          menu item for which to draw its label
   * @param textRect
   *          rectangle specifiying position of the text relative to the given
   *          menu item
   * @param text
   *          label of the menu item
   */
  protected void paintText(Graphics g, JMenuItem menuItem, Rectangle textRect,
                           String text)
  {
    Font f = menuItem.getFont();
    g.setFont(f);
    FontMetrics fm = g.getFontMetrics(f);

    if (text != null && !text.equals(""))
      {
        if (menuItem.isEnabled())
          {
            // Menu item is considered to be highlighted when it is selected.
            // But not if it's a JCheckBoxMenuItem
            ButtonModel mod = menuItem.getModel();
            if ((menuItem.isSelected() && checkIcon == null)
                || (mod != null && mod.isArmed())
                && (menuItem.getParent() instanceof MenuElement))
              g.setColor(selectionForeground);
            else
              g.setColor(menuItem.getForeground());
          }
        else
          // FIXME: should fix this to use 'disabledForeground', but its
          // default value in BasicLookAndFeel is null.

          // FIXME: should there be different foreground colours for selected
          // or deselected, when disabled?
          g.setColor(Color.gray);

        int mnemonicIndex = menuItem.getDisplayedMnemonicIndex();

        if (mnemonicIndex != -1)
          BasicGraphicsUtils.drawStringUnderlineCharAt(g, text, mnemonicIndex,
                                                       textRect.x,
                                                       textRect.y
                                                           + fm.getAscent());
        else
          BasicGraphicsUtils.drawString(g, text, 0, textRect.x,
                                        textRect.y + fm.getAscent());
      }
  }

  /**
   * This method uninstalls the components for this {@link JMenuItem}.
   * 
   * @param menuItem
   *          The {@link JMenuItem} to uninstall components for.
   */
  protected void uninstallComponents(JMenuItem menuItem)
  {
    // FIXME: need to implement
  }

  /**
   * This method uninstalls the defaults and sets any objects created during
   * install to null
   */
  protected void uninstallDefaults()
  {
    menuItem.setForeground(null);
    menuItem.setBackground(null);
    menuItem.setBorder(null);
    menuItem.setMargin(null);
    menuItem.setBackground(null);
    menuItem.setBorder(null);
    menuItem.setFont(null);
    menuItem.setForeground(null);
    menuItem.setMargin(null);
    acceleratorFont = null;
    acceleratorForeground = null;
    acceleratorSelectionForeground = null;
    arrowIcon = null;
    selectionBackground = null;
    selectionForeground = null;
    acceleratorDelimiter = null;
  }

  /**
   * Uninstalls any keyboard actions.
   */
  protected void uninstallKeyboardActions()
  {   
    SwingUtilities.replaceUIInputMap(menuItem,
                                     JComponent.WHEN_IN_FOCUSED_WINDOW, null);
  }

  /**
   * Unregisters all the listeners that this UI delegate was using.
   */
  protected void uninstallListeners()
  {
    menuItem.removeMouseListener(mouseInputListener);
    menuItem.removeMenuDragMouseListener(menuDragMouseListener);
    menuItem.removeMenuKeyListener(menuKeyListener);
    menuItem.removeItemListener(itemListener);
    menuItem.removePropertyChangeListener(propertyChangeListener);
  }

  /**
   * Performs the opposite of installUI. Any properties or resources that need
   * to be cleaned up will be done now. It will also uninstall any listeners it
   * has. In addition, any properties of this UI will be nulled.
   * 
   * @param c
   *          The {@link JComponent} that is having this UI uninstalled.
   */
  public void uninstallUI(JComponent c)
  {
    uninstallListeners();
    uninstallDefaults();
    uninstallComponents(menuItem);
    menuItem = null;
  }

  /**
   * This method calls paint.
   * 
   * @param g
   *          The graphics context used to paint this menu item
   * @param c
   *          The menu item to paint
   */
  public void update(Graphics g, JComponent c)
  {
    paint(g, c);
  }

  /**
   * Return text representation of the specified accelerator
   * 
   * @param accelerator
   *          Accelerator for which to return string representation
   * @return $String$ Text representation of the given accelerator
   */
  private String getAcceleratorText(KeyStroke accelerator)
  {
    // convert keystroke into string format
    String modifiersText = "";
    int modifiers = accelerator.getModifiers();
    char keyChar = accelerator.getKeyChar();
    int keyCode = accelerator.getKeyCode();

    if (modifiers != 0)
      modifiersText = KeyEvent.getKeyModifiersText(modifiers)
                      + acceleratorDelimiter;

    if (keyCode == KeyEvent.VK_UNDEFINED)
      return modifiersText + keyChar;
    else
      return modifiersText + KeyEvent.getKeyText(keyCode);
  }

  /**
   * Calculates and return rectange in which accelerator should be displayed
   * 
   * @param accelerator
   *          accelerator for which to return the display rectangle
   * @param fm
   *          The font metrics used to measure the text
   * @return $Rectangle$ reactangle which will be used to display accelerator
   */
  private Rectangle getAcceleratorRect(KeyStroke accelerator, FontMetrics fm)
  {
    int width = fm.stringWidth(getAcceleratorText(accelerator));
    int height = fm.getHeight();
    return new Rectangle(0, 0, width, height);
  }

  /**
   * Paints accelerator inside menu item
   * 
   * @param g
   *          The graphics context used to paint the border
   * @param menuItem
   *          Menu item for which to draw accelerator
   * @param acceleratorRect
   *          rectangle representing position of the accelerator relative to the
   *          menu item
   * @param acceleratorText
   *          accelerator's text
   */
  private void paintAccelerator(Graphics g, JMenuItem menuItem,
                                Rectangle acceleratorRect,
                                String acceleratorText)
  {
    g.setFont(acceleratorFont);
    FontMetrics fm = g.getFontMetrics(acceleratorFont);

    if (menuItem.isEnabled())
      g.setColor(acceleratorForeground);
    else
      // FIXME: should fix this to use 'disabledForeground', but its
      // default value in BasicLookAndFeel is null.
      g.setColor(Color.gray);

    BasicGraphicsUtils.drawString(g, acceleratorText, 0, acceleratorRect.x,
                                  acceleratorRect.y + fm.getAscent());
  }

  /**
   * This class handles mouse events occuring inside the menu item. Most of the
   * events are forwarded for processing to MenuSelectionManager of the current
   * menu hierarchy.
   */
  protected class MouseInputHandler implements MouseInputListener
  {
    /**
     * Creates a new MouseInputHandler object.
     */
    protected MouseInputHandler()
    {
      // Nothing to do here.
    }

    /**
     * This method is called when mouse is clicked on the menu item. It forwards
     * this event to MenuSelectionManager.
     * 
     * @param e
     *          A {@link MouseEvent}.
     */
    public void mouseClicked(MouseEvent e)
    {
      MenuSelectionManager manager = MenuSelectionManager.defaultManager();
      manager.processMouseEvent(e);
    }

    /**
     * This method is called when mouse is dragged inside the menu item. It
     * forwards this event to MenuSelectionManager.
     * 
     * @param e
     *          A {@link MouseEvent}.
     */
    public void mouseDragged(MouseEvent e)
    {
      MenuSelectionManager manager = MenuSelectionManager.defaultManager();
      manager.processMouseEvent(e);
    }

    /**
     * This method is called when mouse enters menu item. When this happens menu
     * item is considered to be selected and selection path in
     * MenuSelectionManager is set. This event is also forwarded to
     * MenuSelection Manager for further processing.
     * 
     * @param e
     *          A {@link MouseEvent}.
     */
    public void mouseEntered(MouseEvent e)
    {
      Component source = (Component) e.getSource();
      if (source.getParent() instanceof MenuElement)
        {
          MenuSelectionManager manager = MenuSelectionManager.defaultManager();
          manager.setSelectedPath(getPath());
          manager.processMouseEvent(e);
        }
    }

    /**
     * This method is called when mouse exits menu item. The event is forwarded
     * to MenuSelectionManager for processing.
     * 
     * @param e
     *          A {@link MouseEvent}.
     */
    public void mouseExited(MouseEvent e)
    {
      MenuSelectionManager manager = MenuSelectionManager.defaultManager();
      manager.processMouseEvent(e);
    }

    /**
     * This method is called when mouse is inside the menu item. This event is
     * forwarder to MenuSelectionManager for further processing.
     * 
     * @param e
     *          A {@link MouseEvent}.
     */
    public void mouseMoved(MouseEvent e)
    {
      MenuSelectionManager manager = MenuSelectionManager.defaultManager();
      manager.processMouseEvent(e);
    }

    /**
     * This method is called when mouse is pressed. This event is forwarded to
     * MenuSelectionManager for further processing.
     * 
     * @param e
     *          A {@link MouseEvent}.
     */
    public void mousePressed(MouseEvent e)
    {
      MenuSelectionManager manager = MenuSelectionManager.defaultManager();
      manager.processMouseEvent(e);
    }

    /**
     * This method is called when mouse is released. If the mouse is released
     * inside this menuItem, then this menu item is considered to be chosen and
     * the menu hierarchy should be closed.
     * 
     * @param e
     *          A {@link MouseEvent}.
     */
    public void mouseReleased(MouseEvent e)
    {
      Rectangle size = menuItem.getBounds();
      MenuSelectionManager manager = MenuSelectionManager.defaultManager();
      if (e.getX() > 0 && e.getX() < size.width && e.getY() > 0
          && e.getY() < size.height)
        {
          manager.clearSelectedPath();
          menuItem.doClick();
        }

      else
        manager.processMouseEvent(e);
    }
  }

  /**
   * This class handles mouse dragged events.
   */
  private class MenuDragMouseHandler implements MenuDragMouseListener
  {
    /**
     * Tbis method is invoked when mouse is dragged over the menu item.
     * 
     * @param e
     *          The MenuDragMouseEvent
     */
    public void menuDragMouseDragged(MenuDragMouseEvent e)
    {
      MenuSelectionManager manager = MenuSelectionManager.defaultManager();
      manager.setSelectedPath(e.getPath());
    }

    /**
     * Tbis method is invoked when mouse enters the menu item while it is being
     * dragged.
     * 
     * @param e
     *          The MenuDragMouseEvent
     */
    public void menuDragMouseEntered(MenuDragMouseEvent e)
    {
      MenuSelectionManager manager = MenuSelectionManager.defaultManager();
      manager.setSelectedPath(e.getPath());
    }

    /**
     * Tbis method is invoked when mouse exits the menu item while it is being
     * dragged
     * 
     * @param e the MenuDragMouseEvent
     */
    public void menuDragMouseExited(MenuDragMouseEvent e)
    {
      // TODO: What should be done here, if anything?
    }

    /**
     * Tbis method is invoked when mouse was dragged and released inside the
     * menu item.
     * 
     * @param e
     *          The MenuDragMouseEvent
     */
    public void menuDragMouseReleased(MenuDragMouseEvent e)
    {
      MenuElement[] path = e.getPath();

      if (path[path.length - 1] instanceof JMenuItem)
        ((JMenuItem) path[path.length - 1]).doClick();

      MenuSelectionManager manager = MenuSelectionManager.defaultManager();
      manager.clearSelectedPath();
    }
  }

  /**
   * This class handles key events occuring when menu item is visible on the
   * screen.
   */
  private class MenuKeyHandler implements MenuKeyListener
  {
    /**
     * This method is invoked when key has been pressed
     * 
     * @param e
     *          A {@link MenuKeyEvent}.
     */
    public void menuKeyPressed(MenuKeyEvent e)
    {
      // TODO: What should be done here, if anything?
    }

    /**
     * This method is invoked when key has been pressed
     * 
     * @param e
     *          A {@link MenuKeyEvent}.
     */
    public void menuKeyReleased(MenuKeyEvent e)
    {
      // TODO: What should be done here, if anything?
    }

    /**
     * This method is invoked when key has been typed It handles the mnemonic
     * key for the menu item.
     * 
     * @param e
     *          A {@link MenuKeyEvent}.
     */
    public void menuKeyTyped(MenuKeyEvent e)
    {
      // TODO: What should be done here, if anything?
    }
  }
  
  /**
   * Helper class that listens for item changes to the properties of the {@link
   * JMenuItem}.
   */
  private class ItemHandler implements ItemListener
  {
    /**
     * This method is called when one of the menu item changes.
     *
     * @param evt A {@link ItemEvent}.
     */
    public void itemStateChanged(ItemEvent evt)
    {
      boolean state = false;
      if (menuItem instanceof JCheckBoxMenuItem)
        {
          if (evt.getStateChange() == ItemEvent.SELECTED)
            state = true;
          ((JCheckBoxMenuItem) menuItem).setState(state);
        }
      menuItem.revalidate();
      menuItem.repaint();
    }
  }
}
