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

import gnu.classpath.SystemProperties;

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
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
import java.awt.font.FontRenderContext;
import java.awt.font.TextLayout;
import java.awt.geom.AffineTransform;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;

import javax.swing.AbstractAction;
import javax.swing.AbstractButton;
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
import javax.swing.text.View;

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
   * A PropertyChangeListener to make UI updates after property changes.
   */
  private PropertyChangeHandler propertyChangeListener;

  /**
   * The view rectangle used for layout of the menu item.
   */
  private Rectangle viewRect;

  /**
   * The rectangle that holds the area of the label.
   */
  private Rectangle textRect;

  /**
   * The rectangle that holds the area of the accelerator.
   */
  private Rectangle accelRect;

  /**
   * The rectangle that holds the area of the icon.
   */
  private Rectangle iconRect;

  /**
   * The rectangle that holds the area of the icon.
   */
  private Rectangle arrowIconRect;

  /**
   * The rectangle that holds the area of the check icon.
   */
  private Rectangle checkIconRect;

  /**
   * A rectangle used for temporary storage to avoid creation of new
   * rectangles.
   */
  private Rectangle cachedRect;

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
      String property = e.getPropertyName();
      if (property.equals("accelerator"))
        {
          InputMap map = SwingUtilities.getUIInputMap(menuItem, 
              JComponent.WHEN_IN_FOCUSED_WINDOW);
          if (map != null)
            map.remove((KeyStroke) e.getOldValue());
          else
            map = new ComponentInputMapUIResource(menuItem);

          KeyStroke accelerator = (KeyStroke) e.getNewValue();
          if (accelerator != null)
            map.put(accelerator, "doClick");
        }
      // TextLayout caching for speed-up drawing of text.
      else if ((property.equals(AbstractButton.TEXT_CHANGED_PROPERTY)
                || property.equals("font"))
               && SystemProperties.getProperty("gnu.javax.swing.noGraphics2D")
               == null)
        {
          AbstractButton b = (AbstractButton) e.getSource();
          String text = b.getText();
          if (text == null)
            text = "";
          FontRenderContext frc = new FontRenderContext(new AffineTransform(),
                                                        false, false);
          TextLayout layout = new TextLayout(text, b.getFont(), frc);
          b.putClientProperty(BasicGraphicsUtils.CACHED_TEXT_LAYOUT, layout);
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

    // Initialize rectangles for layout.
    viewRect = new Rectangle();
    textRect = new Rectangle();
    iconRect = new Rectangle();
    arrowIconRect = new Rectangle();
    checkIconRect = new Rectangle();
    accelRect = new Rectangle();
    cachedRect = new Rectangle();
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
    menuItem.doClick(0);
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

    Component c = menuItem;
    while (c instanceof MenuElement)
      {
        path.add(0, c);

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
    String accelText = getAcceleratorString(m);

    // Layout the menu item. The result gets stored in the rectangle
    // fields of this class.
    resetRectangles(null);
    layoutMenuItem(m, accelText);

    // The union of the text and icon areas is the label area.
    cachedRect.setBounds(textRect);
    Rectangle pref = SwingUtilities.computeUnion(iconRect.x, iconRect.y,
                                                 iconRect.width,
                                                 iconRect.height,
                                                 cachedRect);

    // Find the widest menu item text and accelerator and store it in
    // client properties of the parent, so that we can align the accelerators
    // properly. Of course, we only need can do this, if the parent is
    // a JComponent and this menu item is not a toplevel menu.
    Container parent = m.getParent();
    if (parent != null && parent instanceof JComponent
        && !(m instanceof JMenu && ((JMenu) m).isTopLevelMenu()))
      {
        JComponent p = (JComponent) parent;

        // The widest text so far.
        Integer maxTextWidth = (Integer) p.getClientProperty("maxTextWidth");
        int maxTextValue = maxTextWidth == null ? 0 : maxTextWidth.intValue();
        if (pref.width < maxTextValue)
          pref.width = maxTextValue;
        else
          p.putClientProperty("maxTextWidth", new Integer(pref.width));

        // The widest accelerator so far.
        Integer maxAccelWidth = (Integer) p.getClientProperty("maxAccelWidth");
        int maxAccelValue = maxAccelWidth == null ? 0
                                                  : maxAccelWidth.intValue();
        if (accelRect.width > maxAccelValue)
          {
            maxAccelValue = accelRect.width;
            p.putClientProperty("maxAccelWidth", new Integer(accelRect.width));
          }
        pref.width += maxAccelValue;
        pref.width += defaultTextIconGap;
      }

    // Add arrow and check size if appropriate.
    if (! (m instanceof JMenu && ((JMenu) m).isTopLevelMenu()))
      {
        pref.width += checkIconRect.width;
        pref.width += defaultTextIconGap;
        pref.width += arrowIconRect.width;
        pref.width += defaultTextIconGap;
      }

    // Add a gap ~2 times as wide as the defaultTextIconGap.
    pref.width += 2 * defaultTextIconGap;

    // Respect the insets of the menu item.
    Insets i = m.getInsets();
    pref.width += i.left + i.right;
    pref.height += i.top + i.bottom;

    // Return a copy, so that nobody messes with our textRect.
    return pref.getSize();
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
    return getPreferredMenuItemSize(c, checkIcon, arrowIcon, 
                                    defaultTextIconGap);
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
    acceleratorForeground = UIManager.getColor(prefix 
        + ".acceleratorForeground");
    acceleratorSelectionForeground = UIManager.getColor(prefix 
        + ".acceleratorSelectionForeground");
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
    InputMap focusedWindowMap = SwingUtilities.getUIInputMap(menuItem, 
        JComponent.WHEN_IN_FOCUSED_WINDOW);
    if (focusedWindowMap == null)
      focusedWindowMap = new ComponentInputMapUIResource(menuItem);
    KeyStroke accelerator = menuItem.getAccelerator();
    if (accelerator != null)
      focusedWindowMap.put(accelerator, "doClick");
    SwingUtilities.replaceUIInputMap(menuItem, 
        JComponent.WHEN_IN_FOCUSED_WINDOW, focusedWindowMap);
    
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
    // Fire synthetic property change event to let the listener update
    // the TextLayout cache.
    propertyChangeListener.propertyChange(new PropertyChangeEvent(menuItem,
                                                                  "font", null,
                                                          menuItem.getFont()));
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
    paintMenuItem(g, c, checkIcon, arrowIcon, selectionBackground,
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
    Color saved = g.getColor();
    if (mod.isArmed() || ((menuItem instanceof JMenu) && mod.isSelected()))
      {
        g.setColor(bgColor);
        g.fillRect(0, 0, menuItem.getWidth(), menuItem.getHeight());
      }
    else if (menuItem.isOpaque())
      {
        g.setColor(menuItem.getBackground());
        g.fillRect(0, 0, menuItem.getWidth(), menuItem.getHeight());
      }
    g.setColor(saved);
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

    // Fetch fonts.
    Font oldFont = g.getFont();
    Font font = c.getFont();
    g.setFont(font);
    FontMetrics accelFm = m.getFontMetrics(acceleratorFont);

    // Create accelerator string.
    String accelText = getAcceleratorString(m);

    // Layout menu item. The result gets stored in the rectangle fields
    // of this class.
    resetRectangles(m);

    layoutMenuItem(m, accelText);

    // Paint the background.
    paintBackground(g, m, background);

    Color oldColor = g.getColor();

    // Paint the check icon.
    if (checkIcon != null)
      {
        checkIcon.paintIcon(m, g, checkIconRect.x, checkIconRect.y);
      }

    // Paint the icon.
    ButtonModel model = m.getModel();
    if (m.getIcon() != null)
      {
        // Determine icon depending on the menu item
        // state (normal/disabled/pressed).
        Icon icon;
        if (! m.isEnabled())
          {
            icon = m.getDisabledIcon();
          }
        else if (model.isPressed() && model.isArmed())
          {
            icon = m.getPressedIcon();
            if (icon == null)
              {
                icon = m.getIcon();
              }
          }
        else
          {
            icon = m.getIcon();
          }

        if (icon != null)
          {
            icon.paintIcon(m, g, iconRect.x, iconRect.y);
          }
      }

    // Paint the text.
    String text = m.getText();
    if (text != null)
      {
        // Handle HTML.
        View html = (View) m.getClientProperty(BasicHTML.propertyKey);
        if (html != null)
          {
            html.paint(g, textRect);
          }
        else
          {
            paintText(g, m, textRect, text);
          }
      }

    // Paint accelerator text.
    if (! accelText.equals(""))
      {
        // Align the accelerator text. In getPreferredMenuItemSize() we
        // store a client property 'maxAccelWidth' in the parent which holds
        // the maximum accelerator width for the children of this parent.
        // We use this here to align the accelerators properly.
        int accelOffset = 0;
        Container parent = m.getParent();
        if (parent != null && parent instanceof JComponent)
          {
            JComponent p = (JComponent) parent;
            Integer maxAccelWidth =
              (Integer) p.getClientProperty("maxAccelWidth");
            int maxAccelValue = maxAccelWidth == null ? 0
                                                    : maxAccelWidth.intValue();
            accelOffset = maxAccelValue - accelRect.width;
          }

        g.setFont(acceleratorFont);
        if (! m.isEnabled())
          {
            // Paint accelerator disabled.
            g.setColor(disabledForeground);
          }
        else
          {
            if (m.isArmed() || (m instanceof JMenu && m.isSelected()))
              g.setColor(acceleratorSelectionForeground);
            else
              g.setColor(acceleratorForeground);
          }
        g.drawString(accelText, accelRect.x - accelOffset,
                     accelRect.y + accelFm.getAscent());
      }

    // Paint arrow.
    if (arrowIcon != null
        && ! (m instanceof JMenu && ((JMenu) m).isTopLevelMenu()))
      {
        arrowIcon.paintIcon(m, g, arrowIconRect.x, arrowIconRect.y);
      }

    g.setFont(oldFont);
    g.setColor(oldColor);

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
          BasicGraphicsUtils.drawStringUnderlineCharAt(menuItem, g, text,
                                                       mnemonicIndex,
                                                       textRect.x,
                                                       textRect.y
                                                           + fm.getAscent());
        else
          BasicGraphicsUtils.drawString(menuItem, g, text, 0, textRect.x,
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
    c.putClientProperty(BasicGraphicsUtils.CACHED_TEXT_LAYOUT, null);
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
      MenuSelectionManager manager = MenuSelectionManager.defaultManager();
      int x = e.getX();
      int y = e.getY();
      if (x > 0 && x < menuItem.getWidth() && y > 0
          && y < menuItem.getHeight())
        {
          doClick(manager);
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
      MenuSelectionManager manager = e.getMenuSelectionManager();
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
      MenuSelectionManager manager = e.getMenuSelectionManager();
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
      // Nothing to do here yet.
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
      MenuSelectionManager manager = e.getMenuSelectionManager();
      int x = e.getX();
      int y = e.getY();
      if (x >= 0 && x < menuItem.getWidth() && y >= 0
          && y < menuItem.getHeight())
        doClick(manager);
      else
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

  /**
   * A helper method to create the accelerator string from the menu item's
   * accelerator property. The returned string is empty if there is
   * no accelerator defined.
   *
   * @param m the menu item
   *
   * @return the accelerator string, not null
   */
  private String getAcceleratorString(JMenuItem m)
  {
    // Create accelerator string.
    KeyStroke accel = m.getAccelerator();
    String accelText = "";
    if (accel != null)
      {
        int mods = accel.getModifiers();
        if (mods > 0)
          {
            accelText = KeyEvent.getKeyModifiersText(mods);
            accelText += acceleratorDelimiter;
          }
        int keycode = accel.getKeyCode();
        if (keycode != 0)
          accelText += KeyEvent.getKeyText(keycode);
        else
          accelText += accel.getKeyChar();
      }
    return accelText;
  }

  /**
   * Resets the cached layout rectangles. If <code>i</code> is not null, then
   * the view rectangle is set to the inner area of the component, otherwise
   * it is set to (0, 0, Short.MAX_VALUE, Short.MAX_VALUE), this is needed
   * for layouting.
   *
   * @param i the component for which to initialize the rectangles
   */
  private void resetRectangles(JMenuItem i)
  {
    // Reset rectangles.
    iconRect.setBounds(0, 0, 0, 0);
    textRect.setBounds(0, 0, 0, 0);
    accelRect.setBounds(0, 0, 0, 0);
    checkIconRect.setBounds(0, 0, 0, 0);
    arrowIconRect.setBounds(0, 0, 0, 0);
    if (i == null)
      viewRect.setBounds(0, 0, Short.MAX_VALUE, Short.MAX_VALUE);
    else
      {
        Insets insets = i.getInsets();
        viewRect.setBounds(insets.left, insets.top,
                           i.getWidth() - insets.left - insets.right,
                           i.getHeight() - insets.top - insets.bottom);
      }
  }

  /**
   * A helper method that lays out the menu item. The layout is stored
   * in the fields of this class.
   *
   * @param m the menu item to layout
   * @param accelText the accelerator text
   */
  private void layoutMenuItem(JMenuItem m, String accelText)
  {
    // Fetch the fonts.
    Font font = m.getFont();
    FontMetrics fm = m.getFontMetrics(font);
    FontMetrics accelFm = m.getFontMetrics(acceleratorFont);

    String text = m.getText();
    SwingUtilities.layoutCompoundLabel(m, fm, text, m.getIcon(),
                                       m.getVerticalAlignment(),
                                       m.getHorizontalAlignment(),
                                       m.getVerticalTextPosition(),
                                       m.getHorizontalTextPosition(),
                                       viewRect, iconRect, textRect,
                                       defaultTextIconGap);

    // Initialize accelerator width and height.
    if (! accelText.equals(""))
      {
        accelRect.width = accelFm.stringWidth(accelText);
        accelRect.height = accelFm.getHeight();
      }

    // Initialize check and arrow icon width and height.
    if (! (m instanceof JMenu && ((JMenu) m).isTopLevelMenu()))
      {
        if (checkIcon != null)
          {
            checkIconRect.width = checkIcon.getIconWidth();
            checkIconRect.height = checkIcon.getIconHeight();
          }
        if (arrowIcon != null)
          {
            arrowIconRect.width = arrowIcon.getIconWidth();
            arrowIconRect.height = arrowIcon.getIconHeight();
          }
      }

    // The union of the icon and text of the menu item is the 'label area'.
    cachedRect.setBounds(textRect);
    Rectangle labelRect = SwingUtilities.computeUnion(iconRect.x,
                                                      iconRect.y,
                                                      iconRect.width,
                                                      iconRect.height,
                                                      cachedRect);
    textRect.x += defaultTextIconGap;
    iconRect.x += defaultTextIconGap;

    // Layout accelerator rect.
    accelRect.x = viewRect.x + viewRect.width - arrowIconRect.width
      - defaultTextIconGap - accelRect.width;
    // Layout check and arrow icons only when not in toplevel menu.
    if (! (m instanceof JMenu && ((JMenu) m).isTopLevelMenu()))
      {
        checkIconRect.x = viewRect.x + defaultTextIconGap;
        textRect.x += defaultTextIconGap + checkIconRect.width;
        iconRect.x += defaultTextIconGap + checkIconRect.width;
        arrowIconRect.x = viewRect.x + viewRect.width - defaultTextIconGap
          - arrowIconRect.width;
      }

    // Align the accelerator text and all the icons vertically centered to
    // the menu text.
    accelRect.y = labelRect.y + (labelRect.height / 2)
      - (accelRect.height / 2);
    if (! (m instanceof JMenu && ((JMenu) m).isTopLevelMenu()))
      {
        arrowIconRect.y = labelRect.y + (labelRect.height / 2)
          - (arrowIconRect.height / 2);
        checkIconRect.y = labelRect.y + (labelRect.height / 2)
          - (checkIconRect.height / 2);
      }
  }
}
