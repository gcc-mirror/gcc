/* BasicMenuItemUI.java
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
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;

import javax.swing.AbstractButton;
import javax.swing.ButtonModel;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;
import javax.swing.MenuElement;
import javax.swing.MenuSelectionManager;
import javax.swing.SwingUtilities;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.event.MenuDragMouseEvent;
import javax.swing.event.MenuDragMouseListener;
import javax.swing.event.MenuKeyEvent;
import javax.swing.event.MenuKeyListener;
import javax.swing.event.MouseInputListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.MenuItemUI;


public class BasicMenuItemUI extends MenuItemUI
{
  /** Font to be used when displaying menu item's accelerator. */
  protected Font acceleratorFont;

  /** Color to be used when displaying menu item's accelerator. */  
  protected Color acceleratorForeground;

  /** Color to be used when displaying menu item's accelerator
   * when menu item is selected.
   */ 
  protected Color acceleratorSelectionForeground;
  
  /**
   * Icon that is displayed after the text to indicated that this menu
   * contains submenu.
   */
  protected Icon arrowIcon;
  
  /**
   * Icon that is displayed before the text. This icon is only used in
   * JCheckBoxMenuItem or JRadioBoxMenuItem.
   */
  protected Icon checkIcon;

  /** Number of spaces between icon and text. */  
  protected int defaultTextIconGap = 4;
  
  /** Color of the text when menu item is disabled*/
  protected Color disabledForeground;
  
  /** The menu Drag mouse listener listening to the menu item. */
  protected MenuDragMouseListener menuDragMouseListener;
  
  /** The menu item itself*/
  protected JMenuItem menuItem;
  
  /** Menu Key listener listening to the menu item. */
  protected MenuKeyListener menuKeyListener;
  
  /** mouse input listener listening to menu item. */
  protected MouseInputListener mouseInputListener;
  
  /** Indicates if border should be painted  */
  protected boolean oldBorderPainted;

  /** Color of text that is used when menu item is selected */
  protected Color selectionBackground;
  
  /** Color of the background that is used when menu item is selected.*/ 
  protected Color selectionForeground;

  /** String that separates description of the modifiers and the key*/  
  private String acceleratorDelimiter;
  
  /** Number of spaces between accelerator and menu item's label. */
  private int defaultAcceleratorLabelGap = 4;

  // Constructor Summary
  BasicMenuItemUI()
  {
    mouseInputListener = createMouseInputListener(menuItem);
    menuDragMouseListener = createMenuDragMouseListener(menuItem);
    menuKeyListener = createMenuKeyListener(menuItem);
  }

  // Method Summary
  protected MenuDragMouseListener createMenuDragMouseListener(JComponent c)
  {
    return new MenuDragMouseHandler();
  }

  protected MenuKeyListener createMenuKeyListener(JComponent c)
  {
    return new MenuKeyHandler();
  }

  protected MouseInputListener createMouseInputListener(JComponent c)
  {
    return new MouseInputHandler();
  }

  public static ComponentUI createUI(JComponent c)
  {
    return new BasicMenuItemUI();
  }

  protected void doClick(MenuSelectionManager msm)
  {
    // TODO
  }

  public Dimension getMaximumSize(JComponent c)
  {
    // TODO    
    return null;
  }

  public Dimension getMinimumSize(JComponent c)
  {
    // TODO
    return null;
  }

  public MenuElement[] getPath()
  {
    // TODO
    return null;
  }

  protected Dimension getPreferredMenuItemSize(JComponent c, Icon checkIcon,
                                               Icon arrowIcon,
                                               int defaultTextIconGap)
  {
    // TODO
    return null;
  }

  public Dimension getPreferredSize(JComponent c)
  {
    AbstractButton b = (AbstractButton) c;
    Dimension d = BasicGraphicsUtils.getPreferredButtonSize(b,
                                                            defaultTextIconGap);
    
    // if menu item has accelerator then take accelerator's size into account
    // when calculating preferred size.
    
    KeyStroke accelerator = ((JMenuItem) c).getAccelerator();
    Rectangle rect;
    if (accelerator != null)
      {
        rect = getAcceleratorRect(accelerator,
                                  b.getToolkit().getFontMetrics(acceleratorFont));
        
        // add width of accelerator's text
        d.width = d.width + rect.width + defaultAcceleratorLabelGap;

        // adjust the heigth of the preferred size if necessary
        if (d.height < rect.height)
          d.height = rect.height;
      }

    if (checkIcon != null)
      {
        d.width = d.width + checkIcon.getIconWidth() + defaultTextIconGap;
        if (checkIcon.getIconHeight() > d.height)
          d.height = checkIcon.getIconHeight();
      }
      
    return d;
  }

  protected String getPropertyPrefix()
  {
    // TODO
    return null;
  }

  protected void installComponents(JMenuItem menuItem)
  {
    // TODO
  }

  protected void installDefaults()
  {
    UIDefaults defaults = UIManager.getLookAndFeelDefaults();

    menuItem.setBackground(defaults.getColor("MenuItem.background"));
    menuItem.setBorder(defaults.getBorder("MenuItem.border"));
    menuItem.setFont(defaults.getFont("MenuItem.font"));
    menuItem.setForeground(defaults.getColor("MenuItem.foreground"));
    menuItem.setMargin(defaults.getInsets("MenuItem.margin"));
    acceleratorFont = defaults.getFont("MenuItem.acceleratorFont");
    acceleratorForeground = defaults.getColor("MenuItem.acceleratorForeground");
    acceleratorSelectionForeground = defaults.getColor("MenuItem.acceleratorSelectionForeground");
    arrowIcon = defaults.getIcon("MenuItem.arrowIcon");
    selectionBackground = defaults.getColor("MenuItem.selectionBackground");
    selectionForeground = defaults.getColor("MenuItem.selectionForeground");
    acceleratorDelimiter = defaults.getString("MenuItem.acceleratorDelimiter");
  }

  protected void installKeyboardActions()
  {
    // TODO
  }

  protected void installListeners()
  {
    menuItem.addMouseListener(mouseInputListener);
    menuItem.addMenuDragMouseListener(menuDragMouseListener);
    menuItem.addMenuKeyListener(menuKeyListener);
  }

  public void installUI(JComponent c)
  {
    super.installUI(c);
    menuItem = (JMenuItem) c;
    installDefaults();
    installListeners();
  }

  public void paint(Graphics g, JComponent c)
  {
    paintMenuItem(g, c, checkIcon, arrowIcon, c.getBackground(), 
                  c.getForeground(), defaultTextIconGap);
  }

  protected void paintBackground(Graphics g, JMenuItem menuItem, Color bgColor)
  {
    Dimension size = getPreferredSize(menuItem);
    Color foreground = g.getColor();
    g.setColor(bgColor);
    g.drawRect(0, 0, size.width, size.height);
    g.setColor(foreground);
  }

  protected void paintMenuItem(Graphics g, JComponent c, Icon checkIcon,
                               Icon arrowIcon, Color background,
                               Color foreground, int defaultTextIconGap)
  {
    AbstractButton b = (AbstractButton) c;
    Rectangle tr = new Rectangle(); // text rectangle
    Rectangle ir = new Rectangle(); // icon rectangle
    Rectangle vr = new Rectangle(); // view rectangle
    Rectangle br = new Rectangle(); // border rectangle
    Rectangle ar = new Rectangle(); // accelerator rectangle
    Rectangle cr = new Rectangle(); // checkIcon rectangle
    
    int vertAlign = b.getVerticalAlignment();
    int horAlign = b.getHorizontalAlignment();
    int vertTextPos = b.getVerticalTextPosition();
    int horTextPos = b.getHorizontalTextPosition();

    Font f = c.getFont();
    g.setFont(f);
    FontMetrics fm = g.getFontMetrics(f);    
    SwingUtilities.calculateInnerArea(b, br);
    SwingUtilities.calculateInsetArea(br, b.getMargin(), vr);        
    paintBackground(g, (JMenuItem) c, c.getBackground());
    
    if ((b.getModel().isArmed() && b.getModel().isPressed()))
      {
        if (((AbstractButton) b).isContentAreaFilled())
          {
            g.setColor(b.getBackground().darker());
            g.fillRect(br.x, br.y, br.width, br.height);
          }
      }
    else
      {
        if (((AbstractButton) b).isContentAreaFilled())
          {
            g.setColor(b.getBackground());
            g.fillRect(br.x, br.y, br.width, br.height);
          }
      }
    
    
    if (checkIcon != null)
      {
        SwingUtilities.layoutCompoundLabel(c, fm, null, checkIcon, vertAlign,
                                           horAlign, vertTextPos, horTextPos,
                                           vr, cr, tr, defaultTextIconGap);
        checkIcon.paintIcon(c, g, cr.x, cr.y);

        // We need to calculate position of the menu text and position of
        // user menu icon if there exists one relative to the check icon.
	// So we need to adjust view rectangle s.t. its starting point is at
	// checkIcon.width + defaultTextIconGap. 
	 
	 vr.x = cr.x + cr.width + defaultTextIconGap;
      }

    if (arrowIcon != null)
      {
        // FIXME: if this menu contains a submenu, we need to draw arrow icon 
        // here as well
      }


    // paint text and user menu icon if it exists	     
    SwingUtilities.layoutCompoundLabel(c, fm, b.getText(), b.getIcon(),
                                       vertAlign, horAlign, vertTextPos,
                                       horTextPos, vr, ir, tr,
                                       defaultTextIconGap);

    paintText(g, (JMenuItem) c, tr, b.getText());   
    
    // paint icon
    // FIXME: should paint different icon at different button state's.
    // i.e disabled icon when button is disabled.. etc.
    
    /*  
    Icon i = b.getIcon();
    if (i != null)
      {
         int x = ir.x;
         int y = ir.y;
         i.paintIcon(c, g, x, y);
      }
    */
        
    // paint accelerator    
    String acceleratorText = "";
    if (((JMenuItem) c).getAccelerator() != null)
      {
        acceleratorText = getAcceleratorText(((JMenuItem) c).getAccelerator());
        fm = g.getFontMetrics(acceleratorFont);
        ar.width = fm.stringWidth(acceleratorText);
        ar.x = br.width - ar.width;      
        vr.x = br.width - ar.width;
	
        SwingUtilities.layoutCompoundLabel(c, fm, acceleratorText, null,
                                          vertAlign, horAlign, vertTextPos,
                                          horTextPos, vr, ir, ar,
                                          defaultTextIconGap); 
	
        paintAccelerator(g, (JMenuItem) c, ar, acceleratorText);
	
      }          

  }

  protected void paintText(Graphics g, JMenuItem menuItem, Rectangle textRect,
                           String text)
  {
    Font f = menuItem.getFont();
    g.setFont(f);
    FontMetrics fm = g.getFontMetrics(f);
    g.setColor(menuItem.getForeground());

    BasicGraphicsUtils.drawString(g, text, 0, textRect.x,
                                 textRect.y + fm.getAscent());
  }

  protected void uninstallComponents(JMenuItem menuItem)
  {
    // TODO
  }

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

  protected void uninstallKeyboardActions()
  {
    // TODO
  }

  protected void uninstallListeners()
  {
    menuItem.removeMouseListener(mouseInputListener);
    menuItem.removeMenuDragMouseListener(menuDragMouseListener);
    menuItem.removeMenuKeyListener(menuKeyListener);
  }

  public void uninstallUI(JComponent c)
  {
    uninstallListeners();
    uninstallDefaults();
    menuItem = null;
  }

  public void update(Graphics g, JComponent c)
  {
    // TODO
  }

  private String getAcceleratorText(KeyStroke accelerator)
  {
    
    // convert keystroke into string format
    
    String modifiersText = "";
    int modifiers = accelerator.getModifiers();
    char keyChar = accelerator.getKeyChar();
    int keyCode = accelerator.getKeyCode();

    if (modifiers != 0)
      modifiersText = KeyEvent.getKeyModifiersText(modifiers) +
                      acceleratorDelimiter;

    if (keyCode == KeyEvent.VK_UNDEFINED)
      return modifiersText + keyChar;
    else
      return modifiersText + KeyEvent.getKeyText(keyCode);
  }

  private Rectangle getAcceleratorRect(KeyStroke accelerator, FontMetrics fm)
  {    
    int width = fm.stringWidth(getAcceleratorText(accelerator));
    int height = fm.getHeight();
    return new Rectangle(0, 0, width, height);
  }

  private void paintAccelerator(Graphics g, JMenuItem menuItem, Rectangle acceleratorRect,
                           String acceleratorText)
  {
    g.setFont(acceleratorFont);
    FontMetrics fm = g.getFontMetrics(acceleratorFont);
    g.setColor(acceleratorForeground);
    BasicGraphicsUtils.drawString(g, acceleratorText, 0, acceleratorRect.x,
                                 acceleratorRect.y + fm.getAscent());
  }

  protected class MouseInputHandler implements MouseInputListener
  {
    protected MouseInputHandler()
    {
    }

    public void mouseClicked(MouseEvent e)
    {
    }

    public void mouseDragged(MouseEvent e)
    {
    }

    public void mouseEntered(MouseEvent e)
    {
      if (e.getSource() instanceof AbstractButton)
        {
          AbstractButton button = (AbstractButton) e.getSource();
          ButtonModel model = button.getModel();

          if (button.isRolloverEnabled())
            {
              model.setRollover(true);
            }

          if (model.isPressed() &&
              ((e.getModifiers() & InputEvent.BUTTON1_MASK) != 0))
            {
              model.setArmed(true);
            }
          else
            {
              model.setArmed(false);
            }
        }
    }

    public void mouseExited(MouseEvent e)
    {
      if (e.getSource() instanceof AbstractButton)
        {
          AbstractButton button = (AbstractButton) e.getSource();
          ButtonModel model = button.getModel();

          if (button.isRolloverEnabled())
            {
              model.setRollover(false);
            }

          model.setArmed(false);
        }
    }

    public void mouseMoved(MouseEvent e)
    {
    }

    public void mousePressed(MouseEvent e)
    {
      if (e.getSource() instanceof AbstractButton)
        {
          AbstractButton button = (AbstractButton) e.getSource();
          ButtonModel model = button.getModel();

          if ((e.getModifiers() & InputEvent.BUTTON1_MASK) != 0)
            {
              // It is important that these transitions happen in this order.
              model.setArmed(true);
              model.setPressed(true);
            }
        }
    }

    public void mouseReleased(MouseEvent e)
    {

      if (e.getSource() instanceof AbstractButton)
        {
          AbstractButton button = (AbstractButton) e.getSource();
          ButtonModel model = button.getModel();

          if ((e.getModifiers() & InputEvent.BUTTON1_MASK) != 0)
            {
              // It is important that these transitions happen in this order.
              model.setPressed(false);
              model.setArmed(false);
            }
        }
    }
  }

  protected class MenuDragMouseHandler implements MenuDragMouseListener
  {
    public void menuDragMouseDragged(MenuDragMouseEvent e)
    {
      // TODO        
    }

    public void menuDragMouseEntered(MenuDragMouseEvent e)
    {
      // TODO        
    }

    public void menuDragMouseExited(MenuDragMouseEvent e)
    {
      // TODO        
    }

    public void menuDragMouseReleased(MenuDragMouseEvent e)
    {
      // TODO        
    }
  }

  protected class MenuKeyHandler implements MenuKeyListener
  {
    public void menuKeyPressed(MenuKeyEvent e)
    {
      // TODO        
    }

    public void menuKeyReleased(MenuKeyEvent e)
    {
      // TODO        
    }

    public void menuKeyTyped(MenuKeyEvent e)
    {
      // TODO    
    }
  }

  protected class PropertyChangeHandler
  {
    public void propertyChange(PropertyChangeEvent evt)
    {
      // TODO        
    }
  }
}
