/* BasicButtonListener.java --
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

import java.awt.event.ActionEvent;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.font.FontRenderContext;
import java.awt.font.TextLayout;
import java.awt.geom.AffineTransform;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.AbstractAction;
import javax.swing.AbstractButton;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.ButtonModel;
import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.ActionMapUIResource;
import javax.swing.plaf.ButtonUI;

public class BasicButtonListener
  implements MouseListener, MouseMotionListener, FocusListener, ChangeListener,
             PropertyChangeListener
{
  /**
   * Implements the keyboard action for Swing buttons.
   */
  private class ButtonAction
    extends AbstractAction
  {
    /**
     * The key for pressed action.
     */
    static final String PRESSED = "pressed";

    /**
     * The key for released action.
     */
    static final String RELEASED = "released";

    /**
     * Performs the action.
     */
    public void actionPerformed(ActionEvent event)
    {
      Object cmd = getValue("__command__");
      AbstractButton b = (AbstractButton) event.getSource();
      ButtonModel m = b.getModel();
      if (PRESSED.equals(cmd))
        {
          m.setArmed(true);
          m.setPressed(true);
          if (! b.isFocusOwner())
            b.requestFocus();
        }
      else if (RELEASED.equals(cmd))
        {
          m.setPressed(false);
          m.setArmed(false);
        }
    }

    /**
     * Indicates if this action is enabled.
     *
     * @param source the source of the action
     *
     * @return <code>true</code> when enabled, <code>false</code> otherwise
     */
    public boolean isEnabled(Object source)
    {
      boolean enabled = true;
      if (source instanceof AbstractButton)
        {
          AbstractButton b = (AbstractButton) source;
          enabled = b.isEnabled();
        }
      return enabled;
    }
  }

  public BasicButtonListener(AbstractButton b)
  {
    // Do nothing here.
  }
  
  public void propertyChange(PropertyChangeEvent e)
  {
    // Store the TextLayout for this in a client property for speed-up
    // painting of the label.
    String property = e.getPropertyName();
    AbstractButton b = (AbstractButton) e.getSource();
    if ((property.equals(AbstractButton.TEXT_CHANGED_PROPERTY)
         || property.equals("font"))
        && SystemProperties.getProperty("gnu.javax.swing.noGraphics2D")
        == null)
      {
        String text = b.getText();
        if (text == null)
          text = "";
        FontRenderContext frc = new FontRenderContext(new AffineTransform(),
                                                      false, false);
        TextLayout layout = new TextLayout(text, b.getFont(), frc);
        b.putClientProperty(BasicGraphicsUtils.CACHED_TEXT_LAYOUT, layout);

        // Update HTML renderer.
        BasicHTML.updateRenderer(b, b.getText());
      }
    else if (property.equals(AbstractButton.CONTENT_AREA_FILLED_CHANGED_PROPERTY))
      {
        checkOpacity(b);
      }
  }

  /**
   * Checks the <code>contentAreaFilled</code> property and updates the
   * opaque property of the button.
   *
   * @param b the button to check
   */
  protected void checkOpacity(AbstractButton b) 
  {    
    b.setOpaque(b.isContentAreaFilled());
  }
  
  public void focusGained(FocusEvent e) 
  {    
    if (e.getSource() instanceof AbstractButton)
      {
        AbstractButton button = (AbstractButton) e.getSource();
        if (button.isFocusPainted())
          button.repaint();   
      }
  }
  
  public void focusLost(FocusEvent e)
  {
    if (e.getSource() instanceof AbstractButton)
      {
        AbstractButton button = (AbstractButton) e.getSource();
        if (button.isFocusPainted())
          button.repaint();   
      }
  }
  
  public void installKeyboardActions(JComponent c)
  {
    ButtonUI ui = ((AbstractButton) c).getUI();
    if (ui instanceof BasicButtonUI)
      {
        // Install InputMap.
        BasicButtonUI basicUI = (BasicButtonUI) ui;
        String prefix = basicUI.getPropertyPrefix(); 
        InputMap focusInputMap =
          (InputMap) UIManager.get(prefix + "focusInputMap");
        SwingUtilities.replaceUIInputMap(c, JComponent.WHEN_FOCUSED,
                                         focusInputMap);

        ActionMap am = (ActionMap) UIManager.get(prefix + "actionMap");
        if (am == null)
          {
            am = createDefaultActionMap();
            UIManager.put(prefix + "actionMap", am);
          }
        SwingUtilities.replaceUIActionMap(c, am);
      }
    
    c.getActionMap().put("pressed", 
                         new AbstractAction() 
                         {
                           public void actionPerformed(ActionEvent e)          
                           {
                             AbstractButton button = (AbstractButton) e.getSource();
                             ButtonModel model = button.getModel();
                             // It is important that these transitions happen in this order.
                             model.setArmed(true);
                             model.setPressed(true);
                           }
                         });
    
    c.getActionMap().put("released", 
                         new AbstractAction() 
                         {
                           public void actionPerformed(ActionEvent e)          
                           {
                             AbstractButton button = (AbstractButton) e.getSource();
                             ButtonModel model = button.getModel();
                             // It is important that these transitions happen in this order.
                             model.setPressed(false);
                             model.setArmed(false);
                           }
                       });    
  }

  /**
   * Creates and returns the default action map for Swing buttons.
   *
   * @return the default action map for Swing buttons
   */
  private ActionMap createDefaultActionMap()
  {
    Action action = new ButtonAction();
    ActionMapUIResource am = new ActionMapUIResource();
    am.put(ButtonAction.PRESSED, action);
    am.put(ButtonAction.RELEASED, action);
    return am;
  }

  public void uninstallKeyboardActions(JComponent c)
  {
    SwingUtilities.replaceUIActionMap(c, null);
    SwingUtilities.replaceUIInputMap(c, JComponent.WHEN_FOCUSED, null);
  }
  
  public void stateChanged(ChangeEvent e)
  {
    // Need to repaint when the button state changes.
    ((AbstractButton) e.getSource()).repaint();
  }
  
  public void mouseMoved(MouseEvent e)
  {
    // Nothing to do here.
  }
  
  public void mouseDragged(MouseEvent e)
  {
    // Nothing to do here.
  }
  
  public void mouseClicked(MouseEvent e)
  {
    // Nothing to do here.
  }

  /**
   * Accept a mouse press event and arm the button.
   *
   * @param e The mouse press event to accept
   */
  public void mousePressed(MouseEvent e)
  {
    if (e.getSource() instanceof AbstractButton)
      {
        AbstractButton button = (AbstractButton) e.getSource();
        ButtonModel model = button.getModel();
        if (SwingUtilities.isLeftMouseButton(e))
          {
            // It is important that these transitions happen in this order.
            model.setArmed(true);
            model.setPressed(true);

            if (! button.isFocusOwner() && button.isRequestFocusEnabled())
              button.requestFocus();
          }
      }
  }

  /**
   * Accept a mouse release event and set the button's 
   * "pressed" property to <code>true</code>, if the model
   * is armed. If the model is not armed, ignore the event.
   *
   * @param e The mouse release event to accept
   */
  public void mouseReleased(MouseEvent e)
  {
    if (e.getSource() instanceof AbstractButton)
      {
        AbstractButton button = (AbstractButton) e.getSource();
        ButtonModel model = button.getModel();
        if (e.getButton() == MouseEvent.BUTTON1)
          {
            // It is important that these transitions happen in this order.
            model.setPressed(false);
            model.setArmed(false);
          }
      }
  }

  /**
   * Accept a mouse enter event and set the button's "rollover" property to
   * <code>true</code>, if the button's "rolloverEnabled" property is
   * <code>true</code>. If the button is currently armed and the mouse
   * button is not held down, this enter event will also disarm the model.
   *
   * @param e The mouse enter event to accept
   */
  public void mouseEntered(MouseEvent e)
  {
    if (e.getSource() instanceof AbstractButton)
      {
        AbstractButton button = (AbstractButton) e.getSource();
        ButtonModel model = button.getModel();
        if (button.isRolloverEnabled()
            && ! SwingUtilities.isLeftMouseButton(e))
          model.setRollover(true);

        if (model.isPressed())
          model.setArmed(true);
      }
  }

  /**
   * Accept a mouse exit event and set the button's model's "rollover"
   * property to <code>false</code>, if it's "rolloverEnabled" property is
   * <code>true</code>. Also disarm the button.
   *
   * @param e The mouse exit event to accept
   */
  public void mouseExited(MouseEvent e)
  {
    if (e.getSource() instanceof AbstractButton)
      {
        AbstractButton button = (AbstractButton) e.getSource();
        ButtonModel model = button.getModel();
        if (button.isRolloverEnabled())
          model.setRollover(false);
        model.setArmed(false);
      }
  }
}
