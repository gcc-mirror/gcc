/* BasicButtonListener.java --
   Copyright (C) 2004 Free Software Foundation, Inc.

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

import java.awt.event.ActionEvent;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.AbstractAction;
import javax.swing.AbstractButton;
import javax.swing.ButtonModel;
import javax.swing.JComponent;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

public class BasicButtonListener
  implements MouseListener, MouseMotionListener, FocusListener, 
             ChangeListener, PropertyChangeListener
{
  public BasicButtonListener(AbstractButton b)
  {
    // Do nothing here.
  }
  
  public void propertyChange(PropertyChangeEvent e)
  {
  }
  
  protected void checkOpacity(AbstractButton b) 
  {    
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
        ButtonModel model = button.getModel();
        model.setArmed(false);

        if (button.isFocusPainted())
          button.repaint();   
      }
  }
  
  public void installKeyboardActions(JComponent c)
  {
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
  
  public void uninstallKeyboardActions(JComponent c)
  {
    c.getActionMap().put("pressed", null);
    c.getActionMap().put("released", null);
  }
  
  public void stateChanged(ChangeEvent e)
  {
  }
  
  public void mouseMoved(MouseEvent e)
  {
  }
  
  public void mouseDragged(MouseEvent e)
  {
  }
  
  public void mouseClicked(MouseEvent e)
  {
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
        if ((e.getModifiersEx() & InputEvent.BUTTON1_DOWN_MASK) != 0)
          {
            // It is important that these transitions happen in this order.
            model.setArmed(true);
            model.setPressed(true);
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
        if ((e.getModifiersEx() & InputEvent.BUTTON1_DOWN_MASK) != 0)
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
        if (button.isRolloverEnabled())
          model.setRollover(true);
        
        if (model.isPressed() 
            && (e.getModifiersEx() & InputEvent.BUTTON1_DOWN_MASK) != 0)
          model.setArmed(true);
        else
          model.setArmed(false);
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
