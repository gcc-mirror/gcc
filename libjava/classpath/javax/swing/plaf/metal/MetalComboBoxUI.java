/* MetalComboBoxUI.java
   Copyright (C) 2005 Free Software Foundation, Inc.

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
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.ComboBoxEditor;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicComboBoxUI;
import javax.swing.plaf.basic.BasicComboPopup;
import javax.swing.plaf.basic.ComboPopup;


/**
 * A UI delegate for the {@link JComboBox} component.
 */
public class MetalComboBoxUI extends BasicComboBoxUI
{
  /**
   * A layout manager that arranges the editor component (if active) and the
   * button that make up the combo box.
   */
  public class MetalComboBoxLayoutManager
    extends BasicComboBoxUI.ComboBoxLayoutManager
  {
    /**
     * Creates a new instance of the layout manager.
     */
    public MetalComboBoxLayoutManager()
    {
      // Nothing to do here.
    }
    
    /**
     * Arranges the editor (if visible) and button that comprise the combo
     * box.
     * 
     * @param parent  the parent.
     */
    public void layoutContainer(Container parent)
    {
      layoutComboBox(parent, this);
    }
    
    /**
     * Calls the <code>layoutContainer(Container)</code> method in the super 
     * class.
     * 
     * @param parent  the container.
     */
    public void superLayout(Container parent)
    {
      super.layoutContainer(parent);
    }
  }
  
  /**
   * A listener used to handle property changes in the {@link JComboBox} 
   * component, to ensure that the UI delegate accurately reflects the current
   * state in the rendering onscreen.
   */
  public class MetalPropertyChangeListener
    extends BasicComboBoxUI.PropertyChangeHandler
  {
    /**
     * Creates a new listener.
     */
    public MetalPropertyChangeListener()
    {
      // Nothing to do here.
    }
    
    /**
     * Handles a property change event, updating the UI components as
     * appropriate.
     * 
     * @param e  the event.
     */
    public void propertyChange(PropertyChangeEvent e)
    {
      super.propertyChange(e);
      String name = e.getPropertyName();
      if (name.equals("editable"))
        editablePropertyChanged(e);
      else if (name.equals("enabled"))
        {
          if (arrowButton instanceof MetalComboBoxButton)
            {
              arrowButton.setFocusable(!comboBox.isEditable()
                                       && comboBox.isEnabled());
              comboBox.repaint();
            }
        }
      else if (name.equals("background"))
        {
          Color c = (Color) e.getNewValue();
          arrowButton.setBackground(c);
          listBox.setBackground(c);
        }
      else if (name.equals("foreground"))
        {
          Color c = (Color) e.getNewValue();
          arrowButton.setForeground(c);
          listBox.setForeground(c);
        }
    }
  }

  /**
   * A popup menu for the combo-box.
   * 
   * @see #createPopup()
   *
   * @deprecated 1.4
   */
  public class MetalComboPopup extends BasicComboPopup
  {
    /**
     * Creates a new popup.
     * 
     * @param cBox  the combo box.
     */
    public MetalComboPopup(JComboBox cBox)
    {
      super(cBox); 
    }
    
    public void delegateFocus(MouseEvent e)
    {
      super.delegateFocus(e);
    }
  }
  
  /**
   * Constructs a new instance of MetalComboBoxUI.
   */
  public MetalComboBoxUI()
  {
    super();
  }

  /**
   * Returns an instance of MetalComboBoxUI.
   *
   * @param component the component for which we return an UI instance
   *
   * @return an instance of MetalComboBoxUI
   */
  public static ComponentUI createUI(JComponent component)
  {
    return new MetalComboBoxUI();
  }
  
  /**
   * Creates an editor for the combo box.
   * 
   * @return An editor.
   */
  protected ComboBoxEditor createEditor()
  {
    return new MetalComboBoxEditor.UIResource();   
  }
  
  /**
   * Creates a popup for the combo box.
   * 
   * @return A popup.
   */
  protected ComboPopup createPopup()
  {
    return super.createPopup();
  }
  
  /**
   * Creates a new button for use in rendering the JComboBox.
   * 
   * @return A button.
   */
  protected JButton createArrowButton()
  {
    JButton button = new MetalComboBoxButton(comboBox, new MetalComboBoxIcon(), 
            currentValuePane, listBox);  
    button.setMargin(new Insets(0, 1, 1, 3));
    return button;
  }
  
  /**
   * Creates a new property change listener.
   * 
   * @return A new property change listener.
   */
  public PropertyChangeListener createPropertyChangeListener()
  {
    return new MetalPropertyChangeListener();
  }
  
  public void paint(Graphics g, JComponent c)
  {
    // do nothing, the button and text field are painted elsewhere
  }
  
  /**
   * Updates the button and text field to reflect a change in the 'editable'
   * property.
   * 
   * @param e  the event.
   * 
   * @deprecated 1.4
   */
  protected void editablePropertyChanged(PropertyChangeEvent e)
  {
    if (arrowButton instanceof MetalComboBoxButton)
      {
        MetalComboBoxButton b = (MetalComboBoxButton) arrowButton;
        b.setIconOnly(comboBox.isEditable());
        b.setFocusable(!comboBox.isEditable() && comboBox.isEnabled());
        comboBox.repaint();
      }
  }
  
  /**
   * Creates a new layout manager for the UI delegate.
   * 
   * @return A new layout manager.
   */
  protected LayoutManager createLayoutManager()
  {
    return new MetalComboBoxLayoutManager();
  }
  
  /**
   * Not used in Classpath.
   * 
   * @deprecated 1.4
   */
  protected void removeListeners()
  {
    // no longer used in JDK 1.4 
  }
  
  /**
   * Returns the minimum size for the combo.
   * 
   * @param c  the component
   * 
   * @return The minimum size for the combo box.
   */
  public Dimension getMinimumSize(JComponent c)
  {
    if (!isMinimumSizeDirty)
      return new Dimension(cachedMinimumSize);

    Dimension d;
    if (!comboBox.isEditable() && arrowButton != null
        && arrowButton instanceof MetalComboBoxButton)
      {
        MetalComboBoxButton b = (MetalComboBoxButton) arrowButton;
        d = getDisplaySize();
        Insets arrowInsets = b.getInsets();
        Insets comboInsets = comboBox.getInsets();
        Icon icon = b.getComboIcon();
        d.width += comboInsets.left + comboInsets.right;
        d.width += arrowInsets.left + arrowInsets.right;
        d.width += arrowInsets.right + icon.getIconWidth();
        d.height += comboInsets.top + comboInsets.bottom;
        d.height += arrowInsets.top + arrowInsets.bottom;
      }
    else if (comboBox.isEditable() && arrowButton != null && editor != null)
      {
        d = super.getMinimumSize(c);
        Insets arrowMargin = arrowButton.getMargin();
        d.height += arrowMargin.top + arrowMargin.bottom;
        d.width += arrowMargin.left + arrowMargin.right;
      }
    else
      {
        d = super.getMinimumSize(c);
      }
    cachedMinimumSize.setSize(d.width, d.height);
    isMinimumSizeDirty = false;
    return new Dimension(cachedMinimumSize);
  }
  
  /**
   * Configures the editor for this combo box.
   */
  public void configureEditor()
  {
    super.configureEditor();
    if (popupKeyListener != null)
      editor.removeKeyListener(popupKeyListener);
    if (focusListener != null)
      editor.addFocusListener(focusListener);
  }

  /**
   * Unconfigures the editor for this combo box.
   */
  public void unconfigureEditor()
  {
    super.unconfigureEditor();
    if (focusListener != null)
      editor.removeFocusListener(focusListener);
  }
  
  /** 
   * Lays out the ComboBox
   */
  public void layoutComboBox(Container parent,
                             MetalComboBoxUI.MetalComboBoxLayoutManager manager)
  {
    if (comboBox.isEditable())
      manager.superLayout(parent);
    else if (arrowButton != null)
      {
        Insets comboInsets = comboBox.getInsets();
        int width = comboBox.getWidth();
        int height = comboBox.getHeight();
        arrowButton.setBounds(comboInsets.left, comboInsets.top,
                              width - (comboInsets.left + comboInsets.right),
                              height - (comboInsets.top + comboInsets.bottom));
      }
  }
}
