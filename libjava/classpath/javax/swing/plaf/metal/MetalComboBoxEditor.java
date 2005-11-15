/* MetalComboBoxEditor.java
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
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Insets;

import javax.swing.JTextField;
import javax.swing.plaf.basic.BasicComboBoxEditor;
import javax.swing.plaf.metal.MetalLookAndFeel;
import javax.swing.plaf.metal.MetalBorders.Flush3DBorder;

/**
 * An editor used by the {@link MetalComboBoxUI} class.
 */
public class MetalComboBoxEditor extends BasicComboBoxEditor
{
  /**
   * A border used for the {@link JTextField} component.
   */
  static class MetalComboBoxEditorBorder extends Flush3DBorder
  {
    /**
     * Creates a new border instance.
     */
    public MetalComboBoxEditorBorder()
    {
      // Nothing to do here.
    }
    
    /**
     * Paints the border for the specified component.
     * 
     * @param c  the component (ignored).
     * @param g  the graphics device.
     * @param x  the x-coordinate.
     * @param y  the y-coordinate.
     * @param w  the width.
     * @param h  the height.
     */
    public void paintBorder(Component c, Graphics g, int x, int y, int w, 
        int h)
    {   
      Color savedColor = g.getColor();
      if (c.isEnabled())
        g.setColor(MetalLookAndFeel.getControlDarkShadow());  
      else
        g.setColor(MetalLookAndFeel.getControlShadow());
      g.drawLine(x, y, x + w - 1, y);
      g.drawLine(x, y, x, y + h - 2);
      g.drawLine(x + 2, y + h - 2, x + w - 1, y + h - 2);
      g.setColor(MetalLookAndFeel.getControl());
      g.drawLine(x + 1, y + h - 2, x + 1, y + h - 2);
      g.setColor(MetalLookAndFeel.getWhite());
      g.drawLine(x, y + h - 1, x + w - 1, y + h - 1);
      g.setColor(savedColor);
    }

    /**
     * Measures the width of this border.
     *
     * @param c the component whose border is to be measured.
     *
     * @return an Insets object whose <code>left</code>, <code>right</code>,
     *         <code>top</code> and <code>bottom</code> fields indicate the
     *         width of the border at the respective edge, which is zero
     *         for the default implementation provided by AbstractButton.
     *
     * @see #getBorderInsets(java.awt.Component, java.awt.Insets)
     */
    public Insets getBorderInsets(Component c)
    {
      return editorBorderInsets;
    }    
  }
    
  /**
   * A subclass of {@link MetalComboBoxEditor} that implements the 
   * {@link javax.swing.plaf.UIResource} interface.
   */
  public static class UIResource extends MetalComboBoxEditor
    implements javax.swing.plaf.UIResource
  {
    /**
     * Creates a new instance.
     */
    public UIResource()
    {
      // Nothing to do here.
    }
  }
  
  /** The editor's border insets. */
  protected static Insets editorBorderInsets = new Insets(4, 2, 4, 0);
  
  /**
   * Creates a new editor.
   */
  public MetalComboBoxEditor()
  {
    super();
    editor.setBorder(new MetalComboBoxEditorBorder());
  }
  
}
