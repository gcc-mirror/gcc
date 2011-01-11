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

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Insets;

import javax.swing.JTextField;
import javax.swing.border.AbstractBorder;
import javax.swing.plaf.basic.BasicComboBoxEditor;
import javax.swing.plaf.metal.MetalLookAndFeel;

/**
 * An editor used by the {@link MetalComboBoxUI} class.
 */
public class MetalComboBoxEditor extends BasicComboBoxEditor
{
  /**
   * A border used for the {@link JTextField} component.
   */
  static class MetalComboBoxEditorBorder extends AbstractBorder
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
      g.translate(x, y);
      if (MetalLookAndFeel.getCurrentTheme() instanceof OceanTheme)
        {
          g.setColor(MetalLookAndFeel.getControlDarkShadow());
          g.drawLine(0, 0, w - 1, 0);
          g.drawLine(0, 0, 0, h - 1);
          g.drawLine(0, h - 1, w - 1, h - 1);
          g.setColor(MetalLookAndFeel.getControlShadow());
          g.drawLine(1, 1, w - 2, 1);
          g.drawLine(1, 1, 1, h - 2);
          g.drawLine(1, h - 2, w - 1, h - 2);
          g.drawLine(w - 1, 1, w - 1, h - 2);
        }
      else
        {
          g.setColor(MetalLookAndFeel.getControlDarkShadow());
          g.drawLine(0, 0, w - 1, 0);
          g.drawLine(0, 0, 0, h - 2);
          g.drawLine(0, h - 2, w - 1, h - 2);
          g.setColor(MetalLookAndFeel.getControlHighlight());
          g.drawLine(1, 1, w - 1, 1);
          g.drawLine(1, 1, 1, h - 1);
          g.drawLine(1, h - 1, w - 1, h - 1);
          g.setColor(MetalLookAndFeel.getControl());
          g.drawLine(1, h - 2, 1, h - 2);
        }
      g.translate(-x, -y);
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

  /**
   * A special textfield implementation for the MetalComboBoxEditor.
   */
  private class EditorTextField extends JTextField
  {
    EditorTextField(String s, int columns)
    {
      super(s, columns);
    }

    /**
     * Tests seem to show that the textfield in MetalComboBoxEditors have
     * a height + 4.
     */
    public Dimension getPreferredSize()
    {
      Dimension size = super.getPreferredSize();
      size.height += 4;
      return size;
    }

    /**
     * Tests seem to show that the textfield in MetalComboBoxEditors have
     * a height + 4.
     */
    public Dimension getMinimumSize()
    {
      Dimension size = super.getMinimumSize();
      size.height += 4;
      return size;
    }
  }

  /** The editor's border insets. */
  protected static Insets editorBorderInsets = new Insets(2, 2, 2, 0);

  /**
   * Creates a new editor.
   */
  public MetalComboBoxEditor()
  {
    editor = new EditorTextField("", 9);
    editor.setBorder(new MetalComboBoxEditorBorder());
  }

}
