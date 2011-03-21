/* MultiTextUI.java --
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

package javax.swing.plaf.multi;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.util.Iterator;
import java.util.Vector;

import javax.accessibility.Accessible;
import javax.swing.JComponent;
import javax.swing.LookAndFeel;
import javax.swing.UIManager;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.TextUI;
import javax.swing.text.BadLocationException;
import javax.swing.text.EditorKit;
import javax.swing.text.JTextComponent;
import javax.swing.text.Position;
import javax.swing.text.View;
import javax.swing.text.Position.Bias;

/**
 * A UI delegate that that coordinates multiple {@link TextUI}
 * instances, one from the primary look and feel, and one or more from the
 * auxiliary look and feel(s).
 *
 * @see UIManager#addAuxiliaryLookAndFeel(LookAndFeel)
 */
public class MultiTextUI extends TextUI
{

  /** A list of references to the actual component UIs. */
  protected Vector uis;

  /**
   * Creates a new <code>MultiTextUI</code> instance.
   *
   * @see #createUI(JComponent)
   */
  public MultiTextUI()
  {
    uis = new Vector();
  }

  /**
   * Creates a delegate object for the specified component.  If any auxiliary
   * look and feels support this component, a <code>MultiTextUI</code> is
   * returned, otherwise the UI from the default look and feel is returned.
   *
   * @param target  the component.
   *
   * @see MultiLookAndFeel#createUIs(ComponentUI, Vector, JComponent)
   */
  public static ComponentUI createUI(JComponent target)
  {
    MultiTextUI mui = new MultiTextUI();
    return MultiLookAndFeel.createUIs(mui, mui.uis, target);
  }

  /**
   * Calls the {@link ComponentUI#installUI(JComponent)} method for all
   * the UI delegates managed by this <code>MultiTextUI</code>.
   *
   * @param c  the component.
   */
  public void installUI(JComponent c)
  {
    Iterator iterator = uis.iterator();
    while (iterator.hasNext())
    {
      ComponentUI ui = (ComponentUI) iterator.next();
      ui.installUI(c);
    }
  }

  /**
   * Calls the {@link ComponentUI#uninstallUI(JComponent)} method for all
   * the UI delegates managed by this <code>MultiTextUI</code>.
   *
   * @param c  the component.
   */
  public void uninstallUI(JComponent c)
  {
    Iterator iterator = uis.iterator();
    while (iterator.hasNext())
    {
      ComponentUI ui = (ComponentUI) iterator.next();
      ui.uninstallUI(c);
    }
  }

  /**
   * Returns an array containing the UI delegates managed by this
   * <code>MultiTextUI</code>.  The first item in the array is always
   * the UI delegate from the installed default look and feel.
   *
   * @return An array of UI delegates.
   */
  public ComponentUI[] getUIs()
  {
    return MultiLookAndFeel.uisToArray(uis);
  }

  /**
   * Calls the {@link ComponentUI#contains(JComponent, int, int)} method for all
   * the UI delegates managed by this <code>MultiTextUI</code>,
   * returning the result for the UI delegate from the primary look and
   * feel.
   *
   * @param c  the component.
   * @param x  the x-coordinate.
   * @param y  the y-coordinate.
   *
   * @return <code>true</code> if the specified (x, y) coordinate falls within
   *         the bounds of the component as rendered by the UI delegate in the
   *         primary look and feel, and <code>false</code> otherwise.
   */
  public boolean contains(JComponent c, int x, int y)
  {
    boolean result = false;
    Iterator iterator = uis.iterator();
    // first UI delegate provides the return value
    if (iterator.hasNext())
      {
        ComponentUI ui = (ComponentUI) iterator.next();
        result = ui.contains(c, x, y);
      }
    // return values from auxiliary UI delegates are ignored
    while (iterator.hasNext())
      {
        ComponentUI ui = (ComponentUI) iterator.next();
        /* boolean ignored = */ ui.contains(c, x, y);
      }
    return result;
  }

  /**
   * Calls the {@link ComponentUI#update(Graphics, JComponent)} method for all
   * the UI delegates managed by this <code>MultiTextUI</code>.
   *
   * @param g  the graphics device.
   * @param c  the component.
   */
  public void update(Graphics g, JComponent c)
  {
    Iterator iterator = uis.iterator();
    while (iterator.hasNext())
    {
      ComponentUI ui = (ComponentUI) iterator.next();
      ui.update(g, c);
    }
  }

  /**
   * Calls the <code>paint(Graphics, JComponent)</code> method for all the UI
   * delegates managed by this <code>MultiTextUI</code>.
   *
   * @param g  the graphics device.
   * @param c  the component.
   */
  public void paint(Graphics g, JComponent c)
  {
    Iterator iterator = uis.iterator();
    while (iterator.hasNext())
    {
      ComponentUI ui = (ComponentUI) iterator.next();
      ui.paint(g, c);
    }
  }

  /**
   * Calls the {@link ComponentUI#getPreferredSize(JComponent)} method for all
   * the UI delegates managed by this <code>MultiTextUI</code>,
   * returning the preferred size for the UI delegate from the primary look and
   * feel.
   *
   * @param c  the component.
   *
   * @return The preferred size returned by the UI delegate from the primary
   *         look and feel.
   */
  public Dimension getPreferredSize(JComponent c)
  {
    Dimension result = null;
    Iterator iterator = uis.iterator();
    // first UI delegate provides the return value
    if (iterator.hasNext())
      {
        ComponentUI ui = (ComponentUI) iterator.next();
        result = ui.getPreferredSize(c);
      }
    // return values from auxiliary UI delegates are ignored
    while (iterator.hasNext())
      {
        ComponentUI ui = (ComponentUI) iterator.next();
        /* Dimension ignored = */ ui.getPreferredSize(c);
      }
    return result;
  }

  /**
   * Calls the {@link ComponentUI#getMinimumSize(JComponent)} method for all
   * the UI delegates managed by this <code>MultiTextUI</code>,
   * returning the minimum size for the UI delegate from the primary look and
   * feel.
   *
   * @param c  the component.
   *
   * @return The minimum size returned by the UI delegate from the primary
   *         look and feel.
   */
  public Dimension getMinimumSize(JComponent c)
  {
    Dimension result = null;
    Iterator iterator = uis.iterator();
    // first UI delegate provides the return value
    if (iterator.hasNext())
      {
        ComponentUI ui = (ComponentUI) iterator.next();
        result = ui.getMinimumSize(c);
      }
    // return values from auxiliary UI delegates are ignored
    while (iterator.hasNext())
      {
        ComponentUI ui = (ComponentUI) iterator.next();
        /* Dimension ignored = */ ui.getMinimumSize(c);
      }
    return result;
  }

  /**
   * Calls the {@link ComponentUI#getMaximumSize(JComponent)} method for all
   * the UI delegates managed by this <code>MultiTextUI</code>,
   * returning the maximum size for the UI delegate from the primary look and
   * feel.
   *
   * @param c  the component.
   *
   * @return The maximum size returned by the UI delegate from the primary
   *         look and feel.
   */
  public Dimension getMaximumSize(JComponent c)
  {
    Dimension result = null;
    Iterator iterator = uis.iterator();
    // first UI delegate provides the return value
    if (iterator.hasNext())
      {
        ComponentUI ui = (ComponentUI) iterator.next();
        result = ui.getMaximumSize(c);
      }
    // return values from auxiliary UI delegates are ignored
    while (iterator.hasNext())
      {
        ComponentUI ui = (ComponentUI) iterator.next();
        /* Dimension ignored = */ ui.getMaximumSize(c);
      }
    return result;
  }

  /**
   * Calls the {@link ComponentUI#getAccessibleChildrenCount(JComponent)} method
   * for all the UI delegates managed by this <code>MultiTextUI</code>,
   * returning the count for the UI delegate from the primary look and
   * feel.
   *
   * @param c  the component.
   *
   * @return The count returned by the UI delegate from the primary
   *         look and feel.
   */
  public int getAccessibleChildrenCount(JComponent c)
  {
    int result = 0;
    Iterator iterator = uis.iterator();
    // first UI delegate provides the return value
    if (iterator.hasNext())
      {
        ComponentUI ui = (ComponentUI) iterator.next();
        result = ui.getAccessibleChildrenCount(c);
      }
    // return values from auxiliary UI delegates are ignored
    while (iterator.hasNext())
      {
        ComponentUI ui = (ComponentUI) iterator.next();
        /* int ignored = */ ui.getAccessibleChildrenCount(c);
      }
    return result;
  }

  /**
   * Calls the {@link ComponentUI#getAccessibleChild(JComponent, int)} method
   * for all the UI delegates managed by this <code>MultiTextUI</code>,
   * returning the child for the UI delegate from the primary look and
   * feel.
   *
   * @param c  the component
   * @param i  the child index.
   *
   * @return The child returned by the UI delegate from the primary
   *         look and feel.
   */
  public Accessible getAccessibleChild(JComponent c, int i)
  {
    Accessible result = null;
    Iterator iterator = uis.iterator();
    // first UI delegate provides the return value
    if (iterator.hasNext())
      {
        ComponentUI ui = (ComponentUI) iterator.next();
        result = ui.getAccessibleChild(c, i);
      }
    // return values from auxiliary UI delegates are ignored
    while (iterator.hasNext())
      {
        ComponentUI ui = (ComponentUI) iterator.next();
        /* Accessible ignored = */ ui.getAccessibleChild(c, i);
      }
    return result;
  }

  /**
   * Calls the {@link TextUI#modelToView(JTextComponent, int)} method for all
   * the UI delegates managed by this <code>MultiTextUI</code>,
   * returning the bounds for the UI delegate from the primary look and
   * feel.
   *
   * @param tc  the text component.
   *
   * @return The bounds returned by the UI delegate from the primary
   *         look and feel.
   */
  public Rectangle modelToView(JTextComponent tc, int pos)
      throws BadLocationException
  {
    Rectangle result = null;
    Iterator iterator = uis.iterator();
    // first UI delegate provides the return value
    if (iterator.hasNext())
      {
        TextUI ui = (TextUI) iterator.next();
        result = ui.modelToView(tc, pos);
      }
    // return values from auxiliary UI delegates are ignored
    while (iterator.hasNext())
      {
        TextUI ui = (TextUI) iterator.next();
        /* Rectangle ignored = */ ui.modelToView(tc, pos);
      }
    return result;
  }

  /**
   * Calls the {@link TextUI#modelToView(JTextComponent, int, Position.Bias)}
   * method for all the UI delegates managed by this <code>MultiTextUI</code>,
   * returning the bounds for the UI delegate from the primary look and
   * feel.
   *
   * @param tc  the text component.
   *
   * @return The bounds returned by the UI delegate from the primary
   *         look and feel.
   */
  public Rectangle modelToView(JTextComponent tc, int pos, Bias bias)
          throws BadLocationException
  {
    Rectangle result = null;
    Iterator iterator = uis.iterator();
    // first UI delegate provides the return value
    if (iterator.hasNext())
      {
        TextUI ui = (TextUI) iterator.next();
        result = ui.modelToView(tc, pos, bias);
      }
    // return values from auxiliary UI delegates are ignored
    while (iterator.hasNext())
      {
        TextUI ui = (TextUI) iterator.next();
        /* Rectangle ignored = */ ui.modelToView(tc, pos, bias);
      }
    return result;
  }

  /**
   * Calls the {@link TextUI#viewToModel(JTextComponent, Point)} method for all
   * the UI delegates managed by this <code>MultiTextUI</code>,
   * returning the position for the UI delegate from the primary look and
   * feel.
   *
   * @param t  the text component.
   * @param pt  the point.
   *
   * @return The position returned by the UI delegate from the primary
   *         look and feel.
   */
  public int viewToModel(JTextComponent t, Point pt)
  {
    int result = 0;
    Iterator iterator = uis.iterator();
    // first UI delegate provides the return value
    if (iterator.hasNext())
      {
        TextUI ui = (TextUI) iterator.next();
        result = ui.viewToModel(t, pt);
      }
    // return values from auxiliary UI delegates are ignored
    while (iterator.hasNext())
      {
        TextUI ui = (TextUI) iterator.next();
        /* int ignored = */ ui.viewToModel(t, pt);
      }
    return result;
  }

  /**
   * Calls the {@link TextUI#viewToModel(JTextComponent, Point, Bias[])} method
   * for all the UI delegates managed by this <code>MultiTextUI</code>,
   * returning the position for the UI delegate from the primary look and
   * feel.
   *
   * @param tc  the text component.
   *
   * @return The position returned by the UI delegate from the primary
   *         look and feel.
   */
  public int viewToModel(JTextComponent tc, Point loc, Bias[] outBias)
  {
    int result = 0;
    Iterator iterator = uis.iterator();
    // first UI delegate provides the return value
    if (iterator.hasNext())
      {
        TextUI ui = (TextUI) iterator.next();
        result = ui.viewToModel(tc, loc, outBias);
      }
    // return values from auxiliary UI delegates are ignored
    while (iterator.hasNext())
      {
        TextUI ui = (TextUI) iterator.next();
        /* int ignored = */ ui.viewToModel(tc, loc, outBias);
      }
    return result;
  }

  /**
   * Calls the {@link TextUI#getNextVisualPositionFrom(JTextComponent, int,
   * Position.Bias, int, Position.Bias[])} method for all
   * the UI delegates managed by this <code>MultiTextUI</code>,
   * returning the position for the UI delegate from the primary look and
   * feel.
   *
   * @param tc  the text component.
   *
   * @return The position returned by the UI delegate from the primary
   *         look and feel.
   */
  public int getNextVisualPositionFrom(JTextComponent tc, int pos, Bias bias,
          int direction, Bias[] outBias) throws BadLocationException
  {
    int result = 0;
    Iterator iterator = uis.iterator();
    // first UI delegate provides the return value
    if (iterator.hasNext())
      {
        TextUI ui = (TextUI) iterator.next();
        result = ui.getNextVisualPositionFrom(tc, pos, bias, direction,
                outBias);
      }
    // return values from auxiliary UI delegates are ignored
    while (iterator.hasNext())
      {
        TextUI ui = (TextUI) iterator.next();
        /* int ignored = */ ui.getNextVisualPositionFrom(tc, pos, bias,
            direction, outBias);
      }
    return result;
  }

  /**
   * Calls the {@link TextUI#damageRange(JTextComponent, int, int)} method for
   * all the UI delegates managed by this <code>MultiTextUI</code>.
   *
   * @param tc  the component.
   * @param start  the start position.
   * @param end  the end position.
   */
  public void damageRange(JTextComponent tc, int start, int end)
  {
    Iterator iterator = uis.iterator();
    while (iterator.hasNext())
    {
      TextUI ui = (TextUI) iterator.next();
      ui.damageRange(tc, start, end);
    }
  }

  /**
   * Calls the {@link TextUI#damageRange(JTextComponent, int, int,
   * Position.Bias, Position.Bias)} method for all the UI delegates managed by
   * this <code>MultiTextUI</code>.
   *
   * @param tc  the component.
   * @param start  the start position.
   * @param end  the end position.
   * @param startBias  the start bias.
   * @param endBias  the end bias.
   */
  public void damageRange(JTextComponent tc, int start, int end,
        Bias startBias, Bias endBias)
  {
    Iterator iterator = uis.iterator();
    while (iterator.hasNext())
    {
      TextUI ui = (TextUI) iterator.next();
      ui.damageRange(tc, start, end, startBias, endBias);
    }
  }

  /**
   * Calls the {@link TextUI#getEditorKit(JTextComponent)} method for all
   * the UI delegates managed by this <code>MultiTextUI</code>,
   * returning the editor kit for the UI delegate from the primary look and
   * feel.
   *
   * @param tc  the text component.
   *
   * @return The editor kit returned by the UI delegate from the primary
   *         look and feel.
   */
  public EditorKit getEditorKit(JTextComponent tc)
  {
    EditorKit result = null;
    Iterator iterator = uis.iterator();
    // first UI delegate provides the return value
    if (iterator.hasNext())
      {
        TextUI ui = (TextUI) iterator.next();
        result = ui.getEditorKit(tc);
      }
    // return values from auxiliary UI delegates are ignored
    while (iterator.hasNext())
      {
        TextUI ui = (TextUI) iterator.next();
        /* EditorKit ignored = */ ui.getEditorKit(tc);
      }
    return result;
  }

  /**
   * Calls the {@link TextUI#getRootView(JTextComponent)} method for all
   * the UI delegates managed by this <code>MultiTextUI</code>,
   * returning the view for the UI delegate from the primary look and
   * feel.
   *
   * @param tc  the text component.
   *
   * @return The view returned by the UI delegate from the primary
   *         look and feel.
   */
  public View getRootView(JTextComponent tc)
  {
    View result = null;
    Iterator iterator = uis.iterator();
    // first UI delegate provides the return value
    if (iterator.hasNext())
      {
        TextUI ui = (TextUI) iterator.next();
        result = ui.getRootView(tc);
      }
    // return values from auxiliary UI delegates are ignored
    while (iterator.hasNext())
      {
        TextUI ui = (TextUI) iterator.next();
        /* View ignored = */ ui.getRootView(tc);
      }
    return result;
  }

}
