/* ComponentView.java -- 
   Copyright (C) 2002, 2004, 2005 Free Software Foundation, Inc.

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

package javax.swing.text;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Shape;

import javax.swing.SwingConstants;

/**
 * A {@link View} implementation that is able to render arbitrary
 * {@link Component}s. This uses the attribute
 * {@link StyleConstants#ComponentAttribute} to determine the
 * <code>Component</code> that should be rendered. This <code>Component</code>
 * becomes a direct child of the <code>JTextComponent</code> that contains
 * this <code>ComponentView</code>, so this view must not be shared between
 * multiple <code>JTextComponent</code>s.
 *
 * @author original author unknown
 * @author Roman Kennke (roman@kennke.org)
 */
// FIXME: This class is a complete stub and needs to be implemented properly.
public class ComponentView extends View
{
  /**
   * Creates a new instance of <code>ComponentView</code> for the specified
   * <code>Element</code>.
   *
   * @param elem the element that this <code>View</code> is rendering
   */
  public ComponentView(Element elem)
  {
    super(elem);
  }

  /**
   * Creates the <code>Component</code> that this <code>View</code> is
   * rendering. The <code>Component</code> is determined using
   * the {@link StyleConstants#ComponentAttribute} of the associated
   * <code>Element</code>.
   *
   * @return the component that is rendered
   */
  protected  Component createComponent()
  {
    return StyleConstants.getComponent(getElement().getAttributes());
  }

  /**
   * Returns the alignment of this <code>View</code> along the specified axis.
   *
   * @param axis either {@link View#X_AXIS} or {@link View#Y_AXIS}
   *
   * @return the alignment of this <code>View</code> along the specified axis
   */
  public float getAlignment(int axis)
  {
    return 0;
  }

  /**
   * Returns the <code>Component</code> that is rendered by this
   * <code>ComponentView</code>.
   *
   * @return the <code>Component</code> that is rendered by this
   *         <code>ComponentView</code>
   */
  public final Component getComponent()
  {
    return null;
  }

  /**
   * Returns the maximum span of this <code>View</code> along the specified
   * axis.
   *
   * This will return {@link Component#getMaximumSize()} for the specified
   * axis.
   *
   * @return the maximum span of this <code>View</code> along the specified
   *         axis
   */
  public float getMaximumSpan(int axis)
  {
    return 0;
  }

  public float getMinimumSpan(int axis)
  {
    // TODO: Implement this properly.
    return 0;
  }

  public float getPreferredSpan(int axis)
  {
    // TODO: Implement this properly.
    return 0;
  }

  public Shape modelToView(int pos, Shape a, Position.Bias b)
    throws BadLocationException
  {
    // TODO: Implement this properly.
    return null;
  }
    
  public void paint(Graphics g, Shape a)
  {
    // TODO: Implement this properly.
  }
  
  public void setParent(View p)
  {
    // TODO: Implement this properly.
  }
    
  public void setSize(float width, float height)
  {
    // TODO: Implement this properly.
  }
    
  public int viewToModel(float x, float y, Shape a, Position.Bias[] bias)
  {
    // TODO: Implement this properly.
    return 0;
  }

  /**
   * Maps coordinates from the <code>View</code>'s space into a position
   * in the document model.
   *
   * @param x the x coordinate in the view space
   * @param y the y coordinate in the view space
   * @param a the allocation of this <code>View</code>
   * @param b the bias to use
   *
   * @return the position in the document that corresponds to the screen
   *         coordinates <code>x, y</code>
   */
  public int viewToModel(float x, float y, Shape a, Position.Bias b)
  {
    // FIXME: Implement this properly.
    return 0;
  }

  /**
   * Returns the document position that is (visually) nearest to the given
   * document position <code>pos</code> in the given direction <code>d</code>.
   *
   * @param c the text component
   * @param pos the document position
   * @param b the bias for <code>pos</code>
   * @param d the direction, must be either {@link SwingConstants#NORTH},
   *        {@link SwingConstants#SOUTH}, {@link SwingConstants#WEST} or
   *        {@link SwingConstants#EAST}
   * @param biasRet an array of {@link Position.Bias} that can hold at least
   *        one element, which is filled with the bias of the return position
   *        on method exit
   *
   * @return the document position that is (visually) nearest to the given
   *         document position <code>pos</code> in the given direction
   *         <code>d</code>
   *
   * @throws BadLocationException if <code>pos</code> is not a valid offset in
   *         the document model
   */
  public int getNextVisualPositionFrom(JTextComponent c, int pos,
                                       Position.Bias b, int d,
                                       Position.Bias[] biasRet)
    throws BadLocationException
  {
    // TODO: Implement this properly.
    throw new AssertionError("Not implemented yet.");
  }
}
