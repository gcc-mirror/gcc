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
import java.awt.Container;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;

import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;

/**
 * A {@link View} implementation that is able to render arbitrary
 * {@link Component}s. This uses the attribute
 * {@link StyleConstants#ComponentAttribute} to determine the
 * <code>Component</code> that should be rendered. This <code>Component</code>
 * becomes a direct child of the <code>JTextComponent</code> that contains
 * this <code>ComponentView</code>, so this view must not be shared between
 * multiple <code>JTextComponent</code>s.
 *
 * @author Roman Kennke (kennke@aicas.com)
 * @author original author unknown
 */
public class ComponentView extends View
{

  /**
   * The component that is displayed by this view.
   */
  private Component comp;

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
  protected Component createComponent()
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
    float align;
    if (axis == X_AXIS)
      align = getComponent().getAlignmentX();
    else if (axis == Y_AXIS)
      align = getComponent().getAlignmentY();
    else
      throw new IllegalArgumentException();
    return align;
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
    if (comp == null)
      comp = createComponent();
    return comp;
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
    float span;
    if (axis == X_AXIS)
      span = getComponent().getMaximumSize().width;
    else if (axis == Y_AXIS)
      span = getComponent().getMaximumSize().height;
    else
      throw new IllegalArgumentException();
    return span;
  }

  public float getMinimumSpan(int axis)
  {
    float span;
    if (axis == X_AXIS)
      span = getComponent().getMinimumSize().width;
    else if (axis == Y_AXIS)
      span = getComponent().getMinimumSize().height;
    else
      throw new IllegalArgumentException();
    return span;
  }

  public float getPreferredSpan(int axis)
  {
    float span;
    if (axis == X_AXIS)
      span = getComponent().getPreferredSize().width;
    else if (axis == Y_AXIS)
      span = getComponent().getPreferredSize().height;
    else
      throw new IllegalArgumentException();
    return span;
  }

  public Shape modelToView(int pos, Shape a, Position.Bias b)
    throws BadLocationException
  {
    Element el = getElement();
    if (pos < el.getStartOffset() || pos >= el.getEndOffset())
      throw new BadLocationException("Illegal offset for this view", pos);
    Rectangle r = a.getBounds();
    Component c = getComponent();
    return new Rectangle(r.x, r.y, c.getWidth(), c.getHeight());
  }

  /**
   * The real painting behavour is performed by normal component painting,
   * triggered by the text component that hosts this view. This method does
   * not paint by itself. However, it sets the size of the component according
   * to the allocation that is passed here.
   *
   * @param g the graphics context
   * @param a the allocation of the child
   */
  public void paint(Graphics g, Shape a)
  {
    Rectangle r = a.getBounds();
    getComponent().setBounds(r.x, r.y, r.width, r.height);
  }

  /**
   * This sets up the component when the view is added to its parent, or
   * cleans up the view when it is removed from its parent.
   *
   * When this view is added to a parent view, the component of this view
   * is added to the container that hosts this view. When <code>p</code> is
   * <code>null</code>, then the view is removed from it's parent and we have
   * to also remove the component from it's parent container.
   *
   * @param p the parent view or <code>null</code> if this view is removed
   *        from it's parent
   */
  public void setParent(final View p)
  {
    if (SwingUtilities.isEventDispatchThread())
      setParentImpl(p);
    else
      SwingUtilities.invokeLater
      (new Runnable()
       {
         public void run()
         {
           setParentImpl(p);
         }
       });
  }

  /**
   * The implementation of {@link #setParent}. This is package private to
   * avoid a synthetic accessor method.
   *
   * @param p the parent view to set
   */
  void setParentImpl(View p)
  {
    if (p != null)
      {
        Component c = getComponent();
        p.getContainer().add(c);
      }
    else
      {
        Component c = getComponent();
        Container parent = c.getParent();
        parent.remove(c);
        comp = null;
      }
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
  public int viewToModel(float x, float y, Shape a, Position.Bias[] b)
  {
    // The element should only have one character position and it is clear
    // that this position is the position that best matches the given screen
    // coordinates, simply because this view has only this one position.
    Element el = getElement();
    return el.getStartOffset();
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
    // FIXME: Implement this method.
    throw new AssertionError("Not yet implemented");
  }
}
