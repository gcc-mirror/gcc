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
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;

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
   * A special container that sits between the component and the hosting
   * container. This is used to propagate invalidate requests and cache
   * the component's layout sizes.
   */
  private class Interceptor
    extends Container
  {
    Dimension min;
    Dimension pref;
    Dimension max;
    float alignX;
    float alignY;

    /**
     * Creates a new instance that hosts the specified component.
     */
    Interceptor(Component c)
    {
      setLayout(null);
      add(c);
      cacheComponentSizes();
    }

    /**
     * Intercepts the normal invalidate call and propagates the invalidate
     * request up using the View's preferenceChanged().
     */
    public void invalidate()
    {
      super.invalidate();
      if (getParent() != null)
        preferenceChanged(null, true, true);
    }

    /**
     * This is overridden to simply cache the layout sizes.
     */
    public void doLayout()
    {
      cacheComponentSizes();
    }

    /**
     * Overridden to also reshape the component itself.
     */
    public void reshape(int x, int y, int w, int h)
    {
      super.reshape(x, y, w, h);
      if (getComponentCount() > 0)
        getComponent(0).setSize(w, h);
      cacheComponentSizes();
    }

    /**
     * Overridden to also show the component.
     */
    public void show()
    {
      super.show();
      if (getComponentCount() > 0)
        getComponent(0).setVisible(true);
    }

    /**
     * Overridden to also hide the component.
     */
    public void hide()
    {
      super.hide();
      if (getComponentCount() > 0)
        getComponent(0).setVisible(false);
    }

    /**
     * Overridden to return the cached value.
     */
    public Dimension getMinimumSize()
    {
      maybeValidate();
      return min;
    }

    /**
     * Overridden to return the cached value.
     */
    public Dimension getPreferredSize()
    {
      maybeValidate();
      return pref;
    }

    /**
     * Overridden to return the cached value.
     */
    public Dimension getMaximumSize()
    {
      maybeValidate();
      return max;
    }

    /**
     * Overridden to return the cached value.
     */
    public float getAlignmentX()
    {
      maybeValidate();
      return alignX;
    }

    /**
     * Overridden to return the cached value.
     */
    public float getAlignmentY()
    {
      maybeValidate();
      return alignY;
    }

    /**
     * Validates the container only when necessary.
     */
    private void maybeValidate()
    {
      if (! isValid())
        validate();
    }

    /**
     * Fetches the component layout sizes into the cache.
     */
    private void cacheComponentSizes()
    {
      if (getComponentCount() > 0)
        {
          Component c = getComponent(0);
          min = c.getMinimumSize();
          pref = c.getPreferredSize();
          max = c.getMaximumSize();
          alignX = c.getAlignmentX();
          alignY = c.getAlignmentY();
        }
    }
  }

  /**
   * The component that is displayed by this view.
   */
  private Component comp;

  /**
   * The intercepting container.
   */
  private Interceptor interceptor;

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
    float align = 0.0F;
    // I'd rather throw an IllegalArgumentException for illegal axis,
    // but the Harmony testsuite indicates fallback to super behaviour.
    if (interceptor != null && (axis == X_AXIS || axis == Y_AXIS))
      {
        if (axis == X_AXIS)
          align = interceptor.getAlignmentX();
        else if (axis == Y_AXIS)
          align = interceptor.getAlignmentY();
        else
          assert false : "Must not reach here";
      }
    else
      align = super.getAlignment(axis);
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
    if (axis != X_AXIS && axis != Y_AXIS)
      throw new IllegalArgumentException("Illegal axis");
    float span = 0;
    if (interceptor != null)
      {
        if (axis == X_AXIS)
          span = interceptor.getMaximumSize().width;
        else if (axis == Y_AXIS)
          span = interceptor.getMaximumSize().height;
        else
          assert false : "Must not reach here";
      }
    return span;
  }

  public float getMinimumSpan(int axis)
  {
    if (axis != X_AXIS && axis != Y_AXIS)
      throw new IllegalArgumentException("Illegal axis");
    float span = 0;
    if (interceptor != null)
      {
        if (axis == X_AXIS)
          span = interceptor.getMinimumSize().width;
        else if (axis == Y_AXIS)
          span = interceptor.getMinimumSize().height;
        else
          assert false : "Must not reach here";
      }
    return span;
  }

  public float getPreferredSpan(int axis)
  {
    if (axis != X_AXIS && axis != Y_AXIS)
      throw new IllegalArgumentException("Illegal axis");
    float span = 0;
    if (interceptor != null)
      {
        if (axis == X_AXIS)
          span = interceptor.getPreferredSize().width;
        else if (axis == Y_AXIS)
          span = interceptor.getPreferredSize().height;
        else
          assert false : "Must not reach here";
      }
    return span;
  }

  public Shape modelToView(int pos, Shape a, Position.Bias b)
    throws BadLocationException
  {
    int p0 = getStartOffset();
    int p1 = getEndOffset();
    if (pos >= p0 && pos <= p1)
      {
        Rectangle viewRect = a.getBounds();
        if (pos == p1)
          viewRect.x += viewRect.width;
        viewRect.width = 0;
        return viewRect;
      }
    else
      throw new BadLocationException("Illegal position", pos);
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
    if (interceptor != null)
      {
        Rectangle r = a instanceof Rectangle ? (Rectangle) a : a.getBounds();
        interceptor.setBounds(r.x, r.y, r.width, r.height);
      }
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
    super.setParent(p);
    if (SwingUtilities.isEventDispatchThread())
      setParentImpl();
    else
      SwingUtilities.invokeLater
      (new Runnable()
       {
         public void run()
         {
           Document doc = getDocument();
           try
             {
               if (doc instanceof AbstractDocument)
                 ((AbstractDocument) doc).readLock();
               setParentImpl();
               Container host = getContainer();
               if (host != null)
                 {
                   preferenceChanged(null, true, true);
                   host.repaint();
                 }
             }
           finally
             {
               if (doc instanceof AbstractDocument)
                 ((AbstractDocument) doc).readUnlock();
             }
         }
       });
  }

  /**
   * The implementation of {@link #setParent}. This is package private to
   * avoid a synthetic accessor method.
   */
  void setParentImpl()
  {
    View p = getParent();
    if (p != null)
      {
        Container c = getContainer();
        if (c != null)
          {
            if (interceptor == null)
              {
                // Create component and put it inside the interceptor.
                Component created = createComponent();
                if (created != null)
                  {
                    comp = created;
                    interceptor = new Interceptor(comp);
                  }
              }
            if (interceptor != null)
              {
                // Add the interceptor to the hosting container.
                if (interceptor.getParent() == null)
                  c.add(interceptor, this);
              }
          }
      }
    else
      {
        if (interceptor != null)
          {
            Container parent = interceptor.getParent();
            if (parent != null)
              parent.remove(interceptor);
          }
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
    int pos;
    // I'd rather do the following. The harmony testsuite indicates
    // that a simple cast is performed.
    //Rectangle r = a instanceof Rectangle ? (Rectangle) a : a.getBounds();
    Rectangle r = (Rectangle) a;
    if (x < r.x + r.width / 2)
      {
        b[0] = Position.Bias.Forward;
        pos = getStartOffset();
      }
    else
      {
        b[0] = Position.Bias.Backward;
        pos = getEndOffset();
      }
    return pos;
  }
}
