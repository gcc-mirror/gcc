/* ComponentUI.java --
   Copyright (C) 2002, 2003, 2004  Free Software Foundation, Inc.

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


package javax.swing.plaf;

import java.awt.Dimension;
import java.awt.Graphics;

import javax.accessibility.Accessible;
import javax.swing.JComponent;

/**
 * The abstract base class for all delegates that provide the
 * pluggable look and feel for Swing components. User applications
 * should not need to access this class; it is internal to Swing
 * and the look-and-feel implementations.
 *
 * <p><img src="doc-files/ComponentUI-1.png" width="700" height="550"
 * alt="[UML diagram illustrating the architecture for pluggable
 * look and feels]" /></p>
 *
 * <p>Components such as {@link javax.swing.JSlider} do not directly
 * implement operations related to the look and feel of the user
 * interface, such as painting or layout. Instead, they use a delegate
 * object for all such tasks. In the case of <code>JSlider</code>, the
 * user interface would be provided by some concrete subclass of
 * {@link javax.swing.plaf.SliderUI}.
 *
 * <p>Soon after its creation, a <code>ComponentUI</code> will be sent
 * an {@link #installUI} message. The <code>ComponentUI</code> will
 * react by setting properties such as the border or the background
 * color of the <code>JComponent</code> for which it provides its
 * services. Soon before the end of its lifecycle, the
 * <code>ComponentUI</code> will receive an {@link #uninstallUI}
 * message, at which time the <code>ComponentUI</code> is expected to
 * undo any changes.</p>
 *
 * <p>Note that the <code>ui</code> of a <code>JComponent</code>
 * changes whenever the user switches between look and feels.  For
 * example, the <code>ui</code> property of a <code>JSlider</code>
 * could change from an instance of <code>MetalSliderUI</code> to an
 * instance of <code>FooSliderUI</code>. This switch can happen at any
 * time, but it will always be performed from inside the Swing thread.</p>
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public abstract class ComponentUI 
{
  /**
   * Constructs a new UI delegate.
   */
  public ComponentUI()
  {
  }
  
  
  /**
   * Sets up the specified component so it conforms the the design
   * guidelines of the implemented look and feel. When the look and
   * feel changes, a <code>ComponentUI</code> delegate is created.
   * The delegate object then receives an <code>installUI</code>
   * message.
   *
   * <p>This method should perform the following tasks:</p>
   *
   * <ul>
   * <li>Set visual properties such as borders, fonts, colors, or
   *     icons. However, no change should be performed for those
   *     properties whose values have been directly set by the client
   *     application. To allow the distinction, LookAndFeels are expected
   *     to use values that implement the {@link UIResource} marker
   *     interface, such as {@link BorderUIResource} or {@link
   *     ColorUIResource}.</li>
   * <li>If necessary, install a {@link java.awt.LayoutManager}.</li>
   * <li>Embed custom sub-components. For instance, the UI delegate
   *     for a {@link javax.swing.JSplitPane} might install a special
   *     component for the divider.</li>
   * <li>Register event listeners.</li>
   * <li>Set up properties related to keyborad navigation, such as
   *     mnemonics or focus traversal policies.</li>
   * </ul>
   *
   * @param c the component for which this delegate will provide
   *        services.
   *
   * @see #uninstallUI
   * @see javax.swing.JComponent#setUI
   * @see javax.swing.JComponent#updateUI
   */
  public void installUI(JComponent c)
  {
    // The default implementation does not change any properties.
  }


  /**
   * Puts the specified component into the state it had before
   * {@link #installUI} was called.
   *
   * @param c the component for which this delegate has provided
   *        services.
   *
   * @see #installUI
   * @see javax.swing.JComponent#setUI
   * @see javax.swing.JComponent#updateUI
   */
  public void uninstallUI(JComponent c)
  {
    // The default implementation does not change any properties.
  }
  
  
  /**
   * Paints the component according to the design guidelines
   * of the look and feel. Most subclasses will want to override
   * this method.
   *
   * @param g the graphics for painting.
   *
   * @param c the component for which this delegate performs
   *          services.
   */
  public void paint(Graphics g, JComponent c)
  {
  }
  
  
  /**
   * Fills the specified component with its background color
   * (unless the <code>opaque</code> property is <code>false</code>)
   * before calling {@link #paint}.
   *
   * <p>It is unlikely that a subclass needs to override this method.
   * The actual rendering should be performed by the {@link #paint}
   * method.
   *
   * @param g the graphics for painting.
   *
   * @param c the component for which this delegate performs
   *          services.
   *
   * @see #paint
   * @see javax.swing.JComponent#paintComponent
   */
  public void update(Graphics g, JComponent c)
  {
    if (c.isOpaque())
    {
      g.setColor(c.getBackground());
      g.fillRect(0, 0, c.getWidth(), c.getHeight());
    }
    paint(g, c);
  }
  
  
  /**
   * Determines the preferred size of a component. The default
   * implementation returns <code>null</code>, which means that
   * <code>c</code>&#x2019;s layout manager should be asked to
   * calculate the preferred size.
   *
   * @param c the component for which this delegate performs services.
   *
   * @return the preferred size, or <code>null</code> to indicate that
   *         <code>c</code>&#x2019;s layout manager should be asked
   *         for the preferred size.
   */
  public Dimension getPreferredSize(JComponent c)
  {
    return null;
  }
  
  
  /**
   * Determines the minimum size of a component. The default
   * implementation calls {@link #getPreferredSize}, but subclasses
   * might want to override this.
   *
   * @param c the component for which this delegate performs services.
   *
   * @return the minimum size, or <code>null</code> to indicate that
   *         <code>c</code>&#x2019;s layout manager should be asked
   *         to calculate the minimum size.
   */
  public Dimension getMinimumSize(JComponent c)
  {
    return getPreferredSize(c);
  }


  /**
   * Determines the maximum size of a component. The default
   * implementation calls {@link #getPreferredSize}, but subclasses
   * might want to override this.
   *
   * @param c the component for which this delegate performs services.
   *
   * @return the maximum size, or <code>null</code> to indicate that
   *         <code>c</code>&#x2019;s layout manager should be asked
   *         to calculate the maximum size.
   */
  public Dimension getMaximumSize(JComponent c)
  {
    return getPreferredSize(c);
  }


  /**
   * Determines whether a click into the component at a specified
   * location is considered as having hit the component. The default
   * implementation checks whether the point falls into the
   * component&#x2019;s bounding rectangle. Some subclasses might want
   * to override this, for example in the case of a rounded button.
   *
   * @param c the component for which this delegate performs services.
   *
   * @param x the x coordinate of the point, relative to the local
   *        coordinate system of the component. Zero would be be
   *        component&#x2019;s left edge, irrespective of the location
   *        inside its parent.
   *
   * @param y the y coordinate of the point, relative to the local
   *        coordinate system of the component. Zero would be be
   *        component&#x2019;s top edge, irrespective of the location
   *        inside its parent.
   */
  public boolean contains(JComponent c, int x, int y)
  {    
    /* JComponent.contains calls the ui delegate for hit
     * testing. Therefore, endless mutual recursion would result if we
     * called c.contains(x, y) here.
     *
     * The previous Classpath implementation called the deprecated
     * method java.awt.Component.inside. In the Sun implementation, it
     * can be observed that inside, other than contains, does not call
     * the ui delegate.  But that inside() behaves different to
     * contains() clearly is in violation of the method contract, and
     * it is not something that a good implementation should rely upon
     * -- even if Classpath ends up being forced to replicate this
     * apparent bug of the Sun implementation.
     */
    return (x >= 0) && (x < c.getWidth())
      && (y >= 0) && (y < c.getHeight());
  }
  
  
  /**
   * Creates a delegate object for the specified component.  Users
   * should use the <code>createUI</code> method of a suitable
   * subclass. The implementation of <code>ComponentUI</code>
   * always throws an error.
   *
   * @param c the component for which a UI delegate is requested.
   */
  public static ComponentUI createUI(JComponent c)
  {
    throw new Error(
      "javax.swing.plaf.ComponentUI does not implement createUI; call "
      + "createUI on a subclass.");
  }
  

  /**
   * Counts the number of accessible children in the component.  The
   * default implementation delegates the inquiry to the {@link
   * javax.accessibility.AccessibleContext} of <code>c</code>.
   *
   * @param c the component whose accessible children
   *        are to be counted.
   */
  public int getAccessibleChildrenCount(JComponent c)
  {
    return c.getAccessibleContext().getAccessibleChildrenCount();
  }


  /**
   * Returns the specified accessible child of the component. The
   * default implementation delegates the inquiry to the {@link
   * javax.accessibility.AccessibleContext} of <code>c</code>.
   *
   * @param i the index of the accessible child, starting at zero.
   *
   * @param c the component whose <code>i</code>-th accessible child
   *        is requested.
   */
  public Accessible getAccessibleChild(JComponent c, int i)
  {
    return c.getAccessibleContext().getAccessibleChild(i);
  }
}
