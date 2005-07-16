/* BorderUIResource.java
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


package javax.swing.plaf;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Insets;
import java.io.Serializable;

import javax.swing.Icon;
import javax.swing.border.BevelBorder;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.border.LineBorder;
import javax.swing.border.MatteBorder;
import javax.swing.border.TitledBorder;

/**
 * A wrapper for {@link javax.swing.border.Border} that also
 * implements the {@link UIResource} marker interface.  This is useful
 * for implementing pluggable look-and-feels: When switching the
 * current LookAndFeel, only those borders are replaced that are
 * marked as {@link UIResource}.  For this reason, a look-and-feel
 * should always install borders that implement
 * <code>UIResource</code>, such as the borders provided by this
 * class.
 *
 * @serial
 * @serialField delegate Border the <code>Border</code> wrapped
 *
 * @author Brian Jones (cbj@gnu.org)
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class BorderUIResource 
  extends Object 
  implements Border, UIResource, Serializable
{
  /**
   * Verified using the <code>serialver</code> tool
   * of Apple/Sun JDK 1.3.1 on MacOS X 10.1.5.
   */
  static final long serialVersionUID = -3440553684010079691L;


  /**
   * A shared instance of an {@link EtchedBorderUIResource}, or
   * <code>null</code> if the {@link #getEtchedBorderUIResource()}
   * method has not yet been called.
   */
  private static Border etchedBorderUIResource;


  /**
   * A shared instance of a {@link BevelBorderUIResource} whose
   * <code>bevelType</code> is {@link
   * javax.swing.border.BevelBorder#LOWERED}, or <code>null</code> if
   * the {@link #getLoweredBevelBorderUIResource()} has not yet been
   * called.
   */
  private static Border loweredBevelBorderUIResource;
  
  
  /**
   * A shared instance of a {@link BevelBorderUIResource} whose
   * <code>bevelType</code> is {@link
   * javax.swing.border.BevelBorder#RAISED}, or <code>null</code> if
   * the {@link #getRaisedBevelBorderUIResource()} has not yet been
   * called.
   */
  private static Border raisedBevelBorderUIResource;
  
  
  /**
   * A shared instance of a {@link LineBorderUIResource} for
   * a one-pixel thick black line, or <code>null</code> if
   * the {@link #getBlackLineBorderUIResource()} has not yet been
   * called.
   */
  private static Border blackLineBorderUIResource;


  /**
   * Returns a shared instance of an etched border which also
   * is marked as an {@link UIResource}.
   *
   * @see javax.swing.border.EtchedBorder
   */
  public static Border getEtchedBorderUIResource()
  {
    /* Swing is not designed to be thread-safe, so there is no
     * need to synchronize the access to the global variable.
     */
    if (etchedBorderUIResource == null)
      etchedBorderUIResource = new EtchedBorderUIResource();
    return etchedBorderUIResource;
  }
  

  /**
   * Returns a shared instance of {@link BevelBorderUIResource} whose
   * <code>bevelType</code> is {@link
   * javax.swing.border.BevelBorder#LOWERED}.
   *
   * @see javax.swing.border.BevelBorder
   */
  public static Border getLoweredBevelBorderUIResource()
  {
    /* Swing is not designed to be thread-safe, so there is no
     * need to synchronize the access to the global variable.
     */
    if (loweredBevelBorderUIResource == null)
      loweredBevelBorderUIResource = new BevelBorderUIResource(
        BevelBorder.LOWERED);
    return loweredBevelBorderUIResource;
  }


  /**
   * Returns a shared instance of {@link BevelBorderUIResource} whose
   * <code>bevelType</code> is {@link
   * javax.swing.border.BevelBorder#RAISED}.
   *
   * @see javax.swing.border.BevelBorder
   */
  public static Border getRaisedBevelBorderUIResource()
  {
    /* Swing is not designed to be thread-safe, so there is no
     * need to synchronize the access to the global variable.
     */
    if (raisedBevelBorderUIResource == null)
      raisedBevelBorderUIResource = new BevelBorderUIResource(
        BevelBorder.RAISED);
    return raisedBevelBorderUIResource;
  }
  
  
  /**
   * Returns a shared instance of {@link LineBorderUIResource} for
   * a black, one-pixel width border.
   *
   * @see javax.swing.border.LineBorder
   */
  public static Border getBlackLineBorderUIResource()
  {
    /* Swing is not designed to be thread-safe, so there is no
     * need to synchronize the access to the global variable.
     */
    if (blackLineBorderUIResource == null)
      blackLineBorderUIResource = new LineBorderUIResource(Color.black);
    return blackLineBorderUIResource;
  }


  /**
   * The wrapped border.
   */
  private Border delegate;
  
  
  /**
   * Constructs a <code>BorderUIResource</code> for wrapping
   * a <code>Border</code> object.
   * 
   * @param delegate the border to be wrapped.
   */
  public BorderUIResource(Border delegate)
  {
    if (delegate == null)
      throw new IllegalArgumentException();
    
    this.delegate = delegate;
  }

  
  /**
   * Paints the border around an enclosed component by calling
   * the <code>paintBorder</code> method of the wrapped delegate.
   *
   * @param c the component whose border is to be painted.
   * @param g the graphics for painting.
   * @param x the horizontal position for painting the border.
   * @param y the vertical position for painting the border.
   * @param width the width of the available area for painting the border.
   * @param height the height of the available area for painting the border.
   */
  public void paintBorder(Component c, Graphics g,
                          int x, int y, int width, int height)
  {
    delegate.paintBorder(c, g, x, y, width, height);
  }
  
  
  /**
   * Measures the width of this border by calling the
   * <code>getBorderInsets</code> method of the wrapped
   * delegate.
   *
   * @param c the component whose border is to be measured.
   *
   * @return an Insets object whose <code>left</code>, <code>right</code>,
   *         <code>top</code> and <code>bottom</code> fields indicate the
   *         width of the border at the respective edge.
   */
  public Insets getBorderInsets(Component c)
  { 
    return delegate.getBorderInsets(c);
  }
  
  
  /**
   * Determines whether this border fills every pixel in its area
   * when painting by calling the <code>isBorderOpaque</code>
   * method of the wrapped delegate.
   *
   * @return <code>true</code> if the border is fully opaque, or
   *         <code>false</code> if some pixels of the background
   *         can shine through the border.
   */
  public boolean isBorderOpaque()
  { 
    return delegate.isBorderOpaque();
  }


  /**
   * A {@link javax.swing.border.BevelBorder} that also implements the
   * {@link UIResource} marker interface.  This is useful for
   * implementing pluggable look-and-feels: When switching the current
   * LookAndFeel, only those borders are replaced that are marked as
   * {@link UIResource}.  For this reason, a look-and-feel should
   * always install borders that implement <code>UIResource</code>,
   * such as the borders provided by this class.
   *
   * @author Brian Jones (cbj@gnu.org)
   * @author Sascha Brawer (brawer@dandelis.ch)
   */
  public static class BevelBorderUIResource 
    extends BevelBorder
    implements UIResource, Serializable
  {
    private static final long serialVersionUID = -1275542891108351642L;
    
    /**
     * Constructs a BevelBorderUIResource whose colors will be derived
     * from the background of the enclosed component. The background
     * color is retrieved each time the border is painted, so a border
     * constructed by this method will automatically reflect a change
     * to the component&#x2019;s background color.
     *
     * <p><img src="../border/doc-files/BevelBorder-1.png"
     * width="500" height="150"
     * alt="[An illustration showing raised and lowered BevelBorders]" /></p>
     *
     * @param bevelType the desired appearance of the border. The value
     *        must be either {@link javax.swing.border.BevelBorder#RAISED}
     *        or {@link javax.swing.border.BevelBorder#LOWERED}.
     *
     * @throws IllegalArgumentException if <code>bevelType</code> has
     *         an unsupported value.
     */
    public BevelBorderUIResource(int bevelType) 
    { 
      super(bevelType);
    }
    
    
    /**
     * Constructs a BevelBorderUIResource given its appearance type
     * and two colors for its highlight and shadow.
     *
     * <p><img src="../border/doc-files/BevelBorder-2.png" width="500"
     * height="150" alt="[An illustration showing BevelBorders that were
     * constructed with this method]" /></p>
     *
     * @param bevelType the desired appearance of the border. The value
     *        must be either {@link javax.swing.border.BevelBorder#RAISED}
     *        or {@link javax.swing.border.BevelBorder#LOWERED}.
     *
     * @param highlight the color that will be used for the inner side
     *        of the highlighted edges (top and left if if
     *        <code>bevelType</code> is {@link
     *        javax.swing.border.BevelBorder#RAISED}; bottom and right
     *        otherwise). The color for the outer side is a brightened
     *        version of this color.
     *
     * @param shadow the color that will be used for the outer side of
     *        the shadowed edges (bottom and right if
     *        <code>bevelType</code> is {@link
     *        javax.swing.border.BevelBorder#RAISED}; top and left
     *        otherwise). The color for the inner side is a brightened
     *        version of this color.
     *
     * @throws IllegalArgumentException if <code>bevelType</code> has
     *         an unsupported value.
     *
     * @throws NullPointerException if <code>highlight</code> or
     *         <code>shadow</code> is <code>null</code>.
     */
    public BevelBorderUIResource(int bevelType, 
                                 Color highlight, 
                                 Color shadow) 
    {
      super(bevelType, highlight, shadow);
    }


    /**
     * Constructs a BevelBorderUIResource given its appearance type
     * and all its colors.
     *
     * <p><img src="../border/doc-files/BevelBorder-3.png" width="500"
     * height="150" alt="[An illustration showing BevelBorders that
     * were constructed with this method]" /></p>
     *
     * @param bevelType the desired appearance of the border. The value
     *        must be either {@link javax.swing.border.BevelBorder#RAISED}
     *        or {@link javax.swing.border.BevelBorder#LOWERED}.
     *
     * @param highlightOuter the color that will be used for the outer
     *        side of the highlighted edges (top and left if
     *        <code>bevelType</code> is {@link
     *        javax.swing.border.BevelBorder#RAISED}; bottom and right
     *        otherwise).
     *
     * @param highlightInner the color that will be used for the inner
     *        side of the highlighted edges.
     *
     * @param shadowOuter the color that will be used for the outer
     *        side of the shadowed edges (bottom and right if
     *        <code>bevelType</code> is {@link
     *        javax.swing.border.BevelBorder#RAISED}; top and left
     *        otherwise).
     *
     * @param shadowInner the color that will be used for the inner
     *        side of the shadowed edges.
     *
     * @throws IllegalArgumentException if <code>bevelType</code> has
     *         an unsupported value.
     *
     * @throws NullPointerException if one of the passed colors
     *         is <code>null</code>.
     */
    public BevelBorderUIResource(int bevelType,
                                 Color highlightOuter,
                                 Color highlightInner,
                                 Color shadowOuter,
                                 Color shadowInner) 
    {
      super(bevelType,
            highlightOuter, highlightInner,
            shadowOuter, shadowInner);
    }
  }
  
  
  /**
   * A {@link javax.swing.border.CompoundBorder} that also implements the
   * {@link UIResource} marker interface.  This is useful for
   * implementing pluggable look-and-feels: When switching the current
   * LookAndFeel, only those borders are replaced that are marked as
   * {@link UIResource}.  For this reason, a look-and-feel should
   * always install borders that implement <code>UIResource</code>,
   * such as the borders provided by this class.
   *
   * @author Brian Jones (cbj@gnu.org)
   * @author Sascha Brawer (brawer@dandelis.ch)
   */
  public static class CompoundBorderUIResource
    extends CompoundBorder
    implements UIResource, Serializable
  {
    private static final long serialVersionUID = 7550017084975167341L;
    
    /**
     * Constructs a CompoundBorderUIResource with the specified inside
     * and outside borders.
     *
     * @param outsideBorder the outside border, which is painted to the
     *        outside of both <code>insideBorder</code> and the enclosed
     *        component. It is acceptable to pass <code>null</code>, in
     *        which case no outside border is painted.
     *
     * @param insideBorder the inside border, which is painted to
     *        between <code>outsideBorder</code> and the enclosed
     *        component. It is acceptable to pass <code>null</code>, in
     *        which case no inside border is painted.
     */
    public CompoundBorderUIResource(Border outsideBorder,
                                    Border insideBorder)
    {
      super(outsideBorder, insideBorder);
    }
  }
  
  
  /**
   * An {@link javax.swing.border.EmptyBorder} that also implements the
   * {@link UIResource} marker interface.  This is useful for
   * implementing pluggable look-and-feels: When switching the current
   * LookAndFeel, only those borders are replaced that are marked as
   * {@link UIResource}.  For this reason, a look-and-feel should
   * always install borders that implement <code>UIResource</code>,
   * such as the borders provided by this class.
   *
   * <p><img src="../border/doc-files/EmptyBorder-1.png"
   * width="290" height="200"
   * alt="[An illustration of EmptyBorder]" /></p>
   *
   * @author Brian Jones (cbj@gnu.org)
   * @author Sascha Brawer (brawer@dandelis.ch)
   */
  public static class EmptyBorderUIResource 
    extends EmptyBorder
    implements UIResource, Serializable
  {
    private static final long serialVersionUID = -4914187529340071708L;
    
    /**
     * Constructs an empty border given the number of pixels required
     * on each side.
     *
     * @param top the number of pixels that the border will need
     *        for its top edge.
     *
     * @param left the number of pixels that the border will need
     *        for its left edge.
     *
     * @param bottom the number of pixels that the border will need
     *        for its bottom edge.
     *
     * @param right the number of pixels that the border will need
     *        for its right edge.
     */
    public EmptyBorderUIResource(int top, int left, int bottom, int right)
    {
      super(top, left, bottom, right);
    }
    
    
    /**
     * Constructs an empty border given the number of pixels required
     * on each side, passed in an Insets object.
     *
     * @param insets the Insets for the new border.
     */
    public EmptyBorderUIResource(Insets insets)
    {
      super(insets);
    }
  }
  
  
  /**
   * An {@link javax.swing.border.EtchedBorder} that also implements the
   * {@link UIResource} marker interface.  This is useful for
   * implementing pluggable look-and-feels: When switching the current
   * LookAndFeel, only those borders are replaced that are marked as
   * {@link UIResource}.  For this reason, a look-and-feel should
   * always install borders that implement <code>UIResource</code>,
   * such as the borders provided by this class.
   *
   * <p><img src="../border/doc-files/EtchedBorder-1.png" width="500"
   * height="200" alt="[An illustration of the two EtchedBorder
   * variants]" /></p>
   *
   * @author Brian Jones (cbj@gnu.org)
   * @author Sascha Brawer (brawer@dandelis.ch)
   */
  public static class EtchedBorderUIResource
    extends EtchedBorder
    implements UIResource, Serializable
  {
    private static final long serialVersionUID = -8186391754165296656L;
    
    /**
     * Constructs an EtchedBorderUIResource that appears lowered into
     * the surface. The colors will be derived from the background
     * color of the enclosed Component when the border gets painted.
     */
    public EtchedBorderUIResource()
    {
      super();
    }
    
    
    /**
     * Constructs an EtchedBorderUIResource with the specified
     * appearance. The colors will be derived from the background
     * color of the enclosed Component when the border gets painted.
     *
     * <p><img src="../border/doc-files/EtchedBorder-1.png"
     * width="500" height="200" alt="[An illustration of the two
     * EtchedBorder variants]" /></p>
     *
     * @param etchType the desired appearance of the border. The value
     *        must be either {@link javax.swing.border.EtchedBorder#RAISED}
     *        or {@link javax.swing.border.EtchedBorder#LOWERED}.
     *
     * @throws IllegalArgumentException if <code>etchType</code> has
     *         an unsupported value.
     */
    public EtchedBorderUIResource(int etchType) 
    {
      super(etchType);
    }
    
    
    /**
     * Constructs a lowered EtchedBorderUIResource, explicitly
     * selecting the colors that will be used for highlight and
     * shadow.
     *
     * @param highlight the color that will be used for painting
     *        the highlight part of the border.
     *
     * @param shadow the color that will be used for painting
     *        the shadow part of the border.
     *
     * @see #EtchedBorderUIResource(int, Color, Color)
     */
    public EtchedBorderUIResource(Color highlight, Color shadow)
    {
      super(highlight, shadow);
    }
    
    
    /**
     * Constructs an EtchedBorderUIResource with the specified
     * appearance, explicitly selecting the colors that will be used
     * for highlight and shadow.
     *
     * <p><img src="../border/doc-files/EtchedBorder-2.png" width="500"
     * height="200" alt="[An illustration that shows which pixels get
     * painted in what color]" /></p>
     *
     * @param etchType the desired appearance of the border. The value
     *        must be either {@link javax.swing.border.EtchedBorder#RAISED}
     *        or {@link javax.swing.border.EtchedBorder#LOWERED}.
     *
     * @param highlight the color that will be used for painting
     *        the highlight part of the border.
     *
     * @param shadow the color that will be used for painting
     *        the shadow part of the border.
     *
     * @throws IllegalArgumentException if <code>etchType</code> has
     *         an unsupported value.
     */
    public EtchedBorderUIResource(int etchType,
                                  Color highlight, Color shadow)
    {
      super(etchType, highlight, shadow);
    }
  }
  
  
  /**
   * A {@link javax.swing.border.LineBorder} that also implements the
   * {@link UIResource} marker interface.  This is useful for
   * implementing pluggable look-and-feels: When switching the current
   * LookAndFeel, only those borders are replaced that are marked as
   * {@link UIResource}.  For this reason, a look-and-feel should
   * always install borders that implement <code>UIResource</code>,
   * such as the borders provided by this class.
   *
   * <p><img src="../border/doc-files/LineBorder-1.png" width="500"
   * height="200" alt="[An illustration of two LineBorders]" /></p>
   *
   * @author Brian Jones (cbj@gnu.org)
   * @author Sascha Brawer (brawer@dandelis.ch)
   */
  public static class LineBorderUIResource
    extends LineBorder
    implements UIResource, Serializable
  {
    private static final long serialVersionUID = -6171232338180172310L;
    
    /**
     * Constructs a LineBorderUIResource given its color.  The border
     * will be one pixel thick and have plain corners.
     *
     * @param color the color for drawing the border.
     */
    public LineBorderUIResource(Color color)
    {
      super(color); 
    }
    
    
    /**
     * Constructs a LineBorder given its color and thickness.  The
     * border will have plain corners.
     *
     * @param color the color for drawing the border.
     * @param thickness the width of the line in pixels.
     */
    public LineBorderUIResource(Color color, int thickness)
    {
      super(color, thickness);
    }
    
    
    /* Note: Since JDK1.3, javax.swing.border.LineBorder also has a
     * constructor which accepts a value for the roundedCorners
     * property. However, as of JDK1.4.1, the LineBorderUIResource
     * subclass does not have a corresponding constructor.
     * 
     * A request for enhancing the Swing API has been filed with Sun:
     * http://developer.java.sun.com/developer/bugParade/bugs/4879999.html
     */
  }


  /**
   * A {@link javax.swing.border.MatteBorder} that also implements the
   * {@link UIResource} marker interface.  This is useful for
   * implementing pluggable look-and-feels: When switching the current
   * LookAndFeel, only those borders are replaced that are marked as
   * {@link UIResource}.  For this reason, a look-and-feel should
   * always install borders that implement <code>UIResource</code>,
   * such as the borders provided by this class.
   *
   * <p><img src="../border/doc-files/MatteBorder-1.png" width="500"
   * height="150" alt="[An illustration of two MatteBorders]" /></p>
   *
   * @author Brian Jones (cbj@gnu.org)
   * @author Sascha Brawer (brawer@dandelis.ch)
   */
  public static class MatteBorderUIResource
    extends MatteBorder
    implements UIResource, Serializable
  {
    private static final long serialVersionUID = -8107923147541851122L;
    
    /**
     * Constructs a MatteBorderUIResource given the width on each side
     * and a fill color.
     *
     * <p><img src="../border/doc-files/MatteBorder-2.png" width="500"
     * height="150" alt="[A picture of a MatteBorder made by this
     * constructor]" /></p>
     *
     * @param top the width of the border at its top edge.
     * @param left the width of the border at its left edge.
     * @param bottom the width of the border at its bottom edge.
     * @param right the width of the border at its right edge.
     * @param matteColor the color for filling the border.
     */
    public MatteBorderUIResource(int top, int left,
                                 int bottom, int right,
                                 Color color)
    {
      super(top, left, bottom, right, color);
    }
    
    
    /**
     * Constructs a MatteBorderUIResource given the width on each side
     * and an icon for tiling the border area.
     *
     * <p><img src="../border/doc-files/MatteBorder-4.png" width="500"
     * height="150" alt="[A picture of a MatteBorder made by this
     * constructor]" /></p>
     *
     * @param top the width of the border at its top edge.
     * @param left the width of the border at its left edge.
     * @param bottom the width of the border at its bottom edge.
     * @param right the width of the border at its right edge.
     * @param tileIcon an icon for tiling the border area.
     */
    public MatteBorderUIResource(int top, int left,
                                 int bottom, int right,
                                 Icon tileIcon)
    {
      super(top, left, bottom, right, tileIcon);
    }
    
    
    /**
     * Constructs a MatteBorderUIResource given an icon for tiling the
     * border area. The icon width is used for the border insets at
     * the left and right edge, the icon height for the top and bottom
     * edge.
     *
     * <p><img src="../border/doc-files/MatteBorder-6.png" width="500"
     * height="150" alt="[A picture of a MatteBorder made by this
     * constructor]" /></p>
     *
     * @param tileIcon an icon for tiling the border area. 
     */
    public MatteBorderUIResource(Icon tileIcon)
    {
      super(tileIcon);
    }
  }
  
  
  /**
   * A {@link javax.swing.border.TitledBorder} that also implements the
   * {@link UIResource} marker interface.  This is useful for
   * implementing pluggable look-and-feels: When switching the current
   * LookAndFeel, only those borders are replaced that are marked as
   * {@link UIResource}.  For this reason, a look-and-feel should
   * always install borders that implement <code>UIResource</code>,
   * such as the borders provided by this class.
   *
   * @author Brian Jones (cbj@gnu.org)
   * @author Sascha Brawer (brawer@dandelis.ch)
   */
  public static class TitledBorderUIResource
    extends TitledBorder
    implements UIResource, Serializable
  {
    private static final long serialVersionUID = 7667113547406407427L;
    
    /**
     * Constructs a TitledBorderUIResource given the text of its title.
     *
     * @param title the title text, or <code>null</code> to use no
     *        title text.
     */
    public TitledBorderUIResource(String title)
    {
      super(title);
    }
    
    
    /**
     * Constructs an initially untitled TitledBorderUIResource
     * given another border.
     *
     * @param border the border underneath the title, or
     *        <code>null</code> to use a default from
     *        the current look and feel.
     */
    public TitledBorderUIResource(Border border)
    {
      super(border);
    }
    
    
    /**
     * Constructs a TitledBorder given its border and title text.
     *
     * @param border the border underneath the title, or
     *        <code>null</code> to use a default from
     *        the current look and feel.
     *
     * @param title the title text, or <code>null</code>
     *        to use no title text.
     */
    public TitledBorderUIResource(Border border, String title)
    {
      super(border, title);
    }


    /**
     * Constructs a TitledBorderUIResource given its border, title
     * text, horizontal alignment, and vertical position.
     *
     * @param border the border underneath the title, or
     *        <code>null</code> to use a default
     *        from the current look and feel.
     *
     * @param title the title text, or <code>null</code>
     *        to use no title text.
     *
     * @param titleJustification the horizontal alignment of the title
     *        text in relation to the border. The value must be one of
     *        {@link javax.swing.border.TitledBorder#LEFT},
     *        {@link javax.swing.border.TitledBorder#CENTER},
     *        {@link javax.swing.border.TitledBorder#RIGHT},
     *        {@link javax.swing.border.TitledBorder#LEADING},
     *        {@link javax.swing.border.TitledBorder#TRAILING}, or
     *        {@link javax.swing.border.TitledBorder#DEFAULT_JUSTIFICATION}.
     *
     * @param titlePosition the vertical position of the title text
     *        in relation to the border. The value must be one of
     *        {@link javax.swing.border.TitledBorder#ABOVE_TOP},
     *        {@link javax.swing.border.TitledBorder#TOP},
     *        {@link javax.swing.border.TitledBorder#BELOW_TOP},
     *        {@link javax.swing.border.TitledBorder#ABOVE_BOTTOM},
     *        {@link javax.swing.border.TitledBorder#BOTTOM},
     *        {@link javax.swing.border.TitledBorder#BELOW_BOTTOM},
     *        or {@link javax.swing.border.TitledBorder#DEFAULT_POSITION}.
     *
     * @throws IllegalArgumentException if <code>titleJustification</code>
     *         or <code>titlePosition</code> have an unsupported value.
     */
    public TitledBorderUIResource(Border border, String title,
                                  int titleJustification,
                                  int titlePosition)
    {
      super(border, title, titleJustification, titlePosition);
    }


    /**
     * Constructs a TitledBorder given its border, title text,
     * horizontal alignment, vertical position, and font.
     *
     * @param border the border underneath the title, or
     *        <code>null</code> to use a default
     *        from the current look and feel.
     *
     * @param title the title text, or <code>null</code>
     *        to use no title text.
     *
     * @param titleJustification the horizontal alignment of the title
     *        text in relation to the border. The value must be one of
     *        {@link javax.swing.border.TitledBorder#LEFT},
     *        {@link javax.swing.border.TitledBorder#CENTER},
     *        {@link javax.swing.border.TitledBorder#RIGHT},
     *        {@link javax.swing.border.TitledBorder#LEADING},
     *        {@link javax.swing.border.TitledBorder#TRAILING}, or
     *        {@link javax.swing.border.TitledBorder#DEFAULT_JUSTIFICATION}.
     *
     * @param titlePosition the vertical position of the title text
     *        in relation to the border. The value must be one of
     *        {@link javax.swing.border.TitledBorder#ABOVE_TOP},
     *        {@link javax.swing.border.TitledBorder#TOP},
     *        {@link javax.swing.border.TitledBorder#BELOW_TOP},
     *        {@link javax.swing.border.TitledBorder#ABOVE_BOTTOM},
     *        {@link javax.swing.border.TitledBorder#BOTTOM},
     *        {@link javax.swing.border.TitledBorder#BELOW_BOTTOM},
     *        or {@link javax.swing.border.TitledBorder#DEFAULT_POSITION}.
     *
     * @param titleFont the font for the title text, or <code>null</code>
     *        to use a default from the current look and feel.
     *
     * @throws IllegalArgumentException if <code>titleJustification</code>
     *         or <code>titlePosition</code> have an unsupported value.
     */
    public TitledBorderUIResource(Border border, String title,
                                  int titleJustification,
                                  int titlePosition,
                                  Font titleFont)
    {
      super(border, title, titleJustification, titlePosition,
            titleFont);
    }
    
    
    /**
     * Constructs a TitledBorder given its border, title text,
     * horizontal alignment, vertical position, font, and color.
     *
     * @param border the border underneath the title, or
     *        <code>null</code> to use a default
     *        from the current look and feel.
     *
     * @param title the title text, or <code>null</code>
     *        to use no title text.
     *
     * @param titleJustification the horizontal alignment of the title
     *        text in relation to the border. The value must be one of
     *        {@link javax.swing.border.TitledBorder#LEFT},
     *        {@link javax.swing.border.TitledBorder#CENTER},
     *        {@link javax.swing.border.TitledBorder#RIGHT},
     *        {@link javax.swing.border.TitledBorder#LEADING},
     *        {@link javax.swing.border.TitledBorder#TRAILING}, or
     *        {@link javax.swing.border.TitledBorder#DEFAULT_JUSTIFICATION}.
     *
     * @param titlePosition the vertical position of the title text
     *        in relation to the border. The value must be one of
     *        {@link javax.swing.border.TitledBorder#ABOVE_TOP},
     *        {@link javax.swing.border.TitledBorder#TOP},
     *        {@link javax.swing.border.TitledBorder#BELOW_TOP},
     *        {@link javax.swing.border.TitledBorder#ABOVE_BOTTOM},
     *        {@link javax.swing.border.TitledBorder#BOTTOM},
     *        {@link javax.swing.border.TitledBorder#BELOW_BOTTOM},
     *        or {@link javax.swing.border.TitledBorder#DEFAULT_POSITION}.
     *
     * @param titleFont the font for the title text, or <code>null</code>
     *        to use a default from the current look and feel.
     *
     * @param titleColor the color for the title text, or <code>null</code>
     *        to use a default from the current look and feel.
     *
     * @throws IllegalArgumentException if <code>titleJustification</code>
     *         or <code>titlePosition</code> have an unsupported value.
     */
    public TitledBorderUIResource(Border border, String title,
                                  int titleJustification, int titlePosition,
                                  Font titleFont, Color titleColor)
    {
      super(border, title, titleJustification, titlePosition,
            titleFont, titleColor);
    }
  }
}

