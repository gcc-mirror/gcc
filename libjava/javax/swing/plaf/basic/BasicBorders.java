/* BasicBorders.java
   Copyright (C) 2003 Free Software Foundation, Inc.

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

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Insets;
import java.io.Serializable;
import javax.swing.AbstractButton;
import javax.swing.ButtonModel;
import javax.swing.JButton;
import javax.swing.JPopupMenu;
import javax.swing.JToolBar;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.border.AbstractBorder;
import javax.swing.border.Border;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.BorderUIResource;


/**
 * Provides various borders for the Basic look and feel.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class BasicBorders
{
  /**
   * A MarginBorder that gets shared by multiple components.
   * Created on demand by the private helper function {@link
   * #getMarginBorder()}.
   */
  private static MarginBorder sharedMarginBorder;


  /**
   * Returns a border for drawing push buttons.
   *
   * <p>The colors of the border are retrieved from the
   * <code>UIDefaults</code> of the currently active look and feel
   * using the keys <code>&#x201c;Button.shadow&#x201d;</code>,
   * <code>&#x201c;Button.darkShadow&#x201d;</code>,
   * <code>&#x201c;Button.light&#x201d;</code>, and
   * <code>&#x201c;Button.highlight&#x201d;</code>.
   *
   * <p><img src="BasicBorders.ButtonBorder-1.png" width="300"
   * height="170" alt="[A screen shot of the returned border]" />
   *
   * @return a {@link
   *         javax.swing.plaf.BorderUIResource#CompoundBorderUIResource}
   *         whose outer border is a {@link #ButtonBorder} and whose
   *         inner border is a {@link #MarginBorder}.
   */
  public static Border getButtonBorder()
  {
    UIDefaults defaults;
    Border outer;

    defaults = UIManager.getLookAndFeelDefaults();

    /* The keys for UIDefaults have been determined by writing a
     * test program that dumps the UIDefaults to stdout; that program
     * was run on a JDK 1.4.1_01 for GNU/Linux. Note that in the API,
     * the key "light" is usually called "highlight", and "highlight"
     * is usually called "lightHighlight".
     */
    outer = new ButtonBorder(defaults.getColor("Button.shadow"),
                             defaults.getColor("Button.darkShadow"),
                             defaults.getColor("Button.light"),
                             defaults.getColor("Button.highlight"));

    /* While the inner border is shared between multiple buttons,
     * we do not share the outer border because ButtonBorders store
     * their border colors. We cannot guarantee that the colors
     * (which come from UIDefaults) are unchanged between invocations
     * of getButtonBorder. We could store the last colors, and share
     * the button border if the colors are the same as in the last
     * invocation, but it probably is not worth the effort.
     */
    return new BorderUIResource.CompoundBorderUIResource(
      outer,
      /* inner */ getMarginBorder());
  }


  /**
   * Returns a border for drawing radio buttons.
   *
   * <p>The colors of the border are retrieved from the
   * <code>UIDefaults</code> of the currently active look and feel
   * using the keys <code>&#x201c;RadioButton.shadow&#x201d;</code>,
   * <code>&#x201c;RadioButton.darkShadow&#x201d;</code>,
   * <code>&#x201c;RadioButton.light&#x201d;</code>, and
   * <code>&#x201c;RadioButton.highlight&#x201d;</code>.
   *
   * <p><img src="BasicBorders.RadioButtonBorder-1.png" width="300"
   * height="135" alt="[A screen shot of the returned border]" />
   *
   * @return a {@link
   *         javax.swing.plaf.BorderUIResource#CompoundBorderUIResource}
   *         whose outer border is a {@link #RadioButtonBorder} and whose
   *         inner border is a {@link #MarginBorder}.
   */
  public static Border getRadioButtonBorder()
  {
    UIDefaults defaults;
    Border outer;

    defaults = UIManager.getLookAndFeelDefaults();

    /* The keys for UIDefaults have been determined by writing a
     * test program that dumps the UIDefaults to stdout; that program
     * was run on a JDK 1.4.1_01 for GNU/Linux. Note that in the API,
     * the key "light" is usually called "highlight", and "highlight"
     * is usually called "lightHighlight".
     */
    outer = new RadioButtonBorder(
      defaults.getColor("RadioButton.shadow"),
      defaults.getColor("RadioButton.darkShadow"),
      defaults.getColor("RadioButton.light"),
      defaults.getColor("RadioButton.highlight"));

    /* While the inner border is shared between multiple buttons, we
     * do not share the outer border because RadioButtonBorders, being
     * ButtonBorders, store their border colors. We cannot guarantee
     * that the colors (which come from UIDefaults) are unchanged
     * between invocations of getButtonBorder. We could store the last
     * colors, and share the button border if the colors are the same
     * as in the last invocation, but it probably is not worth the
     * effort.
     */
    return new BorderUIResource.CompoundBorderUIResource(
      outer,
      /* inner */ getMarginBorder());
  }


  /**
   * Returns a border for drawing toggle buttons.
   *
   * <p>The colors of the border are retrieved from the
   * <code>UIDefaults</code> of the currently active look and feel
   * using the keys <code>&#x201c;ToggleButton.shadow&#x201d;</code>,
   * <code>&#x201c;ToggleButton.darkShadow&#x201d;</code>,
   * <code>&#x201c;ToggleButton.light&#x201d;</code>, and
   * <code>&#x201c;ToggleButton.highlight&#x201d;</code>.
   *
   * <p><img src="BasicBorders.ToggleButtonBorder-1.png" width="270"
   * height="135" alt="[A screen shot of the returned border]" />
   *
   * @return a {@link
   *         javax.swing.plaf.BorderUIResource#CompoundBorderUIResource}
   *         whose outer border is a {@link #ToggleButtonBorder} and whose
   *         inner border is a {@link #MarginBorder}.
   */
  public static Border getToggleButtonBorder()
  {
    UIDefaults defaults;
    Border outer;

    defaults = UIManager.getLookAndFeelDefaults();

    /* The keys for UIDefaults have been determined by writing a
     * test program that dumps the UIDefaults to stdout; that program
     * was run on a JDK 1.4.1_01 for GNU/Linux. Note that in the API,
     * the key "light" is usually called "highlight", and "highlight"
     * is usually called "lightHighlight".
     */
    outer = new ToggleButtonBorder(
      defaults.getColor("ToggleButton.shadow"),
      defaults.getColor("ToggleButton.darkShadow"),
      defaults.getColor("ToggleButton.light"),
      defaults.getColor("ToggleButton.highlight"));

    /* While the inner border is shared between multiple buttons, we
     * do not share the outer border because ToggleButtonBorders, being
     * ButtonBorders, store their border colors. We cannot guarantee
     * that the colors (which come from UIDefaults) are unchanged
     * between invocations of getButtonBorder. We could store the last
     * colors, and share the button border if the colors are the same
     * as in the last invocation, but it probably is not worth the
     * effort.
     */
    return new BorderUIResource.CompoundBorderUIResource(
      outer,
      /* inner */ getMarginBorder());
  }


  /**
   * Returns a border for drawing a two-pixel thick separator line
   * below menu bars.
   *
   * <p>The colors of the border are retrieved from the
   * <code>UIDefaults</code> of the currently active look and feel
   * using the keys <code>&#x201c;MenuBar.shadow&#x201d;</code> and
   * <code>&#x201c;MenuBar.highlight&#x201d;</code>.
   *
   * <p><img src="BasicBorders.MenuBarBorder-1.png" width="500"
   * height="140" alt="[A screen shot of a JMenuBar with this border]" />
   *
   * @return a {@link #MenuBarBorder}.
   *
   * @see javax.swing.JMenuBar
   */
  public static Border getMenuBarBorder()
  {
    UIDefaults defaults;

    defaults = UIManager.getLookAndFeelDefaults();
    return new MenuBarBorder(defaults.getColor("MenuBar.shadow"),
                             defaults.getColor("MenuBar.highlight"));
  }


  /**
   * Returns a shared MarginBorder.
   */
  static Border getMarginBorder()  // intentionally not public
  {
    /* Swing is not designed to be thread-safe, so there is no
     * need to synchronize the access to the global variable.
     */
    if (sharedMarginBorder == null)
      sharedMarginBorder = new MarginBorder();

    return sharedMarginBorder;
  }
  
  
  /**
   * A border whose appearance depends on the state of
   * the enclosed button.
   *
   * <p><img src="BasicBorders.ButtonBorder-1.png" width="300"
   * height="170" alt="[A screen shot of this border]" />
   *
   * @see javax.swing.plaf.basic.BasicGraphicsUtils#drawBezel
   *
   * @author Sascha Brawer (brawer@dandelis.ch)
   */
  public static class ButtonBorder
    extends AbstractBorder
    implements Serializable, UIResource
  {
    /**
     * Determined using the <code>serialver</code> tool
     * of Apple/Sun JDK 1.3.1 on MacOS X 10.1.5.
     */
    static final long serialVersionUID = -157053874580739687L;
    
    
    /**
     * The color for drawing the shaded parts of the border.
     * @see javax.swing.plaf.basic.BasicGraphicsUtils#drawBezel
     */
    protected Color shadow;
    
    
    /**
     * The color for drawing the dark shaded parts of the border.
     * @see javax.swing.plaf.basic.BasicGraphicsUtils#drawBezel
     */
    protected Color darkShadow;
    
    
    /**
     * The color for drawing the highlighted parts of the border.
     * @see javax.swing.plaf.basic.BasicGraphicsUtils#drawBezel
     */
    protected Color highlight;
    
    
    /**
     * The color for drawing the bright highlighted parts of the border.
     * @see javax.swing.plaf.basic.BasicGraphicsUtils#drawBezel
     */
    protected Color lightHighlight;
    
    
    /**
     * Constructs a new border for drawing a button in the Basic
     * look and feel.
     *
     * @param shadow the shadow color.
     * @param darkShadow a darker variant of the shadow color.
     * @param highlight the highlight color.
     * @param lightHighlight a brighter variant of the highlight  color.
     */
    public ButtonBorder(Color shadow, Color darkShadow,
                        Color highlight, Color lightHighlight)
    {
      /* These colors usually come from the UIDefaults of the current
       * look and feel. Use fallback values if the colors are not
       * supplied.  The API specification is silent about what
       * behavior is expected for null colors, so users should not
       * rely on this fallback (which is why it is not documented in
       * the above Javadoc).
       */
      this.shadow = (shadow != null) ? shadow : Color.gray;
      this.darkShadow = (darkShadow != null) ? darkShadow : Color.black;
      this.highlight = (highlight != null) ? highlight : Color.lightGray;
      this.lightHighlight = (lightHighlight != null)
        ? lightHighlight
        : Color.white;
    }
    

    /**
     * Paints the ButtonBorder around a given component.
     *
     * @param c the component whose border is to be painted.
     * @param g the graphics for painting.
     * @param x the horizontal position for painting the border.
     * @param y the vertical position for painting the border.
     * @param width the width of the available area for painting the border.
     * @param height the height of the available area for painting the border.
     *
     * @see javax.swing.plaf.basic.BasicGraphicsUtils#drawBezel
     */
    public void paintBorder(Component c, Graphics  g,
                            int x, int y, int width, int height)
    {
      ButtonModel bmodel = null;
      
      if (c instanceof AbstractButton)
        bmodel = ((AbstractButton) c).getModel();
      
      BasicGraphicsUtils.drawBezel(
        g, x, y, width, height,
        /* pressed */ (bmodel != null)
                        && /* mouse button pressed */ bmodel.isPressed()
                        && /* mouse inside */ bmodel.isArmed(),
        /* default */ (c instanceof JButton)
                        && ((JButton) c).isDefaultButton(),
        shadow, darkShadow, highlight, lightHighlight);
    }
    
    
    /**
     * Measures the width of this border.
     *
     * <p>Although the thickness of the actually painted border
     * depends on the state of the enclosed component, this
     * measurement always returns the same amount of pixels.  Indeed,
     * it would be rather confusing if a button was appearing to
     * change its size depending on whether it is pressed or not.
     *
     * @param c the component whose border is to be measured.
     *
     * @return an Insets object whose <code>left</code>,
     *         <code>right</code>, <code>top</code> and
     *         <code>bottom</code> fields indicate the width of the
     *         border at the respective edge.
     *
     * @see #getBorderInsets(java.awt.Component, java.awt.Insets) 
     */
    public Insets getBorderInsets(Component c)
    {
      /* There is no obvious reason for overriding this method, but we
       * try to have exactly the same API as the Sun reference
       * implementation.
       */
      return getBorderInsets(c, null);
    }

    
    /**
     * Measures the width of this border, storing the results into a
     * pre-existing Insets object.
     *
     * <p>Although the thickness of the actually painted border
     * depends on the state of the enclosed component, this
     * measurement always returns the same amount of pixels.  Indeed,
     * it would be rather confusing if a button was appearing to
     * change its size depending on whether it is pressed or not.
     *
     * @param insets an Insets object for holding the result values.
     *        After invoking this method, the <code>left</code>,
     *        <code>right</code>, <code>top</code> and
     *        <code>bottom</code> fields indicate the width of the
     *        border at the respective edge.
     *
     * @return the same object that was passed for <code>insets</code>.
     *
     * @see #getBorderInsets()
     */
    public Insets getBorderInsets(Component c, Insets insets)
    {
      /* The exact amount has been determined using a test program
       * that was run on the Sun reference implementation. With
       * Apple/Sun JDK 1.3.1 on MacOS X 10.1.5, the result is
       * [3, 3, 3, 3]. With Sun JDK 1.4.1_01 on Linux/x86, the
       * result is [2, 3, 3, 3]. We use the values from the 1.4.1_01
       * release.
       */
      if (insets == null)
        return new Insets(2, 3, 3, 3);

      insets.top = 2;
      insets.bottom = insets.left = insets.right = 3;
      return insets;
    }
  }
  
  
  public static class FieldBorder
  {
    public FieldBorder(Color shadow, Color darkShadow,
                       Color highlight, Color lightHighlight)
    {
    }
  } // class FieldBorder


  /**
   * An invisible, but spacing border whose margin is determined
   * by calling the <code>getMargin()</code> method of the enclosed
   * component.  If the enclosed component has no such method,
   * this border will not occupy any space.
   *
   * <p><img src="BasicBorders.MarginBorder-1.png" width="325"
   * height="200" alt="[An illustration that shows how MarginBorder
   * determines its borders]" />
   *
   * @author Sascha Brawer (brawer@dandelis.ch)
   */
  public static class MarginBorder
    extends AbstractBorder
    implements Serializable, UIResource
  {
    /**
     * Determined using the <code>serialver</code> tool
     * of Apple/Sun JDK 1.3.1 on MacOS X 10.1.5.
     */
    static final long serialVersionUID = -3035848353448896090L;
    
    
    /**
     * Constructs a new MarginBorder.
     */
    public MarginBorder()
    {
    }
    
    
    /**
     * Measures the width of this border.
     *
     * @param c the component whose border is to be measured.
     *
     * @return an Insets object whose <code>left</code>, <code>right</code>,
     *         <code>top</code> and <code>bottom</code> fields indicate the
     *         width of the border at the respective edge.
     *
     * @see #getBorderInsets(java.awt.Component, java.awt.Insets)
     */
    public Insets getBorderInsets(Component c)
    {
      return getBorderInsets(c, new Insets(0, 0, 0, 0));
    }
    
    
    /**
     * Determines the insets of this border by calling the
     * <code>getMargin()</code> method of the enclosed component.  The
     * resulting margin will be stored into the the <code>left</code>,
     * <code>right</code>, <code>top</code> and <code>bottom</code>
     * fields of the passed <code>insets</code> parameter.
     *
     * <p>Unfortunately, <code>getMargin()</code> is not a method of
     * {@link javax.swing.JComponent} or some other common superclass
     * of things with margins. While reflection could be used to
     * determine the existence of this method, this would be slow on
     * many virtual machines. Therefore, the current implementation
     * knows about {@link javax.swing.AbstractButton#getMargin()},
     * {@link javax.swing.JPopupMenu#getMargin()}, and {@link
     * javax.swing.JToolBar#getMargin()}. If <code>c</code> is an
     * instance of a known class, the respective
     * <code>getMargin()</code> method is called to determine the
     * correct margin. Otherwise, a zero-width margin is returned.
     *
     * @param c the component whose border is to be measured.
     *
     * @return the same object that was passed for <code>insets</code>,
     *         but with changed fields.
     */
    public Insets getBorderInsets(Component c, Insets insets)
    {
      Insets margin = null;

      /* This is terrible object-oriented design. See the above Javadoc
       * for an excuse.
       */
      if (c instanceof AbstractButton)
        margin = ((AbstractButton) c).getMargin();
      else if (c instanceof JPopupMenu)
        margin = ((JPopupMenu) c).getMargin();
      else if (c instanceof JToolBar)
        margin = ((JToolBar) c).getMargin();

      if (margin == null)
        insets.top = insets.left = insets.bottom = insets.right = 0;
      else
      {
        insets.top = margin.top;
        insets.left = margin.left;
        insets.bottom = margin.bottom;
        insets.right = margin.right;
      }

      return insets;
    }
  }
  

  /**
   * A border for drawing a separator line below JMenuBar.
   *
   * <p><img src="BasicBorders.MenuBarBorder-1.png" width="500"
   * height="140" alt="[A screen shot of a JMenuBar with this border]" />
   *
   * @author Sascha Brawer (brawer@dandelis.ch)
   */
  public static class MenuBarBorder
    extends AbstractBorder
    implements UIResource
  {
    /**
     * Determined using the <code>serialver</code> tool
     * of Apple/Sun JDK 1.3.1 on MacOS X 10.1.5.
     */
    static final long serialVersionUID = -6909056571935227506L;
    
    
    /**
     * The shadow color, which is used for the upper line of the
     * two-pixel thick bottom edge.
     */
    private Color shadow;


    /**
     * The highlight color, which is used for the lower line of the
     * two-pixel thick bottom edge.
     */
    private Color highlight;


    /**
     * Constructs a new MenuBarBorder for drawing a JMenuBar in
     * the Basic look and feel.
     *
     * <p><img src="BasicBorders.MenuBarBorder-1.png" width="500"
     * height="140" alt="[A screen shot of a JMenuBar with this
     * border]" />
     *
     * @param shadow the shadow color, which is used for the upper
     *        line of the two-pixel thick bottom edge.
     *
     * @param highlight the shadow color, which is used for the lower
     *        line of the two-pixel thick bottom edge.
     */
    public MenuBarBorder(Color shadow, Color highlight)
    {
      /* These colors usually come from the UIDefaults of the current
       * look and feel. Use fallback values if the colors are not
       * supplied.  The API specification is silent about what
       * behavior is expected for null colors, so users should not
       * rely on this fallback (which is why it is not documented in
       * the above Javadoc).
       */
      this.shadow = (shadow != null) ? shadow : Color.gray;
      this.highlight = (highlight != null) ? highlight : Color.white;
    }


    /**
     * Paints the MenuBarBorder around a given component.
     *
     * @param c the component whose border is to be painted, usually
     *        an instance of {@link javax.swing.JMenuBar}.
     *
     * @param g the graphics for painting.
     * @param x the horizontal position for painting the border.
     * @param y the vertical position for painting the border.
     * @param width the width of the available area for painting the border.
     * @param height the height of the available area for painting the border.
     */
    public void paintBorder(Component c, Graphics  g,
                            int x, int y, int width, int height)
    {
      Color oldColor;

      /* To understand this code, it might be helpful to look at the
       * image "BasicBorders.MenuBarBorder-1.png" that is included
       * with the JavaDoc. It is located in the "doc-files"
       * subdirectory.
       */
      oldColor = g.getColor();
      y = y + height - 2;
      try
      {
        g.setColor(shadow);
        g.drawLine(x, y, x + width - 2, y);
        g.drawLine(x, y + 1, x, y + 1);
        g.drawLine(x + width - 2, y + 1, x + width - 2, y + 1);

        g.setColor(highlight);
        g.drawLine(x + 1, y + 1, x + width - 3, y + 1);
        g.drawLine(x + width - 1, y, x + width - 1, y + 1);        
      }
      finally
      {
        g.setColor(oldColor);
      }
    }


    /**
     * Measures the width of this border.
     *
     * @param c the component whose border is to be measured.
     *
     * @return an Insets object whose <code>left</code>,
     *         <code>right</code>, <code>top</code> and
     *         <code>bottom</code> fields indicate the width of the
     *         border at the respective edge.
     *
     * @see #getBorderInsets(java.awt.Component, java.awt.Insets)
     */
    public Insets getBorderInsets(Component c)
    {
      /* There is no obvious reason for overriding this method, but we
       * try to have exactly the same API as the Sun reference
       * implementation.
       */
      return getBorderInsets(c, null);
    }


    /**
     * Measures the width of this border, storing the results into a
     * pre-existing Insets object.
     *
     * @param insets an Insets object for holding the result values.
     *        After invoking this method, the <code>left</code>,
     *        <code>right</code>, <code>top</code> and
     *        <code>bottom</code> fields indicate the width of the
     *        border at the respective edge.
     *
     * @return the same object that was passed for <code>insets</code>.
     *
     * @see #getBorderInsets()
     */
    public Insets getBorderInsets(Component c, Insets insets)
    {
      /* The exact amount has been determined using a test program
       * that was run on the Apple/Sun JDK 1.3.1 on MacOS X, and the
       * Sun JDK 1.4.1_01 on GNU/Linux for x86. Both gave [0,0,2,0],
       * which was expected from looking at the screen shot.
       */
      if (insets == null)
        return new Insets(0, 0, 2, 0);

      insets.left = insets.right = insets.top = 0;
      insets.bottom = 2;
      return insets;
    }
  }


  /**
   * A border for drawing radio buttons in the Basic look and feel.
   *
   * <p><img src="BasicBorders.RadioButtonBorder-1.png" width="300"
   * height="135" alt="[A screen shot of this border]" />
   *
   * <p>Note about the screen shot: Normally, the
   * <code>borderPainted</code> property is <code>false</code> for
   * JRadioButtons. For this screen shot, it has been set to
   * <code>true</code> so the borders get drawn. Also, a
   * concretization of the Basic look and would typically provide
   * icons for the various states of radio buttons.
   *
   * <p>Note that the focus rectangle is invisible If the radio button
   * is currently selected. While it might be debatable whether this
   * makes a lot of sense, this behavior can be observed in the Sun
   * reference implementation (in JDK 1.3.1 and 1.4.1). The Classpath
   * implementation tries to exactly replicate the JDK appearance.
   *
   * @see javax.swing.plaf.basic.BasicGraphicsUtils#drawBezel
   *
   * @author Sascha Brawer (brawer@dandelis.ch)
   */
  public static class RadioButtonBorder
    extends ButtonBorder
  {
    /**
     * Determined using the <code>serialver</code> tool
     * of Apple/Sun JDK 1.3.1 on MacOS X 10.1.5.
     */
    static final long serialVersionUID = 1596945751743747369L;


    /**
     * Constructs a new border for drawing a JRadioButton in
     * the Basic look and feel.
     *
     * @param shadow the shadow color.
     * @param darkShadow a darker variant of the shadow color.
     * @param highlight the highlight color.
     * @param lightHighlight a brighter variant of the highlight  color.
     */
    public RadioButtonBorder(Color shadow, Color darkShadow,
                             Color highlight, Color lightHighlight)
    {
      /* The superclass ButtonBorder substitutes null arguments
       * with fallback colors.
       */
      super(shadow, darkShadow, highlight, lightHighlight);
    }


    /**
     * Paints the RadioButtonBorder around a given component.
     *
     * <p>The Sun implementation always seems to draw exactly
     * the same border, irrespective of the state of the button.
     * This is rather surprising, but GNU Classpath emulates the
     * observable behavior.
     *
     * @param c the component whose border is to be painted.
     * @param g the graphics for painting.
     * @param x the horizontal position for painting the border.
     * @param y the vertical position for painting the border.
     * @param width the width of the available area for painting the border.
     * @param height the height of the available area for painting the border.
     *
     * @see javax.swing.plaf.basic.BasicGraphicsUtils#drawBezel
     */
    public void paintBorder(Component c, Graphics  g,
                            int x, int y, int width, int height)
    {
      AbstractButton button = null;
      ButtonModel bmodel = null;
      boolean lowered = false;
      boolean focused = false;

      if (c instanceof AbstractButton)
      {
        button = (AbstractButton) c;
        bmodel = button.getModel();
      }

      if (bmodel != null)
      {
        lowered = button.isSelected()
          || (/* mouse inside */ bmodel.isArmed() && bmodel.isPressed());
        focused = button.hasFocus() && button.isFocusPainted();        
      }

      if (lowered)
        BasicGraphicsUtils.drawLoweredBezel(g, x, y, width, height,
                                            shadow, darkShadow,
                                            highlight, lightHighlight);
      else
        BasicGraphicsUtils.drawBezel(g, x, y, width, height,
                                     /* isPressed */ false,
                                     /* isPefault */ focused,
                                     shadow, darkShadow,
                                     highlight, lightHighlight);
    }
    
    
    /**
     * Measures the width of this border.
     *
     * @param c the component whose border is to be measured.
     *
     * @return an Insets object whose <code>left</code>,
     *         <code>right</code>, <code>top</code> and
     *         <code>bottom</code> fields indicate the width of the
     *         border at the respective edge.
     *
     * @see #getBorderInsets(java.awt.Component, java.awt.Insets) 
     */
    public Insets getBorderInsets(Component c)
    {
      /* There is no obvious reason for overriding this method, but we
       * try to have exactly the same API as the Sun reference
       * implementation.
       */
      return getBorderInsets(c, null);
    }

    
    /**
     * Measures the width of this border, storing the results into a
     * pre-existing Insets object.
     *
     * @param insets an Insets object for holding the result values.
     *        After invoking this method, the <code>left</code>,
     *        <code>right</code>, <code>top</code> and
     *        <code>bottom</code> fields indicate the width of the
     *        border at the respective edge.
     *
     * @return the same object that was passed for <code>insets</code>.
     *
     * @see #getBorderInsets()
     */
    public Insets getBorderInsets(Component c, Insets insets)
    {
      /* The exact amount has been determined using a test program
       * that was run on the Apple/Sun JDK 1.3.1 on MacOS X, and the
       * Sun JDK 1.4.1_01 on GNU/Linux for x86. Both gave [2,2,2,2].
       */
      if (insets == null)
        return new Insets(2, 2, 2, 2);

      insets.left = insets.right = insets.top = insets.bottom = 2;
      return insets;
    }
  }


  public static class RolloverButtonBorder
  {
  } // class RolloverButtonBorder
  public static class SplitPaneBorder
  {
    public SplitPaneBorder(Color highlight, Color shadow)
    {
    }
  } // class SplitPaneBorder


  /**
   * A border for toggle buttons in the Basic look and feel.
   *
   * <p><img src="BasicBorders.ToggleButtonBorder-1.png" width="270"
   * height="135" alt="[A screen shot of this border]" />
   *
   * <p>The Sun implementation always seems to draw exactly
   * the same border, irrespective of the state of the button.
   * This is rather surprising, but GNU Classpath emulates the
   * observable behavior.
   *
   * @see javax.swing.plaf.basic.BasicGraphicsUtils#drawBezel
   *
   * @author Sascha Brawer (brawer@dandelis.ch)
   */
  public static class ToggleButtonBorder
    extends ButtonBorder
  {
    /**
     * Determined using the <code>serialver</code> tool
     * of Apple/Sun JDK 1.3.1 on MacOS X 10.1.5.
     */
    static final long serialVersionUID = -3528666548001058394L;

    
    /**
     * Constructs a new border for drawing a JToggleButton in
     * the Basic look and feel.
     *
     * @param shadow the shadow color.
     * @param darkShadow a darker variant of the shadow color.
     * @param highlight the highlight color.
     * @param lightHighlight a brighter variant of the highlight  color.
     */
    public ToggleButtonBorder(Color shadow, Color darkShadow,
                              Color highlight, Color lightHighlight)
    {
      /* The superclass ButtonBorder substitutes null arguments
       * with fallback colors.
       */
      super(shadow, darkShadow, highlight, lightHighlight);
    }


    /**
     * Paints the ToggleButtonBorder around a given component.
     *
     * <p>The Sun implementation always seems to draw exactly
     * the same border, irrespective of the state of the button.
     * This is rather surprising, but GNU Classpath emulates the
     * observable behavior.
     *
     * @param c the component whose border is to be painted.
     * @param g the graphics for painting.
     * @param x the horizontal position for painting the border.
     * @param y the vertical position for painting the border.
     * @param width the width of the available area for painting the border.
     * @param height the height of the available area for painting the border.
     *
     * @see javax.swing.plaf.basic.BasicGraphicsUtils#drawBezel
     */
    public void paintBorder(Component c, Graphics  g,
                            int x, int y, int width, int height)
    {
      /* The author of this code tried various variants for setting
       * the state of the enclosed JToggleButton, but it seems that
       * the drawn border is always identical. Weird, because this
       * means that the user does not see whether the JToggleButton
       * is selected or not.
       */
      BasicGraphicsUtils.drawBezel(g, x, y, width, height,
                                   /* pressed */ false, 
                                   /* default */ false,
                                   shadow, darkShadow,
                                   highlight, lightHighlight);
    }


    /**
     * Measures the width of this border.
     *
     * @param c the component whose border is to be measured.
     *
     * @return an Insets object whose <code>left</code>,
     *         <code>right</code>, <code>top</code> and
     *         <code>bottom</code> fields indicate the width of the
     *         border at the respective edge.
     *
     * @see #getBorderInsets(java.awt.Component, java.awt.Insets) 
     */
    public Insets getBorderInsets(Component c)
    {
      /* There is no obvious reason for overriding this method, but we
       * try to have exactly the same API as the Sun reference
       * implementation.
       */
      return getBorderInsets(c, null);
    }

    
    /**
     * Measures the width of this border, storing the results into a
     * pre-existing Insets object.
     *
     * @param insets an Insets object for holding the result values.
     *        After invoking this method, the <code>left</code>,
     *        <code>right</code>, <code>top</code> and
     *        <code>bottom</code> fields indicate the width of the
     *        border at the respective edge.
     *
     * @return the same object that was passed for <code>insets</code>.
     *
     * @see #getBorderInsets()
     */
    public Insets getBorderInsets(Component c, Insets insets)
    {
      /* The exact amount has been determined using a test program
       * that was run on the Apple/Sun JDK 1.3.1 on MacOS X, and the
       * Sun JDK 1.4.1_01 on GNU/Linux for x86. Both gave [2,2,2,2].
       */
      if (insets == null)
        return new Insets(2, 2, 2, 2);

      insets.left = insets.right = insets.top = insets.bottom = 2;
      return insets;
    }
  }
}
