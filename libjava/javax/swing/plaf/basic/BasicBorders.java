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
import java.awt.Rectangle;
import java.io.Serializable;
import javax.swing.AbstractButton;
import javax.swing.ButtonModel;
import javax.swing.JButton;
import javax.swing.JPopupMenu;
import javax.swing.JSplitPane;
import javax.swing.JToolBar;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.border.AbstractBorder;
import javax.swing.border.BevelBorder;
import javax.swing.border.Border;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.BorderUIResource;
import javax.swing.text.JTextComponent;


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
   * <p><img src="doc-files/BasicBorders.ButtonBorder-1.png" width="300"
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
   * <p><img src="doc-files/BasicBorders.RadioButtonBorder-1.png" width="300"
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
   * <p><img src="doc-files/BasicBorders.ToggleButtonBorder-1.png" width="270"
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
   * <p><img src="doc-files/BasicBorders.MenuBarBorder-1.png" width="500"
   * height="140" alt="[A screen shot of a JMenuBar with this border]" />
   *
   * @return a {@link #MenuBarBorder}.
   *
   * @see javax.swing.JMenuBar
   */
  public static Border getMenuBarBorder()
  {
    UIDefaults defaults;

    /* See comment in methods above for why this border is not shared. */
    defaults = UIManager.getLookAndFeelDefaults();
    return new MenuBarBorder(defaults.getColor("MenuBar.shadow"),
                             defaults.getColor("MenuBar.highlight"));
  }


  /**
   * Returns a border for drawing a one-pixel thick border around
   * split panes that are interrupted where the divider joins the
   * border.
   *
   * <p>The colors of the border are retrieved from the
   * <code>UIDefaults</code> of the currently active look and feel
   * using the keys <code>&#x201c;SplitPane.darkShadow&#x201d;</code> and
   * <code>&#x201c;SplitPane.highlight&#x201d;</code>.
   *   
   * <p><img src="doc-files/BasicBorders.SplitPaneBorder-1.png" width="520"
   * height="200" alt="[A screen shot for JSplitPane.HORIZONTAL_SPLIT]" />
   *
   * <p><img src="doc-files/BasicBorders.SplitPaneBorder-2.png" width="520"
   * height="200" alt="[A screen shot for JSplitPane.VERTICAL_SPLIT]" />
   *
   * @return a {@link #SplitPaneBorder}.
   *
   * @see javax.swing.JSplitPane
   * @see #getSplitPaneDividerBorder()
   */
  public static Border getSplitPaneBorder()
  {
    UIDefaults defaults;

    /* See comment in methods above for why this border is not shared. */
    defaults = UIManager.getLookAndFeelDefaults();
    return new SplitPaneBorder(defaults.getColor("SplitPane.highlight"),
                               defaults.getColor("SplitPane.darkShadow"));
  }


  /**
   * Returns a border for drawing a one-pixel thick border around
   * the divider of split panes.
   *
   * <p>The colors of the edges that are adjacent to the child components
   * of the <code>JSplitPane</code> are retrieved from the
   * <code>UIDefaults</code> of the currently active look and feel
   * using the keys <code>&#x201c;SplitPane.darkShadow&#x201d;</code> and
   * <code>&#x201c;SplitPane.highlight&#x201d;</code>. The color of the
   * other two edges is the background color of the divider.
   *
   * <p><img src="doc-files/BasicBorders.SplitPaneDividerBorder-1.png"
   * width="520" height="200" alt= 
   * "[A screen shot for JSplitPane.HORIZONTAL_SPLIT]" />
   *
   * @return an instance of <code>SplitPaneDividerBorder</code>, which is
   *         not a public API class of this package.
   *
   * @see javax.swing.JSplitPane
   * @see javax.swing.plaf.basic.BasicSplitPaneDivider
   * @see #getSplitPaneBorder()
   *
   * @since 1.3
   */
  public static Border getSplitPaneDividerBorder()
  {
    UIDefaults defaults;

    /* See comment in methods above for why this border is not shared. */
    defaults = UIManager.getLookAndFeelDefaults();
    return new SplitPaneDividerBorder(
      defaults.getColor("SplitPane.highlight"),
      defaults.getColor("SplitPane.darkShadow"));
  }


  /**
   * Returns a border for drawing a border around a text field
   * that makes the field appear as etched into the surface.
   *
   * <p>The colors of the border are retrieved from the
   * <code>UIDefaults</code> of the currently active look and feel
   * using the keys <code>&#x201c;TextField.shadow&#x201d;</code>,
   * <code>&#x201c;TextField.darkShadow&#x201d;</code>,
   * <code>&#x201c;TextField.light&#x201d;</code>, and
   * <code>&#x201c;TextField.highlight&#x201d;</code>.
   *
   * <p><img src="doc-files/BasicBorders.FieldBorder-1.png" width="500"
   * height="200" alt="[A screen shot of a border returned by
   * this method]" />
   *
   * @return an instance of
   * {@link javax.swing.plaf.basic.BasicBorders$FieldBorder}.
   *
   * @see javax.swing.JTextField
   * @see javax.swing.text.JTextComponent
   */
  public static Border getTextFieldBorder()
  {
    UIDefaults defaults;

    /* See comment in methods above for why this border is not shared. */
    defaults = UIManager.getLookAndFeelDefaults();
    return new FieldBorder(
      defaults.getColor("TextField.shadow"),
      defaults.getColor("TextField.darkShadow"),
      defaults.getColor("TextField.light"),
      defaults.getColor("TextField.highlight"));
  }
  

  /**
   * Returns a two-pixel thick, green
   * <code>LineBorderUIResource</code>.  This is so ugly that look and
   * feels better use different borders for their progress bars, or
   * they will look really terrible.
   *
   * <p><img src="doc-files/BasicBorders-1.png" width="120" height="80"
   * alt="[A screen shot of a border returned by this method]" />
   */
  public static Border getProgressBarBorder()
  {
    /* There does not seem to exist a way to parametrize the color
     * or thickness of the border through UIDefaults.
     */
    return new BorderUIResource.LineBorderUIResource(Color.green, 2);
  }


  /**
   * Returns a border that is composed of a raised bevel border and a
   * one-pixel thick line border.
   *
   * <p><img src="doc-files/BasicBorders-2.png" width="300" height="200"
   * alt="[A screen shot of a border returned by this method]" />
   *
   * <p>The colors of the border are retrieved from the
   * <code>UIDefaults</code> of the currently active look and feel
   * using the keys <code>&#x201c;InternalFrame.borderShadow&#x201d;</code>,
   * <code>&#x201c;InternalFrame.borderDarkShadow&#x201d;</code>,
   * <code>&#x201c;InternalFrame.borderLight&#x201d;</code>,
   * <code>&#x201c;InternalFrame.borderHighlight&#x201d;</code>, and
   * (for the inner one-pixel thick line)
   * <code>&#x201c;InternalFrame.borderColor&#x201d;</code>.
   */
  public static Border getInternalFrameBorder()
  {
    UIDefaults defaults;
    Color shadow, darkShadow, highlight, lightHighlight, line;

    /* See comment in methods above for why this border is not shared. */
    defaults = UIManager.getLookAndFeelDefaults();
    
    shadow = defaults.getColor("InternalFrame.borderShadow");
    darkShadow = defaults.getColor("InternalFrame.borderDarkShadow");
    highlight = defaults.getColor("InternalFrame.borderLight");
    lightHighlight = defaults.getColor("InternalFrame.borderHighlight");
    line = defaults.getColor("InternalFrame.borderColor");

    return new BorderUIResource.CompoundBorderUIResource(
      /* outer border */
      new BorderUIResource.BevelBorderUIResource(
        BevelBorder.RAISED,
        (highlight != null) ? highlight : Color.lightGray,
        (lightHighlight != null) ? lightHighlight : Color.white,
        (darkShadow != null) ? darkShadow : Color.black,
        (shadow != null) ? shadow : Color.gray),

      /* inner border */
      new BorderUIResource.LineBorderUIResource(
        (line != null) ? line : Color.lightGray));
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
   * <p><img src="doc-files/BasicBorders.ButtonBorder-1.png" width="300"
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
  
  
  /**
   * A border that makes its enclosed component appear as lowered
   * into the surface. Typically used for text fields.
   *
   * <p><img src="doc-files/BasicBorders.FieldBorder-1.png" width="500"
   * height="200" alt="[A screen shot of this border]" />
   *
   * @see javax.swing.plaf.basic.BasicGraphicsUtils#drawEtchedRect
   *
   * @author Sascha Brawer (brawer@dandelis.ch)
   */
  public static class FieldBorder
    extends AbstractBorder
    implements UIResource
  {
    /**
     * Determined using the <code>serialver</code> tool
     * of Apple/Sun JDK 1.3.1 on MacOS X 10.1.5.
     */
    static final long serialVersionUID = 949220756998454908L;


    /**
     * The color for drawing the outer half of the top and left
     * edges.
     */
    protected Color shadow;


    /**
     * The color for drawing the inner half of the top and left
     * edges.
     */
    protected Color darkShadow;


    /**
     * The color for drawing the inner half of the bottom and right
     * edges.
     */
    protected Color highlight;


    /**
     * The color for drawing the outer half of the bottom and right
     * edges.
     */
    protected Color lightHighlight;


    /**
     * Constructs a new border for drawing a text field in the Basic
     * look and feel.
     *
     * @param shadow the color for drawing the outer half
     *        of the top and left edges.
     *
     * @param darkShadow the color for drawing the inner half
     *        of the top and left edges.
     *
     * @param highlight the color for drawing the inner half
     *        of the bottom and right edges.
     *
     * @param lightHighlight the color for drawing the outer half
     *        of the bottom and right edges.
     */
    public FieldBorder(Color shadow, Color darkShadow,
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
        ? lightHighlight : Color.white;
    }

    
    /**
     * Paints the FieldBorder around a given component.
     *
     * @param c the component whose border is to be painted.
     * @param g the graphics for painting.
     * @param x the horizontal position for painting the border.
     * @param y the vertical position for painting the border.
     * @param width the width of the available area for painting the border.
     * @param height the height of the available area for painting the border.
     *
     * @see javax.swing.plaf.basic.BasicGraphicsUtils#drawEtchedRect
     */
    public void paintBorder(Component c, Graphics  g,
                            int x, int y, int width, int height)
    {
      BasicGraphicsUtils.drawEtchedRect(g, x, y, width, height,
                                        shadow, darkShadow,
                                        highlight, lightHighlight);
    }
    
    
    /**
     * Measures the width of this border.
     *
     * @param c the component whose border is to be measured.
     *        If <code>c</code> is an instance of {@link
     *        javax.swing.text.JTextComponent}, its margin is
     *        added to the border size.
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
      return getBorderInsets(c, null);
    }


    /**
     * Measures the width of this border, storing the results into a
     * pre-existing Insets object.
     *
     * @param c the component whose border is to be measured.
     *        If <code>c</code> is an instance of {@link
     *        javax.swing.text.JTextComponent}, its margin is
     *        added to the border size.
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
      if (insets == null)
        insets = new Insets(2, 2, 2, 2);
      else
        insets.top = insets.left = insets.bottom = insets.right = 2;

      if (c instanceof JTextComponent)
      {
        Insets margin = ((JTextComponent) c).getMargin();
        insets.top += margin.top;
        insets.left += margin.left;
        insets.bottom += margin.bottom;
        insets.right += margin.right;
      }

      return insets;
    }
  }
  
  
  /**
   * An invisible, but spacing border whose margin is determined
   * by calling the <code>getMargin()</code> method of the enclosed
   * component.  If the enclosed component has no such method,
   * this border will not occupy any space.
   *
   * <p><img src="doc-files/BasicBorders.MarginBorder-1.png" width="325"
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
     * {@link javax.swing.JPopupMenu#getMargin()}, {@link
     * javax.swing.JToolBar#getMargin()}, and {@link
     * javax.swing.text.JTextComponent}. If <code>c</code> is an
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
      else if (c instanceof JTextComponent)
        margin = ((JTextComponent) c).getMargin();
      
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
   * <p><img src="doc-files/BasicBorders.MenuBarBorder-1.png" width="500"
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
     * <p><img src="doc-files/BasicBorders.MenuBarBorder-1.png" width="500"
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
   * <p><img src="doc-files/BasicBorders.RadioButtonBorder-1.png" width="300"
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


  /**
   * A one-pixel thick border for rollover buttons, for example in
   * tool bars.
   *
   * @since 1.4
   * @author Sascha Brawer (brawer@dandelis.ch)
   */
  public static class RolloverButtonBorder
    extends ButtonBorder
  {
    /**
     * Determined using the <code>serialver</code> tool
     * of Sun JDK 1.4.1_01 on GNU/Linux 2.4.20 for x86.
     */
    static final long serialVersionUID = 1976364864896996846L;


    /**
     * Constructs a new border for drawing a roll-over button
     * in the Basic look and feel.
     *
     * @param shadow the shadow color.
     * @param darkShadow a darker variant of the shadow color.
     * @param highlight the highlight color.
     * @param lightHighlight a brighter variant of the highlight  color.
     */
    public RolloverButtonBorder(Color shadow, Color darkShadow,
                                Color highlight, Color lightHighlight)
    {
      super(shadow, darkShadow, highlight, lightHighlight);
    }


    /**
     * Paints the border around a rollover button.  If <code>c</code>
     * is not an {@link javax.swing.AbstractButton} whose model
     * returns <code>true</code> for {@link
     * javax.swing.ButtonModel#isRollver}, nothing gets painted at
     * all.
     *
     * @param c the button whose border is to be painted.
     * @param g the graphics for painting.
     * @param x the horizontal position for painting the border.
     * @param y the vertical position for painting the border.
     * @param width the width of the available area for painting the border.
     * @param height the height of the available area for painting the border.
     */
    public void paintBorder(Component c, Graphics  g,
                            int x, int y, int width, int height)
    {
      ButtonModel bmodel = null;
      boolean drawPressed;
      Color oldColor = g.getColor();
      int x2, y2;

      if (c instanceof AbstractButton)
        bmodel = ((AbstractButton) c).getModel();

      /* Draw nothing if c is not a rollover button. */
      if ((bmodel == null) || !bmodel.isRollover())
        return;

      /* Draw nothing if the mouse is pressed, but outside the button. */
      if (bmodel.isPressed() && !bmodel.isArmed())
        return;

      drawPressed = bmodel.isSelected() || bmodel.isPressed();
      x2 = x + width - 1;
      y2 = y + height - 1;

      try
      {
        g.setColor(drawPressed ? shadow : lightHighlight);
        g.drawLine(x, y, x2 - 1, y);     // top edge
        g.drawLine(x, y + 1, x, y2 - 1); // left edge

        g.setColor(drawPressed ? lightHighlight : shadow);
        g.drawLine(x, y2, x2, y2);       // bottom edge
        g.drawLine(x2, y, x2, y2 - 1);   // right edge
      }
      finally
      {
        g.setColor(oldColor);
      }
    }
  }


  /**
   * A border for JSplitPanes in the Basic look and feel. The divider
   * in the middle of the JSplitPane has its own border class, of which
   * an instance can be obtained with {@link #getSplitPaneDividerBorder()}.
   *
   * <p><img src="doc-files/BasicBorders.SplitPaneBorder-1.png" width="520"
   * height="200" alt="[A screen shot for JSplitPane.HORIZONTAL_SPLIT]" />
   *
   * <p><img src="doc-files/BasicBorders.SplitPaneBorder-2.png" width="520"
   * height="200" alt="[A screen shot for JSplitPane.VERTICAL_SPLIT]" />
   *
   * <p>In contrast to the other borders of the Basic look and feel,
   * this class is not serializable. While this might be unintended,
   * GNU Classpath follows the specification in order to be fully
   * compatible with the Sun reference implementation.
   *
   * <p>In the Sun JDK, the bottom edge of the divider also gets
   * painted if the orientation of the enclosed JSplitPane is
   * <code>JSplitPane.VERTICAL_SPLIT</code> (at least in versions
   * 1.3.1 and 1.4.1).  GNU Classpath does not replicate this bug. A
   * report has been filed with Sun (bug ID 4885629).
   *
   * <p>Note that the bottom left pixel of the border has a different
   * color depending on the orientation of the enclosed JSplitPane.
   * Although this is visually inconsistent, Classpath replicates the
   * appearance of the Sun reference implementation. A bug report has
   * been filed with Sun (review ID 188774).
   *
   * @see {@link #getSplitPaneBorder()}
   * @see {@link #getSplitPaneDividerBorder()}
   *
   * @author Sascha Brawer (brawer@dandelis.ch)
   */
  public static class SplitPaneBorder
    implements Border, UIResource
  {
    /**
     * Indicates that the top edge shall be not be painted
     * by {@link #paintRect(java.awt.Graphics, int, int, int, int, int)}.
     */
    private static final int SUPPRESS_TOP = 1;


    /**
     * Indicates that the left edge shall be not be painted
     * by {@link #paintRect(java.awt.Graphics, int, int, int, int, int)}.
     */
    private static final int SUPPRESS_LEFT = 2;


    /**
     * Indicates that the bottom edge shall be not be painted
     * by {@link #paintRect(java.awt.Graphics, int, int, int, int, int)}.
     */
    private static final int SUPPRESS_BOTTOM = 4;


    /**
     * Indicates that the right edge shall be not be painted
     * by {@link #paintRect(java.awt.Graphics, int, int, int, int, int)}.
     */
    private static final int SUPPRESS_RIGHT = 8;


    /**
     * The color for drawing the bottom and right edges of the border.
     */
    protected Color highlight;


    /**
     * The color for drawing the top and left edges of the border.
     */
    protected Color shadow;


    /**
     * Constructs a new border for drawing a JSplitPane in the Basic
     * look and feel.  The divider in the middle of the JSplitPane has
     * its own border class, <code>SplitPaneDividerBorder</code>.
     *
     * @param shadow the shadow color.
     * @param highlight the highlight color.
     */
    public SplitPaneBorder(Color highlight, Color shadow)
    {
      /* These colors usually come from the UIDefaults of the current
       * look and feel. Use fallback values if the colors are not
       * supplied.  The API specification is silent about what
       * behavior is expected for null colors, so users should not
       * rely on this fallback (which is why it is not documented in
       * the above Javadoc).
       */
      this.shadow = (shadow != null) ? shadow : Color.black;
      this.highlight = (highlight != null) ? highlight : Color.white;
    }


    /**
     * Paints the border around a <code>JSplitPane</code>.
     *
     * <p><img src="doc-files/BasicBorders.SplitPaneBorder-1.png" width="520"
     * height="200" alt="[A screen shot for JSplitPane.HORIZONTAL_SPLIT]" />
     *
     * <p><img src="doc-files/BasicBorders.SplitPaneBorder-2.png" width="520"
     * height="200" alt="[A screen shot for JSplitPane.VERTICAL_SPLIT]" />
     *
     * @param c the <code>JSplitPane</code> whose border is to be painted.
     * @param g the graphics for painting.
     * @param x the horizontal position for painting the border.
     * @param y the vertical position for painting the border.
     * @param width the width of the available area for painting the border.
     * @param height the height of the available area for painting the border.
     */
    public void paintBorder(Component c, Graphics  g,
                            int x, int y, int width, int height)
    {
      JSplitPane splitPane;
      Component content;

      if (!(c instanceof JSplitPane))
        return;

      splitPane = (JSplitPane) c;
      switch (splitPane.getOrientation())
      {
      case JSplitPane.HORIZONTAL_SPLIT:
        if ((content = splitPane.getLeftComponent()) != null)
          paintRect(g, SUPPRESS_RIGHT, true, x, y, content.getBounds());
        if ((content = splitPane.getRightComponent()) != null)
          paintRect(g, SUPPRESS_LEFT, true, x, y, content.getBounds());
        break;

      case JSplitPane.VERTICAL_SPLIT:
        if ((content = splitPane.getTopComponent()) != null)
          paintRect(g, SUPPRESS_BOTTOM, false, x, y, content.getBounds());
        if ((content = splitPane.getBottomComponent()) != null)
          paintRect(g, SUPPRESS_TOP, false, x, y, content.getBounds());
        break;
      }
    }


    /**
     * Paints a border around a child of a <code>JSplitPane</code>,
     * omitting some of the edges.
     *
     * @param g the graphics for painting.
     *
     * @param suppress a bit mask indicating the set of suppressed
     *        edges, for example <code>SUPPRESS_TOP | SUPPRESS_RIGHT</code>.
     *
     * @param x the x coordinate of the SplitPaneBorder.
     *
     * @param y the y coordinate of the SplitPaneBorder.
     *
     * @param shadeBottomLeftPixel <code>true</code> to paint the
     *        bottom left pixel in the shadow color,
     *        <code>false</code> for the highlight color. The Basic
     *        look and feel uses the highlight color for the bottom
     *        left pixel of the border of a JSplitPane whose
     *        orientation is VERTICAL_SPLIT, and the shadow color
     *        otherwise. While this might be a strange distinction,
     *        Classpath tries to look identical to the reference
     *        implementation. A bug report has been filed with Sun;
     *        its review ID is 188774. We currently replicate the
     *        Sun behavior.
     *
     * @param rect the bounds of the child of JSplitPane whose
     *        border is to be painted.
     */
    private void paintRect(Graphics g, int suppress,
                           boolean shadeBottomLeftPixel,
                           int x, int y,
                           Rectangle rect)
    {
      if (rect == null)
        return;

      /* On each edge, the border exceeds the enclosed child by one
       * pixel. See the image "BasicBorders.SplitPaneBorder-1.png" in
       * the directory "doc-files".
       */
      x += rect.x - 1;
      y += rect.y - 1;
      int right = x + rect.width + 1;
      int bottom = y + rect.height + 1;
      
      Color oldColor = g.getColor();
      try
      {
        g.setColor(shadow);
        if ((suppress & SUPPRESS_TOP) == 0)
          g.drawLine(x, y, right, y);
        if ((suppress & SUPPRESS_LEFT) == 0)
          g.drawLine(x, y, x, bottom);
        else
          g.drawLine(x, bottom, x, bottom); // one pixel

        g.setColor(highlight);
        if ((suppress & SUPPRESS_BOTTOM) == 0)
          g.drawLine(x + (shadeBottomLeftPixel ? 1 : 0), bottom, right, bottom);
        else if (!shadeBottomLeftPixel)
          g.drawLine(x, bottom, x, bottom); // one pixel

        if ((suppress & SUPPRESS_RIGHT) == 0)
          g.drawLine(right, y, right, bottom);
      }
      finally
      {
        g.setColor(oldColor);
      }
    }

    
    /**
     * Measures the width of this border.
     *
     * @param c the component whose border is to be measured, usually
     *        an instance of {@link javax.swing.JSplitPane}.
     *
     * @return an Insets object whose <code>left</code>,
     *         <code>right</code>, <code>top</code> and
     *         <code>bottom</code> fields indicate the width of the
     *         border at the respective edge.
     */
    public Insets getBorderInsets(Component c)
    {
      return new Insets(1, 1, 1, 1);
    }


    /**
     * Determines whether this border fills every pixel in its area
     * when painting.
     *
     * @return <code>false</code> because this border does not
     *         paint over the pixels where the divider joins
     *         the border.
     */
    public boolean isBorderOpaque()
    {
      /* Strangely, the Sun implementation (tested with JDK 1.3.1 and
       * 1.4.1_01) seems to always return true. It could be a bug,
       * but without knowing the details of their implementation, it is
       * hard to decide.
       */
      return false;
    }
  }


  /**
   * A border for the divider inside a JSplitPane.
   *
   * <p><img src="doc-files/BasicBorders.SplitPaneDividerBorder-1.png"
   * width="520" height="200" alt="[A screen shot of this border]" />
   *
   * @author Sascha Brawer (brawer@dandelis.ch)
   */
  private static class SplitPaneDividerBorder
    implements Border, UIResource, Serializable
  {
    /**
     * The highlight color, which is drawn on the left or top edge
     * depending on the orientation of the JSplitPanel.
     */
    protected Color highlight;


    /**
     * The highlight color, which is drawn on the right or bottom edge
     * depending on the orientation of the JSplitPanel.
     */
    protected Color shadow;


    /**
     * Constructs a new border for drawing the divider of a JSplitPane
     * in the Basic look and feel.  The outer parts of the JSplitPane have
     * their own border class, <code>SplitPaneBorder</code>.
     *
     * @param shadow the shadow color.
     * @param highlight the highlight color.
     */
    public SplitPaneDividerBorder(Color highlight, Color shadow)
    {
      this.highlight = (highlight != null) ? highlight : Color.white;
      this.shadow = (shadow != null) ? shadow : Color.black;
    }


    /**
     * Paints the border around the divider of a <code>JSplitPane</code>.
     *
     * <p><img src="doc-files/BasicBorders.SplitPaneDividerBorder-1.png"
     * width="520" height="200" alt="[A picture that shows which pixels
     * get painted in what color]" />
     *
     * @param c the <code>JSplitPane</code> whose divider&#x2019;s border
     *        is to be painted.
     * @param g the graphics for painting.
     * @param x the horizontal position for painting the border.
     * @param y the vertical position for painting the border.
     * @param width the width of the available area for painting the border.
     * @param height the height of the available area for painting the border.
     */
    public void paintBorder(Component c, Graphics  g,
                            int x, int y, int width, int height)
    {
      Color oldColor, dcol;
      int x2, y2;
      JSplitPane sp;

      sp = getSplitPane(c);
      if (sp == null)
        return;

      x2 = x + width - 1;
      y2 = y + height - 1;
      oldColor = g.getColor();
      dcol = c.getBackground();
      try
      {
        switch (sp.getOrientation())
        {
        case JSplitPane.HORIZONTAL_SPLIT:
          g.setColor(dcol);
          g.drawLine(x + 1, y, x2 - 1, y);
          g.drawLine(x + 1, y2, x2 - 1, y2);
          g.setColor(sp.getLeftComponent() != null ? highlight : dcol);
          g.drawLine(x, y, x, y2);
          g.setColor(sp.getRightComponent() != null ? shadow : dcol);
          g.drawLine(x2, y, x2, y2);
          break;

        case JSplitPane.VERTICAL_SPLIT:
          g.setColor(dcol);
          g.drawLine(x, y + 1, x, y2 - 1);
          g.drawLine(x2, y + 1, x2, y2 - 1);
          g.setColor(sp.getTopComponent() != null ? highlight : dcol);
          g.drawLine(x, y, x2, y);
          g.setColor(sp.getBottomComponent() != null ? shadow : dcol);
          g.drawLine(x, y2, x2, y2);
          break;
        }
      }
      finally
      {
        g.setColor(oldColor);
      }
    }


    /**
     * Measures the width of this border.
     *
     * @param c the component whose border is to be measured, usually
     *        an instance of {@link javax.swing.JSplitPane}.
     *
     * @return an Insets object whose <code>left</code>,
     *         <code>right</code>, <code>top</code> and
     *         <code>bottom</code> fields indicate the width of the
     *         border at the respective edge.
     */
    public Insets getBorderInsets(Component c)
    {
      return new Insets(1, 1, 1, 1);
    }


    /**
     * Determines whether this border fills every pixel in its area
     * when painting.
     *
     * @return <code>true</code> if both highlight and shadow
     *         color are fully opaque.
     */
    public boolean isBorderOpaque()
    {
      return (highlight.getAlpha() == 255) && (shadow.getAlpha() == 255);
    }

    
    /**
     * Determines the JSplitPane whose divider is being painted.
     *
     * @param c an instance of BasicSplitPaneDivider.
     *
     * @return a <code>JSplitPane</code>, or <code>null</code> if
     *         <code>c</code> is not an instance of {@link
     *         javax.swing.plaf.basic.BasicSplitPaneDivider}.
     */
    private JSplitPane getSplitPane(Component c)
    {
      if (c instanceof BasicSplitPaneDivider)
        return (((BasicSplitPaneDivider) c).getBasicSplitPaneUI())
          .getSplitPane();
      else
        return null;
    }
  }


  /**
   * A border for toggle buttons in the Basic look and feel.
   *
   * <p><img src="doc-files/BasicBorders.ToggleButtonBorder-1.png"
   * width="270" height="135" alt="[A screen shot of this border]" />
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
