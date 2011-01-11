package gnu.javax.swing.text.html;


import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.io.Serializable;

import javax.swing.Icon;
import javax.swing.plaf.metal.MetalLookAndFeel;

/**
 * Creates icons for ImageView. The icons reflect the basic ideas of the Sun's
 * icons as they would be described in the text (sheet of paper with image and
 * broken sheet of paper with image). They are not pixel to pixel identical and
 * contain elements from the metal icon factory.
 *
 * @author Audrius Meskauskas (audriusa@bioinformatics.org)
 */
public class ImageViewIconFactory
{
  private static Icon noImageIcon;

  private static Icon loadingImageIcon;

  /**
   * This icon reflects the general concept (broken sheet of paper with
   * image), but is currently not pixel to pixel identical with the Sun's
   * implementation.
   */
  public static class NoImageIcon implements Icon, Serializable
  {
    /**
     * Creates a new icon.
     */
    public NoImageIcon()
    {
      // Nothing to do here.
    }

    /**
     * Returns the width of the icon, in pixels.
     *
     * @return The width of the icon.
     */
    public int getIconWidth()
    {
      return 38;
    }

    /**
     * Returns the height of the icon, in pixels.
     *
     * @return The height of the icon.
     */
    public int getIconHeight()
    {
      return 38;
    }

    /**
     * Paints the icon using colors from the {@link MetalLookAndFeel}.
     *
     * @param c
     *          the component (ignored).
     * @param g
     *          the graphics device.
     * @param x
     *          the x-coordinate for the top-left of the icon.
     * @param y
     *          the y-coordinate for the top-left of the icon.
     */
    public void paintIcon(Component c, Graphics g, int x, int y)
    {
      // frame
      Color savedColor = g.getColor();

      g.setColor(MetalLookAndFeel.getBlack());

      g.drawLine(x, y, x + 19, y);

      g.drawLine(x, y + 1, x, y + 5);
      g.drawLine(x, y + 13, x, y + 25);

      g.drawLine(x, y + 25, x + 22, y + 25);

      g.drawLine(x + 22, y + 25, x + 22, y + 21);
      g.drawLine(x + 22, y + 13, x + 22, y + 6);

      g.drawLine(x + 22, y + 6, x + 19, y);

      g.drawLine(x + 17, y + 2, x + 21, y + 6);

      g.drawLine(x + 18, y + 1, x + 19, y + 1);

      g.setColor(MetalLookAndFeel.getControlShadow());

      g.drawLine(x + 1, y + 1, x + 17, y + 1);

      g.drawLine(x + 1, y + 1, x + 1, y + 5);
      g.drawLine(x + 1, y + 13, x + 1, y + 24);

      g.drawLine(x + 1, y + 24, x + 21, y + 24);

      g.drawLine(x + 21, y + 24, x + 21, y + 21);
      g.drawLine(x + 21, y + 13, x + 21, y + 7);

      g.drawLine(x + 18, y + 2, x + 20, y + 4);

      // Breaking line

      // Shadow
      g.drawLine(x + 1, y + 6, x + 20, y + 13);
      g.drawLine(x + 1, y + 13, x + 20, y + 20);

      // Edge
      g.setColor(MetalLookAndFeel.getBlack());
      g.drawLine(x, y + 6, x + 21, y + 14);
      g.drawLine(x, y + 12, x + 21, y + 20);

      // Picture

      y += 1;
      x += 3;

      g.setColor(MetalLookAndFeel.getBlack());

      // roof
      g.drawLine(x + 4, y + 5, x + 8, y + 1);
      g.drawLine(x + 8, y + 1, x + 15, y + 8);

      // chimney
      g.drawLine(x + 11, y + 2, x + 11, y + 4);
      g.drawLine(x + 12, y + 2, x + 12, y + 5);

      g.setColor(MetalLookAndFeel.getControlDarkShadow());

      // roof paint
      int xx = x + 8;
      for (int i = 0; i < 4; i++)
        g.drawLine(xx - i, y + 2 + i, xx + i, y + 2 + i);
      g.fillRect(x + 4, y + 6, 9, 2);

      // base of house
      g.drawLine(x + 3, y + 14, x + 3, y + 18);
      g.drawLine(x + 3, y + 18, x + 13, y + 18);

      g.setColor(savedColor);
    }
  }

  /**
   * This icon reflects the general concept (sheet of paper with image), but is
   * currently not pixel to pixel identical with the Sun's implementation.
   */
  public static class LoadingImageIcon implements Icon, Serializable
  {

    /**
     * Creates a new icon.
     */
    public LoadingImageIcon()
    {
      // Nothing to do here.
    }

    /**
     * Returns the width of the icon, in pixels.
     *
     * @return The width of the icon.
     */
    public int getIconWidth()
    {
      return 38;
    }

    /**
     * Returns the height of the icon, in pixels.
     *
     * @return The height of the icon.
     */
    public int getIconHeight()
    {
      return 38;
    }

    /**
     * Paints the icon using colors from the {@link MetalLookAndFeel}.
     *
     * @param c
     *          the component (ignored).
     * @param g
     *          the graphics device.
     * @param x
     *          the x-coordinate for the top-left of the icon.
     * @param y
     *          the y-coordinate for the top-left of the icon.
     */
    public void paintIcon(Component c, Graphics g, int x, int y)
    {
      // frame
      Color savedColor = g.getColor();

      g.setColor(Color.black);
      g.drawLine(x, y, x + 19, y);
      g.drawLine(x, y + 1, x, y + 25);
      g.drawLine(x, y + 25, x + 22, y + 25);
      g.drawLine(x + 22, y + 25, x + 22, y + 6);
      g.drawLine(x + 22, y + 6, x + 19, y);

      g.drawLine(x + 17, y + 2, x + 21, y + 6);
      g.drawLine(x + 18, y + 1, x + 19, y + 1);

      g.setColor(new Color(204, 204, 255));

      g.drawLine(x + 1, y + 1, x + 17, y + 1);
      g.drawLine(x + 1, y + 1, x + 1, y + 24);
      g.drawLine(x + 1, y + 24, x + 21, y + 24);
      g.drawLine(x + 21, y + 24, x + 21, y + 7);
      g.drawLine(x + 18, y + 2, x + 20, y + 4);

      // Picture (house)

      y += 3;
      x += 3;

      g.setColor(MetalLookAndFeel.getBlack());

      // roof
      g.drawLine(x + 1, y + 8, x + 8, y + 1);
      g.drawLine(x + 8, y + 1, x + 15, y + 8);

      // base of house
      g.drawLine(x + 3, y + 6, x + 3, y + 15);
      g.drawLine(x + 3, y + 15, x + 13, y + 15);
      g.drawLine(x + 13, y + 6, x + 13, y + 15);

      // door frame
      g.drawLine(x + 6, y + 9, x + 6, y + 15);
      g.drawLine(x + 6, y + 9, x + 10, y + 9);
      g.drawLine(x + 10, y + 9, x + 10, y + 15);

      // chimney
      g.drawLine(x + 11, y + 2, x + 11, y + 4);
      g.drawLine(x + 12, y + 2, x + 12, y + 5);

      g.setColor(MetalLookAndFeel.getControlDarkShadow());

      // roof paint
      int xx = x + 8;
      for (int i = 0; i < 4; i++)
        g.drawLine(xx - i, y + 2 + i, xx + i, y + 2 + i);
      g.fillRect(x + 4, y + 6, 9, 2);

      // door knob
      g.drawLine(x + 9, y + 12, x + 9, y + 12);

      // house paint
      g.setColor(MetalLookAndFeel.getPrimaryControl());
      g.drawLine(x + 4, y + 8, x + 12, y + 8);
      g.fillRect(x + 4, y + 9, 2, 6);
      g.fillRect(x + 11, y + 9, 2, 6);

      g.setColor(savedColor);
    }
  }

  public static Icon getNoImageIcon()
  {
    if (noImageIcon == null)
      noImageIcon = new NoImageIcon();
    return noImageIcon;
  }

  public static Icon getLoadingImageIcon()
  {
    if (loadingImageIcon == null)
      loadingImageIcon = new LoadingImageIcon();
    return loadingImageIcon;
  }

}
