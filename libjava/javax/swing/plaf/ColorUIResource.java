package javax.swing.plaf;
import java.awt.Color;
/**
 * STUBBED
 */
public class ColorUIResource extends Color implements UIResource
{
  public ColorUIResource(Color c)
  {
    super(c.getRGB());
  }
  public ColorUIResource(float r, float g, float b)
  {
    super(r, g, b, 1.0f);
  }
  public ColorUIResource(int rgb)
  {
    super(rgb, false);
  }
  public ColorUIResource(int r, int g, int b)
  {
    super(r, g, b, 255);
  }
} // class ColorUIResource
