package javax.swing.plaf;
import java.awt.Font;
/**
 * STUBBED
 */
public class FontUIResource extends Font implements UIResource
{
  public FontUIResource(Font f)
  {
    super(f.getName(), f.getStyle(), f.getSize());
  }
  public FontUIResource(String name, int style, int size)
  {
    super(name, style, size);
  }
} // class FontUIResource
