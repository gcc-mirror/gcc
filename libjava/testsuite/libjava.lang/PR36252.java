import java.io.UnsupportedEncodingException;

public class PR36252
{
  public static void main(String[] args)
  {
    try {
      byte[] txt = new byte[] {-55, 87, -55, -42, -55, -20};
      // This new String(...) should not throw an OutOfMemoryError.
      String s = new String(txt, 0, 6, "MS932");
    } catch (UnsupportedEncodingException e) {
      // Silently ignore.
    }
    System.out.println("ok");
  }
}
