import java.io.*;

// Test case for http://gcc.gnu.org/PR12866
// From Mark Wielaard
public class InnerExcept
{
  static private void createFile() throws IOException
  {
    new File("/dev/null");
  }

  class Inner
  {
    private void m() throws IOException
    {
      createFile();
    }
  }
}
