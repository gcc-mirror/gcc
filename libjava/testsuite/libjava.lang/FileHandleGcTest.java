// Make sure that file handles are garbage collected.
import java.io.*;
public class FileHandleGcTest
{
  static void kill () throws FileNotFoundException
  {
    for (int i = 0; i < 65536; i++)
      {
	FileInputStream f = new FileInputStream ("/dev/null");
      }
  }

  public static void
  main (String argv [])
  {
    try
      {
	kill ();
      }
    catch (FileNotFoundException _)
      {
      }
  }
}
