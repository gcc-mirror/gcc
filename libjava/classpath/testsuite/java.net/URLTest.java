import java.net.*;
import java.io.*;

public class URLTest
{
  public static void main(String args[])
    {
      try {
	URL url = new URL("http://www.hungry.com/");
	InputStream stream = url.openStream();

	int size = 0;

	while (-1 != stream.read()) { size++; }

	stream.close();

	System.out.println("PASSED: new URL() size=" + size );
      } catch (Exception e) {
	System.out.println("FAILED: " + e);
      }
    }
}
