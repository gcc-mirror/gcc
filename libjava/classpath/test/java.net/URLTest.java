/* Test URL's */

import java.net.*;
import java.io.*;

public class URLTest
{

public static void
main(String argv[])
{
  System.out.println("Starting URL tests");

  /* Simple URL test */

  System.out.println("Test 1: Simple URL test");

  try
    { 
      URL url = new URL("http", "www.fsf.org", 80, "/");

      if (!url.getProtocol().equals("http") ||
          !url.getHost().equals("www.fsf.org") ||
          url.getPort() != 80 ||
          !url.getFile().equals("/"))
      System.out.println("FAILED: Simple URL test");

      System.out.println("URL is: " + url.toString());

      URLConnection uc = url.openConnection();

      if (uc instanceof HttpURLConnection)
         System.out.println("Got the expected connection type");

      HttpURLConnection hc = (HttpURLConnection)uc;

      hc.connect();

      System.out.flush();
      System.out.println("Dumping response headers");
      for (int i = 0; ; i++)
        {
          String key = hc.getHeaderFieldKey(i);
          if (key == null)
            break;

          System.out.println(key + ": " + hc.getHeaderField(i));
        }

      System.out.flush();
      System.out.println("Dumping contents");

      BufferedReader br = new BufferedReader(new 
                              InputStreamReader(hc.getInputStream()));

      for (String str = br.readLine(); str != null; str = br.readLine())
        {
          System.out.println(str);
        }
      System.out.flush();
       
      hc.disconnect();

      System.out.println("Content Type: " + hc.getContentType());
      System.out.println("Content Encoding: " + hc.getContentEncoding());
      System.out.println("Content Length: " + hc.getContentLength());
      System.out.println("Date: " + hc.getDate());
      System.out.println("Expiration: " + hc.getExpiration());
      System.out.println("Last Modified: " + hc.getLastModified());

      System.out.println("PASSED: Simple URL test");
    }
  catch(IOException e)
    {
      System.out.println("FAILED: Simple URL test: " + e);
    }

  // Parsing test
  System.out.println("Test 2: URL parsing test");
  try
    {
      URL url = new URL("http://www.urbanophile.com/arenn/trans/trans.html#mis");
      if (!url.toString().equals(
          "http://www.urbanophile.com/arenn/trans/trans.html#mis"))
        System.out.println("FAILED: Parse URL test: " + url.toString());
      else {
        System.out.println("Parsed ok: " + url.toString());
        url = new URL("http://www.foo.com:8080/#");
        if (!url.toString().equals("http://www.foo.com:8080/#"))
          System.out.println("FAILED: Parse URL test: " + url.toString());
        else {
          System.out.println("Parsed ok: " + url.toString());
          url = new URL("http://www.bar.com/test:file/");
          if (!url.toString().equals("http://www.bar.com/test:file/"))
            System.out.println("FAILED: Parse URL test: " + url.toString());
          else {
            System.out.println("Parsed ok: " + url.toString());
            url = new URL("http://www.gnu.org");
            if (!url.toString().equals("http://www.gnu.org/"))
              System.out.println("FAILED: Parse URL test: " + url.toString());
            else {
              System.out.println("Parsed ok: " + url.toString());
              url = new URL("HTTP://www.fsf.org/");
              if (!url.toString().equals("http://www.fsf.org/"))
                System.out.println("FAILED: Parse URL test: " + url.toString());
              else { 
                System.out.println("Parsed ok: " + url.toString());
                System.out.println("PASSED: URL parse test");
              }
            }
          }
        }
      }
    }
  catch (IOException e)
    {
      System.out.println("FAILED: URL parsing test: " + e);
    }

  // getContent test
  System.out.println("Test 3: getContent test");
  try
    {
      URL url = new URL("http://localhost/~arenn/services.txt");

      Object obj = url.getContent();
      System.out.println("Object type is: " + obj.getClass().getName());

      if (obj instanceof InputStream)
        {
          System.out.println("Got InputStream, so dumping contents");
          BufferedReader br = new BufferedReader(new 
                                  InputStreamReader((InputStream)obj));

          for (String str = br.readLine(); str != null; str = br.readLine())
             System.out.println(str);

          br.close();
        }
      else
        {
          System.out.println("FAILED: Object is not an InputStream");
        }

      System.out.println("PASSED: getContent test");
    }
  catch (IOException e)
    {
      System.out.println("FAILED: getContent test: " + e);
    }

  System.out.println("URL test complete");
}

}

