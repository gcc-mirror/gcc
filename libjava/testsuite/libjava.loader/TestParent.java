import java.net.*;

public class TestParent
{
  public static void main (String[] args)
  {
    URLClassLoader ucl =
      (URLClassLoader) ClassLoader.getSystemClassLoader();
    URL urls[] = ucl.getURLs ();

    MyLoader parent = new MyLoader (urls);

    MyLoader t1 = new MyLoader (urls, parent);
    MyLoader t2 = new MyLoader (urls, parent);

    Class c1, c2;

    try {

      c1 = t1.loadClass ("dummy");
      c2 = t2.loadClass ("dummy");

    }
    catch (Exception e) {
      e.printStackTrace ();
    }
  }
}

