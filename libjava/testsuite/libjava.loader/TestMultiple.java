import java.net.*;

public class TestMultiple
{
  public static void main (String[] args)
  {
    URLClassLoader ucl = 
      (URLClassLoader) ClassLoader.getSystemClassLoader();
    URL urls[] = ucl.getURLs ();

    MyLoader t1 = new MyLoader (urls);
    MyLoader t2 = new MyLoader (urls);

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


