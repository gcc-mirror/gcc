import java.net.*;
import java.lang.reflect.*;

public class TestLeak
{
  class MyLoader extends URLClassLoader
  {
    public MyLoader (URL urls[])
    {
      super (urls);
    }
  }

  public static void main (String[] args)
  {
    URLClassLoader ucl = 
      (URLClassLoader) ClassLoader.getSystemClassLoader();
    URL urls[] = ucl.getURLs ();
    Class ifaces[] = new Class[1];
    ifaces[0] = java.lang.Comparable.class;

    try {
      for (int i = 0; i < 100; i++)
	{
	  Proxy.getProxyClass (new MyLoader (urls), ifaces);
	} 
    } catch (Exception e) {
      e.printStackTrace ();
    }
  }
}


