import java.net.*;

public class MyLoader extends URLClassLoader
{
  public MyLoader (URL urls[])
  {
    super (urls);
  }

  public MyLoader (URL urls[], ClassLoader parent)
  {
    super (urls, parent);
  }
}
