import java.net.*;

public class MyLoader extends URLClassLoader
{
  public MyLoader (URL urls[])
  {
    super (urls);
  }
}
