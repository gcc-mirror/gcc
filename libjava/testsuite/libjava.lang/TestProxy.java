import java.lang.reflect.*;
import java.net.*;

public class TestProxy
{
  public class MyInvocationHandler implements InvocationHandler
  {
    public Object invoke (Object proxy,
			  Method method,
			  Object[] args)
      throws Throwable
    {
      System.out.println (args[0]);
      return null;
    }
  }

  public static void main (String[] args)
  {
    try {
      InvocationHandler ih = new MyInvocationHandler();
      
      SocketOptions c = (SocketOptions)
	Proxy.newProxyInstance (SocketOptions.class.getClassLoader(),
				new Class[]{SocketOptions.class},
				ih);
      
      c.getOption (555);

    } catch (Exception e) {
      e.printStackTrace ();
    }
  }
}
