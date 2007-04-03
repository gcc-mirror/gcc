import java.lang.reflect.*;

interface Twas
{
  Object brillig();
}

interface Slithy
{
  void toves(int gyre);
}

public class ProxyTest
{
  static class MyInvocationHandler implements InvocationHandler
  {
    public Object invoke(Object proxy, Method method, Object[] args)
    {
      System.out.println (method.getDeclaringClass());
      System.out.println (args == null
			  ? args
			  : args.getClass().getName());
      return this;
    }
  }

  public static void main(String[] argv)
    throws InstantiationException, IllegalAccessException
  {
    Twas wabe
      = (Twas)Proxy.newProxyInstance(ProxyTest.class.getClassLoader(),
				     new Class[] { Slithy.class, Twas.class },
				     new MyInvocationHandler());
    wabe.brillig();
    ((Slithy)wabe).toves(2);
  }
}
