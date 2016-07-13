import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

public class ReturnInvocationHandler implements InvocationHandler
{
  private Object obj;
  public ReturnInvocationHandler(Object obj)
  {
    this.obj = obj;
  }
  public Object invoke(Object proxy, Method m, Object[] args) throws Throwable
  {
    Object result;
    try
    {
      result = m.invoke(obj, args);
    }
    catch (Exception e)
    {
      throw e;
    }
    return result;
  }
}
