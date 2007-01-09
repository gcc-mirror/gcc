// Test various reflection methods.

import java.lang.annotation.Inherited;
import java.lang.reflect.Method;
import java.lang.reflect.Field;

public class PR29495
{
  public class container<T>
  {
    // This class has a synthetic field...

    public T get(T v) { return v; }
  }

  public class concrete extends container<String>
  {
    // This makes us have a synthetic bridge method.
    public String get(String v) { return "hi" + v; }
  }

  // varargs method
  public static void va(Object... args)
  {
  }

  public static void check(boolean x, String m)
  {
    if (! x)
      System.out.println("fail: " + m);
  }

  public static void main(String[] args) throws Throwable
  {
    check (Inherited.class.isAnnotation(), "Inherited isAnnotation");

    Method m = PR29495.class.getDeclaredMethod("va", new Class[] { Object[].class });
    check (m.isVarArgs(), "va isVarArgs");

    m = concrete.class.getDeclaredMethod("get", new Class[] { Object.class });
    check (m.isSynthetic(), "get isSynthetic");
    check (m.isBridge(), "get isBridge");

    Field[] fs = container.class.getDeclaredFields();
    boolean ok = false;
    for (int i = 0; i < fs.length; ++i)
      {
	if (fs[i].isSynthetic())
	  {
	    ok = true;
	    break;
	  }
      }
    check (ok, "container has synthetic field");
  }
}
