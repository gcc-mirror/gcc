// Test a `throw' across a libffi call.

import java.lang.reflect.*;

public class invokethrow
{
  public static void doit () throws Throwable
  {
    throw new Throwable ("hi!");
  }

  public static void main (String[] args)
  {
    Class k = invokethrow.class;
    try
      {
	Class[] noargs = new Class[0];
	Method m = k.getMethod ("doit", noargs);
	m.invoke (null, null);
      }
    catch (InvocationTargetException x1)
      {
	System.out.println (x1.getTargetException ().getMessage ());
      }
    catch (Throwable _)
      {
      }
  }
}
