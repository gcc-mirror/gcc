import java.lang.reflect.*;
public class pr8415
{
  public static void meth () throws NullPointerException
  {
    throw new NullPointerException();
  }

  public static void main(String[] args) throws Throwable
  {
    Class k = pr8415.class;
    Method m = k.getMethod ("meth", new Class[0]);
    System.out.println(m);
  }
}
