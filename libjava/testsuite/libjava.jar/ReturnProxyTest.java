import java.lang.reflect.Proxy;

public class ReturnProxyTest
{
  public static void main(String[] args)
  {
    ReturnTypes orig = new ReturnTypesImpl();
    Object o = Proxy.newProxyInstance(orig.getClass().getClassLoader(),
				      new Class<?>[] { ReturnTypes.class },
				      new ReturnInvocationHandler(orig));
    ReturnTypes rt = (ReturnTypes)o;

    System.out.println(orig.getBoolean());
    System.out.println(orig.getChar());
    System.out.println(orig.getByte());
    System.out.println(orig.getShort());
    System.out.println(orig.getInt());
    System.out.println(orig.getLong());

    System.out.println(rt.getBoolean());
    System.out.println(rt.getChar());
    System.out.println(rt.getByte());
    System.out.println(rt.getShort());
    System.out.println(rt.getInt());
    System.out.println(rt.getLong());
  }
}
