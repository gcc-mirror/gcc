public class bytearray
{
  public static void main (String[] argv) throws Throwable {
    Class c = Class.forName ("[Ljava.lang.String;");
    c = Class.forName ("[B");
    System.out.println (c);
    c = ClassLoader.getSystemClassLoader().loadClass ("[[Ljava.lang.String;");
    System.out.println (c);
  }
}
