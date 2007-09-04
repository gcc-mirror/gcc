class Foo
{
  Class bar()
  {
    return gnu.classpath.VMStackWalker.getCallingClass();
  }
}

public class WalkerTest
{
  public static void main(String[] argv)
  {
    System.out.println(new Foo().bar());
  }
}
