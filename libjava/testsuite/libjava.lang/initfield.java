// gcj generated buggy code when we reference a field of a
// non-constant member that we inherit from an interface.

interface iface
{
  final value x = new value();
}

final class value
{
  Object field = "maude";
}

public class initfield implements iface
{
  public static void main(String[] args)
  {
    System.out.println(x.field);
  }
}
