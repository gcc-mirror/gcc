// Regression test that overrides a virtual method with a final one.

class override1
{
  public int x1 () { return 3; }
}

public class override extends override1
{
  public final int x1() { return 5; }
  public final int x2() { return 7; }

  public static void main(String[] args)
  {
    override z = new override();
    System.out.println(z.x1());
    System.out.println(z.x2());
  }
}
