class A
{
}

public class Scope3
{
  public static void main(String args[])
  {
    new Scope3();
  }

  public Scope3()
  {
    A a = new A();
  }

  class A
  {
  }
}
