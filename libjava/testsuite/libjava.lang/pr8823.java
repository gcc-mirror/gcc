interface I
{
  public void m();
}

abstract class A implements I
{
  // But doesn't define m()
}

public class pr8823 extends A // which means it implements I
{
  public static void main(String[] args)
  {
    // Defining c as either I or C will work.
    A c = new pr8823();
    c.m();
  }

  public void m()
  {
    System.out.println("Hello World!");
  }
}
