// gcj used to generate incorrect bytecode for
// staticMethod().staticMethod()
public class pr16789
{
  public void foo()
  {
    System.out.println(Thread.currentThread().holdsLock(this));
  }

  public static void main(String[] args)
  {
    new pr16789().foo();
  }
}

