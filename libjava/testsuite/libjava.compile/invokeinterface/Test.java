public class Test implements B
{
  public static void main(String args[])
  {
    Test t = new Test();
    B a = (B) t;
    a.a();
  }

  public void a()
  {
    System.out.println("A");
  }

  public void b()
  {
    System.out.println("B");
  }  
}
