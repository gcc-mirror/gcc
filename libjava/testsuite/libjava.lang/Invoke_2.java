public class Invoke_2
{
  static int s;
  
  public static void foo (int a, int b)
  {
    System.out.println(a + " " + b);
  }

  public static void main(String[] args) {
    foo (bar(), s);
  }

  public static int bar() 
  {
    s = 33;
    return 99;
  }
}

