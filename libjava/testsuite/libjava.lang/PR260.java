class A
{
  static
  {
    System.out.println ("A initialized");
  }
}

public class PR260
{
  public static void main(String args[])
  {
    Class klass = A.class;
  }
}
