class Base
{
  int someNum()
  {
    System.out.println ("ok");
    return 0;
  }
}

public class PR242 extends Base
{
  public static void main(String args[])
  {
    new PR242();
  }
  
  PR242()
  {
    new Inner().a();
  }
     
  class Inner
  {
    public int dummy()
    {
      System.out.println ("wrong method called!!");
      return -1;
    }
  
    public void a()
    {
      System.out.println ("...");
      System.out.println (someNum());    
    }
  }
}
