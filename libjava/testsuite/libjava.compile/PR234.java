// PR234.java
public class PR234 extends B implements I
{
  public static void main(String args[])
  {
    PR234 x = new PR234();
    x.m();
  }
}

// B.java
class B
{
  public void m()
  {
    System.out.println ("yes");
  }
}

// I.java
interface I
{
  public void m();
}
