class X
{
  public Y getY()
  {
    return new Y(1);
  } 
}

class Y extends X
{
  int i;

  Y(int i)
  {
    this.i = i;
  }
    
  public Y getY()
  {
    return new Y(2);
  } 
}

class A
{
  X x = new Y(-1);
  public X getX() { return x; }
}

public class PR6204 extends A
{
  public Y getY() { return super.getX().getY(); }
  
  public static void main(String[] args)
  {
    System.out.println (new PR6204().getY().i);
  }
}
