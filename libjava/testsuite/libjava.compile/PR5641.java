interface I
{
  void m();
}
abstract class C implements I {}
class Foo
{
  void Bar(C c)
  {
    c.m();
  }
  void blah(C c)
  {
    c.m();
  }

  public static void main (String[] args)
  {
  }
}
