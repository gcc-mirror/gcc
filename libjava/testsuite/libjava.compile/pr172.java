// jc1 (2.96 20000313) says "`A_Inner' not found"

public class InnerConstructor
{
  class A_Inner
  {
    A_Inner (A_Inner i) {}
  }
}
