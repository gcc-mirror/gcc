// gcj (20000313) reports "Type `x' not found in the declaration of the
// return type of method `getX'."

public class pr176
{
  class A
  {
    x getX()
    {
      return new x();
    }

    class x {}
  }
}
