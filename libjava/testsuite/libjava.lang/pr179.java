// Extended regression test for the PR 179.
//
// This tests the ".class" language syntax, initialization behaviour for 
// Class.isInstance() and Class.isAssignableFrom(), and isAssignableFrom()
// functionality in the event that an interface argument that is not 
// implemented by any loaded class is given.

class A
{
  static 
  {
    System.out.println("A initialized");
  }
}

interface IA {}

class B implements IA
{
  static 
  {
    System.out.println("B initialized");
  }
}

class C
{
  static 
  {
    System.out.println("C initialized");
  }
}

interface IB {}

public class pr179
{
  public static void main(String[] args)
  {
    System.out.println (A.class.isAssignableFrom (Object.class));
    System.out.println (IB.class.isAssignableFrom (B.class));
    System.out.println (IA.class.isAssignableFrom (B.class));
    A a = new A();
    System.out.println (C.class.isInstance (a));
    C c = new C();
    System.out.println (C.class.isInstance (c));
  }
}

/* Expected Output:
A initialized
false
B initialized
false
true
C initialized
false
true
*/
