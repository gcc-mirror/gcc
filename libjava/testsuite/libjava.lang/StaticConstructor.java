// Test to make sure static initializers are called

class bar
{
  public static int zog;
  public static int zag;

  static
  {
    zog = 12;
    zag = 2;
  }

  public bar() { } 
}

public class StaticConstructor
{
  static int foo ()
  {
    return new bar().zog;
  }

  public static void main(String args[])
  {
    System.out.println ("" + (foo() + bar.zag));
  }
}

