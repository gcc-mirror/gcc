// instance.java -- test the `instanceof' operator.

import java.util.EventListener;

public class instance implements EventListener
{
  public static void main (String[] args)
  {
    Object x1 = new instance ();
    EventListener x2 = new instance ();
    IllegalArgumentException iae
      = new IllegalArgumentException ("any random class");
    String x3 = "zardoz";
    Object x4 = "zardoz";

    // Test simple object stuff
    System.out.println (x1 instanceof Object);
    System.out.println (x1 instanceof IllegalArgumentException);
    System.out.println (x1 instanceof EventListener);
    System.out.println (x1 instanceof String);
    System.out.println ("=");

    // Test with value which is an interface.
    System.out.println (x2 instanceof Object);
    System.out.println (x2 instanceof IllegalArgumentException);
    System.out.println (x2 instanceof EventListener);
    System.out.println ("=");

    // Test with value which is a final class.
    System.out.println (x3 instanceof Object);
    System.out.println (x3 instanceof String);
    System.out.println ("=");

    // Test with value which is a random class.
    System.out.println (iae instanceof Object);
    System.out.println (iae instanceof IllegalArgumentException);
    System.out.println (iae instanceof EventListener);
    System.out.println ("=");

    // Test with value which is a final class, but not known
    // statically.
    System.out.println (x4 instanceof Object);
    System.out.println (x4 instanceof IllegalArgumentException);
    System.out.println (x4 instanceof EventListener);
    System.out.println (x4 instanceof String);
    System.out.println (x4 instanceof int[]);
  }
}
