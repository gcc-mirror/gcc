// Finalize.java - Test code for finalizers.

import java.io.*;
import java.util.*;
import java.lang.Runtime;

public final class Finalize_1
{
  public static void main (String[] args)
  {
    Finalize_1 f;
    Runtime t;

    t = Runtime.getRuntime ();

    for (int i = 0; i < 3; ++i)
      f = new Finalize_1 ();
    f = null;

    t.gc ();
    t.runFinalization ();
  }

  public void finalize () throws Throwable
  {
    System.out.print ("Finalize ");
  }
}
