// Copyright (c) 1998, 1999  Red Hat, Inc.

// Written by Tom Tromey <tromey@cygnus.com>

import gnu.testlet.*;

public class DejaGNUTestHarness extends SimpleTestHarness
{
  static String dejasrcdir;

  public String getSourceDirectory ()
  {
    return dejasrcdir;
  }

  private DejaGNUTestHarness ()
  {
    super (/* verbose */ true, /* debug */ false);
  }

  public static void main (String[] args)
  {
    dejasrcdir = args.length > 0 ? args[0] : "";
    DejaGNUTestHarness harness = new DejaGNUTestHarness ();
    // This might seem weird, given that we check args.length above.
    // However, in some cases the expect code rewrites this runtest
    // invocation to have an explicit name for the test to run.
    harness.runtest (args[1]);
    System.exit(harness.done());
  }
}
