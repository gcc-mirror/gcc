// Test JVMTI event notifications

public class events
{
  public static native void do_events_tests ();

  public static void main (String[] args)
  {
    System.out.println ("JVMTI event notification tests");
    do_events_tests ();
  }
}
