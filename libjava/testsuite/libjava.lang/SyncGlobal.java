// Test suitability of alignment of statically-allocated Objects.
class SyncGlobal
{
  private static final byte[] global_1 = { 1 };
  private static final byte[] global_2 = { 2, 3 };
  private static final byte[] global_3 = { 4, 5, 6 };
  private static final byte[] global_4 = { 7, 8, 9, 10 };
  private static final byte[] global_5 = { 11, 12, 13, 14, 15 };
  private static final byte[] global_6 = { 16, 17, 18, 19, 20, 21 };
  private static final byte[] global_7 = { 22, 23, 24, 25, 26, 27, 28 };
  private static final byte[] global_8 = { 29, 30, 31, 32, 33, 34, 35, 36 };

  public static void main (String args[])
  {
    synchronized (global_1) { System.out.println ("PASS1"); }
    synchronized (global_2) { System.out.println ("PASS2"); }
    synchronized (global_3) { System.out.println ("PASS3"); }
    synchronized (global_4) { System.out.println ("PASS4"); }
    synchronized (global_5) { System.out.println ("PASS5"); }
    synchronized (global_6) { System.out.println ("PASS6"); }
    synchronized (global_7) { System.out.println ("PASS7"); }
    synchronized (global_8) { System.out.println ("PASS8"); }
  }
}
