// Class final_inner
// Generated on Tue Jan 18 13:35:19 PST 2000
//

class final_inner {

  void foo (final String s, final int i) {
      class bar {
          void printI () { System.out.println (s+i); }
      }
      new bar ().printI ();
  }

  public static void main (String[] arg)
  {
    System.out.println ("Testing class `final_inner'...");
    new final_inner ().foo ("The Number ", 666);
  }
}
