// Class final_int
// Generated on Sat Feb 12 01:27:46 PST 2000

class final_int {

  final int x = 30;

  class foo {
    int bar () {
      return x;
    }
  }
  void bar () {
    int x = this.new foo ().bar ();
    System.out.println (x);
  }
  public static void main (String[] arg)
  {
    System.out.println ("Testing class `final_int'...");
    new final_int().bar ();
  }
}
