// Class inner3
// Generated on Tue Dec  7 11:37:43 PST 1999
//

class inner3 {
  int bar;
  public static void main (String[] arg)
  {
    System.out.println ("Testing class `inner3'...");
    new inner3().bar ();
  }
  void bar () {
    t xx = this.new t ();
    xx.bar ();
  }
  void foo () { bar = 3; }
  class t {
    void bar () {
      inner3.this.foo ();
      System.out.println (inner3.this.bar);
    }
  }
}
