// Class inner2
// Generated on Mon Dec  6 14:32:34 PST 1999
//

class inner2 {
  int foo = 1999;
  void foo () 
  {
    inner2.this.foo = 666;
    System.out.println (inner2.this.foo);
  }
  void print () {System.out.println (foo);}
  public static void main (String[] arg)
  {
    System.out.println ("Testing class `inner2'...");
    new inner2().foo ();
  }
}
