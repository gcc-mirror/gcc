// Class direct_read
// Generated on Sat Nov 13 23:26:34 UTC 1999
//

class direct_read {

  int foo;

  class direct_read_inner {
    void test () {
      int x = foo;
      System.out.println ("x="+x);
    }
  }

  void foo ()
  {
    foo = 670;
    direct_read_inner inn = this.new direct_read_inner ();
    inn.test ();
  }  
  public static void main (String[] arg)
  {
    System.out.println ("Testing class `direct_read'...");
    new direct_read().foo ();
  }
}
