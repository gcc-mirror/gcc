// Class private_direct_read
// Generated on Tue Nov 16 15:04:13 UTC 1999
//

class private_direct_read {

  private int foo;

  class private_direct_read_inner {
    void test () {
      int x = foo;
      System.out.println ("x="+x);
    }
  }
  void foo ()
  {
    foo = 670;
    private_direct_read_inner inn = this.new private_direct_read_inner ();
    inn.test ();
  }  
  public static void main (String[] arg)
  {
    System.out.println ("Testing class `private_direct_read'...");
    new private_direct_read().foo ();
  }
}
