// Class private_direct_write
// Generated on Tue Nov 16 15:05:54 UTC 1999
//

class private_direct_write {

  private int foo;

  class private_direct_write_inner {
    void test () {
      foo = 670;
    }
  }
  void foo ()
  {
    private_direct_write_inner inn = this.new private_direct_write_inner ();
    inn.test ();
    System.out.println ("foo="+foo);
  }  
  public static void main (String[] arg)
  {
    System.out.println ("Testing class `private_direct_write'...");
    new private_direct_write().foo ();
  }
}
