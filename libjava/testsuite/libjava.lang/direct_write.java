// Class direct_write
// Generated on Mon Nov 15 17:10:56 UTC 1999
//

class direct_write {

  int foo;

  class direct_write_inner {
    void test () {
      foo  = 670;
    }
  }

  void foo ()
  {
    foo = 650;
    direct_write_inner inn = this.new direct_write_inner ();
    inn.test ();
    System.out.println ("foo="+foo);
  }  
  public static void main (String[] arg)
  {
    System.out.println ("Testing class `direct_write'...");
    new direct_write().foo ();
  }
}
