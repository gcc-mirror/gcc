// Class private_indirect_write
// Generated on Tue Nov 16 15:44:49 UTC 1999
//

class private_indirect_write {

  private int foo;

  class private_indirect_write_inner {
    void test () {
    }

    class other {
      void test () {
        foo = 670;
      }
    }

  }
  void foo ()
  {
    private_indirect_write_inner inn = this.new private_indirect_write_inner ();
    private_indirect_write_inner.other x = inn.new other ();
    x.test ();
    System.out.println ("foo="+foo);
  }  
  public static void main (String[] arg)
  {
    System.out.println ("Testing class `private_indirect_write'...");
    new private_indirect_write().foo ();
  }
}
