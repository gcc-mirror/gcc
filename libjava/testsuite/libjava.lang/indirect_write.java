// Class indirect_write
// Generated on Tue Nov 16 15:01:24 UTC 1999
//

class indirect_write {

  int foo;

  class indirect_write_inner {
    void test () {
    }

    class other {
      void testx () {
        foo = 670;
      }
    }

  }
  void foo ()
  {
    indirect_write_inner inn = this.new indirect_write_inner ();
    indirect_write_inner.other x = inn.new other ();
    x.testx();
    System.out.println ("foo="+foo);
  }  
  public static void main (String[] arg)
  {
    System.out.println ("Testing class `indirect_write'...");
    new indirect_write().foo ();
  }
}
