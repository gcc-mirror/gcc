// Class private_indirect_read
// Generated on Tue Nov 16 15:34:56 UTC 1999
//

class private_indirect_read {

  private int foo;

  class private_indirect_read_inner {
    void test () {
    }
    class other {
      void testx () {
        int x = foo;
        System.out.println ("x="+x);
      }
    }

  }
  void foo ()
  {
    foo=670;
    private_indirect_read_inner inn = this.new private_indirect_read_inner ();
    private_indirect_read_inner.other o = inn.new other ();
    o.testx();
  }  
  public static void main (String[] arg)
  {
    System.out.println ("Testing class `private_indirect_read'...");
    new private_indirect_read().foo ();
  }
}
