// Class indirect_read
// Generated on Mon Nov 15 17:20:40 UTC 1999
//

class indirect_read {

  int foo;

  class indirect_read_inner {
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
    foo = 670;
    indirect_read_inner inn = this.new indirect_read_inner ();
    indirect_read_inner.other o = inn.new other ();
    o.testx ();
  }  
  public static void main (String[] arg)
  {
    System.out.println ("Testing class `indirect_read'...");
    new indirect_read().foo ();
  }
}
