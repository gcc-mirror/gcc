// Class indirect
// Generated on Tue Nov 16 15:53:14 UTC 1999
// Several indirection to enclosing class

class indirect {

  private int foo;

  class indirect_inner {
    class other {
      class inner {
        void test () {
          int x = foo;
          System.out.println ("x="+foo);
	  foo = 671;
        }
      }
    }

  }
  void foo ()
  {
    foo = 670; 
    indirect_inner inn = this.new indirect_inner ();
    this.new indirect_inner().new other().new inner ().test ();
    System.out.println ("foo="+foo);
  }  
  public static void main (String[] arg)
  {
    System.out.println ("Testing class `indirect'...");
    new indirect().foo ();
  }
}
