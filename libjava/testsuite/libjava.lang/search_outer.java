// Class search_outer.java
// Generated on Thu Nov 18 18:40:43 UTC 1999
//

class search_outer {

  private int foo;

  class search_outer_inner {
    void test () {
      foo++;
      System.out.println ("foo="+foo);
      foo += 3;
      System.out.println ("foo="+foo);
    }
  }
  void foo ()
  {
    foo = 3;
    search_outer_inner inn = this.new search_outer_inner ();
    inn.test ();
  }  
  public static void main (String[] arg)
  {
    System.out.println ("Testing class `search_outer'...");
    new search_outer().foo ();
  }
}
