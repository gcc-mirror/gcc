// Class update_outer
// Generated on Thu Nov 18 21:37:21 UTC 1999
//

class update_outer {

  private String foo;

  class update_outer_inner {
    void test () {
      foo += " M$";
      System.out.println ("foo=`"+foo+"'");
    }


  }
  void foo ()
  {
    foo = "780";
    update_outer_inner inn = this.new update_outer_inner ();
    inn.test ();
  }  
  public static void main (String[] arg)
  {
    System.out.println ("Testing class `update_outer'...");
    new update_outer().foo ();
  }
}
