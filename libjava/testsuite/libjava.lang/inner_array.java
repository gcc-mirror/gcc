// Class inner_array
// Generated on Fri Nov 19 13:19:47 PST 1999
//

class inner_array {

  private int[] foo;

  class array_inner {
    void test () {
      int x = foo[2];
      System.out.println ("x="+x);
      foo [1] = 34;
      foo [1]++;
    }
  }
  void foo ()
  {
    foo = new int [3];
    foo[2]=670;
    array_inner inn = this.new array_inner ();
    inn.test ();
    System.out.println ("foo[1]="+foo[1]);
  }  
  public static void main (String[] arg)
  {
    System.out.println ("Testing class `inner_array'...");
    new inner_array().foo ();
  }
}
