// Class inner1
// Generated on Thu Nov  4 16:35:03 PST 1999
//

class inner1 {
  int i;
  void foo () {
    inner1.z x1 = new z();
    inner1.z.y x = x1.new y();
    x.bar ();
    x.print();
  }
  public static void main (String[] arg)
  {
    System.out.println ("Testing class `inner1'...");
    new inner1 ().foo();
  }
  class z {
    int j;
    void foo () {
      inner1.this.i = 3; 
    }
    class y { 
      int k;
      void bar () {
        inner1.this.i = 3;
        z.this.j = 4;
        y.this.k = 34;
      }
      void print () {
        System.out.println ("i="+i+", j="+j+", k="+k);
      }
    }
  }
}

