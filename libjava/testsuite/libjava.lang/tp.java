// Class tp
// Generated on Thu Nov  4 16:35:03 PST 1999
//

class tp {
  private int i;
  void foo () {
    tp.z x1 = new z();
    tp.z.y x = x1.new y();
    x.bar ();
    x.print();
  }
  public static void main (String[] arg)
  {
    System.out.println ("Testing class `tp'...");
    new tp ().foo();
  }
  class z {
    private int j;
    void foo () {
      tp.this.i = 3; 
    }
    class y { 
      private int k;
      void bar () {
        tp.this.i = 3;
        z.this.j = 4;
        y.this.k = 34;
      }
      void print () {
        System.out.println ("i="+i+", j="+j+", k="+k);
      }
    }
  }
}

