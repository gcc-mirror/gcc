// Class t
// Generated on Thu Nov  4 16:35:03 PST 1999
//

class tmi {
  int i;
  void foo () {
    tmi.z x1 = new z();
    tmi.z.y x = x1.new y();
    x.bar ();
    x.print();
    tmi.this.i = 666;
    x.print();
    tmi.this.print();
  }
  void print () {
    System.out.println ("tmi.print()");
  }
  public static void main (String[] arg)
  {
    System.out.println ("Testing class `tmi'...");
    new tmi ().foo();
  }
  class z {
    int j;
    void foo () {
      tmi.this.i = 3; 
    }
    class y { 
      int k;
      void bar () {
        tmi.this.i = 3;
        tmi.this.print ();
        z.this.j = 4;
        y.this.k = 34;
      }
      void print () {
        System.out.println ("i="+i+", j="+j+", k="+k);
      }
    }
  }
}

