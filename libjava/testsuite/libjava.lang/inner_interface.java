// Class inner_interface
// Generated on Fri Feb 11 16:29:04 PST 2000
// Simple inner interface example.

class inner_interface {
  interface bar {
      static final int field = 14022000;
      public void inner_interface ();
  }

  class baz implements inner_interface.bar {
    public void inner_interface ()
    {
      System.out.println ("This is baz.inner_interface "+field);
    }
  }

  void x () {
      this.new baz ().inner_interface ();
  }

  public static void main (String[] arg)
  {
    System.out.println ("Testing class `inner_interface'...");
    new inner_interface ().x();
  }
}
