// Class anon
// Generated on Wed Dec 29 10:07:09 PST 1999
//


interface itf {int count = 0;void setCount (int i);int getCount ();}

class anon {

  int count = 34;

  class x implements itf {
      int count = 3;
      public void setCount (int j) { }
      public int getCount () { return count*3; }
  }


  itf foo () {
      class y implements itf {
	  int count = 3;
	  public void setCount (int j) { count = j; }
	  public int getCount () { return count+1; }
      }
      return new y ();
  }

  itf bar () {
    return new itf () { 
      // The class defined right here will implement `itf'
      int count = 5;
      public void setCount (int j) { count = j; }
      public int getCount () { return count+1; }
    } ;
  }

  void test () {
    itf a = foo ();
    itf b = bar ();
    x   c = this.new x ();
    System.out.println (a.getCount ());
    System.out.println (b.getCount ());
    System.out.println (c.getCount ());
    System.out.println (this.count);
  }
  
  public static void main (String[] arg)
  {
    System.out.println ("Testing class `anon'...");
    new anon ().test ();
  }
}
