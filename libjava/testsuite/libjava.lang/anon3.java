// Class anon3
// Generated on Wed Dec 29 10:07:09 PST 1999
//


class anon3 {

  itf bar () {
    return new itf () { 
      int count = 5;
      public void setCount (int j) { count = 3; }
      public int getCount () { return count; }
    } ;
  }

  void test () {
   itf x = bar ();
   System.out.println (x.getCount ());
  }
  
  public static void main (String[] arg)
  {
    System.out.println ("Testing class `anon3'...");
    new anon3 ().test ();
  }
}

interface itf { void setCount (int j); int getCount(); int count = 0; }
