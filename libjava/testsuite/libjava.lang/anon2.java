// Class anon2
// Generated on Wed Dec 29 10:07:09 PST 1999
//


class anon2 {

  int count = 34;
  int field;

  anon2 () { System.out.println ("anon2()"); } 

  anon2 (foobar x) { 
    System.out.println ("Yikes!"+x.zoink());
  }

  anon2 foo () {
      class y extends anon2 {
          int count = 3;
          public void setCount (int j) { count = j; }
          public int getCount () { return count+1; }
	  y (int i) { System.out.println ("y(int)"); }
      }
      return new y (3);
  }

  anon2 bar () {
    foobar xyz = new foobar ();
    return new anon2 (xyz) { 
      int count = 5;
      public void setCount (int j) { field = 3; count = j; }
      public int getCount () { return count+1; }
    } ;
  }

  void test () {
   anon2 b = bar ();
   anon2 c = foo ();
  }
  
  public static void main (String[] arg)
  {
    System.out.println ("Testing class `anon2'...");
    new anon2 ().test ();
  }
}

class foobar {
  public String zoink() { return " zoinked"; }
}
