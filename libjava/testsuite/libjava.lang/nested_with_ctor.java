// Class nested_with_ctor
// Generated on Mon Jan 31 18:31:47 PST 2000
// The nested class contains explicit constructors. Their argument
// lists should be augmented with the alias initializer values when
// the ctors are seen declared (as opposed to generated.)

class nested_with_ctor {
  
  void fct(final String s, final int i)
  {
      class nested {
          String buffer = s+i;
          String getString () { return buffer; }
          nested (int i) { buffer = "(int)"+i; }
          nested () {}
      }
      nested x = new nested ();
      System.out.println (x.getString ());
      nested y = new nested (123);
      System.out.println (y.getString ());
  }
  public static void main (String[] arg)
  {
    System.out.println ("Testing class `nested_with_ctor'...");
    new nested_with_ctor ().fct ("Yikes!", 321);
  }
}
