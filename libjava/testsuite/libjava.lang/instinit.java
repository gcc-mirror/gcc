// Class instinit
// Generated on Wed Feb  2 12:31:16 PST 2000
// Simple instance initializer test case.

class instinit extends foo {

    String buffer = "No Oink! Oink!";

    /* Instance initializer */
    {
        System.out.println ("Oinking...");
    }
    {
        buffer = "Oink! Oink!";
    }
  
    public static void main (String[] arg)
    {
        System.out.println ("Testing class `instinit'...");
        System.out.println (new instinit ().buffer);
    }
}
