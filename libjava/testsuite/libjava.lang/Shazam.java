import java.io.*;

public class Shazam {

  private static String shazam ()
    {
	try {
	    return "shazam";
	} finally {
	    System.out.println ("The next line should say \"shazam\"");
	}
    }

  public static void main (String[] args) 
    {
	System.out.println (shazam ());
    }
}
