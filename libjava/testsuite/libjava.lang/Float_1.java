/* 

Date: 25 Aug 1998 16:04:00 -0000
From: Andrew Haley <aph@pasanda.cygnus.co.uk>
To: java-project@cygnus.com
Subject: Help: vtable problem?

My little program:

-----------------------------------------------------------------------
import java.lang.*;

  public class widget
  {
    public static void main (String argv[])
    {
      int test = Float.floatToIntBits((float)2.0);
      String s = Integer.toHexString(test);
      
      System.out.print (s+"\n");
    }

  }
-----------------------------------------------------------------------
prints out

40000000

with Sun's interpreter, but prints out

true

when compiled with gcj; PrintStream dispatches a string arg as a
boolean rather than as a String.  I've tried to rebuild everything.

?

Thanks,
Andrew.

*/

public class Float_1
{
  public static void main (String argv[])
    {
      int test = Float.floatToIntBits((float)2.0);
      String s = Integer.toHexString(test);
      
      System.out.print (s+"\n");
    }
}
