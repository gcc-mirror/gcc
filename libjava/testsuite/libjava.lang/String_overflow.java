class String_overflow
{
  static void getChars()
  {
    String source = "abcdefg";
    char[] dest = new char [3];

    try
      {
	source.getChars (0, 5,	// Source
			 dest, (1<<31) - 1);
	Fail ("getChars", "Should not have succeeded");
      }
    catch (Throwable e)
      {
	ExpectArrayIndex ("getChars", e);
      }
  }

    /* How do I stop a compiler warning causing a test to fail?
  static void getBytes()
  {
    String source = "abcdefg";
    byte[] dest = new byte[3];

    try
      {
	source.getBytes (0, 5, dest, (1<<31) - 1);
	Fail ("getBytes", "Should not have succeeded");
      }
    catch (Throwable e)
      {
	ExpectArrayIndex ("getBytes", e);
      }
  }
    */

  static void regionMatches()
  {
    if ("abcdefg".regionMatches (4, "abcdefg", 4, -1))
      {
	Fail ("regionMatches", "Should not return true");
      }

    try
      {
	if ("abcdefg".regionMatches (4, "abcdefg", 4, (1<<31)-1))
	  {
	    Fail ("regionMatches (2nd)", "Should not return true");
	  }
      }
    catch (Throwable e)
      {
	Fail ("regionMatches (2nd)", e);
      }
  }

  static void regionMatchesCase()
  {
    if ("abcdefg".regionMatches (true, 4, "abcdefg", 4, -1))
      {
	Fail ("regionMatchesCase", "Should not return true");
      }

    try
      {
	if ("abcdefg".regionMatches (true, 4, "abcdefg", 4, (1<<31)-1))
	  {
	    Fail ("regionMatchesCase (2nd)", "Should not return true");
	  }
      }
    catch (Throwable e)
      {
	Fail ("regionMatchesCase (2nd)", e);
      }
  }

  static void startsWith()
  {
    // We make the arg pretty big to try and cause a segfault.
    String s = new String ("abcdef");
    StringBuffer b = new StringBuffer (1000000);
    b.setLength (1000000);
    String arg = new String (b);

    try
      {
	s.startsWith (arg, (1<<31) - 1000000);
      }
    catch (Throwable e)
      {
	Fail ("startsWith", e);
      }
  }

  static void valueOf()
  {
    char[] array = new char[] {'a', 'b', 'c', 'd', 'e'};
    try
      {
	String.valueOf (array, 4, (1<<31)-1);
	Fail ("valueOf", "should not succeed");
      }
    catch (Throwable e)
      {
	ExpectArrayIndex ("valueOf", e);
      }
  }

  public static void main (String[] args) throws Throwable
  {
    getChars();
    //    getBytes();
    regionMatches();
    regionMatchesCase();
    startsWith();
    valueOf();

    if (tests_failed == 0)
      System.out.println ("ok");
  }

  static void ExpectArrayIndex (String test, Throwable e)
  {
    if (e instanceof ArrayIndexOutOfBoundsException)
      return;

    Fail (test, e);
  }

  static void Fail (String test, Object problem)
  {
    ++tests_failed;
    System.err.print (test);
    System.err.print ('\t');
    System.err.println (problem);
  }

  static int tests_failed;
}
