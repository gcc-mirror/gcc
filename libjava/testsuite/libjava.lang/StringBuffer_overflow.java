/* This tests some corner cases of arithmetic in StringBuffer.  */

/* These tests can all be run on a 32 bit machine with modest amounts
 * of memory.  */

/* The symptom of the problem is that ArrayIndexOutOfBoundsException
 * gets thrown, while the documentation says that
 * StringIndexOutOfBoundsException should be thrown.  */

class StringBuffer_overflow
{
  /* Test correct exception on getChars.  */
  static void getChars()
  {
    StringBuffer b = new StringBuffer ("x");
    char[] s = new char [1];
    try
      {
	// The substring we are attempting to obtain is invalid,
	// so we should get a StringIndexOutOfBoundsException.
	b.getChars (1, -1 << 31, s, 0);
	Fail ("getChars", "no exception");
      }
    catch (Throwable e)
      {
	ExpectStringIndex ("getChars()", e);
      }
  }

  /* Test correct exception on append with bogus count. */
  static void append()
  {
    StringBuffer s = new StringBuffer("a");
    try
      {
	s.append ("".toCharArray(), 1, (1<<31)-1);
	Fail ("append", "no exception");
      }
    catch (Throwable e)
      {
	ExpectStringIndex ("append", e);
      }
  }

  // Check that append still more or less works.
  static void appendbasic()
  {
    StringBuffer s = new StringBuffer();

    try
      {
	if (!new StringBuffer().append ("abcdefg".toCharArray())
	    .toString().equals ("abcdefg"))
	  {
	    Fail ("appendbasic", "append gives incorrect result");
	  }
      }
    catch (Throwable e)
      {
	Fail ("appendbasic", e);
      }
  }

  /* Test correct expception on substring with bogus indexes.  */
  static void substring()
  {
    StringBuffer s = new StringBuffer ("abc");
    try
      {
	// end - begin == -2 - ((1<<31)-1) == (1<<31) - 1 > 0.  */
	s.substring ((1<<31)-1, -2);
	Fail ("substring", "no exception");
      }
    catch (Throwable e)
      {
	ExpectStringIndex ("substring", e);
      }
  }

  static void insert()
  {
    StringBuffer s = new StringBuffer ("");
    try
      {
	s.insert (0, "abcd".toCharArray(), (1<<31)-1, 1);
	Fail ("insert", "no exception");
      }
    catch (Throwable e)
      {
	ExpectStringIndex ("insert", e);
      }
  }


  public static void main (String[] unused)
  {
    getChars();
    append();
    appendbasic();
    substring();
    insert();

    if (tests_failed == 0)
      {
	System.out.println ("ok");
      }
  }

  static int tests_failed = 0;

  static void ExpectStringIndex (String name, Throwable exception)
  {
    if (! (exception instanceof StringIndexOutOfBoundsException))
      {
	Fail (name, exception);
      }
  }
  static void Fail (String name, Object why)
  {
    ++tests_failed;

    System.err.print (name);
    System.err.print ('\t');
    System.err.println (why);
  }
}
