public class PR12374 {

  /* We weren't coping with field refs on a string constant...  */

  Object Foo()
  {
    return "".CASE_INSENSITIVE_ORDER;
  }

  /* Special casing access to array.length while analysing syntax is
     evil.  Especially when it means we can't cope with a type
     called length.  */

  class length
  {
    static final int i = 2;
  }

  int bar()
  {
    return length.i;
  }

  public static void main (String[] argv)
  {
  }
}
