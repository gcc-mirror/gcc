// gcj had a problem with "SomeClass.field++" when gimplifying.

class helper
{
  static int value;
}

public class pr17329
{
  static void doit ()
  {
    helper.value += 2;
  }
}
