public class pr8955
{
  static final int val = Integer.MIN_VALUE;
  void foo()
  {
    switch(1) {
    case val:
      break;
    case 1:
      break;
    }
  }
}
