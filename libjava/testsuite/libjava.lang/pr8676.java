public class pr8676 {
  // The problem here was that this function couldn't be compiled to
  // bytecode.
  private void f(long j) {
    boolean x = (1 << j) != 0;
  }

  public static void main(String[] args)
  {
  }
}
