public class PR20056 {
  int x;
  PR20056(int x) {}
  PR20056(PR20056 f) {
    // The verifier rejected the generated code in this case.
    this(f.x = 0);
  }

  public static void main(String[] args)
  {
    System.out.println("maude");
  }
}
