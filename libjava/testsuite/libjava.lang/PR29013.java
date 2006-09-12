public class PR29013 {
  public static int result() { return 5; }

  public static void computeResult() { result(); }

  public static void main(String[] args) {
    computeResult();
  }
}
