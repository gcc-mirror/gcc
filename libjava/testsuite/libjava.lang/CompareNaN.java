/*
 * JLS 4.2.3 specifies that (x op y) must be false if either x or y
 * is NaN and op is one of <, >, <=, >=, or ==.
 *
 * Some targets may need specific options wired into libgcj.spec
 * to pass this test.  For example, alpha-linux requires -mieee
 * to prevent an unrecoverable fp trap.
 */

public class CompareNaN {
  public static void main(String[] args) {
    double x = Double.NaN;
    System.out.println(x == x);
  }
}
