public class PR140 {
  public static void fill(int[] a) {
    for (int i = 0; i < a.length; i++) {
      a[i] = i;
    }
  }
  public static void main(String[] args) {
    int[] a = new int[3];
    fill(a);
    a.length = 3000;
    fill(a);
  }
}
