public final class PR56 {
  public static void main(String[] args) {
    Object o = args;
    int[] a;
    if (!(o instanceof int[]) || (a = (int[])o).length != 2) {
    }
  }
}
