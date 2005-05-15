public class pr21519 {
  char[] source;
  public int compute(int pos) {
    for (int i = 0; i < pos; ++i)
      if (!(source[i] == ' ' || source[i] == '\t')) return -1;
    return pos;
  }
}
