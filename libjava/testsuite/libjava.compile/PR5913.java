class PR5913
{
  public static void main(String[] args)
  {
    boolean test1 = ("" + 1) instanceof String;
    // This also tests literal parsing, as mentioned in PR 5902.
    boolean test2 = "" + 0x1instanceof String;
    System.exit((test1 && test2) ? 0 : 1);
  }
}
