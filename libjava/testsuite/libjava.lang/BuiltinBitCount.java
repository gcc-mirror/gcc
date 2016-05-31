class BuiltinBitCount
{
  public static int popcount(int x)
  {
    return Integer.bitCount(x);
  }

  public static int popcountl(long x)
  {
    return Long.bitCount(x);
  }

  public static void main(String[] args)
  {
    if (Integer.bitCount(0) != 0)
      throw new Error();
    if (Integer.bitCount(8) != 1)
      throw new Error();
    if (Integer.bitCount(123456) != 6)
      throw new Error();
    if (Integer.bitCount(-1) != 32)
      throw new Error();
    
    if (Long.bitCount(0) != 0)
      throw new Error();
    if (Long.bitCount(8) != 1)
      throw new Error();
    if (Long.bitCount(123456) != 6)
      throw new Error();
    if (Long.bitCount(-1) != 64)
      throw new Error();

    if (popcount(0) != 0)
      throw new Error();
    if (popcount(8) != 1)
      throw new Error();
    if (popcount(123456) != 6)
      throw new Error();
    if (popcount(-1) != 32)
      throw new Error();

    if (popcountl(0) != 0)
      throw new Error();
    if (popcountl(8) != 1)
      throw new Error();
    if (popcountl(123456) != 6)
      throw new Error();
    if (popcountl(-1) != 64)
      throw new Error();
  }
}
