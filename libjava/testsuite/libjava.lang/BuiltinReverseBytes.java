class BuiltinReverseBytes
{
  public static short bswap16(short x)
  {
    return Short.reverseBytes(x);
  }

  public static int bswap32(int x)
  {
    return Integer.reverseBytes(x);
  }

  public static long bswap64(long x)
  {
    return Long.reverseBytes(x);
  }

  public static void main(String[] args)
  {
    if (Short.reverseBytes((short)0) != (short)0)
      throw new Error();
    if (Short.reverseBytes((short)0x1234) != (short)0x3412)
      throw new Error();
    if (Short.reverseBytes((short)-1) != (short)-1)
      throw new Error();
     
    if (Integer.reverseBytes(0) != 0)
      throw new Error();
    if (Integer.reverseBytes(0x12345678) != 0x78563412)
      throw new Error();
    if (Integer.reverseBytes(-1) != -1)
      throw new Error();

    if (Long.reverseBytes(0L) != 0L)
      throw new Error();
    if (Long.reverseBytes(0x123456789abcde0fL) != 0x0fdebc9a78563412L)
      throw new Error();
    if (Long.reverseBytes(-1L) != -1L)
      throw new Error();

    if (bswap16((short)0) != (short)0)
      throw new Error();
    if (bswap16((short)0x1234) != (short)0x3412)
      throw new Error();
    if (bswap16((short)-1) != (short)-1)
      throw new Error();
     
    if (bswap32(0) != 0)
      throw new Error();
    if (bswap32(0x12345678) != 0x78563412)
      throw new Error();
    if (bswap32(-1) != -1)
      throw new Error();

    if (bswap64(0L) != 0L)
      throw new Error();
    if (bswap64(0x123456789abcde0fL) != 0x0fdebc9a78563412L)
      throw new Error();
    if (bswap64(-1L) != -1L)
      throw new Error();
  }
}
