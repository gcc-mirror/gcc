class utilTest {

  public static void main(String[] argv) throws Throwable {
    byte[] b = new byte[] {
      0x01, 0x23, 0x45, 0x67, (byte) 0x89, (byte) 0xab,
      (byte) 0xcd, (byte) 0xef
    };
    String s = "0123456789ABCDEF";
    System.out.println(toString(b));
    System.out.println(s);
    System.out.println(toString(toBytesFromString(s)));
  }

  // The following comes from the GNU Crypto project gnu.crypto.util.Util

  private static final char[] HEX_DIGITS = {
    '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'
  };

  public static byte[] toBytesFromString(String s) {
    int limit = s.length();
    byte[] result = new byte[((limit + 1) / 2)];
    int i = 0, j = 0;
    if ((limit % 2) == 1) {
      result[j++] = (byte) fromDigit(s.charAt(i++));
    }
    while (i < limit) {
      result[j++] = 
	(byte)((fromDigit(s.charAt(i++)) << 4) | fromDigit(s.charAt(i++)));
    }
    return result;
  }

  public static int fromDigit(char c) {
    if (c >= '0' && c <= '9') {
      return c - '0';
    } else if (c >= 'A' && c <= 'F') {
      return c - 'A' + 10;
    } else if (c >= 'a' && c <= 'f') {
      return c - 'a' + 10;
    } else
      throw new IllegalArgumentException("Invalid hexadecimal digit: " + c);
  }

  public static String toString(byte[] ba) {
    return toString(ba, 0, ba.length);
  }

  public static final String toString(byte[] ba, int offset, int length) {
    char[] buf = new char[length * 2];
    for (int i = 0, j = 0, k; i < length; ) {
      k = ba[offset + i++];
      buf[j++] = HEX_DIGITS[(k >>> 4) & 0x0F];
      buf[j++] = HEX_DIGITS[ k        & 0x0F];
    }
    return new String(buf);
  }
}
