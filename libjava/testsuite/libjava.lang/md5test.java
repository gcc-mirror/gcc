import java.security.*;

class md5test {


    // gnu-crypto/source/gnu/testlet/gnu/crypto/hash/TestOfMD5.java

    public static void main(String[] argv) {
	String[] strings = {
	    "a",
	    "abc",
	    "message digest",
	    "abcdefghijklmnopqrstuvwxyz",
	    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
	    "12345678901234567890123456789012345678901234567890123456789012345678901234567890"
	};

	String[] expected = {
	    "0CC175B9C0F1B6A831C399E269772661",
	    "900150983CD24FB0D6963F7D28E17F72",
	    "F96B697D7CB7938D525A2F31AAF161D0",
	    "C3FCD3D76192E4007DFB496CCA67E13B",
	    "D174AB98D277D9F5A5611C2C9F419D9F",
	    "57EDF4A22BE3C955AC49DA2E2107B67A"
	};

	for (int i = 0; i < strings.length; i++)
	    testString(strings[i], expected[i]);

    }

    public static void testString(String string, String expected) {

	MessageDigest md=null;
	try {
	    md = MessageDigest.getInstance("MD5");
	    md.update(string.getBytes(), 0, string.length());
	    String result = toString(md.digest());
	    System.out.println(expected);
	    System.out.println(result);
	    if (!expected.equals(result))
		System.out.println("NOT EQUAL!");
	} catch (Exception x) {
	    x.printStackTrace();
	}
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

    private static final char[] HEX_DIGITS = "0123456789ABCDEF".toCharArray();

}
