import java.security.*;

class shatest {

    // gnu-crypto/source/gnu/testlet/gnu/crypto/hash/TestOfSha160.java

    public static void main(String[] argv) {
	MessageDigest md=null;
	try {
	    md = MessageDigest.getInstance("SHA-1");
	} catch (Exception e) {
	    e.printStackTrace();
	}
	md.update("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq".getBytes(), 0, 56);
	String exp = "84983E441C3BD26EBAAE4AA1F95129E5E54670F1";
	String result = toString(md.digest());
	System.out.println(exp);
	System.out.println(result);
	if (!exp.equals(result))
	    System.out.println("NOT EQUAL!");

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
