public class PrimlibTest {
	static {
		System.loadLibrary("jnilinktest");
	}

	public static void main(String args[]) {
		Object[] o = new Object[8];
		o[0] = new Boolean(true);
		o[1] = new Byte((byte)1);
		o[2] = new Short((short)2);
		o[3] = new Character((char)3);
		o[4] = new Integer(4);
		o[5] = new Long(5L);
		o[6] = new Float(6F);
		o[7] = new Double(7D);

		String[] s = {"boolean", "byte", "short", "char", "int", "long", "float", "double"};
		for(int i=0;i<8;i++) {
			try {
				System.out.println(PrimlibInterface.unwrapBoolean(o[i]) ? "CONVERTED: UnwrapBoolean(" + s[i] + ")" : "INCORRECT: UnwrapBoolean(" + s[i] + ")");
			} catch(Exception E) {
				System.out.println("EXCEPTION: UnwrapBoolean(" + s[i] + ")");
			}

			try {
				System.out.println(PrimlibInterface.unwrapByte(o[i]) == i ? "CONVERTED: UnwrapByte(" + s[i] + ")" : "INCORRECT: UnwrapByte(" + s[i] + ")");
			} catch(Exception E) {
				System.out.println("EXCEPTION: UnwrapByte(" + s[i] + ")");
			}

			try {
				System.out.println(PrimlibInterface.unwrapShort(o[i]) == i ? "CONVERTED: UnwrapShort(" + s[i] + ")" : "INCORRECT: UnwrapShort(" + s[i] + ")");
			} catch(Exception E) {
				System.out.println("EXCEPTION: UnwrapShort(" + s[i] + ")");
			}

			try {
				System.out.println(PrimlibInterface.unwrapChar(o[i]) == i ? "CONVERTED: UnwrapChar(" + s[i] + ")" : "INCORRECT: UnwrapChar(" + s[i] + ")");
			} catch(Exception E) {
				System.out.println("EXCEPTION: UnwrapChar(" + s[i] + ")");
			}

			try {
				System.out.println(PrimlibInterface.unwrapInt(o[i]) == i ? "CONVERTED: UnwrapInt(" + s[i] + ")" : "INCORRECT: UnwrapInt(" + s[i] + ")");
			} catch(Exception E) {
				System.out.println("EXCEPTION: UnwrapInt(" + s[i] + ")");
			}

			try {
				System.out.println(PrimlibInterface.unwrapLong(o[i]) == i ? "CONVERTED: UnwrapLong(" + s[i] + ")" : "INCORRECT: UnwrapLong(" + s[i] + ")");
			} catch(Exception E) {
				System.out.println("EXCEPTION: UnwrapLong(" + s[i] + ")");
			}

			try {
				System.out.println(PrimlibInterface.unwrapFloat(o[i]) == i ? "CONVERTED: UnwrapFloat(" + s[i] + ")" : "INCORRECT: UnwrapFloat(" + s[i] + ")");
			} catch(Exception E) {
				System.out.println("EXCEPTION: UnwrapFloat(" + s[i] + ")");
			}

			try {
				System.out.println(PrimlibInterface.unwrapDouble(o[i]) == i ? "CONVERTED: UnwrapDouble(" + s[i] + ")" : "INCORRECT: UnwrapDouble(" + s[i] + ")");
			} catch(Exception E) {
				System.out.println("EXCEPTION: UnwrapDouble(" + s[i] + ")");
			}
		}
	}
}

class PrimlibInterface {
	static native boolean unwrapBoolean(Object o);
	static native byte unwrapByte(Object o);
	static native short unwrapShort(Object o);
	static native char unwrapChar(Object o);
	static native int unwrapInt(Object o);
	static native long unwrapLong(Object o);
	static native float unwrapFloat(Object o);
	static native double unwrapDouble(Object o);

	static native Boolean   wrapBoolean(boolean val);
	static native Byte      wrapByte(byte val);
	static native Short     wrapShort(short val);
	static native Character wrapChar(char val);
	static native Integer   wrapInt(int val);
	static native Long      wrapLong(long val);
	static native Float     wrapFloat(float val);
	static native Double    wrapDouble(double val);
}
