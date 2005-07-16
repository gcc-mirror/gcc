import java.lang.reflect.Array;

public class ArrayTest {
	public static void main(String[] args) {
		System.loadLibrary("javalangreflect");

		Object[] objArray = new Object[9];
		boolean[] boolArray = new boolean[9];
		double[] doubleArray = new double[9];
		byte[] byteArray = new byte[9];
		char[] charArray = new char[9];

		try {
			Boolean[][] blahArray = (Boolean[][])Array.newInstance(java.lang.Boolean.class,new int[]{9,1});
			System.out.print(blahArray != null ? "PASSED" : "FAILED");
		} catch(Exception E) {
			System.out.print("FAILED");
			E.printStackTrace();
		}
		System.out.println(": newInstance(Class,int[])");

		try {
			boolean[] blahArray = (boolean[])Array.newInstance(java.lang.Boolean.TYPE, 9);
			System.out.print(blahArray != null ? "PASSED" : "FAILED");
		} catch(Exception E) {
			System.out.print("FAILED");
			E.printStackTrace();
		}
		System.out.println(": newInstance(<primitive Class>,int)");
	
		try {
			objArray = (Object[])Array.newInstance(java.lang.Object.class, 9);
			System.out.print(objArray != null ? "PASSED" : "FAILED");
		} catch(Exception E) {
			System.out.print("FAILED");
		}
		System.out.println(": newInstance(Class,int)");

		try {
			Boolean obj = new Boolean(true);
			Array.set(objArray,0,obj);
			System.out.print(objArray[0] == obj ? "PASSED" : "FAILED");
		} catch(Exception E) {
			System.out.print("FAILED");
		}
		System.out.println(": set()");

		try {
			Array.setBoolean(boolArray,1,true);
			System.out.print(boolArray[1] == true ? "PASSED" : "FAILED");
		} catch(Exception E) {
			System.out.print("FAILED");
		}
		System.out.println(": setBoolean()");

		try {
			Array.setByte(byteArray,2,(byte)2);
			System.out.print(byteArray[2] == 2 ? "PASSED" : "FAILED");
		} catch(Exception E) {
			System.out.print("FAILED");
		}
		System.out.println(": setByte()");

		try {
			Array.setShort(doubleArray,3,(short)3);
			System.out.print(doubleArray[3] == 3 ? "PASSED" : "FAILED");
		} catch(Exception E) {
			System.out.print("FAILED");
		}
		System.out.println(": setShort()");

		try {
			Array.setChar(charArray,4,(char)4);
			System.out.print(charArray[4] == 4 ? "PASSED" : "FAILED");
		} catch(Exception E) {
			System.out.print("FAILED");
		}
		System.out.println(": setChar()");

		try {
			Array.setInt(doubleArray,5,5);
			System.out.print(doubleArray[5] == 5 ? "PASSED" : "FAILED");
		} catch(Exception E) {
			System.out.print("FAILED");
		}
		System.out.println(": setInt()");

		try {
			Array.setLong(doubleArray,6,6);
			System.out.print(doubleArray[6] == 6 ? "PASSED" : "FAILED");
		} catch(Exception E) {
			System.out.print("FAILED");
		}
		System.out.println(": setLong()");

		try {
			Array.setFloat(doubleArray,7,7);
			System.out.print(doubleArray[7] == 7 ? "PASSED" : "FAILED");
		} catch(Exception E) {
			System.out.print("FAILED");
		}
		System.out.println(": setFloat()");

		try {
			Array.setDouble(doubleArray,8,8);
			System.out.print(doubleArray[8] == 8 ? "PASSED" : "FAILED");
		} catch(Exception E) {
			System.out.print("FAILED");
		}
		System.out.println(": setDouble()");

		try {
			Boolean obj = (Boolean)Array.get(objArray,0);
			System.out.print(obj != null ? "PASSED" : "FAILED");
		} catch(Exception E) {
			System.out.print("FAILED");
		}
		System.out.println(": get()");

		try {
			boolArray[1] = true;
			System.out.print(Array.getBoolean(boolArray,1) == true ? "PASSED" : "FAILED");
		} catch(Exception E) {
			System.out.print("FAILED");
		}
		System.out.println(": getBoolean()");

		try {
			byteArray[2] = (byte)2;
			System.out.print(Array.getByte(byteArray,2) == 2 ? "PASSED" : "FAILED");
		} catch(Exception E) {
			System.out.print("FAILED");
		}
		System.out.println(": getByte()");

		try {
			byteArray[3] = (byte)3;
			System.out.print(Array.getShort(byteArray,3) == 3 ? "PASSED" : "FAILED");
		} catch(Exception E) {
			System.out.print("FAILED");
		}
		System.out.println(": getShort()");

		try {
			charArray[4] = (char)4;
			System.out.print(Array.getChar(charArray,4) == 4 ? "PASSED" : "FAILED");
		} catch(Exception E) {
			System.out.print("FAILED");
		}
		System.out.println(": getChar()");

		try {
			byteArray[5] = (byte)5;
			System.out.print(Array.getInt(byteArray,5) == 5 ? "PASSED" : "FAILED");
		} catch(Exception E) {
			System.out.print("FAILED");
		}
		System.out.println(": getInt()");

		try {
			byteArray[6] = (byte)6;
			System.out.print(Array.getLong(byteArray,6) == 6 ? "PASSED" : "FAILED");
		} catch(Exception E) {
			System.out.print("FAILED");
		}
		System.out.println(": getLong()");

		try {
			byteArray[7] = (byte)7;
			System.out.print(Array.getFloat(byteArray,7) == 7 ? "PASSED" : "FAILED");
		} catch(Exception E) {
			System.out.print("FAILED");
		}
		System.out.println(": getFloat()");

		try {
			doubleArray[8] = 8;
			System.out.print(Array.getDouble(doubleArray,8) == 8 ? "PASSED" : "FAILED");
		} catch(Exception E) {
			System.out.print("FAILED");
		}
		System.out.println(": getDouble()");
	}
}
