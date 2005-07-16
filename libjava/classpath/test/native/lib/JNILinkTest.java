public class JNILinkTest {
	static {
		System.loadLibrary("jnilinktest");
	}

	public static void main(String args[]) {
		MethodTester m = new MethodTester();
		Data1 d1 = new Data1();
		Data2 d2 = new Data2();
		int NUM_TESTS=4;
		for(int i=0;i<NUM_TESTS;i++) {
			try {
				if(m.test1(d1,d2))
					System.out.println("SUCCEED: test1");
				else
					System.out.println("FAIL: test1");
			} catch(Exception E) {
				System.out.println("FAIL: test1 (exception)");
			}
		}
		for(int i=0;i<NUM_TESTS;i++) {
			try {
				if(m.test2(d1,d2))
					System.out.println("SUCCEED: test2");
				else
					System.out.println("FAIL: test2");
			} catch(Exception E) {
				System.out.println("FAIL: test2");
			}
		}
		for(int i=0;i<NUM_TESTS;i++) {
			try {
				if(m.test3(d1,d2))
					System.out.println("SUCCEED: test3");
				else
					System.out.println("FAIL: test3");
			} catch(Exception E) {
				System.out.println("FAIL: test3");
			}
		}
		for(int i=0;i<NUM_TESTS;i++) {
			try {
				if(m.test4(d1,d2))
					System.out.println("SUCCEED: test4");
				else
					System.out.println("FAIL: test4");
			} catch(Exception E) {
				System.out.println("FAIL: test4");
			}
		}
		for(int i=0;i<NUM_TESTS;i++) {
			try {
				if(m.test5(d1,d2))
					System.out.println("SUCCEED: test5");
				else
					System.out.println("FAIL: test5");
			} catch(Exception E) {
				System.out.println("FAIL: test5");
			}
		}
		for(int i=0;i<NUM_TESTS;i++) {
			try {
				if(m.test6(d1,d2))
					System.out.println("SUCCEED: test6");
				else
					System.out.println("FAIL: test6");
			} catch(Exception E) {
				System.out.println("FAIL: test5");
			}
		}
	}
}

class MethodTester {
	// class test
	native boolean test1(Data1 d1, Data2 d2);
	// field test
	native boolean test2(Data1 d1, Data2 d2);
	// static field test
	native boolean test3(Data1 d1, Data2 d2);
	// method test
	native boolean test4(Data1 d1, Data2 d2);
	// static method test
	native boolean test5(Data1 d1, Data2 d2);
	// final method test
	native boolean test6(Data1 d1, Data2 d2);
}

class Data1 {
	static boolean staticVar = true;
	private boolean instanceVar = true;
	static boolean staticMethod() { return true; }
	boolean instanceMethod() { return true; }
	boolean finalMethod() { return true; }
}

class Data2 extends Data1 {
	static boolean staticVar = false;
	private boolean instanceVar = false;
	static boolean staticMethod() { return false; }
	boolean instanceMethod() { return false; }
	boolean finalMethod() { return false; }
}
