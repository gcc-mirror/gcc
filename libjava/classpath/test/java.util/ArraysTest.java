//////
// There should be a copyright thing here but I'm in too much of a hurry to add
// one right now. I don't care much what the copyright is though so if someone
// wants to put the appropriate one here, go right ahead (I think GPL probably
// unless that causes a problem with running on proprietory JVMs or testing
// proprietory class libraries or anything.
//////

import java.util.*;

/**
 * Test of most of the methods in the Arrays class. The class prints out a
 * single pass/fail message, and enumerates all failures. The message is in the
 * PASS: or FAIL: format that dejagnu uses, but other than that I don't know
 * enough to make it a "proper" testsuite.
 */
public class ArraysTest {

  static int passed = 0;
  static int failed = 0;

  public static void main(String[] args) {
    testBool();
    testByte();
    testChar();
    testShort();
    testInt();
    testLong();
    testFloat();
    testDouble();
    testObject();
    if (failed != 0) {
      System.out.println(" (" + failed + " fails and " + passed + " passes).");
    } else {
      System.out.println("PASSED: [Arrays] All " + passed + " tests.");
    }
  }

  static void testBool() {
    boolean[] a1 = new boolean[] {true, false, false, true, true, false, true};
    boolean[] a2 = new boolean[] {false, false, false, true, true, true, true};
    boolean[] a3 = new boolean[] {true, false, false, true, true, false, true};
    passfail("boolean equals", !Arrays.equals(a1, a2) && Arrays.equals(a1, a3));
    Arrays.fill(a1, false);
    boolean filled = true;
    for (int i = 0; filled && i < a1.length; i++) {
      filled = a1[i] == false;
    }
    passfail("boolean fill", filled);
  }

  static void testByte() {
    byte[] a1 = new byte[] {3, -2, 0, 1, 4, 0, -5};
    byte[] a2 = new byte[] {-5, -2, 0, 0, 1, 3, 4};
    boolean firstEq = Arrays.equals(a1, a2);
    Arrays.sort(a1);
    boolean sorted = true;
    for (int i = 0; sorted && i < a1.length - 1; i++) {
      sorted = !(a1[i] > a1[i+1]); // the odd way of writing <= is so that we
                                   // aren't tooo mean to NaNs
    }
    passfail("byte sort", sorted);
    passfail("byte search", Arrays.binarySearch(a2, (byte)1) == 4 &&
                            Arrays.binarySearch(a2, (byte)-1) == -3);
    passfail("byte equals", !firstEq && Arrays.equals(a1, a2));
    Arrays.fill(a1, (byte)6);
    boolean filled = true;
    for (int i = 0; filled && i < a1.length; i++) {
      filled = a1[i] == (byte)6;
    }
    passfail("byte fill", filled);
  }

  static void testChar() {
    char[] a1 = new char[] {'i', 'd', 'f', 'g', 'j', 'f', 'a'};
    char[] a2 = new char[] {'a', 'd', 'f', 'f', 'g', 'i', 'j'};
    boolean firstEq = Arrays.equals(a1, a2);
    Arrays.sort(a1);
    boolean sorted = true;
    for (int i = 0; sorted && i < a1.length - 1; i++) {
      sorted = !(a1[i] > a1[i+1]); // the odd way of writing <= is so that we
                                   // aren't tooo mean to NaNs
    }
    passfail("char sort", sorted);
    passfail("char search", Arrays.binarySearch(a2, 'i') == 5 &&
                            Arrays.binarySearch(a2, 'e') == -3);
    passfail("char equals", !firstEq && Arrays.equals(a1, a2));
    Arrays.fill(a1, 'Q');
    boolean filled = true;
    for (int i = 0; filled && i < a1.length; i++) {
      filled = a1[i] == 'Q';
    }
    passfail("char fill", filled);
  }

  static void testShort() {
    short[] a1 = new short[] {3, -2, 0, 1, 4, 0, -5};
    short[] a2 = new short[] {-5, -2, 0, 0, 1, 3, 4};
    boolean firstEq = Arrays.equals(a1, a2);
    Arrays.sort(a1);
    boolean sorted = true;
    for (int i = 0; sorted && i < a1.length - 1; i++) {
      sorted = !(a1[i] > a1[i+1]); // the odd way of writing <= is so that we
                                   // aren't tooo mean to NaNs
    }
    passfail("short sort", sorted);
    passfail("short search", Arrays.binarySearch(a2, (short)1) == 4 &&
                             Arrays.binarySearch(a2, (short)-1) == -3);
    passfail("short equals", !firstEq && Arrays.equals(a1, a2));
    Arrays.fill(a1, (short)6);
    boolean filled = true;
    for (int i = 0; filled && i < a1.length; i++) {
      filled = a1[i] == (short)6;
    }
    passfail("short fill", filled);
  }

  static void testInt() {
    int[] a1 = new int[] {3, -2, 0, 1, 4, 0, -5};
    int[] a2 = new int[] {-5, -2, 0, 0, 1, 3, 4};
    boolean firstEq = Arrays.equals(a1, a2);
    Arrays.sort(a1);
    boolean sorted = true;
    for (int i = 0; sorted && i < a1.length - 1; i++) {
      sorted = !(a1[i] > a1[i+1]); // the odd way of writing <= is so that we
                                   // aren't tooo mean to NaNs
    }
    passfail("int sort", sorted);
    passfail("int search", Arrays.binarySearch(a2, 1) == 4 &&
                           Arrays.binarySearch(a2, -1) == -3);
    passfail("int equals", !firstEq && Arrays.equals(a1, a2));
    Arrays.fill(a1, 6);
    boolean filled = true;
    for (int i = 0; filled && i < a1.length; i++) {
      filled = a1[i] == 6;
    }
    passfail("int fill", filled);
  }

  static void testLong() {
    long[] a1 = new long[] {3, -2, 0, 1, 4, 0, -5};
    long[] a2 = new long[] {-5, -2, 0, 0, 1, 3, 4};
    boolean firstEq = Arrays.equals(a1, a2);
    Arrays.sort(a1);
    boolean sorted = true;
    for (int i = 0; sorted && i < a1.length - 1; i++) {
      sorted = !(a1[i] > a1[i+1]); // the odd way of writing <= is so that we
                                   // aren't tooo mean to NaNs
    }
    passfail("long sort", sorted);
    passfail("long search", Arrays.binarySearch(a2, 1L) == 4 &&
                            Arrays.binarySearch(a2, -1L) == -3);
    passfail("long equals", !firstEq && Arrays.equals(a1, a2));
    Arrays.fill(a1, 6L);
    boolean filled = true;
    for (int i = 0; filled && i < a1.length; i++) {
      filled = a1[i] == 6L;
    }
    passfail("long fill", filled);
  }

  static void testFloat() {
    float[] a1 = new float[] {-4.0f, 75.3f, Float.POSITIVE_INFINITY, -0.0f,
                              -3324.342f, 0.0f, 3.14f, 2.5f, 1.0f, 1.0f};
    float[] a2 = new float[] {-3324.342f, -4.0f, -0.0f, 0.0f, 1.0f, 1.0f, 2.5f,
                              3.14f, 75.3f, Float.POSITIVE_INFINITY};
    boolean firstEq = Arrays.equals(a1, a2);
    Arrays.sort(a1);
    boolean sorted = true;
    for (int i = 0; sorted && i < a1.length - 1; i++) {
      sorted = !(a1[i] > a1[i+1]); // the odd way of writing <= is so that we
                                   // aren't tooo mean to NaNs
    }
    passfail("float sort", sorted);
    passfail("float search", Arrays.binarySearch(a2, 3.14f) == 7 &&
                            Arrays.binarySearch(a2, -1.0f) == -3);
    passfail("float equals", !firstEq && Arrays.equals(a1, a2));
    Arrays.fill(a1, 27.0f);
    boolean filled = true;
    for (int i = 0; filled && i < a1.length; i++) {
      filled = a1[i] == 27.0f;
    }
    passfail("float fill", filled);
  }

  static void testDouble() {
    double[] a1 = new double[] {-4.0d, 75.3d, Double.POSITIVE_INFINITY, -0.0d,
                                -3324.342d, 0.0d, 3.14d, 2.5d, 1.0d, 1.0d};
    double[] a2 = new double[] {-3324.342d, -4.0d, -0.0d, 0.0d, 1.0d, 1.0d,
                                2.5d, 3.14d, 75.3d, Double.POSITIVE_INFINITY};
    boolean firstEq = Arrays.equals(a1, a2);
    Arrays.sort(a1);
    boolean sorted = true;
    for (int i = 0; sorted && i < a1.length - 1; i++) {
      sorted = !(a1[i] > a1[i+1]); // the odd way of writing <= is so that we
                                   // aren't tooo mean to NaNs
    }
    passfail("double sort", sorted);
    passfail("double search", Arrays.binarySearch(a2, 3.14d) == 7 &&
                            Arrays.binarySearch(a2, -1.0d) == -3);
    passfail("double equals", !firstEq && Arrays.equals(a1, a2));
    Arrays.fill(a1, 27.0d);
    boolean filled = true;
    for (int i = 0; filled && i < a1.length; i++) {
      filled = a1[i] == 27.0d;
    }
    passfail("double fill", filled);
  }

  static void testObject() {
    String[] a1 = new String[] {"this", "is", "a", "string", "test", "which",
                                "will", "hopefully", "demonstrate", "that",
                                "sorting", "works"};
    String[] a2 = new String[] {"a", "demonstrate", "hopefully", "is",
                                "sorting", "string", "test", "that", "this",
                                "which", "will", "works"};
    String[] a3 = new String[] {"this", "is", "a", "reverse", "string", "test",
                                "which", "will", "hopefully", "demonstrate",
                                "that", "sorting", "works", "with",
                                "comparators"};
    String[] a4 = new String[] {"works", "with", "will", "which", "this",
                                "that", "test", "string", "sorting", "reverse",
                                "is", "hopefully", "demonstrate", "comparators",
                                "a"};
    final String list = "[works, with, will, which, this, that, test, string," +
                        " sorting, reverse, is, hopefully, demonstrate," +
                        " comparators, a]";
    boolean firstEq = Arrays.equals(a1, a2);
    Arrays.sort(a1);
    boolean sorted = true;
    for (int i = 0; sorted && i < a1.length - 1; i++) {
      sorted = a1[i].compareTo(a1[i+1]) <= 0;
    }
    passfail("object sort", sorted);
    passfail("object search", Arrays.binarySearch(a2, "hopefully") == 2 &&
                            Arrays.binarySearch(a2, "strange") == -6);
    passfail("object equals", !firstEq && Arrays.equals(a1, a2));
    Arrays.fill(a1, "blah");
    boolean filled = true;
    for (int i = 0; filled && i < a1.length; i++) {
      filled = a1[i].equals("blah");
    }
    passfail("object fill", filled);
    Comparator c = new ReverseOrder();
    Arrays.sort(a3, c);
    passfail("comparator sort", Arrays.equals(a3, a4));
    passfail("comparator search", Arrays.binarySearch(a4, "sorting", c) == 8 &&
                                  Arrays.binarySearch(a4, "nice", c) == -11);

    // toList doesn't exist -gcb
//    passfail("toList toString", Arrays.toList(a4).toString().equals(list));
  }

  static void passfail(String desc, boolean didpass) {
    if (didpass) {
      passed++;
    } else {
      if (failed++ != 0) {
        System.out.print(", " + desc);
      } else {
        System.out.print("FAILED: [Arrays] " + desc);
      }
    }
  }
}

class ReverseOrder implements Comparator {
  public int compare(Object a, Object b) {
    return -((Comparable)a).compareTo(b);
  }
}
