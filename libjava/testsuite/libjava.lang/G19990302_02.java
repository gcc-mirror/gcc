public class G19990302_02 {
  public static void main(String args[]) {
    try {
      Object[] arrayObj = new String[3];
      String[] arrayStr = new String[3];
      System.out.println ("Pass 1");
      arrayObj[0] = arrayStr; // String object <-- String array object
      System.out.println ("Pass 2");
    } catch (ArrayStoreException e) {
      System.out.println ("ArrayStoreException");
    }
    System.out.println ("Pass 3");
  }
}

