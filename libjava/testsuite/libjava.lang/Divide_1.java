public class Divide_1
{
  static int b = Integer.parseInt ("-1");  
  int b1 = Integer.parseInt ("-1");  
  static int zero = Integer.parseInt ("0");

  void probe ()
  {
     try {
      int a = Integer.parseInt ("-80000000", 16);
      int c = a/b;
      System.out.println (c);
    } catch (Exception _) {
      System.out.println (_);
    }
  
    try {
      int a = Integer.parseInt ("-80000000", 16);
      int c = a/-1;
      System.out.println (c);
    } catch (Exception _) {
      System.out.println (_);
    }
 
    try {
      int a = Integer.parseInt ("-80000000", 16);
      int c = a%b;
      System.out.println (c);
    } catch (Exception _) {
      System.out.println (_);
    }
 
    try {
      int a = Integer.parseInt ("-80000000", 16);
      int c = a%b1;
      System.out.println (c);
    } catch (Exception _) {
      System.out.println (_);
    }
 
    try {
      int a = Integer.parseInt ("-80000000", 16);
      int c = a%-1;
      System.out.println (c);
    } catch (Exception _) {
      System.out.println (_);
    }
 
    try {
      int a = Integer.parseInt ("8000", 16);
      int b = Integer.parseInt ("0", 16);
      int c = a/b;
      System.out.println (c);
    } catch (Exception _) {
      System.out.println (_);
    }
 
    try {
      int a = Integer.parseInt ("8000", 16);
      int b = Integer.parseInt ("0", 16);
      int c = a%b;
      System.out.println (c);
    } catch (Exception _) {
      System.out.println (_);
    }
 
   try {
      long a = Long.parseLong ("-8000000000000000", 16);
      long c = a/b;
      System.out.println (c);
    } catch (Exception _) {
      System.out.println (_);
    }
 
    try {
      long a = Long.parseLong ("-8000000000000000", 16);
      long c = a%b;
      System.out.println (c);
    } catch (Exception _) {
      System.out.println (_);
    }

    try {
      long a = Long.parseLong ("8000", 16);
      long b = Long.parseLong ("0", 16);
      long c = a/b;
      System.out.println (c);
    } catch (Exception _) {
      System.out.println (_);
    }
 
    try {
      long a = Long.parseLong ("8000", 16);
      long b = Long.parseLong ("0", 16);
      long c = a%b;
      System.out.println (c);
    } catch (Exception _) {
      System.out.println (_);
    }
  }
 
  public static void main (String[] args) {
    new Divide_1 ().probe ();
  }
}
