// This test case was built for java/3096.

class PR3096
{
  static void foo (int x[], int i) {
    ++x[i];
  }
  static void foo (float x[], int i) {
    ++x[i];
  }
  public static void main(String [] args) {
      int a[] = new int [1];
      float f[] = new float [1];
      int b[];
      int i = 0;
      foo (a,0);
      foo (f,0);
      System.out.println (a[0]);
      System.out.println (f[0]);
      System.out.println ((b=a)[0]);
      (b=a)[i]=99;
      b[0]++;
      System.out.println (a[0]+", "+b[0]);
      System.out.println (++a[i]);
      System.out.println (a[i]);
      System.out.println (a[i]++);
      System.out.println (a[i]);
      String s[] = new String [1];
      String y[];
      s[0]="";
      s[0] += "Peace ";
      System.out.println (s[0]);
      (y=s)[0] += "now!";
      System.out.println (s[0]+", "+y[0]);
  }
}
