public class PR6820
{
  static void m(int a, int b) {
    System.out.println("a="+a+" b="+b);
  }

  static int a = 10;

  public static void main(String[] args) {
    int b = 10;
    m(a,++a);
    m(b,++b);
  }
}
           
