import java.util.*;

public class MethodFailure4 {

  public static String call(A obj) {
    return "A";
  }
  public static String call(I obj) {
    return "I";
  }

  interface I {}
  static class A {}
  static class B extends A implements I {}
  static class C extends B {}


  public static A getA() {
    return new A();
  }

  public static B getB() {
    return new B();
  }

  public static C getC() {
    return new C();
  }

  public static I getI() {
    return new C();
  }
  
  // this method invocation is ambiguous
  
  public static void main(String[] argv) {
    call( getC() );
  }

}
