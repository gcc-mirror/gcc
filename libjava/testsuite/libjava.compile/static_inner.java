// File Parent.java
class Parent {
  public static class Kid {
    public Kid(int age) {
      this.age = age;
    }

    int age;
  }
}

// File NewParent.java
public class static_inner extends Parent {

  public static void main(String[] argv) {
    Kid kid = new Kid(2);
  }

}
