interface I {
    void f(Object x);
}

class PR19921 {
    static void g(I i) {
      // gcj used to create invalid bytecode for this.
      i.f(new Object[1][1]);
    }
  public static void main(String[] args) { }
}
