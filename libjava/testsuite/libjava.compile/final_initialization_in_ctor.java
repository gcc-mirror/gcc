// This test case was insipred by
// http://gcc.gnu.org/ml/java/2001-09/msg00181.html

class M {
    int size () { return 3; }
}

class final_initialization_in_ctor {

 final float loadFactor;

  public final_initialization_in_ctor(M m)
  {
      this(Math.max(m.size() * 2, 30), (float)40.0);
  }

  public final_initialization_in_ctor(int initialCapacity, float loadFactor)
  {
      this.loadFactor = loadFactor;
  }
}
