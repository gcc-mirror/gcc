// 15.26: "The type of an assignment expression is the type of the variable"
// (LHS).
class PR13733 {
  String a, c = "";
  Object b;

  void bug() {
    a = (b = c);	// invalid without cast
  }
}
