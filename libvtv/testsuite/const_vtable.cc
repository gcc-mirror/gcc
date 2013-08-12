extern "C" int printf(const char *,...); 
struct V1 {
  int v; 
  virtual int foo(); 
  V1(); 
  ~V1(); 
}; 
struct V2 : virtual V1 {
  int v2; 
  virtual int foo(); 
  V2(); 
  ~V2(); 
}; 
struct C : virtual V1, virtual V2 {
  int c; 
  virtual int foo(); 
  C(); 
  ~C(); 
}; 

struct B {
  int b; }; 
struct D : B, C {
  int d; 
  virtual int bar(); 
  D(); 
  ~D(); 
}; 
extern "C" int printf(const char *,...); 
main() 
{
  try {
    D *d = new D; 
    delete d; 
  } catch (int) {
    printf("Int caught\n"); 
  } 
} 

int V1::foo() {
  printf("V1::foo called\n"); return 1; } 
V1::V1() : v(5) {
  printf("V1 called\n"); } 
V1::~V1() {
  printf("~V1 called\n"); } 

int V2::foo() {
  printf("V2::foo called\n"); return 1; } 
V2::V2() : v2(6) {
  printf("V2 called\n"); } 
V2::~V2() {
  printf("~V2 called\n"); } 

int C::foo() {
  printf("C::foo called %d\n", c); return 1; } 
C::C() : c(7) {
  printf("C called\n"); 
  V1 *vv = this; vv->foo(); 
  C *cp = dynamic_cast<C *>(vv); 
  if (this == cp) {
    printf("PASSED this == cp\n"); 
  } else {
    printf("FAILED this != cp\n"); 
  } 
} 
C::~C() {
  printf("~C called\n"); 
  V1 *vv = this; vv->foo(); 
  C *cp = dynamic_cast<C *>(vv); 
  if (this == cp) {
    printf("PASSED this == cp\n"); 
  } else {
    printf("FAILED this != cp\n"); 
  } 
} 

int D::bar() {
  printf("D::bar called\n"); return 1; } 
D::D() : d(8) {
  printf("D called\n"); throw 5; } 
D::~D() {
  printf("~D called\n"); } 
