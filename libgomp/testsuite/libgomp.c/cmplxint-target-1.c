/* { dg-do link } */

/* PR lto/127589  */

/* Both NVPTX and AMDGCN define HF - and support it in principle (nvptx: sm_XX > 53)
   However, both have currently disabled it at runtime (target hook)
   [For Nvpts, -mexperimental can actually be used to enable it.]

   Hence: Check for the following - otherwise, it would ICE.  */

int f(_Complex int x) {
  return __real__ x > 0;
}

int main() {
  int r;
  _Complex int y = (_Complex int)0;
  #pragma omp target map(from: r), map(to: y)
    r = f(y);
  return r;
}
