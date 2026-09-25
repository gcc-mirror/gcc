/* { dg-do link { target float16 } } */
/* { dg-add-options float16 } */

/* PR lto/127589  */

/* Both NVPTX and AMDGCN define HF - and support it in principle (nvptx: sm_XX > 53)
   However, both have currently disabled it at runtime (target hook)
   [For Nvpts, -mexperimental can actually be used to enable it.]

   Hence: Check for the following - otherwise, it would ICE.  */

/* { dg-error "bit-precision floating-point numbers unsupported .mode '.F'." "" { target { offload_target_nvptx || offload_target_amdgcn } } 0 }  */
/* { dg-excess-errors "Follow-up errors from mkoffload and lto-wrapper" { target { offload_target_nvptx || offload_target_amdgcn } } }  */

int f(_Float16 x) {
  return x > 0.0;
}

int main() {
  int r;
  _Float16 y = (_Float16)0.0;
  #pragma omp target map(from: r), map(to: y)
    r = f(y);
  return r;
}
