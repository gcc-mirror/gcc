/* { dg-do link { target dfp } } */

/* PR lto/127589  */

/* { dg-error "32-bit-precision decimal floating-point numbers unsupported .mode 'SD'." "" { target { offload_target_nvptx || offload_target_amdgcn } } 0 }  */
/* { dg-excess-errors "Follow-up errors from mkoffload and lto-wrapper" { target { offload_target_nvptx || offload_target_amdgcn } } }  */

int f(_Decimal32 x) {
  return x > (_Decimal32)0.0;
}

int main() {
  int r;
  _Decimal32 y = (_Decimal32)0.0;
  #pragma omp target map(from: r), map(to: y)
    r = f(y);
  return r;
}
