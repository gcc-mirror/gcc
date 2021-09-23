// { dg-do run }

extern
#ifdef __cplusplus
"C"
#endif
void abort (void);
float x = 6.0f;

template <int N>
void
bar ()
{
  float v;
  int r;
  #pragma omp atomic compare
  x = x > 8.0f ? 8.0f : x;
  #pragma omp atomic read
  v = x;
  if (v != 6.0f)
    abort ();
  #pragma omp atomic compare
  x = x > 4.0f ? 4.0f : x;
  #pragma omp atomic read
  v = x;
  if (v != 4.0f)
    abort ();
  #pragma omp atomic compare capture
  v = x = x < 8.0f ? 8.0f : x;
  if (v != 8.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 8)
    abort ();
  #pragma omp atomic capture compare
  { v = x; x = x < 12.0f ? 12.0f : x; }
  if (v != 8.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 12.0f)
    abort ();
  #pragma omp atomic capture compare
  { v = x; x = x < 4.0f ? 4.0f : x; }
  if (v != 12.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 12.0f)
    abort ();
  #pragma omp atomic compare
  x = x == 12.0 ? 16.0L : x;
  #pragma omp atomic read
  v = x;
  if (v != 16.0)
    abort ();
  r = 57;
  #pragma omp atomic compare capture
  v = x = x == 15.0f ? r + 7.0f : x;
  if (v != 16.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 16.0f)
    abort ();
  #pragma omp atomic capture, update, compare seq_cst fail(acquire)
  { v = x; x = x == 73.0L - r ? 12.0f : x; }
  if (v != 16.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 12.0f)
    abort ();
  #pragma omp atomic update, compare, capture
  { x = x == 69.0 - r ? 6.0f : x; v = x; }
  if (v != 6.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 6.0f)
    abort ();
  #pragma omp atomic compare
  if (x > 8.0f) { x = 8.0f; }
  #pragma omp atomic read
  v = x;
  if (v != 6.0f)
    abort ();
  #pragma omp atomic compare
  if (x > 4.0) { x = 4.0; }
  #pragma omp atomic read
  v = x;
  if (v != 4.0f)
    abort ();
  #pragma omp atomic compare capture
  { if (x < 8.0f) { x = 8.0f; } v = x; }
  if (v != 8.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 8.0f)
    abort ();
  #pragma omp atomic capture compare
  { v = x; if (x < 12.0f) { x = 12.0f; } }
  if (v != 8.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 12.0f)
    abort ();
  #pragma omp atomic capture compare
  { v = x; if (x < 4.0L) { x = 4.0L; } }
  if (v != 12.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 12.0f)
    abort ();
  #pragma omp atomic compare
  if (x == 12.0f) { x = 16.0L; }
  #pragma omp atomic read
  v = x;
  if (v != 16.0f)
    abort ();
  r = 57.0;
  #pragma omp atomic compare capture
  { if (x == 15.0f) { x = r + 7.0f; } v = x; }
  if (v != 16.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 16.0f)
    abort ();
  #pragma omp atomic capture, update, compare seq_cst fail(acquire)
  { v = x; if (x == 73.0L - r) { x = 12.0L; } }
  if (v != 16.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 12.0f)
    abort ();
  #pragma omp atomic update, compare, capture
  { if (x == 69.0L - r) { x = 6.0; } v = x; }
  if (v != 6.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 6.0f)
    abort ();
  v = 24;
  #pragma omp atomic compare capture
  if (x == 12.0f) { x = 16.0f; } else { v = x; }
  if (v != 6.0f)
    abort ();
  v = 32.0f;
  #pragma omp atomic read
  v = x;
  if (v != 6.0f)
    abort ();
  v = 147.0f;
  #pragma omp atomic capture compare
  if (x == 6.0f) { x = 57.0f; } else { v = x; }
  if (v != 147.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 57.0f)
    abort ();
  #pragma omp atomic update, capture, compare, weak, seq_cst, fail (relaxed)
  { r = x == 137.0f; if (r) { x = 174.0f; } }
  if (r)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 57.0f)
    abort ();
  #pragma omp atomic compare capture fail (relaxed)
  { r = x == 57.0f; if (r) { x = 6.0f; } }
  if (r != 1)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 6.0f)
    abort ();
  v = -5.0f;
  #pragma omp atomic capture compare
  { r = x == 17.0L; if (r) { x = 25.0; } else { v = x; } }
  if (r || v != 6.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 6.0f)
    abort ();
  v = 15.0f;
  #pragma omp atomic capture compare
  { r = x == 6.0f; if (r) { x = 23.0f; } else { v = x; } }
  if (r != 1 || v != 15.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 23.0f)
    abort ();
}

template <typename T, typename U>
void
baz (T &x)
{
  T v;
  U r;
  #pragma omp atomic compare
  x = x > 8.0f ? 8.0f : x;
  #pragma omp atomic read
  v = x;
  if (v != 6.0f)
    abort ();
  #pragma omp atomic compare
  x = x > 4.0f ? 4.0f : x;
  #pragma omp atomic read
  v = x;
  if (v != 4.0f)
    abort ();
  #pragma omp atomic compare capture
  v = x = x < 8.0f ? 8.0f : x;
  if (v != 8.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 8)
    abort ();
  #pragma omp atomic capture compare
  { v = x; x = x < 12.0f ? 12.0f : x; }
  if (v != 8.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 12.0f)
    abort ();
  #pragma omp atomic capture compare
  { v = x; x = x < 4.0f ? 4.0f : x; }
  if (v != 12.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 12.0f)
    abort ();
  #pragma omp atomic compare
  x = x == 12.0 ? 16.0L : x;
  #pragma omp atomic read
  v = x;
  if (v != 16.0)
    abort ();
  r = 57;
  #pragma omp atomic compare capture
  v = x = x == 15.0f ? r + 7.0f : x;
  if (v != 16.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 16.0f)
    abort ();
  #pragma omp atomic capture, update, compare seq_cst fail(acquire)
  { v = x; x = x == 73.0L - r ? 12.0f : x; }
  if (v != 16.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 12.0f)
    abort ();
  #pragma omp atomic update, compare, capture
  { x = x == 69.0 - r ? 6.0f : x; v = x; }
  if (v != 6.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 6.0f)
    abort ();
  #pragma omp atomic compare
  if (x > 8.0f) { x = 8.0f; }
  #pragma omp atomic read
  v = x;
  if (v != 6.0f)
    abort ();
  #pragma omp atomic compare
  if (x > 4.0) { x = 4.0; }
  #pragma omp atomic read
  v = x;
  if (v != 4.0f)
    abort ();
  #pragma omp atomic compare capture
  { if (x < 8.0f) { x = 8.0f; } v = x; }
  if (v != 8.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 8.0f)
    abort ();
  #pragma omp atomic capture compare
  { v = x; if (x < 12.0f) { x = 12.0f; } }
  if (v != 8.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 12.0f)
    abort ();
  #pragma omp atomic capture compare
  { v = x; if (x < 4.0L) { x = 4.0L; } }
  if (v != 12.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 12.0f)
    abort ();
  #pragma omp atomic compare
  if (x == 12.0f) { x = 16.0L; }
  #pragma omp atomic read
  v = x;
  if (v != 16.0f)
    abort ();
  r = 57.0;
  #pragma omp atomic compare capture
  { if (x == 15.0f) { x = r + 7.0f; } v = x; }
  if (v != 16.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 16.0f)
    abort ();
  #pragma omp atomic capture, update, compare seq_cst fail(acquire)
  { v = x; if (x == 73.0L - r) { x = 12.0L; } }
  if (v != 16.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 12.0f)
    abort ();
  #pragma omp atomic update, compare, capture
  { if (x == 69.0L - r) { x = 6.0; } v = x; }
  if (v != 6.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 6.0f)
    abort ();
  v = 24;
  #pragma omp atomic compare capture
  if (x == 12.0f) { x = 16.0f; } else { v = x; }
  if (v != 6.0f)
    abort ();
  v = 32.0f;
  #pragma omp atomic read
  v = x;
  if (v != 6.0f)
    abort ();
  v = 147.0f;
  #pragma omp atomic capture compare
  if (x == 6.0f) { x = 57.0f; } else { v = x; }
  if (v != 147.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 57.0f)
    abort ();
  #pragma omp atomic update, capture, compare, weak, seq_cst, fail (relaxed)
  { r = x == 137.0f; if (r) { x = 174.0f; } }
  if (r)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 57.0f)
    abort ();
  #pragma omp atomic compare capture fail (relaxed)
  { r = x == 57.0f; if (r) { x = 6.0f; } }
  if (r != 1)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 6.0f)
    abort ();
  v = -5.0f;
  #pragma omp atomic capture compare
  { r = x == 17.0L; if (r) { x = 25.0; } else { v = x; } }
  if (r || v != 6.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 6.0f)
    abort ();
  v = 15.0f;
  #pragma omp atomic capture compare
  { r = x == 6.0f; if (r) { x = 23.0f; } else { v = x; } }
  if (r != 1 || v != 15.0f)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 23.0f)
    abort ();
}

int
main ()
{
  bar <0> ();
  #pragma omp atomic write
  x = 6.0f;
  baz <float, int> (x);
}
