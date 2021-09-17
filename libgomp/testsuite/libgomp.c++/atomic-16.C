// { dg-do run }

extern "C" void abort (void);
int x = 6;
int w, y;

int *
foo (void)
{
  if (w)
    abort ();
  return &y;
}

template <int N>
void
bar ()
{
  int v, r;
  #pragma omp atomic compare
  x = x > 8 ? 8 : x;
  #pragma omp atomic read
  v = x;
  if (v != 6)
    abort ();
  #pragma omp atomic compare
  x = x > 4 ? 4 : x;
  #pragma omp atomic read
  v = x;
  if (v != 4)
    abort ();
  #pragma omp atomic compare capture
  v = x = x < 8 ? 8 : x;
  if (v != 8)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 8)
    abort ();
  #pragma omp atomic capture compare
  { v = x; x = x < 12 ? 12 : x; }
  if (v != 8)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 12)
    abort ();
  #pragma omp atomic capture compare
  { v = x; x = x < 4 ? 4 : x; }
  if (v != 12)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 12)
    abort ();
  #pragma omp atomic write
  x = -32;
  #pragma omp atomic capture compare seq_cst fail(relaxed)
  { x = 12U < x ? 12U : x; v = x; }
  if (v != 12)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 12)
    abort ();
  #pragma omp atomic compare
  x = x == 12 ? 16 : x;
  #pragma omp atomic read
  v = x;
  if (v != 16)
    abort ();
  r = 57;
  #pragma omp atomic compare capture
  v = x = x == 15 ? r + 7 : x;
  if (v != 16)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 16)
    abort ();
  #pragma omp atomic capture, update, compare seq_cst fail(acquire)
  { v = x; x = x == 73ULL - r ? 12LL : x; }
  if (v != 16)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 12)
    abort ();
  #pragma omp atomic update, compare, capture
  { x = x == 69LL - r ? (unsigned char) 6 : x; v = x; }
  if (v != 6)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 6)
    abort ();
  #pragma omp atomic compare
  if (x > 8) { x = 8; }
  #pragma omp atomic read
  v = x;
  if (v != 6)
    abort ();
  #pragma omp atomic compare
  if (x > 4) { x = 4; }
  #pragma omp atomic read
  v = x;
  if (v != 4)
    abort ();
  #pragma omp atomic compare capture
  { if (x < 8) { x = 8; } v = x; }
  if (v != 8)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 8)
    abort ();
  #pragma omp atomic capture compare
  { v = x; if (x < 12) { x = 12; } }
  if (v != 8)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 12)
    abort ();
  #pragma omp atomic capture compare
  { v = x; if (x < 4) { x = 4; } }
  if (v != 12)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 12)
    abort ();
  #pragma omp atomic write
  x = -32;
  #pragma omp atomic capture compare seq_cst fail(relaxed)
  { if (12U < x) { x = 12U; } v = x; }
  if (v != 12)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 12)
    abort ();
  #pragma omp atomic compare
  if (x == 12) { x = 16; }
  #pragma omp atomic read
  v = x;
  if (v != 16)
    abort ();
  r = 57;
  #pragma omp atomic compare capture
  { if (x == 15) { x = r + 7; } v = x; }
  if (v != 16)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 16)
    abort ();
  #pragma omp atomic capture, update, compare seq_cst fail(acquire)
  { v = x; if (x == 73ULL - r) { x = 12LL; } }
  if (v != 16)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 12)
    abort ();
  #pragma omp atomic update, compare, capture
  { if (x == 69LL - r) { x = (unsigned char) 6; } v = x; }
  if (v != 6)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 6)
    abort ();
  v = 24;
  #pragma omp atomic compare capture
  if (x == 12) { x = 16; } else { v = x; }
  if (v != 6)
    abort ();
  v = 32;
  #pragma omp atomic read
  v = x;
  if (v != 6)
    abort ();
  v = 147;
  #pragma omp atomic capture compare
  if (x == 6) { x = 57; } else { v = x; }
  if (v != 147)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 57)
    abort ();
  #pragma omp atomic update, capture, compare, weak, seq_cst, fail (relaxed)
  { r = x == 137; if (r) { x = 174; } }
  if (r)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 57)
    abort ();
  #pragma omp atomic compare capture fail (relaxed)
  { r = x == 57; if (r) { x = 6; } }
  if (r != 1)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 6)
    abort ();
  v = -5;
  #pragma omp atomic capture compare
  { r = x == 17; if (r) { x = 25; } else { v = x; } }
  if (r || v != 6)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 6)
    abort ();
  v = 15;
  #pragma omp atomic capture compare
  { r = x == 6; if (r) { x = 23; } else { v = x; } }
  if (r != 1 || v != 15)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 23)
    abort ();
  w = 1;
  #pragma omp atomic compare capture
  if (x == 23) { x = 57; } else { foo ()[0] = x; }
  #pragma omp atomic read
  v = x;
  if (v != 57)
    abort ();
  #pragma omp atomic capture update compare
  { r = x == 57; if (r) { x = 23; } else { foo ()[0] = x; } }
  if (r != 1)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 23)
    abort ();
  w = 0;
  #pragma omp atomic compare capture
  if (x == 24) { x = 57; } else { foo ()[0] = x; }
  if (y != 23)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 23)
    abort ();
  y = -5;
  #pragma omp atomic capture update compare
  {
    r = x == 57;
    if (r)
      {
	x = 27;
      }
    else
      {
	foo ()[0] = x;
      }
  }
  if (r || y != 23)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 23)
    abort ();
}

template <typename T>
void
baz (T &x, T &w, T &y)
{
  T v, r;
  #pragma omp atomic compare
  x = x > 8 ? 8 : x;
  #pragma omp atomic read
  v = x;
  if (v != 6)
    abort ();
  #pragma omp atomic compare
  x = x > 4 ? 4 : x;
  #pragma omp atomic read
  v = x;
  if (v != 4)
    abort ();
  #pragma omp atomic compare capture
  v = x = x < 8 ? 8 : x;
  if (v != 8)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 8)
    abort ();
  #pragma omp atomic capture compare
  { v = x; x = x < 12 ? 12 : x; }
  if (v != 8)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 12)
    abort ();
  #pragma omp atomic capture compare
  { v = x; x = x < 4 ? 4 : x; }
  if (v != 12)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 12)
    abort ();
  #pragma omp atomic write
  x = -32;
  #pragma omp atomic capture compare seq_cst fail(relaxed)
  { x = 12U < x ? 12U : x; v = x; }
  if (v != 12)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 12)
    abort ();
  #pragma omp atomic compare
  x = x == 12 ? 16 : x;
  #pragma omp atomic read
  v = x;
  if (v != 16)
    abort ();
  r = 57;
  #pragma omp atomic compare capture
  v = x = x == 15 ? r + 7 : x;
  if (v != 16)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 16)
    abort ();
  #pragma omp atomic capture, update, compare seq_cst fail(acquire)
  { v = x; x = x == 73ULL - r ? 12LL : x; }
  if (v != 16)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 12)
    abort ();
  #pragma omp atomic update, compare, capture
  { x = x == 69LL - r ? (unsigned char) 6 : x; v = x; }
  if (v != 6)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 6)
    abort ();
  #pragma omp atomic compare
  if (x > 8) { x = 8; }
  #pragma omp atomic read
  v = x;
  if (v != 6)
    abort ();
  #pragma omp atomic compare
  if (x > 4) { x = 4; }
  #pragma omp atomic read
  v = x;
  if (v != 4)
    abort ();
  #pragma omp atomic compare capture
  { if (x < 8) { x = 8; } v = x; }
  if (v != 8)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 8)
    abort ();
  #pragma omp atomic capture compare
  { v = x; if (x < 12) { x = 12; } }
  if (v != 8)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 12)
    abort ();
  #pragma omp atomic capture compare
  { v = x; if (x < 4) { x = 4; } }
  if (v != 12)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 12)
    abort ();
  #pragma omp atomic write
  x = -32;
  #pragma omp atomic capture compare seq_cst fail(relaxed)
  { if (12U < x) { x = 12U; } v = x; }
  if (v != 12)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 12)
    abort ();
  #pragma omp atomic compare
  if (x == 12) { x = 16; }
  #pragma omp atomic read
  v = x;
  if (v != 16)
    abort ();
  r = 57;
  #pragma omp atomic compare capture
  { if (x == 15) { x = r + 7; } v = x; }
  if (v != 16)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 16)
    abort ();
  #pragma omp atomic capture, update, compare seq_cst fail(acquire)
  { v = x; if (x == 73ULL - r) { x = 12LL; } }
  if (v != 16)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 12)
    abort ();
  #pragma omp atomic update, compare, capture
  { if (x == 69LL - r) { x = (unsigned char) 6; } v = x; }
  if (v != 6)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 6)
    abort ();
  v = 24;
  #pragma omp atomic compare capture
  if (x == 12) { x = 16; } else { v = x; }
  if (v != 6)
    abort ();
  v = 32;
  #pragma omp atomic read
  v = x;
  if (v != 6)
    abort ();
  v = 147;
  #pragma omp atomic capture compare
  if (x == 6) { x = 57; } else { v = x; }
  if (v != 147)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 57)
    abort ();
  #pragma omp atomic update, capture, compare, weak, seq_cst, fail (relaxed)
  { r = x == 137; if (r) { x = 174; } }
  if (r)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 57)
    abort ();
  #pragma omp atomic compare capture fail (relaxed)
  { r = x == 57; if (r) { x = 6; } }
  if (r != 1)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 6)
    abort ();
  v = -5;
  #pragma omp atomic capture compare
  { r = x == 17; if (r) { x = 25; } else { v = x; } }
  if (r || v != 6)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 6)
    abort ();
  v = 15;
  #pragma omp atomic capture compare
  { r = x == 6; if (r) { x = 23; } else { v = x; } }
  if (r != 1 || v != 15)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 23)
    abort ();
  w = 1;
  #pragma omp atomic compare capture
  if (x == 23) { x = 57; } else { foo ()[0] = x; }
  #pragma omp atomic read
  v = x;
  if (v != 57)
    abort ();
  #pragma omp atomic capture update compare
  { r = x == 57; if (r) { x = 23; } else { foo ()[0] = x; } }
  if (r != 1)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 23)
    abort ();
  w = 0;
  #pragma omp atomic compare capture
  if (x == 24) { x = 57; } else { foo ()[0] = x; }
  if (y != 23)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 23)
    abort ();
  y = -5;
  #pragma omp atomic capture update compare
  {
    r = x == 57;
    if (r)
      {
	x = 27;
      }
    else
      {
	foo ()[0] = x;
      }
  }
  if (r || y != 23)
    abort ();
  #pragma omp atomic read
  v = x;
  if (v != 23)
    abort ();
}

int
main ()
{
  bar <0> ();
  #pragma omp atomic write
  x = 6;
  y = 0;
  w = 0;
  baz (x, w, y);
}
