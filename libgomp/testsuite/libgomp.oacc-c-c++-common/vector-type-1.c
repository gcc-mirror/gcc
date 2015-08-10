#define vector __attribute__ ((vector_size (4 * sizeof(int))))

int main(void)
{
  vector int vi = { 12, -34, -56, 78 };

#pragma acc parallel copy(vi)
  {
    if (vi[0] != 12
	|| vi[1] != -34
	|| vi[2] != -56
	|| vi[3] != 78)
      __builtin_abort();
    vector int vi_ = { -21, -43, 65, 87 };
    vi = vi_;
  }
  if (vi[0] != -21
      || vi[1] != -43
      || vi[2] != 65
      || vi[3] != 87)
    __builtin_abort();

  return 0;
}
