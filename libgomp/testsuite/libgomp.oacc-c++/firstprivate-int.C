/* Verify the GOMP_MAP_FIRSTPRIVATE_INT optimization on various types.
   This test is similer to the test in libgomp.oacc-c-c++-common, but
   it focuses on reference types. */

#include <assert.h>
#include <stdint.h>
#include <complex.h>

void test_ref (int8_t &i8i, int8_t &i8o, int16_t &i16i, int16_t &i16o,
	       int32_t &i32i, int32_t &i32o, int64_t &i64i, int64_t &i64o,
	       uint8_t &u8i, uint8_t &u8o, uint16_t &u16i, uint16_t &u16o,
	       uint32_t &u32i, uint32_t &u32o, uint64_t &u64i, uint64_t &u64o,
	       float &r32i, float &r32o, double &r64i, double &r64o,
	       int _Complex &cii, int _Complex &cio,
	       float _Complex &cfi, float _Complex &cfo,
	       double _Complex &cdi, double _Complex &cdo)
{
#pragma acc parallel firstprivate (i8i,i16i,i32i,i64i,u8i,u16i,u32i,u64i) \
  firstprivate(r32i,r64i,cii,cfi,cdi) copyout(i8o,i16o,i32o,i64o) \
  copyout(u8o,u16o,u32o,u64o,r32o,r64o,cio,cfo,cdo) num_gangs(1)
  {
    i8o = i8i;
    i16o = i16i;
    i32o = i32i;
    i64o = i64i;

    u8o = u8i;
    u16o = u16i;
    u32o = u32i;
    u64o = u64i;

    r32o = r32i;
    r64o = r64i;

    cio = cii;
    cfo = cfi;
    cdo = cdi;
  }
}

int
main ()
{
  int8_t  i8i  = -1, i8o;
  int16_t i16i = -2, i16o;
  int32_t i32i = -3, i32o;
  int64_t i64i = -4, i64o;

  uint8_t  u8i  = 1,  u8o;
  uint16_t u16i = 2, u16o;
  uint32_t u32i = 3, u32o;
  uint64_t u64i = 4, u64o;

  float  r32i = .5, r32o;
  double r64i = .25, r64o;

  int _Complex    cii = 2, cio;
  float _Complex  cfi = 4, cfo;
  double _Complex cdi = 8, cdo;

  test_ref (i8i, i8o, i16i, i16o, i32i, i32o, i64i, i64o, u8i, u8o, u16i,
	    u16o, u32i, u32o, u64i, u64o, r32i, r32o, r64i, r64o, cii, cio,
	    cfi, cfo, cdi, cdo);

  assert (i8o == i8i);
  assert (i16o == i16i);
  assert (i32o == i32i);
  assert (i64o == i64i);

  assert (u8o == u8i);
  assert (u16o == u16i);
  assert (u32o == u32i);
  assert (u64o == u64i);

  assert (r32o == r32i);
  assert (r64o == r64i);

  assert (cio == cii);
  assert (cfo == cfi);
  assert (cdo == cdi);

  return 0;
}
