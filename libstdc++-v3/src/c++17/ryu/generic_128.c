// Copyright 2018 Ulf Adams
//
// The contents of this file may be used under the terms of the Apache License,
// Version 2.0.
//
//    (See accompanying file LICENSE-Apache or copy at
//     http://www.apache.org/licenses/LICENSE-2.0)
//
// Alternatively, the contents of this file may be used under the terms of
// the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE-Boost or copy at
//     https://www.boost.org/LICENSE_1_0.txt)
//
// Unless required by applicable law or agreed to in writing, this software
// is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.

// Runtime compiler options:
// -DRYU_DEBUG Generate verbose debugging output to stdout.




#ifdef RYU_DEBUG
static char* s(uint128_t v) {
  int len = decimalLength(v);
  char* b = (char*) malloc((len + 1) * sizeof(char));
  for (int i = 0; i < len; i++) {
    const uint32_t c = (uint32_t) (v % 10);
    v /= 10;
    b[len - 1 - i] = (char) ('0' + c);
  }
  b[len] = 0;
  return b;
}
#endif

#define ONE ((uint128_t) 1)

struct floating_decimal_128 generic_binary_to_decimal(
    const uint128_t ieeeMantissa, const uint32_t ieeeExponent, const bool ieeeSign,
    const uint32_t mantissaBits, const uint32_t exponentBits, const bool explicitLeadingBit) {
#ifdef RYU_DEBUG
  printf("IN=");
  for (int32_t bit = 127; bit >= 0; --bit) {
    printf("%u", (uint32_t) ((bits >> bit) & 1));
  }
  printf("\n");
#endif

  const uint32_t bias = (1u << (exponentBits - 1)) - 1;

  if (ieeeExponent == 0 && ieeeMantissa == 0) {
    struct floating_decimal_128 fd;
    fd.mantissa = 0;
    fd.exponent = 0;
    fd.sign = ieeeSign;
    return fd;
  }
  if (ieeeExponent == ((1u << exponentBits) - 1u)) {
    struct floating_decimal_128 fd;
    fd.mantissa = explicitLeadingBit ? ieeeMantissa & ((ONE << (mantissaBits - 1)) - 1) : ieeeMantissa;
    fd.exponent = FD128_EXCEPTIONAL_EXPONENT;
    fd.sign = ieeeSign;
    return fd;
  }

  int32_t e2;
  uint128_t m2;
  // We subtract 2 in all cases so that the bounds computation has 2 additional bits.
  if (explicitLeadingBit) {
    // mantissaBits includes the explicit leading bit, so we need to correct for that here.
    if (ieeeExponent == 0) {
      e2 = 1 - bias - mantissaBits + 1 - 2;
    } else {
      e2 = ieeeExponent - bias - mantissaBits + 1 - 2;
    }
    m2 = ieeeMantissa;
  } else {
    if (ieeeExponent == 0) {
      e2 = 1 - bias - mantissaBits - 2;
      m2 = ieeeMantissa;
    } else {
      e2 = ieeeExponent - bias - mantissaBits - 2;
      m2 = (ONE << mantissaBits) | ieeeMantissa;
    }
  }
  const bool even = (m2 & 1) == 0;
  const bool acceptBounds = even;

#ifdef RYU_DEBUG
  printf("-> %s %s * 2^%d\n", ieeeSign ? "-" : "+", s(m2), e2 + 2);
#endif

  // Step 2: Determine the interval of legal decimal representations.
  const uint128_t mv = 4 * m2;
  // Implicit bool -> int conversion. True is 1, false is 0.
  const uint32_t mmShift =
      (ieeeMantissa != (explicitLeadingBit ? ONE << (mantissaBits - 1) : 0))
      || (ieeeExponent == 0);

  // Step 3: Convert to a decimal power base using 128-bit arithmetic.
  uint128_t vr, vp, vm;
  int32_t e10;
  bool vmIsTrailingZeros = false;
  bool vrIsTrailingZeros = false;
  if (e2 >= 0) {
    // I tried special-casing q == 0, but there was no effect on performance.
    // This expression is slightly faster than max(0, log10Pow2(e2) - 1).
    const uint32_t q = log10Pow2(e2) - (e2 > 3);
    e10 = q;
    const int32_t k = FLOAT_128_POW5_INV_BITCOUNT + pow5bits(q) - 1;
    const int32_t i = -e2 + q + k;
    uint64_t pow5[4];
    generic_computeInvPow5(q, pow5);
    vr = mulShift(4 * m2, pow5, i);
    vp = mulShift(4 * m2 + 2, pow5, i);
    vm = mulShift(4 * m2 - 1 - mmShift, pow5, i);
#ifdef RYU_DEBUG
    printf("%s * 2^%d / 10^%d\n", s(mv), e2, q);
    printf("V+=%s\nV =%s\nV-=%s\n", s(vp), s(vr), s(vm));
#endif
    // floor(log_5(2^128)) = 55, this is very conservative
    if (q <= 55) {
      // Only one of mp, mv, and mm can be a multiple of 5, if any.
      if (mv % 5 == 0) {
        vrIsTrailingZeros = multipleOfPowerOf5(mv, q - 1);
      } else if (acceptBounds) {
        // Same as min(e2 + (~mm & 1), pow5Factor(mm)) >= q
        // <=> e2 + (~mm & 1) >= q && pow5Factor(mm) >= q
        // <=> true && pow5Factor(mm) >= q, since e2 >= q.
        vmIsTrailingZeros = multipleOfPowerOf5(mv - 1 - mmShift, q);
      } else {
        // Same as min(e2 + 1, pow5Factor(mp)) >= q.
        vp -= multipleOfPowerOf5(mv + 2, q);
      }
    }
  } else {
    // This expression is slightly faster than max(0, log10Pow5(-e2) - 1).
    const uint32_t q = log10Pow5(-e2) - (-e2 > 1);
    e10 = q + e2;
    const int32_t i = -e2 - q;
    const int32_t k = pow5bits(i) - FLOAT_128_POW5_BITCOUNT;
    const int32_t j = q - k;
    uint64_t pow5[4];
    generic_computePow5(i, pow5);
    vr = mulShift(4 * m2, pow5, j);
    vp = mulShift(4 * m2 + 2, pow5, j);
    vm = mulShift(4 * m2 - 1 - mmShift, pow5, j);
#ifdef RYU_DEBUG
    printf("%s * 5^%d / 10^%d\n", s(mv), -e2, q);
    printf("%d %d %d %d\n", q, i, k, j);
    printf("V+=%s\nV =%s\nV-=%s\n", s(vp), s(vr), s(vm));
#endif
    if (q <= 1) {
      // {vr,vp,vm} is trailing zeros if {mv,mp,mm} has at least q trailing 0 bits.
      // mv = 4 m2, so it always has at least two trailing 0 bits.
      vrIsTrailingZeros = true;
      if (acceptBounds) {
        // mm = mv - 1 - mmShift, so it has 1 trailing 0 bit iff mmShift == 1.
        vmIsTrailingZeros = mmShift == 1;
      } else {
        // mp = mv + 2, so it always has at least one trailing 0 bit.
        --vp;
      }
    } else if (q < 127) { // TODO(ulfjack): Use a tighter bound here.
      // We need to compute min(ntz(mv), pow5Factor(mv) - e2) >= q-1
      // <=> ntz(mv) >= q-1  &&  pow5Factor(mv) - e2 >= q-1
      // <=> ntz(mv) >= q-1    (e2 is negative and -e2 >= q)
      // <=> (mv & ((1 << (q-1)) - 1)) == 0
      // We also need to make sure that the left shift does not overflow.
      vrIsTrailingZeros = multipleOfPowerOf2(mv, q - 1);
#ifdef RYU_DEBUG
      printf("vr is trailing zeros=%s\n", vrIsTrailingZeros ? "true" : "false");
#endif
    }
  }
#ifdef RYU_DEBUG
  printf("e10=%d\n", e10);
  printf("V+=%s\nV =%s\nV-=%s\n", s(vp), s(vr), s(vm));
  printf("vm is trailing zeros=%s\n", vmIsTrailingZeros ? "true" : "false");
  printf("vr is trailing zeros=%s\n", vrIsTrailingZeros ? "true" : "false");
#endif

  // Step 4: Find the shortest decimal representation in the interval of legal representations.
  uint32_t removed = 0;
  uint8_t lastRemovedDigit = 0;
  uint128_t output;

  while (vp / 10 > vm / 10) {
    vmIsTrailingZeros &= vm % 10 == 0;
    vrIsTrailingZeros &= lastRemovedDigit == 0;
    lastRemovedDigit = (uint8_t) (vr % 10);
    vr /= 10;
    vp /= 10;
    vm /= 10;
    ++removed;
  }
#ifdef RYU_DEBUG
  printf("V+=%s\nV =%s\nV-=%s\n", s(vp), s(vr), s(vm));
  printf("d-10=%s\n", vmIsTrailingZeros ? "true" : "false");
#endif
  if (vmIsTrailingZeros) {
    while (vm % 10 == 0) {
      vrIsTrailingZeros &= lastRemovedDigit == 0;
      lastRemovedDigit = (uint8_t) (vr % 10);
      vr /= 10;
      vp /= 10;
      vm /= 10;
      ++removed;
    }
  }
#ifdef RYU_DEBUG
  printf("%s %d\n", s(vr), lastRemovedDigit);
  printf("vr is trailing zeros=%s\n", vrIsTrailingZeros ? "true" : "false");
#endif
  if (vrIsTrailingZeros && (lastRemovedDigit == 5) && (vr % 2 == 0)) {
    // Round even if the exact numbers is .....50..0.
    lastRemovedDigit = 4;
  }
  // We need to take vr+1 if vr is outside bounds or we need to round up.
  output = vr +
      ((vr == vm && (!acceptBounds || !vmIsTrailingZeros)) || (lastRemovedDigit >= 5));
  const int32_t exp = e10 + removed;

#ifdef RYU_DEBUG
  printf("V+=%s\nV =%s\nV-=%s\n", s(vp), s(vr), s(vm));
  printf("O=%s\n", s(output));
  printf("EXP=%d\n", exp);
#endif

  struct floating_decimal_128 fd;
  fd.mantissa = output;
  fd.exponent = exp;
  fd.sign = ieeeSign;
  return fd;
}

static inline int copy_special_str(char * const result, const struct floating_decimal_128 fd) {
  if (fd.mantissa) {
    memcpy(result, "NaN", 3);
    return 3;
  }
  if (fd.sign) {
    result[0] = '-';
  }
  memcpy(result + fd.sign, "Infinity", 8);
  return fd.sign + 8;
}

int generic_to_chars(const struct floating_decimal_128 v, char* const result) {
  if (v.exponent == FD128_EXCEPTIONAL_EXPONENT) {
    return copy_special_str(result, v);
  }

  // Step 5: Print the decimal representation.
  int index = 0;
  if (v.sign) {
    result[index++] = '-';
  }

  uint128_t output = v.mantissa;
  const uint32_t olength = decimalLength(output);

#ifdef RYU_DEBUG
  printf("DIGITS=%s\n", s(v.mantissa));
  printf("OLEN=%u\n", olength);
  printf("EXP=%u\n", v.exponent + olength);
#endif

  for (uint32_t i = 0; i < olength - 1; ++i) {
    const uint32_t c = (uint32_t) (output % 10);
    output /= 10;
    result[index + olength - i] = (char) ('0' + c);
  }
  result[index] = '0' + (uint32_t) (output % 10); // output should be < 10 by now.

  // Print decimal point if needed.
  if (olength > 1) {
    result[index + 1] = '.';
    index += olength + 1;
  } else {
    ++index;
  }

  // Print the exponent.
  result[index++] = 'e';
  int32_t exp = v.exponent + olength - 1;
  if (exp < 0) {
    result[index++] = '-';
    exp = -exp;
  } else
    result[index++] = '+';

  uint32_t elength = decimalLength(exp);
  if (elength == 1)
    ++elength;
  for (uint32_t i = 0; i < elength; ++i) {
    const uint32_t c = exp % 10;
    exp /= 10;
    result[index + elength - 1 - i] = (char) ('0' + c);
  }
  index += elength;
  return index;
}
