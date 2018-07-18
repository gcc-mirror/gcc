// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Hash code using AES intrinsics.

#include "runtime.h"

uintptr aeshashbody(void*, uintptr, uintptr, Slice)
	__asm__(GOSYM_PREFIX "runtime.aeshashbody");

uintptr aeshashbody(void*, uintptr, uintptr, Slice)
	__attribute__((no_split_stack));

#if (defined(__i386__) || defined(__x86_64__)) && defined(HAVE_AS_X86_AES)

#include <emmintrin.h>
#include <tmmintrin.h>
#include <wmmintrin.h>

// Force appropriate CPU level.  We won't call here unless the CPU
// supports it.

#pragma GCC target("ssse3", "aes")

#ifdef __x86_64__

// aeshashbody implements a hash function using AES instructions
// available in recent x86 processors. Note this is not encryption,
// just hashing.
//
// This is written to produce exactly the same results as the gc
// implementation, not because that matters, but just to ensure that
// this does something reasonable.
uintptr aeshashbody(void* p, uintptr seed, uintptr size, Slice aeskeysched) {
	__m128i mseed, mseed2, mseed3, mseed4, mseed5, mseed6, mseed7, mseed8;
	__m128i mval, mval2, mval3, mval4, mval5, mval6, mval7, mval8;

	// Start with hash seed.
	mseed = _mm_cvtsi64_si128(seed);
	// Get 16 bits of length.
	mseed = _mm_insert_epi16(mseed, size, 4);
	// Repeat length 4 times total.
	mseed = _mm_shufflehi_epi16(mseed, 0);
	// Save unscrambled seed.
	mseed2 = mseed;
	// XOR in per-process seed.
	mseed ^= _mm_loadu_si128(aeskeysched.__values);
	// Scramble seed.
	mseed = _mm_aesenc_si128(mseed, mseed);

	if (size <= 16) {
		if (size == 0) {
			// Return scrambled input seed.
			return _mm_cvtsi128_si64(_mm_aesenc_si128(mseed, mseed));
		} else if (size < 16) {
			if ((((uintptr)(p) + 16) & 0xff0) != 0) {
				static const uint64 masks[32]
				  __attribute__ ((aligned(16))) =
				  {
				    0x0000000000000000, 0x0000000000000000,
				    0x00000000000000ff, 0x0000000000000000,
				    0x000000000000ffff, 0x0000000000000000,
				    0x0000000000ffffff, 0x0000000000000000,
				    0x00000000ffffffff, 0x0000000000000000,
				    0x000000ffffffffff, 0x0000000000000000,
				    0x0000ffffffffffff, 0x0000000000000000,
				    0x00ffffffffffffff, 0x0000000000000000,
				    0xffffffffffffffff, 0x0000000000000000,
				    0xffffffffffffffff, 0x00000000000000ff,
				    0xffffffffffffffff, 0x000000000000ffff,
				    0xffffffffffffffff, 0x0000000000ffffff,
				    0xffffffffffffffff, 0x00000000ffffffff,
				    0xffffffffffffffff, 0x000000ffffffffff,
				    0xffffffffffffffff, 0x0000ffffffffffff,
				    0xffffffffffffffff, 0x00ffffffffffffff
				  };

				// 16 bytes loaded at p won't cross a page
				// boundary, so we can load directly.
				mval = _mm_loadu_si128(p);
				mval &= *(const __m128i*)(&masks[size*2]);
			} else {
				static const uint64 shifts[32]
				  __attribute__ ((aligned(16))) =
				  {
				    0x0000000000000000, 0x0000000000000000,
				    0xffffffffffffff0f, 0xffffffffffffffff,
				    0xffffffffffff0f0e, 0xffffffffffffffff,
				    0xffffffffff0f0e0d, 0xffffffffffffffff,
				    0xffffffff0f0e0d0c, 0xffffffffffffffff,
				    0xffffff0f0e0d0c0b, 0xffffffffffffffff,
				    0xffff0f0e0d0c0b0a, 0xffffffffffffffff,
				    0xff0f0e0d0c0b0a09, 0xffffffffffffffff,
				    0x0f0e0d0c0b0a0908, 0xffffffffffffffff,
				    0x0e0d0c0b0a090807, 0xffffffffffffff0f,
				    0x0d0c0b0a09080706, 0xffffffffffff0f0e,
				    0x0c0b0a0908070605, 0xffffffffff0f0e0d,
				    0x0b0a090807060504, 0xffffffff0f0e0d0c,
				    0x0a09080706050403, 0xffffff0f0e0d0c0b,
				    0x0908070605040302, 0xffff0f0e0d0c0b0a,
				    0x0807060504030201, 0xff0f0e0d0c0b0a09,
				  };

				// address ends in 1111xxxx. Might be
				// up against a page boundary, so load
				// ending at last byte.  Then shift
				// bytes down using pshufb.
				mval = _mm_loadu_si128((void*)((char*)p - 16 + size));
				mval = _mm_shuffle_epi8(mval, *(const __m128i*)(&shifts[size*2]));
			}
		} else {
			mval = _mm_loadu_si128(p);
		}

		// XOR data with seed.
		mval ^= mseed;
		// Scramble combo 3 times.
		mval = _mm_aesenc_si128(mval, mval);
		mval = _mm_aesenc_si128(mval, mval);
		mval = _mm_aesenc_si128(mval, mval);
		return _mm_cvtsi128_si64(mval);
	} else if (size <= 32) {
		// Make second starting seed.
		mseed2 ^= _mm_loadu_si128((void*)((char*)aeskeysched.__values + 16));
		mseed2 = _mm_aesenc_si128(mseed2, mseed2);
		// Load data to be hashed.
		mval = _mm_loadu_si128(p);
		mval2 = _mm_loadu_si128((void*)((char*)p + size - 16));
		// XOR with seed.
		mval ^= mseed;
		mval2 ^= mseed2;
		// Scramble 3 times.
		mval = _mm_aesenc_si128(mval, mval);
		mval2 = _mm_aesenc_si128(mval2, mval2);
		mval = _mm_aesenc_si128(mval, mval);
		mval2 = _mm_aesenc_si128(mval2, mval2);
		mval = _mm_aesenc_si128(mval, mval);
		mval2 = _mm_aesenc_si128(mval2, mval2);
		// Combine results.
		mval ^= mval2;
		return _mm_cvtsi128_si64(mval);
	} else if (size <= 64) {
		// Make 3 more starting seeds.
		mseed3 = mseed2;
		mseed4 = mseed2;
		mseed2 ^= _mm_loadu_si128((void*)((char*)aeskeysched.__values + 16));
		mseed3 ^= _mm_loadu_si128((void*)((char*)aeskeysched.__values + 32));
		mseed4 ^= _mm_loadu_si128((void*)((char*)aeskeysched.__values + 48));
		mseed2 = _mm_aesenc_si128(mseed2, mseed2);
		mseed3 = _mm_aesenc_si128(mseed3, mseed3);
		mseed4 = _mm_aesenc_si128(mseed4, mseed4);

		mval = _mm_loadu_si128(p);
		mval2 = _mm_loadu_si128((void*)((char*)p + 16));
		mval3 = _mm_loadu_si128((void*)((char*)p + size - 32));
		mval4 = _mm_loadu_si128((void*)((char*)p + size - 16));

		mval ^= mseed;
		mval2 ^= mseed2;
		mval3 ^= mseed3;
		mval4 ^= mseed4;

		mval = _mm_aesenc_si128(mval, mval);
		mval2 = _mm_aesenc_si128(mval2, mval2);
		mval3 = _mm_aesenc_si128(mval3, mval3);
		mval4 = _mm_aesenc_si128(mval4, mval4);

		mval = _mm_aesenc_si128(mval, mval);
		mval2 = _mm_aesenc_si128(mval2, mval2);
		mval3 = _mm_aesenc_si128(mval3, mval3);
		mval4 = _mm_aesenc_si128(mval4, mval4);

		mval = _mm_aesenc_si128(mval, mval);
		mval2 = _mm_aesenc_si128(mval2, mval2);
		mval3 = _mm_aesenc_si128(mval3, mval3);
		mval4 = _mm_aesenc_si128(mval4, mval4);

		mval ^= mval3;
		mval2 ^= mval4;
		mval ^= mval2;
		return _mm_cvtsi128_si64(mval);
	} else if (size <= 128) {
		// Make 7 more starting seeds.
		mseed3 = mseed2;
		mseed4 = mseed2;
		mseed5 = mseed2;
		mseed6 = mseed2;
		mseed7 = mseed2;
		mseed8 = mseed2;
		mseed2 ^= _mm_loadu_si128((void*)((char*)aeskeysched.__values + 16));
		mseed3 ^= _mm_loadu_si128((void*)((char*)aeskeysched.__values + 32));
		mseed4 ^= _mm_loadu_si128((void*)((char*)aeskeysched.__values + 48));
		mseed5 ^= _mm_loadu_si128((void*)((char*)aeskeysched.__values + 64));
		mseed6 ^= _mm_loadu_si128((void*)((char*)aeskeysched.__values + 80));
		mseed7 ^= _mm_loadu_si128((void*)((char*)aeskeysched.__values + 96));
		mseed8 ^= _mm_loadu_si128((void*)((char*)aeskeysched.__values + 112));
		mseed2 = _mm_aesenc_si128(mseed2, mseed2);
		mseed3 = _mm_aesenc_si128(mseed3, mseed3);
		mseed4 = _mm_aesenc_si128(mseed4, mseed4);
		mseed5 = _mm_aesenc_si128(mseed5, mseed5);
		mseed6 = _mm_aesenc_si128(mseed6, mseed6);
		mseed7 = _mm_aesenc_si128(mseed7, mseed7);
		mseed8 = _mm_aesenc_si128(mseed8, mseed8);

		// Load data.
		mval = _mm_loadu_si128(p);
		mval2 = _mm_loadu_si128((void*)((char*)p + 16));
		mval3 = _mm_loadu_si128((void*)((char*)p + 32));
		mval4 = _mm_loadu_si128((void*)((char*)p + 48));
		mval5 = _mm_loadu_si128((void*)((char*)p + size - 64));
		mval6 = _mm_loadu_si128((void*)((char*)p + size - 48));
		mval7 = _mm_loadu_si128((void*)((char*)p + size - 32));
		mval8 = _mm_loadu_si128((void*)((char*)p + size - 16));

		// XOR with seed.
		mval ^= mseed;
		mval2 ^= mseed2;
		mval3 ^= mseed3;
		mval4 ^= mseed4;
		mval5 ^= mseed5;
		mval6 ^= mseed6;
		mval7 ^= mseed7;
		mval8 ^= mseed8;

		// Scramble 3 times.
		mval = _mm_aesenc_si128(mval, mval);
		mval2 = _mm_aesenc_si128(mval2, mval2);
		mval3 = _mm_aesenc_si128(mval3, mval3);
		mval4 = _mm_aesenc_si128(mval4, mval4);
		mval5 = _mm_aesenc_si128(mval5, mval5);
		mval6 = _mm_aesenc_si128(mval6, mval6);
		mval7 = _mm_aesenc_si128(mval7, mval7);
		mval8 = _mm_aesenc_si128(mval8, mval8);

		mval = _mm_aesenc_si128(mval, mval);
		mval2 = _mm_aesenc_si128(mval2, mval2);
		mval3 = _mm_aesenc_si128(mval3, mval3);
		mval4 = _mm_aesenc_si128(mval4, mval4);
		mval5 = _mm_aesenc_si128(mval5, mval5);
		mval6 = _mm_aesenc_si128(mval6, mval6);
		mval7 = _mm_aesenc_si128(mval7, mval7);
		mval8 = _mm_aesenc_si128(mval8, mval8);

		mval = _mm_aesenc_si128(mval, mval);
		mval2 = _mm_aesenc_si128(mval2, mval2);
		mval3 = _mm_aesenc_si128(mval3, mval3);
		mval4 = _mm_aesenc_si128(mval4, mval4);
		mval5 = _mm_aesenc_si128(mval5, mval5);
		mval6 = _mm_aesenc_si128(mval6, mval6);
		mval7 = _mm_aesenc_si128(mval7, mval7);
		mval8 = _mm_aesenc_si128(mval8, mval8);

		// Combine results.
		mval ^= mval5;
		mval2 ^= mval6;
		mval3 ^= mval7;
		mval4 ^= mval8;
		mval ^= mval3;
		mval2 ^= mval4;
		mval ^= mval2;
		return _mm_cvtsi128_si64(mval);
	} else {
		// Make 7 more starting seeds.
		mseed3 = mseed2;
		mseed4 = mseed2;
		mseed5 = mseed2;
		mseed6 = mseed2;
		mseed7 = mseed2;
		mseed8 = mseed2;
		mseed2 ^= _mm_loadu_si128((void*)((char*)aeskeysched.__values + 16));
		mseed3 ^= _mm_loadu_si128((void*)((char*)aeskeysched.__values + 32));
		mseed4 ^= _mm_loadu_si128((void*)((char*)aeskeysched.__values + 48));
		mseed5 ^= _mm_loadu_si128((void*)((char*)aeskeysched.__values + 64));
		mseed6 ^= _mm_loadu_si128((void*)((char*)aeskeysched.__values + 80));
		mseed7 ^= _mm_loadu_si128((void*)((char*)aeskeysched.__values + 96));
		mseed8 ^= _mm_loadu_si128((void*)((char*)aeskeysched.__values + 112));
		mseed2 = _mm_aesenc_si128(mseed2, mseed2);
		mseed3 = _mm_aesenc_si128(mseed3, mseed3);
		mseed4 = _mm_aesenc_si128(mseed4, mseed4);
		mseed5 = _mm_aesenc_si128(mseed5, mseed5);
		mseed6 = _mm_aesenc_si128(mseed6, mseed6);
		mseed7 = _mm_aesenc_si128(mseed7, mseed7);
		mseed8 = _mm_aesenc_si128(mseed8, mseed8);

		// Start with last (possibly overlapping) block.
		mval = _mm_loadu_si128((void*)((char*)p + size - 128));
		mval2 = _mm_loadu_si128((void*)((char*)p + size - 112));
		mval3 = _mm_loadu_si128((void*)((char*)p + size - 96));
		mval4 = _mm_loadu_si128((void*)((char*)p + size - 80));
		mval5 = _mm_loadu_si128((void*)((char*)p + size - 64));
		mval6 = _mm_loadu_si128((void*)((char*)p + size - 48));
		mval7 = _mm_loadu_si128((void*)((char*)p + size - 32));
		mval8 = _mm_loadu_si128((void*)((char*)p + size - 16));

		// XOR in seed.
		mval ^= mseed;
		mval2 ^= mseed2;
		mval3 ^= mseed3;
		mval4 ^= mseed4;
		mval5 ^= mseed5;
		mval6 ^= mseed6;
		mval7 ^= mseed7;
		mval8 ^= mseed8;

		// Compute number of remaining 128-byte blocks.
		size--;
		size >>= 7;
		do {
			// Scramble state.
			mval = _mm_aesenc_si128(mval, mval);
			mval2 = _mm_aesenc_si128(mval2, mval2);
			mval3 = _mm_aesenc_si128(mval3, mval3);
			mval4 = _mm_aesenc_si128(mval4, mval4);
			mval5 = _mm_aesenc_si128(mval5, mval5);
			mval6 = _mm_aesenc_si128(mval6, mval6);
			mval7 = _mm_aesenc_si128(mval7, mval7);
			mval8 = _mm_aesenc_si128(mval8, mval8);

			// Scramble state, XOR in a block.
			mval = _mm_aesenc_si128(mval, _mm_loadu_si128(p));
			mval2 = _mm_aesenc_si128(mval2, _mm_loadu_si128((void*)((char*)p + 16)));
			mval3 = _mm_aesenc_si128(mval3, _mm_loadu_si128((void*)((char*)p + 32)));
			mval4 = _mm_aesenc_si128(mval4, _mm_loadu_si128((void*)((char*)p + 48)));
			mval5 = _mm_aesenc_si128(mval5, _mm_loadu_si128((void*)((char*)p + 64)));
			mval6 = _mm_aesenc_si128(mval6, _mm_loadu_si128((void*)((char*)p + 80)));
			mval7 = _mm_aesenc_si128(mval7, _mm_loadu_si128((void*)((char*)p + 96)));
			mval8 = _mm_aesenc_si128(mval8, _mm_loadu_si128((void*)((char*)p + 112)));

			p = (void*)((char*)p + 128);
		} while (--size > 0);

		// 3 more scrambles to finish.
		mval = _mm_aesenc_si128(mval, mval);
		mval2 = _mm_aesenc_si128(mval2, mval2);
		mval3 = _mm_aesenc_si128(mval3, mval3);
		mval4 = _mm_aesenc_si128(mval4, mval4);
		mval5 = _mm_aesenc_si128(mval5, mval5);
		mval6 = _mm_aesenc_si128(mval6, mval6);
		mval7 = _mm_aesenc_si128(mval7, mval7);
		mval8 = _mm_aesenc_si128(mval8, mval8);
		mval = _mm_aesenc_si128(mval, mval);
		mval2 = _mm_aesenc_si128(mval2, mval2);
		mval3 = _mm_aesenc_si128(mval3, mval3);
		mval4 = _mm_aesenc_si128(mval4, mval4);
		mval5 = _mm_aesenc_si128(mval5, mval5);
		mval6 = _mm_aesenc_si128(mval6, mval6);
		mval7 = _mm_aesenc_si128(mval7, mval7);
		mval8 = _mm_aesenc_si128(mval8, mval8);
		mval = _mm_aesenc_si128(mval, mval);
		mval2 = _mm_aesenc_si128(mval2, mval2);
		mval3 = _mm_aesenc_si128(mval3, mval3);
		mval4 = _mm_aesenc_si128(mval4, mval4);
		mval5 = _mm_aesenc_si128(mval5, mval5);
		mval6 = _mm_aesenc_si128(mval6, mval6);
		mval7 = _mm_aesenc_si128(mval7, mval7);
		mval8 = _mm_aesenc_si128(mval8, mval8);

		mval ^= mval5;
		mval2 ^= mval6;
		mval3 ^= mval7;
		mval4 ^= mval8;
		mval ^= mval3;
		mval2 ^= mval4;
		mval ^= mval2;
		return _mm_cvtsi128_si64(mval);
	}
}

#else // !defined(__x86_64__)

// The 32-bit version of aeshashbody.

uintptr aeshashbody(void* p, uintptr seed, uintptr size, Slice aeskeysched) {
	__m128i mseed, mseed2, mseed3, mseed4;
	__m128i mval, mval2, mval3, mval4;

	// Start with hash seed.
	mseed = _mm_cvtsi32_si128(seed);
	// Get 16 bits of length.
	mseed = _mm_insert_epi16(mseed, size, 4);
	// Replace size with its low 2 bytes repeated 4 times.
	mseed = _mm_shufflehi_epi16(mseed, 0);
	// Save unscrambled seed.
	mseed2 = mseed;
	// XOR in per-process seed.
	mseed ^= _mm_loadu_si128(aeskeysched.__values);
	// Scramble seed.
	mseed = _mm_aesenc_si128(mseed, mseed);

	if (size <= 16) {
		if (size == 0) {
			// Return scrambled input seed.
			return _mm_cvtsi128_si32(_mm_aesenc_si128(mseed, mseed));
		} else if (size < 16) {
			if ((((uintptr)(p) + 16) & 0xff0) != 0) {
				static const uint64 masks[32]
				  __attribute__ ((aligned(16))) =
				  {
				    0x0000000000000000, 0x0000000000000000,
				    0x00000000000000ff, 0x0000000000000000,
				    0x000000000000ffff, 0x0000000000000000,
				    0x0000000000ffffff, 0x0000000000000000,
				    0x00000000ffffffff, 0x0000000000000000,
				    0x000000ffffffffff, 0x0000000000000000,
				    0x0000ffffffffffff, 0x0000000000000000,
				    0x00ffffffffffffff, 0x0000000000000000,
				    0xffffffffffffffff, 0x0000000000000000,
				    0xffffffffffffffff, 0x00000000000000ff,
				    0xffffffffffffffff, 0x000000000000ffff,
				    0xffffffffffffffff, 0x0000000000ffffff,
				    0xffffffffffffffff, 0x00000000ffffffff,
				    0xffffffffffffffff, 0x000000ffffffffff,
				    0xffffffffffffffff, 0x0000ffffffffffff,
				    0xffffffffffffffff, 0x00ffffffffffffff
				  };

				// 16 bytes loaded at p won't cross a page
				// boundary, so we can load it directly.
				mval = _mm_loadu_si128(p);
				mval &= *(const __m128i*)(&masks[size*2]);
			} else {
				static const uint64 shifts[32]
				  __attribute__ ((aligned(16))) =
				  {
				    0x0000000000000000, 0x0000000000000000,
				    0xffffffffffffff0f, 0xffffffffffffffff,
				    0xffffffffffff0f0e, 0xffffffffffffffff,
				    0xffffffffff0f0e0d, 0xffffffffffffffff,
				    0xffffffff0f0e0d0c, 0xffffffffffffffff,
				    0xffffff0f0e0d0c0b, 0xffffffffffffffff,
				    0xffff0f0e0d0c0b0a, 0xffffffffffffffff,
				    0xff0f0e0d0c0b0a09, 0xffffffffffffffff,
				    0x0f0e0d0c0b0a0908, 0xffffffffffffffff,
				    0x0e0d0c0b0a090807, 0xffffffffffffff0f,
				    0x0d0c0b0a09080706, 0xffffffffffff0f0e,
				    0x0c0b0a0908070605, 0xffffffffff0f0e0d,
				    0x0b0a090807060504, 0xffffffff0f0e0d0c,
				    0x0a09080706050403, 0xffffff0f0e0d0c0b,
				    0x0908070605040302, 0xffff0f0e0d0c0b0a,
				    0x0807060504030201, 0xff0f0e0d0c0b0a09,
				  };

				// address ends in 1111xxxx. Might be
				// up against a page boundary, so load
				// ending at last byte.  Then shift
				// bytes down using pshufb.
				mval = _mm_loadu_si128((void*)((char*)p - 16 + size));
				mval = _mm_shuffle_epi8(mval, *(const __m128i*)(&shifts[size*2]));
			}
		} else {
			mval = _mm_loadu_si128(p);
		}

		// Scramble input, XOR in seed.
		mval = _mm_aesenc_si128(mval, mseed);
		mval = _mm_aesenc_si128(mval, mval);
		mval = _mm_aesenc_si128(mval, mval);
		return _mm_cvtsi128_si32(mval);
	} else if (size <= 32) {
		// Make second starting seed.
		mseed2 ^= _mm_loadu_si128((void*)((char*)aeskeysched.__values + 16));
		mseed2 = _mm_aesenc_si128(mseed2, mseed2);
		// Load data to be hashed.
		mval = _mm_loadu_si128(p);
		mval2 = _mm_loadu_si128((void*)((char*)p + size - 16));

		// Scramble 3 times.
		mval = _mm_aesenc_si128(mval, mseed);
		mval2 = _mm_aesenc_si128(mval2, mseed2);
		mval = _mm_aesenc_si128(mval, mval);
		mval2 = _mm_aesenc_si128(mval2, mval2);
		mval = _mm_aesenc_si128(mval, mval);
		mval2 = _mm_aesenc_si128(mval2, mval2);

		// Combine results.
		mval ^= mval2;
		return _mm_cvtsi128_si32(mval);
	} else if (size <= 64) {
		// Make 3 more starting seeds.
		mseed3 = mseed2;
		mseed4 = mseed2;
		mseed2 ^= _mm_loadu_si128((void*)((char*)aeskeysched.__values + 16));
		mseed3 ^= _mm_loadu_si128((void*)((char*)aeskeysched.__values + 32));
		mseed4 ^= _mm_loadu_si128((void*)((char*)aeskeysched.__values + 48));
		mseed2 = _mm_aesenc_si128(mseed2, mseed2);
		mseed3 = _mm_aesenc_si128(mseed3, mseed3);
		mseed4 = _mm_aesenc_si128(mseed4, mseed4);

		mval = _mm_loadu_si128(p);
		mval2 = _mm_loadu_si128((void*)((char*)p + 16));
		mval3 = _mm_loadu_si128((void*)((char*)p + size - 32));
		mval4 = _mm_loadu_si128((void*)((char*)p + size - 16));

		mval = _mm_aesenc_si128(mval, mseed);
		mval2 = _mm_aesenc_si128(mval2, mseed2);
		mval3 = _mm_aesenc_si128(mval3, mseed3);
		mval4 = _mm_aesenc_si128(mval4, mseed4);

		mval = _mm_aesenc_si128(mval, mval);
		mval2 = _mm_aesenc_si128(mval2, mval2);
		mval3 = _mm_aesenc_si128(mval3, mval3);
		mval4 = _mm_aesenc_si128(mval4, mval4);

		mval = _mm_aesenc_si128(mval, mval);
		mval2 = _mm_aesenc_si128(mval2, mval2);
		mval3 = _mm_aesenc_si128(mval3, mval3);
		mval4 = _mm_aesenc_si128(mval4, mval4);

		mval ^= mval3;
		mval2 ^= mval4;
		mval ^= mval2;
		return _mm_cvtsi128_si32(mval);
	} else {
		// Make 3 more starting seeds.
		mseed3 = mseed2;
		mseed4 = mseed2;
		mseed2 ^= _mm_loadu_si128((void*)((char*)aeskeysched.__values + 16));
		mseed3 ^= _mm_loadu_si128((void*)((char*)aeskeysched.__values + 32));
		mseed4 ^= _mm_loadu_si128((void*)((char*)aeskeysched.__values + 48));
		mseed2 = _mm_aesenc_si128(mseed2, mseed2);
		mseed3 = _mm_aesenc_si128(mseed3, mseed3);
		mseed4 = _mm_aesenc_si128(mseed4, mseed4);

		// Start with last (possibly overlapping) block.
		mval = _mm_loadu_si128((void*)((char*)p + size - 64));
		mval2 = _mm_loadu_si128((void*)((char*)p + size - 48));
		mval3 = _mm_loadu_si128((void*)((char*)p + size - 32));
		mval4 = _mm_loadu_si128((void*)((char*)p + size - 16));

		// Scramble state once.
		mval = _mm_aesenc_si128(mval, mseed);
		mval2 = _mm_aesenc_si128(mval2, mseed2);
		mval3 = _mm_aesenc_si128(mval3, mseed3);
		mval4 = _mm_aesenc_si128(mval4, mseed4);

		// Compute number of remaining 64-byte blocks.
		size--;
		size >>= 6;
		do {
			// Scramble state, XOR in a block.
			mval = _mm_aesenc_si128(mval, _mm_loadu_si128(p));
			mval2 = _mm_aesenc_si128(mval2, _mm_loadu_si128((void*)((char*)p + 16)));
			mval3 = _mm_aesenc_si128(mval3, _mm_loadu_si128((void*)((char*)p + 32)));
			mval4 = _mm_aesenc_si128(mval4, _mm_loadu_si128((void*)((char*)p + 48)));

			// Scramble state.
			mval = _mm_aesenc_si128(mval, mval);
			mval2 = _mm_aesenc_si128(mval2, mval2);
			mval3 = _mm_aesenc_si128(mval3, mval3);
			mval4 = _mm_aesenc_si128(mval4, mval4);

			p = (void*)((char*)p + 64);
		} while (--size > 0);

		// 2 more scrambles to finish.
		mval = _mm_aesenc_si128(mval, mval);
		mval2 = _mm_aesenc_si128(mval2, mval2);
		mval3 = _mm_aesenc_si128(mval3, mval3);
		mval4 = _mm_aesenc_si128(mval4, mval4);

		mval = _mm_aesenc_si128(mval, mval);
		mval2 = _mm_aesenc_si128(mval2, mval2);
		mval3 = _mm_aesenc_si128(mval3, mval3);
		mval4 = _mm_aesenc_si128(mval4, mval4);

		mval ^= mval3;
		mval2 ^= mval4;
		mval ^= mval2;
		return _mm_cvtsi128_si32(mval);
	}
}

#endif // !defined(__x86_64__)

#else // !defined(__i386__) && !defined(__x86_64__) || !defined(HAVE_AS_X86_AES)

uintptr aeshashbody(void* p __attribute__((unused)),
		    uintptr seed __attribute__((unused)),
		    uintptr size __attribute__((unused)),
		    Slice aeskeysched __attribute__((unused))) {
	// We should never get here on a non-x86 system.
	runtime_throw("impossible call to aeshashbody");
}

#endif // !defined(__i386__) && !defined(__x86_64__) || !defined(HAVE_AS_X86_AES)
