
/*
 * Copyright (c) 1996, 1997 by Doug Bell <dbell@shvn.com>.  All Rights Reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */


// This file has been hacked to compile without the rest of the
// benchmark code.

class OperatorBenchmark {

  public int getSampleCount() { return 0; }
  public int getSampleMillis() { return 0; }
  public boolean go;
  public int useint[];
  public void startTest () { }
  public long finishTest () { return 0; }
  public void startTimer (boolean b) { }
  public void stopTimer (int a, int b) { }
  public void report  (String s) { }
  public void println  (String s) { }

	public int getTestTime () {
		return (int) (100 * getSampleCount() * getSampleMillis()) / 1000;
	}

	public int getRunningTime () {
		return (int) (1.1 * getTestTime());
	}

	public long	runTest () {
		int			dummy1 = 0, dummy2 = 0, dummy3 = 0;  // occupy implicit index slots
		int			cnt, ii;
		byte		b1 = 1, b2 = 2, b3 = 3;
		short		s1 = 1, s2 = 2, s3 = 3;
		int			i1 = 1, i2 = 2, i3 = 3;
		long		l1 = 1, l2 = 2, l3 = 3;
		float		f1 = 1, f2 = 2, f3 = 3;
		double		d1 = 1, d2 = 2, d3 = 3;

		startTest();

		println("--- byte operators, local vars");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				b1++;
			stopTimer(cnt, ii);
		}
		report("byte++");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				b1 += b2;
			stopTimer(cnt, ii);
		}
		report("byte += byte");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				b1 = (byte) (b2 + b3);
			stopTimer(cnt, ii);
		}
		report("byte = byte + byte");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				b1 *= b2;
			stopTimer(cnt, ii);
		}
		report("byte *= byte");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				b1 = (byte) (b2 * b3);
			stopTimer(cnt, ii);
		}
		report("byte = byte * byte");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				b1 *= 2;
			stopTimer(cnt, ii);
		}
		report("byte *= 2");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				b1 <<= 1;
			stopTimer(cnt, ii);
		}
		report("byte <<= 1");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				b1 %= b2;
			stopTimer(cnt, ii);
		}
		report("byte %= byte");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				b1 = (byte) (b2 % b3);
			stopTimer(cnt, ii);
		}
		report("byte = byte % byte");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				b1 /= b2;
			stopTimer(cnt, ii);
		}
		report("byte /= byte");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				b1 = (byte) (b2 / b3);
			stopTimer(cnt, ii);
		}
		report("byte = byte / byte");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				b1 /= 2;
			stopTimer(cnt, ii);
		}
		report("byte /= 2");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				b1 >>= 1;
			stopTimer(cnt, ii);
		}
		report("byte >>= 1");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				b1 >>= i2;
			stopTimer(cnt, ii);
		}
		report("byte >>= int");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				b1 = (byte) (b2 >> i3);
			stopTimer(cnt, ii);
		}
		report("byte = byte >> int");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				b1 |= b2;
			stopTimer(cnt, ii);
		}
		report("byte |= byte");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				b1 = (byte) (b2 | b3);
			stopTimer(cnt, ii);
		}
		report("byte = byte | byte");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				b1 &= b2;
			stopTimer(cnt, ii);
		}
		report("byte &= byte");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				b1 = (byte) (b2 & b3);
			stopTimer(cnt, ii);
		}
		report("byte = byte & byte");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				b1 ^= b2;
			stopTimer(cnt, ii);
		}
		report("byte ^= byte");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				b1 = (byte) (b2 ^ b3);
			stopTimer(cnt, ii);
		}
		report("byte = byte ^ byte");


		println("--- short operators, local vars");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				s1++;
			stopTimer(cnt, ii);
		}
		report("short++");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				s1 += s2;
			stopTimer(cnt, ii);
		}
		report("short += short");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				s1 = (short) (s2 + s3);
			stopTimer(cnt, ii);
		}
		report("short = short + short");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				s1 *= s2;
			stopTimer(cnt, ii);
		}
		report("short *= short");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				s1 = (short) (s2 * s3);
			stopTimer(cnt, ii);
		}
		report("short = short * short");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				s1 *= 2;
			stopTimer(cnt, ii);
		}
		report("short *= 2");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				s1 <<= 1;
			stopTimer(cnt, ii);
		}
		report("short <<= 1");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				s1 %= s2;
			stopTimer(cnt, ii);
		}
		report("short %= short");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				s1 = (short) (s2 % s3);
			stopTimer(cnt, ii);
		}
		report("short = short % short");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				s1 /= s2;
			stopTimer(cnt, ii);
		}
		report("short /= short");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				s1 = (short) (s2 / s3);
			stopTimer(cnt, ii);
		}
		report("short = short / short");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				s1 /= 2;
			stopTimer(cnt, ii);
		}
		report("short /= 2");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				s1 >>= 1;
			stopTimer(cnt, ii);
		}
		report("short >>= 1");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				s1 >>= i2;
			stopTimer(cnt, ii);
		}
		report("short >>= int");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				s1 = (short) (s2 >> i3);
			stopTimer(cnt, ii);
		}
		report("short = short >> int");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				s1 |= s2;
			stopTimer(cnt, ii);
		}
		report("short |= short");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				s1 = (short) (s2 | s3);
			stopTimer(cnt, ii);
		}
		report("short = short | short");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				s1 &= s2;
			stopTimer(cnt, ii);
		}
		report("short &= short");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				s1 = (short) (s2 & s3);
			stopTimer(cnt, ii);
		}
		report("short = short & short");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				s1 ^= s2;
			stopTimer(cnt, ii);
		}
		report("short ^= short");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				s1 = (short) (s2 ^ s3);
			stopTimer(cnt, ii);
		}
		report("short = short ^ short");


		println("--- int operators, local vars");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				i1++;
			stopTimer(cnt, ii);
		}
		report("int++");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				i1 += i2;
			stopTimer(cnt, ii);
		}
		report("int += int");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				i1 = (i2 + i3);
			stopTimer(cnt, ii);
		}
		report("int = int + int");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				i1 *= i2;
			stopTimer(cnt, ii);
		}
		report("int *= int");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				i1 = (i2 * i3);
			stopTimer(cnt, ii);
		}
		report("int = int * int");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				i1 *= 2;
			stopTimer(cnt, ii);
		}
		report("int *= 2");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				i1 <<= 1;
			stopTimer(cnt, ii);
		}
		report("int <<= 1");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				i1 %= i2;
			stopTimer(cnt, ii);
		}
		report("int %= int");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				i1 = (i2 % i3);
			stopTimer(cnt, ii);
		}
		report("int = int % int");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				i1 /= i2;
			stopTimer(cnt, ii);
		}
		report("int /= int");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				i1 = (i2 / i3);
			stopTimer(cnt, ii);
		}
		report("int = int / int");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				i1 /= 2;
			stopTimer(cnt, ii);
		}
		report("int /= 2");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				i1 >>= 1;
			stopTimer(cnt, ii);
		}
		report("int >>= 1");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				i1 >>= i2;
			stopTimer(cnt, ii);
		}
		report("int >>= int");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				i1 = i2 >> i3;
			stopTimer(cnt, ii);
		}
		report("int = int >> int");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				i1 |= i2;
			stopTimer(cnt, ii);
		}
		report("int |= int");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				i1 = i2 | i3;
			stopTimer(cnt, ii);
		}
		report("int = int | int");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				i1 &= i2;
			stopTimer(cnt, ii);
		}
		report("int &= int");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				i1 = i2 & i3;
			stopTimer(cnt, ii);
		}
		report("int = int & int");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				i1 ^= i2;
			stopTimer(cnt, ii);
		}
		report("int ^= int");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				i1 = i2 ^ i3;
			stopTimer(cnt, ii);
		}
		report("int = int ^ int");


		println("--- long operators, local vars");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				l1++;
			stopTimer(cnt, ii);
		}
		report("long++");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				l1 += l2;
			stopTimer(cnt, ii);
		}
		report("long += long");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				l1 = (l2 + l3);
			stopTimer(cnt, ii);
		}
		report("long = long + long");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				l1 *= l2;
			stopTimer(cnt, ii);
		}
		report("long *= long");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				l1 = (l2 * l3);
			stopTimer(cnt, ii);
		}
		report("long = long * long");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				l1 *= 2;
			stopTimer(cnt, ii);
		}
		report("long *= 2");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				l1 <<= 1;
			stopTimer(cnt, ii);
		}
		report("long <<= 1");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				l1 %= l2;
			stopTimer(cnt, ii);
		}
		report("long %= long");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				l1 = (l2 % l3);
			stopTimer(cnt, ii);
		}
		report("long = long % long");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				l1 /= l2;
			stopTimer(cnt, ii);
		}
		report("long /= long");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				l1 = (l2 / l3);
			stopTimer(cnt, ii);
		}
		report("long = long / long");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				l1 /= 2;
			stopTimer(cnt, ii);
		}
		report("long /= 2");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				l1 >>= 1;
			stopTimer(cnt, ii);
		}
		report("long >>= 1");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				l1 >>= i2;
			stopTimer(cnt, ii);
		}
		report("long >>= int");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				l1 = l2 >> i3;
			stopTimer(cnt, ii);
		}
		report("long = long >> int");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				l1 |= l2;
			stopTimer(cnt, ii);
		}
		report("long |= long");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				l1 = l2 | l3;
			stopTimer(cnt, ii);
		}
		report("long = long | long");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				l1 &= l2;
			stopTimer(cnt, ii);
		}
		report("long &= long");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				l1 = l2 & l3;
			stopTimer(cnt, ii);
		}
		report("long = long & long");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				l1 ^= l2;
			stopTimer(cnt, ii);
		}
		report("long ^= long");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				l1 = l2 ^ l3;
			stopTimer(cnt, ii);
		}
		report("long = long ^ long");


		println("--- float operators, local vars");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				f1 += f2;
			stopTimer(cnt, ii);
		}
		report("float += float");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				f1 = (float) (f2 + f3);
			stopTimer(cnt, ii);
		}
		report("float = float + float");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				f1 *= f2;
			stopTimer(cnt, ii);
		}
		report("float *= float");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				f1 = (float) (f2 * f3);
			stopTimer(cnt, ii);
		}
		report("float = float * float");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				f1 %= f2;
			stopTimer(cnt, ii);
		}
		report("float %= float");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				f1 = (float) (f2 % f3);
			stopTimer(cnt, ii);
		}
		report("float = float % float");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				f1 /= f2;
			stopTimer(cnt, ii);
		}
		report("float /= float");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				f1 = (float) (f2 / f3);
			stopTimer(cnt, ii);
		}
		report("float = float / float");


		println("--- double operators, local vars");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				d1 += d2;
			stopTimer(cnt, ii);
		}
		report("double += double");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				d1 = (d2 + d3);
			stopTimer(cnt, ii);
		}
		report("double = double + double");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				d1 *= d2;
			stopTimer(cnt, ii);
		}
		report("double *= double");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				d1 = (d2 * d3);
			stopTimer(cnt, ii);
		}
		report("double = double * double");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				d1 %= d2;
			stopTimer(cnt, ii);
		}
		report("double %= double");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				d1 = (d2 % d3);
			stopTimer(cnt, ii);
		}
		report("double = double % double");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				d1 /= d2;
			stopTimer(cnt, ii);
		}
		report("double /= double");

		for (cnt = getSampleCount();  --cnt >= 0; ) {
			startTimer(true);
			for (ii = 0;  go;  ii++)
				d1 = (d2 / d3);
			stopTimer(cnt, ii);
		}
		report("double = double / double");

		useint[0] = dummy1;  useint[1] = dummy2;  useint[2] = dummy3;
		return finishTest();
	}
}  // class OperatorBenchmark

// EOF
