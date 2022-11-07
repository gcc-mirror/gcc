/* Unicode characters and various properties.
   Copyright (C) 2003-2022 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.


   Copyright (C) 1991-2005 Unicode, Inc.  All rights reserved.
   Distributed under the Terms of Use in
   http://www.unicode.org/copyright.html.

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of the Unicode data files and any associated
   documentation (the "Data Files") or Unicode software and any
   associated documentation (the "Software") to deal in the Data Files
   or Software without restriction, including without limitation the
   rights to use, copy, modify, merge, publish, distribute, and/or
   sell copies of the Data Files or Software, and to permit persons to
   whom the Data Files or Software are furnished to do so, provided
   that (a) the above copyright notice(s) and this permission notice
   appear with all copies of the Data Files or Software, (b) both the
   above copyright notice(s) and this permission notice appear in
   associated documentation, and (c) there is clear notice in each
   modified Data File or in the Software as well as in the
   documentation associated with the Data File(s) or Software that the
   data or software has been modified.

   THE DATA FILES AND SOFTWARE ARE PROVIDED "AS IS", WITHOUT WARRANTY
   OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
   WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT OF THIRD PARTY RIGHTS. IN NO EVENT SHALL THE
   COPYRIGHT HOLDER OR HOLDERS INCLUDED IN THIS NOTICE BE LIABLE FOR
   ANY CLAIM, OR ANY SPECIAL INDIRECT OR CONSEQUENTIAL DAMAGES, OR ANY
   DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
   WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
   ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
   OF THE DATA FILES OR SOFTWARE.

   Except as contained in this notice, the name of a copyright holder
   shall not be used in advertising or otherwise to promote the sale,
   use or other dealings in these Data Files or Software without prior
   written authorization of the copyright holder.  */

static const struct ucnrange ucnranges[] = {
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x00a7 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x00a8 },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x00a9 },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x00aa },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x00ac },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x00ad },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x00ae },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x00af },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x00b1 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x00b4 },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x00b5 },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x00b6 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x00b7 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x00b9 },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x00ba },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x00bb },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x00be },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x00bf },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x00d6 },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x00d7 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x00f6 },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x00f7 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0131 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x0133 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x013e },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x0140 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0148 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x0149 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x017e },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x017f },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x01c3 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x01cc },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x01d4 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x01dc },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x01dd },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x01e3 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x01eb },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x01ef },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x01f0 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x01f3 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x01f5 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x01f9 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x01ff },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0217 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0229 },
{   0|  0|  0|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x022d },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x022f },
{   0|  0|  0|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x0231 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x024f },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x02a8 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x02af },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x02b8 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x02ba },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x02bb },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x02bc },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x02c1 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x02c5 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x02cf },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x02d1 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x02d7 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x02dd },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x02df },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x02e4 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x02eb },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x02ec },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x02ed },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x02ee },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x02ff },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|CTX, 230, 0x0304 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x0305 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|CTX, 230, 0x030c },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x030e },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|CTX, 230, 0x030f },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x0310 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|CTX, 230, 0x0311 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x0312 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|CTX, 230, 0x0314 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 232, 0x0315 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x0319 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 232, 0x031a },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|CTX, 216, 0x031b },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x0320 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 202, 0x0322 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|CTX, 220, 0x0326 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|CTX, 202, 0x0328 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x032c },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|CTX, 220, 0x032e },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x032f },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|CTX, 220, 0x0331 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x0333 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0,   1, 0x0337 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|CTX,   1, 0x0338 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x033c },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x033f },
{   0|  0|  0|C11|N11|CXX23|NXX23|  0|  0|  0|  0, 230, 0x0341 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|CTX, 230, 0x0342 },
{   0|  0|  0|C11|N11|CXX23|NXX23|  0|  0|  0|  0, 230, 0x0344 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|CTX, 240, 0x0345 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x0346 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x0349 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x034c },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x034e },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x034f },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x0352 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x0356 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x0357 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 232, 0x0358 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x035a },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x035b },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 233, 0x035c },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 234, 0x035e },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 233, 0x035f },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 234, 0x0361 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 233, 0x0362 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x036f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0373 },
{   0|  0|  0|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x0374 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0375 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0377 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0379 },
{ C99|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x037a },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x037d },
{   0|  0|  0|C11|  0|    0|    0|CID|  0|  0|  0,   0, 0x037e },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x037f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0383 },
{   0|  0|CXX|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x0384 },
{   0|  0|  0|C11|  0|    0|    0|  0|NFC|  0|  0,   0, 0x0385 },
{ C99|  0|  0|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x0386 },
{   0|  0|  0|C11|  0|CXX23|NXX23|  0|  0|  0|  0,   0, 0x0387 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x038a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x038b },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x038c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x038d },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x0390 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x03a1 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x03a2 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x03a9 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x03b0 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x03c9 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x03ce },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x03cf },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x03d2 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|  0|  0,   0, 0x03d4 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x03d6 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x03d9 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x03da },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x03db },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x03dc },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x03dd },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x03de },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x03df },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x03e0 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x03e1 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x03ef },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x03f2 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x03f3 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x03f5 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x03f6 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x03f8 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x03f9 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x03ff },
{   0|  0|  0|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x0400 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x0401 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0402 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x0403 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0406 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x0407 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x040b },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x040c },
{   0|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x040d },
{ C99|  0|  0|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x040e },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0418 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x0419 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0438 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x0439 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x044f },
{   0|  0|  0|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x0450 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x0451 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0452 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x0453 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0456 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x0457 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x045b },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x045c },
{   0|  0|  0|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x045d },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x045e },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0475 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x0477 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0481 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0482 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x0487 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0489 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x048f },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x04c0 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x04c2 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x04c4 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x04c6 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x04c8 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x04ca },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x04cc },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x04cf },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x04d3 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x04d5 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x04d7 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x04d9 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x04df },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x04e1 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x04e7 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x04e9 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x04eb },
{   0|  0|  0|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x04ed },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x04f5 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x04f7 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x04f9 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x052f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0530 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0556 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0558 },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0559 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x055f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0560 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0586 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x0587 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0588 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0590 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x0591 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x0595 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x0596 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x0599 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 222, 0x059a },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x059b },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x05a1 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x05a7 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x05a9 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x05aa },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x05ac },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 222, 0x05ad },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 228, 0x05ae },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x05af },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  10, 0x05b0 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  11, 0x05b1 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  12, 0x05b2 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  13, 0x05b3 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  14, 0x05b4 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  15, 0x05b5 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  16, 0x05b6 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  17, 0x05b7 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  18, 0x05b8 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  19, 0x05b9 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  19, 0x05ba },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  20, 0x05bb },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  21, 0x05bc },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  22, 0x05bd },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x05be },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  23, 0x05bf },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x05c0 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  24, 0x05c1 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  25, 0x05c2 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x05c3 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x05c4 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x05c5 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x05c6 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  18, 0x05c7 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x05cf },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x05ea },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x05ee },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x05ef },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x05f2 },
{   0|  0|CXX|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x05f4 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x060f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x0617 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  30, 0x0618 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  31, 0x0619 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  32, 0x061a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x061f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0620 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0621 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x0626 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x063a },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x063f },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x064a },
{ C99|  0|CXX|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  27, 0x064b },
{ C99|  0|CXX|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  28, 0x064c },
{ C99|  0|CXX|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  29, 0x064d },
{ C99|  0|CXX|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  30, 0x064e },
{ C99|  0|CXX|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  31, 0x064f },
{ C99|  0|CXX|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  32, 0x0650 },
{ C99|  0|CXX|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  33, 0x0651 },
{ C99|  0|CXX|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  34, 0x0652 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|CTX, 230, 0x0654 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|CTX, 220, 0x0655 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x0656 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x065b },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x065c },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x065e },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x065f },
{ C99|N99|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0669 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x066d },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x066f },
{ C99|  0|CXX|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  35, 0x0670 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0674 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x0678 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x06b7 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x06b9 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x06be },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x06bf },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x06c0 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x06c1 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x06c2 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x06ce },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x06cf },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x06d2 },
{ C99|  0|  0|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x06d3 },
{ C99|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x06d4 },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x06d5 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x06dc },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x06de },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x06e2 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x06e3 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x06e4 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x06e6 },
{ C99|  0|CXX|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x06e7 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x06e8 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x06e9 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x06ea },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x06ec },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x06ed },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x06ef },
{ C99|N99|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x06f9 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x06fc },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x06fe },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x06ff },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x070f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0710 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  36, 0x0711 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x072f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x0730 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x0731 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x0733 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x0734 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x0736 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x0739 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x073a },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x073c },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x073d },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x073e },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x0741 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x0742 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x0743 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x0744 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x0745 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x0746 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x0747 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x0748 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x074a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x074c },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x07a5 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x07b0 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x07b1 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x07bf },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x07c9 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x07ea },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x07f1 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x07f2 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x07f3 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x07f5 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x07f9 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x07fa },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x07fc },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x07fd },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x07ff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0815 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x0819 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x081a },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x0823 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0824 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x0827 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0828 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x082d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x083f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0858 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x085b },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x085f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x086a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x086f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0887 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0888 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x088e },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0897 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x0898 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x089b },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x089f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x08c9 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x08ce },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x08d3 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x08e1 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x08e2 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x08e3 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x08e5 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x08e6 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x08e8 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x08e9 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x08ec },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x08ef },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  27, 0x08f0 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  28, 0x08f1 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  29, 0x08f2 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x08f5 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x08f6 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x08f8 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x08fa },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x08ff },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0900 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0903 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0904 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0928 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x0929 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0930 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x0931 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0933 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x0934 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0939 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x093b },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|CTX,   7, 0x093c },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x093d },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x094c },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x094d },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x094f },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0950 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x0951 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x0952 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x0954 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0957 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x095f },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0961 },
{ C99|  0|CXX|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0962 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0963 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0965 },
{ C99|N99|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x096f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0970 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0980 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0983 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0984 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x098c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x098e },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0990 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0992 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x09a8 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x09a9 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x09b0 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x09b1 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x09b2 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x09b5 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x09b9 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x09bb },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   7, 0x09bc },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x09bd },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|CTX,   0, 0x09be },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x09c4 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x09c6 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x09c8 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x09ca },
{ C99|  0|  0|C11|  0|CXX23|NXX23|  0|NFC|NKC|  0,   0, 0x09cc },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x09cd },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x09ce },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x09d6 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|CTX,   0, 0x09d7 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x09db },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x09dd },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x09de },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x09df },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x09e1 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x09e3 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x09e5 },
{ C99|N99|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x09ef },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x09f1 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x09fb },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x09fc },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x09fd },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x09fe },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0a00 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0a01 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0a02 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0a03 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0a04 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0a0a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0a0e },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0a10 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0a12 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0a28 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0a29 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0a30 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0a31 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0a32 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x0a33 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0a34 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0a35 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x0a36 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0a37 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0a39 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0a3b },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   7, 0x0a3c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0a3d },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0a42 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0a46 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0a48 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0a4a },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0a4c },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x0a4d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0a50 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0a51 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0a58 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x0a5b },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0a5c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0a5d },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x0a5e },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0a65 },
{ C99|N99|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0a6f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0a71 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0a73 },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0a74 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0a75 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0a80 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0a83 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0a84 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0a8b },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0a8c },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0a8d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0a8e },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0a91 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0a92 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0aa8 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0aa9 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0ab0 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0ab1 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0ab3 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0ab4 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0ab9 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0abb },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   7, 0x0abc },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0abd },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0ac5 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0ac6 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0ac9 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0aca },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0acc },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x0acd },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0acf },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0ad0 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0adf },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0ae0 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0ae1 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0ae3 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0ae5 },
{ C99|N99|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0aef },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0af8 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0af9 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0aff },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0b00 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0b03 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0b04 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0b0c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0b0e },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0b10 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0b12 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0b28 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0b29 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0b30 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0b31 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0b33 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0b34 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0b35 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0b39 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0b3b },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   7, 0x0b3c },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0b3d },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|CTX,   0, 0x0b3e },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0b43 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0b44 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0b46 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0b47 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|  0|NFC|NKC|  0,   0, 0x0b48 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0b4a },
{ C99|  0|  0|C11|  0|CXX23|NXX23|  0|NFC|NKC|  0,   0, 0x0b4c },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x0b4d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0b54 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0b55 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|CTX,   0, 0x0b57 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0b5b },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x0b5d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0b5e },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0b61 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0b63 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0b65 },
{ C99|N99|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0b6f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0b70 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0b71 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0b81 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0b82 },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0b83 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0b84 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0b8a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0b8d },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0b90 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0b91 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0b93 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x0b94 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0b95 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0b98 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0b9a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0b9b },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0b9c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0b9d },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0b9f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0ba2 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0ba4 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0ba7 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0baa },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0bad },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0bb5 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0bb6 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0bb9 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0bbd },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|CTX,   0, 0x0bbe },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0bc2 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0bc5 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0bc8 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0bc9 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|  0|NFC|NKC|  0,   0, 0x0bcc },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x0bcd },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0bcf },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0bd0 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0bd6 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|CTX,   0, 0x0bd7 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0be5 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0be6 },
{ C99|N99|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0bef },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0bff },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0c00 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0c03 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0c04 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0c0c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0c0d },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0c10 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0c11 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0c28 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0c29 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0c33 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0c34 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0c39 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0c3b },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   7, 0x0c3c },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0c3d },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0c44 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0c45 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0c47 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|  0|NFC|NKC|  0,   0, 0x0c48 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0c49 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0c4c },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x0c4d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0c54 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  84, 0x0c55 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|CTX,  91, 0x0c56 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0c57 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0c5a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0c5c },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0c5d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0c5f },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0c61 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0c63 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0c65 },
{ C99|N99|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0c6f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0c7f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0c80 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0c81 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0c83 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0c84 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0c8c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0c8d },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0c90 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0c91 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0ca8 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0ca9 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0cb3 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0cb4 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0cb9 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0cbb },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   7, 0x0cbc },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0cbd },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0cbf },
{ C99|  0|  0|C11|  0|CXX23|NXX23|  0|NFC|NKC|  0,   0, 0x0cc0 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0cc1 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|CTX,   0, 0x0cc2 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0cc4 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0cc5 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0cc6 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|  0|NFC|NKC|  0,   0, 0x0cc8 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0cc9 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|  0|NFC|NKC|  0,   0, 0x0ccb },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0ccc },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x0ccd },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0cd4 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|CTX,   0, 0x0cd6 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0cdc },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0cdd },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0cde },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0cdf },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0ce1 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0ce3 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0ce5 },
{ C99|N99|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0cef },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0cf0 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0cf2 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0cf3 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0cff },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0d01 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0d03 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0d04 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0d0c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0d0d },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0d10 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0d11 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0d28 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0d29 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0d39 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0d3a },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x0d3c },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0d3d },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|CTX,   0, 0x0d3e },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0d43 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0d44 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0d45 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0d48 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0d49 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|  0|NFC|NKC|  0,   0, 0x0d4c },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x0d4d },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0d4e },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0d53 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0d56 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|CTX,   0, 0x0d57 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0d5e },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0d5f },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0d61 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0d63 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0d65 },
{ C99|N99|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0d6f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0d79 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0d7f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0d80 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0d83 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0d84 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0d96 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0d99 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0db1 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0db2 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0dbb },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0dbc },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0dbd },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0dbf },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0dc6 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0dc9 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|CTX,   9, 0x0dca },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0dce },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|CTX,   0, 0x0dcf },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0dd4 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0dd5 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0dd6 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0dd7 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0dd9 },
{   0|  0|  0|C11|  0|CXX23|NXX23|  0|NFC|NKC|  0,   0, 0x0dda },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0ddb },
{   0|  0|  0|C11|  0|CXX23|NXX23|  0|NFC|NKC|  0,   0, 0x0dde },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|CTX,   0, 0x0ddf },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0de5 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0def },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0df1 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0df3 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0e00 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0e30 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0e31 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0e32 },
{ C99|  0|CXX|C11|  0|CXX23|NXX23|CID|NFC|  0|  0,   0, 0x0e33 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0e37 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 103, 0x0e39 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x0e3a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0e3f },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0e46 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0e47 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 107, 0x0e49 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 107, 0x0e4b },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0e4e },
{   0|  0|CXX|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0e4f },
{ C99|N99|CXX|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0e59 },
{ C99|  0|CXX|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0e5b },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0e80 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0e82 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0e83 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0e84 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0e85 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0e86 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0e88 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0e89 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0e8a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0e8b },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0e8c },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0e8d },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0e93 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0e97 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0e98 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0e9f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0ea0 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0ea3 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0ea4 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0ea5 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0ea6 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0ea7 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0ea9 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0eab },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0eac },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0eae },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0eaf },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0eb0 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0eb1 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0eb2 },
{ C99|  0|CXX|C11|  0|CXX23|NXX23|CID|NFC|  0|  0,   0, 0x0eb3 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0eb7 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 118, 0x0eb9 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x0eba },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0ebc },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0ebd },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0ebf },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0ec4 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0ec5 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0ec6 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0ec7 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 122, 0x0ecb },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0ecd },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0ece },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0ecf },
{ C99|N99|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0ed9 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0edb },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x0edd },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0edf },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0eff },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0f00 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0f0b },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x0f0c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0f17 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x0f19 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0f1f },
{ C99|N99|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0f29 },
{ C99|N99|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0f33 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0f34 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x0f35 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0f36 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x0f37 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0f38 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 216, 0x0f39 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0f3d },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0f3f },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0f42 },
{ C99|  0|  0|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x0f43 },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0f47 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0f48 },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0f4c },
{ C99|  0|  0|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x0f4d },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0f51 },
{ C99|  0|  0|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x0f52 },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0f56 },
{ C99|  0|  0|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x0f57 },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0f5b },
{ C99|  0|  0|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x0f5c },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0f68 },
{ C99|  0|  0|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x0f69 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0f6c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0f70 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 129, 0x0f71 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 130, 0x0f72 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|  0|  0|  0|  0,   0, 0x0f73 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 132, 0x0f74 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|  0|  0|  0|  0,   0, 0x0f76 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|  0|  0,   0, 0x0f77 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|  0|  0|  0|  0,   0, 0x0f78 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|  0|  0,   0, 0x0f79 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 130, 0x0f7d },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0f7f },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 130, 0x0f80 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|  0|  0|  0|  0,   0, 0x0f81 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x0f83 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x0f84 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0f85 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x0f87 },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0f8b },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x0f8c },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0f8f },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0f92 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|  0|  0|  0|  0,   0, 0x0f93 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0f95 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0f96 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0f97 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0f98 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0f9c },
{ C99|  0|  0|C11|  0|CXX23|NXX23|  0|  0|  0|  0,   0, 0x0f9d },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0fa1 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|  0|  0|  0|  0,   0, 0x0fa2 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0fa6 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|  0|  0|  0|  0,   0, 0x0fa7 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0fab },
{ C99|  0|  0|C11|  0|CXX23|NXX23|  0|  0|  0|  0,   0, 0x0fac },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0fad },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0fb0 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0fb7 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0fb8 },
{ C99|  0|  0|C11|  0|CXX23|NXX23|  0|  0|  0|  0,   0, 0x0fb9 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x0fbc },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0fc5 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x0fc6 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x0fff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1025 },
{   0|  0|  0|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1026 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x102a },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x102d },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|CTX,   0, 0x102e },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1036 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   7, 0x1037 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1038 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x103a },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x103e },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x103f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1049 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x104f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1055 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1059 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x105d },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1060 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1061 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1064 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1066 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x106d },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1070 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1074 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1081 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x108c },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x108d },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x108e },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x109d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x109f },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10c5 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10c6 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10c7 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10cc },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10cd },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10cf },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10f6 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10fa },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10fb },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x10fc },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10ff },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1159 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1160 },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|CTX,   0, 0x1175 },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11a2 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11a7 },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|CTX,   0, 0x11c2 },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11f9 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1248 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1249 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x124d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x124f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1256 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1257 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1258 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1259 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x125d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x125f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1288 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1289 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x128d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x128f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x12b0 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x12b1 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x12b5 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x12b7 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x12be },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x12bf },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x12c0 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x12c1 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x12c5 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x12c7 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x12d6 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x12d7 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1310 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1311 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1315 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1317 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x135a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x135c },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x135f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1368 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1371 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x137f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x138f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x139f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x13f5 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x13f7 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x13fd },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1400 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x166c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x166e },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x167f },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1680 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x169a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x169f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x16ea },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x16ed },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x16f8 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x16ff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1711 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1713 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x1715 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x171e },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1731 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1733 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x1734 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x173f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1751 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1753 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x175f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x176c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x176d },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1770 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1771 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1773 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x177f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x17b3 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x17d1 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x17d2 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x17d3 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x17d6 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x17d7 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x17db },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x17dc },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x17dd },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x17df },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x17e9 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x180a },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x180d },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x180e },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1819 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x181f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1878 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x187f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x18a8 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 228, 0x18a9 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x18aa },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x18af },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x18f5 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x18ff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x191e },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x191f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x192b },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x192f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1938 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 222, 0x1939 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x193a },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x193b },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1945 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x194f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x196d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x196f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1974 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x197f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x19ab },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x19af },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x19c9 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x19cf },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x19da },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x19ff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1a16 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1a17 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x1a18 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1a1b },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1a1f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1a54 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1a5e },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1a5f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x1a60 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1a74 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1a7c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1a7e },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x1a7f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1a89 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1a8f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1a99 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1aa6 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1aa7 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1aaf },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1ab4 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x1aba },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1abc },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x1abd },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1abe },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x1ac0 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1ac2 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x1ac4 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1ac9 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x1aca },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1ace },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1aff },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1b04 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1b05 },
{   0|  0|  0|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1b06 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1b07 },
{   0|  0|  0|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1b08 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1b09 },
{   0|  0|  0|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1b0a },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1b0b },
{   0|  0|  0|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1b0c },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1b0d },
{   0|  0|  0|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1b0e },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1b11 },
{   0|  0|  0|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1b12 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1b33 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   7, 0x1b34 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|CTX,   0, 0x1b35 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1b3a },
{   0|  0|  0|C11|  0|CXX23|NXX23|  0|NFC|NKC|  0,   0, 0x1b3b },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1b3c },
{   0|  0|  0|C11|  0|CXX23|NXX23|  0|NFC|NKC|  0,   0, 0x1b3d },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1b3f },
{   0|  0|  0|C11|  0|CXX23|NXX23|  0|NFC|NKC|  0,   0, 0x1b41 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1b42 },
{   0|  0|  0|C11|  0|CXX23|NXX23|  0|NFC|NKC|  0,   0, 0x1b43 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x1b44 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1b4c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1b4f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1b59 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1b6a },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1b6b },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x1b6c },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1b73 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1b7f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1b82 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1ba0 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1ba9 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x1bab },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1bad },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1baf },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1bb9 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1be5 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   7, 0x1be6 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1bf1 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x1bf3 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1bff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1c23 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1c36 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   7, 0x1c37 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1c3f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1c49 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1c4c },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1c4f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1c59 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1c7d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1c7f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1c88 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1c8f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1cba },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1cbc },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1cbf },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ccf },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1cd2 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1cd3 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   1, 0x1cd4 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x1cd9 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1cdb },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x1cdf },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1ce0 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1ce1 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   1, 0x1ce8 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1cec },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x1ced },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1cf3 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1cf4 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1cf6 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1cf7 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1cf9 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1cfa },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1cff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1d2b },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d2e },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1d2f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d3a },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1d3b },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d4d },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1d4e },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d6a },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1d77 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d78 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1d9a },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1dbf },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1dc1 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x1dc2 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1dc9 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x1dca },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1dcc },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 234, 0x1dcd },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 214, 0x1dce },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x1dcf },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 202, 0x1dd0 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1df5 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 232, 0x1df6 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 228, 0x1df8 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x1df9 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 218, 0x1dfa },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1dfb },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 233, 0x1dfc },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x1dfd },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1dfe },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x1dff },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1e07 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1e09 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1e13 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1e17 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1e1b },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1e1d },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1e2d },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1e2f },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1e37 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1e39 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1e4b },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1e53 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1e5b },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1e5d },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1e63 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1e69 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1e77 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1e7b },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1e99 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1e9a },
{ C99|  0|  0|C11|  0|CXX23|    0|  0|NFC|  0|  0,   0, 0x1e9b },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1e9f },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1ea3 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1eb7 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1ebd },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1ec7 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1ecf },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1ee3 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1ee7 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1ef1 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1ef9 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1eff },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1f15 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1f17 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1f1d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1f1f },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1f45 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1f47 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1f4d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1f4f },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1f57 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1f58 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1f59 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1f5a },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1f5b },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1f5c },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1f5d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1f5e },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1f70 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x1f71 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1f72 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x1f73 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1f74 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x1f75 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1f76 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x1f77 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1f78 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x1f79 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1f7a },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x1f7b },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1f7c },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x1f7d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1f7f },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1fb4 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1fb5 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1fba },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x1fbb },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1fbc },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x1fbd },
{ C99|  0|  0|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x1fbe },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x1fc0 },
{   0|  0|  0|C11|  0|    0|    0|  0|NFC|  0|  0,   0, 0x1fc1 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1fc4 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1fc5 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1fc8 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x1fc9 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1fca },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x1fcb },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1fcc },
{   0|  0|  0|C11|  0|    0|    0|  0|NFC|  0|  0,   0, 0x1fcf },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1fd2 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x1fd3 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1fd5 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1fda },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x1fdb },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1fdc },
{   0|  0|  0|C11|  0|    0|    0|  0|NFC|  0|  0,   0, 0x1fdf },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1fe2 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x1fe3 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1fea },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x1feb },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1fec },
{   0|  0|  0|C11|  0|    0|    0|  0|NFC|  0|  0,   0, 0x1fed },
{   0|  0|  0|C11|  0|    0|    0|  0|  0|  0|  0,   0, 0x1fee },
{   0|  0|  0|C11|  0|    0|    0|CID|  0|  0|  0,   0, 0x1fef },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ff1 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1ff4 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ff5 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1ff8 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x1ff9 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1ffa },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x1ffb },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1ffc },
{   0|  0|  0|C11|  0|    0|    0|  0|  0|  0|  0,   0, 0x1ffd },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x1ffe },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1fff },
{   0|  0|  0|  0|  0|    0|    0|CID|  0|  0|  0,   0, 0x200a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x200d },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2029 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x202e },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|  0|  0,   0, 0x203e },
{ C99|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x2040 },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2053 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x2054 },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x205f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x206f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x2070 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x2071 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2073 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x207e },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x207f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x208e },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x208f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x209c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x20a7 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x20a8 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x20cf },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x20d1 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0,   1, 0x20d3 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x20d7 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0,   1, 0x20da },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x20dc },
{   0|  0|  0|C11|N11|    0|    0|CID|NFC|NKC|  0,   0, 0x20e0 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x20e1 },
{   0|  0|  0|C11|N11|    0|    0|CID|NFC|NKC|  0,   0, 0x20e4 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0,   1, 0x20e6 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x20e7 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x20e8 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x20e9 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0,   1, 0x20eb },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x20ef },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x20f0 },
{   0|  0|  0|C11|N11|    0|    0|CID|NFC|NKC|  0,   0, 0x20ff },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x2101 },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x2102 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x2103 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2104 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x2106 },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x2107 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2108 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x2109 },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x2113 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2114 },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x2115 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x2116 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2117 },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x2118 },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x211d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x211f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x2122 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2123 },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x2124 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2125 },
{ C99|  0|  0|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x2126 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2127 },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x2128 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2129 },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|  0|  0|  0,   0, 0x212a },
{ C99|  0|  0|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x212b },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x212d },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x212e },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x2131 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x2132 },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x2138 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x2139 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x213a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x213b },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x213f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x2140 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2144 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x2149 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x214d },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x214e },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x214f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x215f },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x217f },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x2182 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x2188 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x2189 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x218f },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x245f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x24ea },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x24ff },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2775 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2793 },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2bff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x2c7b },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x2c7d },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x2ce4 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2cea },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x2cee },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x2cf1 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x2cf3 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2cff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x2d25 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2d26 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x2d27 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2d2c },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x2d2d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2d2f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x2d67 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2d6e },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x2d6f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2d7e },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x2d7f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x2d96 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2d9f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x2da6 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2da7 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x2dae },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2daf },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x2db6 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2db7 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x2dbe },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2dbf },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x2dc6 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2dc7 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x2dce },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2dcf },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x2dd6 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2dd7 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x2dde },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2ddf },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x2dff },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2e7f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2e9e },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x2e9f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2ef2 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x2ef3 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2eff },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x2fd5 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2fff },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|  0|  0,   0, 0x3003 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x3004 },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x3007 },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x3020 },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x3029 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 218, 0x302a },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 228, 0x302b },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 232, 0x302c },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 222, 0x302d },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 224, 0x302f },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x3030 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x3035 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x3036 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x3037 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x303a },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x303c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x3040 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x304b },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x304c },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x304d },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x304e },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x304f },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x3050 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x3051 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x3052 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x3053 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x3054 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x3055 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x3056 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x3057 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x3058 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x3059 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x305a },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x305b },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x305c },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x305d },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x305e },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x305f },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x3060 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x3061 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x3062 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x3064 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x3065 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x3066 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x3067 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x3068 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x3069 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x306f },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x3071 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x3072 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x3074 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x3075 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x3077 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x3078 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x307a },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x307b },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x307d },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x3093 },
{   0|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x3094 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x3096 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x3098 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|CTX,   8, 0x309a },
{ C99|  0|CXX|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x309c },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x309d },
{   0|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x309e },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x309f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x30a0 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x30ab },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x30ac },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x30ad },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x30ae },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x30af },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x30b0 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x30b1 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x30b2 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x30b3 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x30b4 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x30b5 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x30b6 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x30b7 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x30b8 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x30b9 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x30ba },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x30bb },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x30bc },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x30bd },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x30be },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x30bf },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x30c0 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x30c1 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x30c2 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x30c4 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x30c5 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x30c6 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x30c7 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x30c8 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x30c9 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x30cf },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x30d1 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x30d2 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x30d4 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x30d5 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x30d7 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x30d8 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x30da },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x30db },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x30dd },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x30f3 },
{ C99|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x30f4 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x30f6 },
{   0|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x30fa },
{ C99|  0|CXX|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x30fb },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x30fc },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x30fd },
{   0|  0|CXX|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x30fe },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x30ff },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x3104 },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x312c },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x312f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x3130 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x318e },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x3191 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x319f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x31bf },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x31ef },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x31ff },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x321e },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x321f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x3247 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x324f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x327e },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x327f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x33ff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x4dbf },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x4dff },
{ C99|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x9fa5 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa48c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xa4cf },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa4fd },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xa4ff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa60c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xa60f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa61f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0xa629 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa62b },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xa63f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa66e },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0xa66f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xa673 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0xa67d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xa67e },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa69b },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0xa69d },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0xa69f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa6ef },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0xa6f1 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xa716 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa71f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xa721 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa76f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0xa770 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa788 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xa78a },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa7ca },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xa7cf },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa7d1 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xa7d2 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa7d3 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xa7d4 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa7d9 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xa7f1 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0xa7f4 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa7f7 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0xa7f9 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa801 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0xa802 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa805 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0xa806 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa80a },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0xa80b },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa822 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0xa827 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xa82b },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0xa82c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xa83f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa873 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xa87f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0xa881 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa8b3 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0xa8c3 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0xa8c4 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0xa8c5 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xa8cf },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0xa8d9 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xa8df },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0xa8f1 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa8f7 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xa8fa },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa8fb },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xa8fc },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa8fe },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0xa909 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa925 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0xa92a },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0xa92d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xa92f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa946 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0xa952 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0xa953 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xa95f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa97c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xa97f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0xa983 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa9b2 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   7, 0xa9b3 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0xa9bf },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0xa9c0 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xa9ce },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa9cf },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0xa9d9 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xa9df },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa9e4 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0xa9e5 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa9ef },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0xa9f9 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xa9fe },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xa9ff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xaa28 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0xaa36 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xaa3f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xaa42 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0xaa43 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xaa4b },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0xaa4d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xaa4f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0xaa59 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xaa5f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xaa76 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xaa79 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xaa7a },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0xaa7d },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xaaaf },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0xaab0 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xaab1 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0xaab3 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0xaab4 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xaab6 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0xaab8 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xaabd },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0xaabf },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xaac0 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0xaac1 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xaac2 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xaada },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xaadd },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xaadf },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xaaea },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0xaaef },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xaaf1 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xaaf4 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0xaaf5 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0xaaf6 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xab00 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xab06 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xab08 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xab0e },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xab10 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xab16 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xab1f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xab26 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xab27 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xab2e },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xab2f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xab5a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xab5b },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0xab5f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xab68 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0xab69 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xab6f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xabe2 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0xabea },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xabeb },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0xabec },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0xabed },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xabef },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0xabf9 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xabff },
{ C99|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xd7a3 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xd7af },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xd7c6 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xd7ca },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xd7fb },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xd7ff },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xf8ff },
{   0|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0xfa0d },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xfa0f },
{   0|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0xfa10 },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xfa11 },
{   0|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0xfa12 },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xfa14 },
{   0|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0xfa1e },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xfa1f },
{   0|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0xfa20 },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xfa21 },
{   0|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0xfa22 },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xfa24 },
{   0|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0xfa26 },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xfa29 },
{   0|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0xfa2d },
{   0|  0|  0|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0xfa6d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xfa6f },
{   0|  0|  0|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0xfad9 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xfaff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0xfb06 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xfb12 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0xfb17 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xfb1c },
{   0|  0|  0|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0xfb1d },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,  26, 0xfb1e },
{   0|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0xfb1f },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0xfb28 },
{   0|  0|CXX|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0xfb29 },
{   0|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0xfb36 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xfb37 },
{   0|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0xfb3c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xfb3d },
{   0|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0xfb3e },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xfb3f },
{   0|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0xfb41 },
{   0|  0|CXX|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xfb42 },
{   0|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0xfb44 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xfb45 },
{   0|  0|CXX|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0xfb4e },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0xfbb1 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xfbd2 },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0xfc5d },
{   0|  0|CXX|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0xfc63 },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0xfd3d },
{   0|  0|CXX|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xfd3f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xfd4f },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0xfd8f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xfd91 },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0xfdc7 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xfdcf },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xfdef },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0xfdf9 },
{   0|  0|CXX|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0xfdfb },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0xfdfc },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xfdff },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0xfe0f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0xfe19 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xfe1f },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0xfe26 },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0xfe2d },
{   0|  0|  0|C11|N11|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0xfe2f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0xfe32 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|  0|  0,   0, 0xfe34 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0xfe44 },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xfe46 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0xfe4c },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|  0|  0,   0, 0xfe4f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0xfe52 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xfe53 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0xfe66 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xfe67 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0xfe6b },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xfe6f },
{   0|  0|CXX|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0xfe70 },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0xfe71 },
{   0|  0|CXX|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0xfe72 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0xfe73 },
{   0|  0|CXX|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0xfe74 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xfe75 },
{   0|  0|CXX|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0xfe76 },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0xfe77 },
{   0|  0|CXX|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0xfe78 },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0xfe79 },
{   0|  0|CXX|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0xfe7a },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0xfe7b },
{   0|  0|CXX|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0xfe7c },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0xfe7d },
{   0|  0|CXX|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0xfe7e },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0xfefc },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xff00 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0xff0f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|  0|  0,   0, 0xff19 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0xff20 },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0xff3a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0xff3e },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|  0|  0,   0, 0xff3f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0xff40 },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0xff5a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0xff65 },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0xff9d },
{   0|  0|CXX|C11|  0|CXX23|NXX23|CID|NFC|  0|  0,   0, 0xff9f },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0xffbe },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xffc1 },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0xffc7 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xffc9 },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0xffcf },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xffd1 },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0xffd7 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xffd9 },
{   0|  0|CXX|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0xffdc },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xffdf },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0xffe6 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xffe7 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0xffee },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xfffd },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xffff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1000b },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1000c },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10026 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10027 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1003a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1003b },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1003d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1003e },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1004d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1004f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1005d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1007f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x100fa },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1013f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10174 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x101fc },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x101fd },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1027f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1029c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1029f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x102d0 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x102df },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x102e0 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x102ff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1031f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1032c },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1034a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1034f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10375 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1037a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1037f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1039d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1039f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x103c3 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x103c7 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x103cf },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x103d0 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x103d5 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x103ff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1049d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1049f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x104a9 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x104af },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x104d3 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x104d7 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x104fb },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x104ff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10527 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1052f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10563 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1056f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1057a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1057b },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1058a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1058b },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10592 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10593 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10595 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10596 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x105a1 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x105a2 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x105b1 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x105b2 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x105b9 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x105ba },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x105bc },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x105ff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10736 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1073f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10755 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1075f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10767 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1077f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10780 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x10785 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10786 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x107b0 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x107b1 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x107ba },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x107ff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10805 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10807 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10808 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10809 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10835 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10836 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10838 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1083b },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1083c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1083e },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10855 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1085f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10876 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1087f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1089e },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x108df },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x108f2 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x108f3 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x108f5 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x108ff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10915 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1091f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10939 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1097f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x109b7 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x109bd },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x109bf },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x109ff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10a00 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x10a03 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10a04 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x10a06 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10a0b },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x10a0c },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x10a0d },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x10a0e },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x10a0f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10a13 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10a14 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10a17 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10a18 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10a35 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10a37 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x10a38 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   1, 0x10a39 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x10a3a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10a3e },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x10a3f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10a5f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10a7c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10a7f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10a9c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10abf },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10ac7 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10ac8 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10ae4 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x10ae5 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x10ae6 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10aff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10b35 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10b3f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10b55 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10b5f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10b72 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10b7f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10b91 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10bff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10c48 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10c7f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10cb2 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10cbf },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10cf2 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10cff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10d23 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x10d27 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10d2f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x10d39 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10e7f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10ea9 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10eaa },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x10eac },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10eaf },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10eb1 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10efc },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x10eff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10f1c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10f26 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10f27 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10f2f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10f45 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x10f47 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x10f4a },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x10f4b },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x10f4c },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x10f50 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10f6f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10f81 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x10f82 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x10f83 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x10f84 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x10f85 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10faf },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10fc4 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10fdf },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x10ff6 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10fff },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11002 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11037 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11045 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x11046 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11065 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1106f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x11070 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11072 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11074 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11075 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1107e },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x1107f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11082 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11099 },
{   0|  0|  0|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1109a },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1109b },
{   0|  0|  0|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x1109c },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x110aa },
{   0|  0|  0|C11|  0|CXX23|    0|  0|NFC|NKC|  0,   0, 0x110ab },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x110af },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x110b8 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x110b9 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|CTX,   7, 0x110ba },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x110c1 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x110c2 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x110cf },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x110e8 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x110ef },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x110f9 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x110ff },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x11102 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11126 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|CTX,   0, 0x11127 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1112d },
{   0|  0|  0|C11|  0|CXX23|NXX23|  0|NFC|NKC|  0,   0, 0x1112f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11132 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x11134 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11135 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1113f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11143 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11144 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11146 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11147 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1114f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11172 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   7, 0x11173 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11175 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11176 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1117f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11182 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x111b2 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x111bf },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x111c0 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x111c4 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x111c8 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x111c9 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   7, 0x111ca },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x111cc },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x111cd },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x111d9 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x111da },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x111db },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x111dc },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x111ff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11211 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11212 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1122b },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11234 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x11235 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   7, 0x11236 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11237 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1123d },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1123e },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11240 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11241 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1127f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11286 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11287 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11288 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11289 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1128d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1128e },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1129d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1129e },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x112a8 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x112af },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x112de },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x112e8 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   7, 0x112e9 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x112ea },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x112ef },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x112f9 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x112ff },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11303 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11304 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1130c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1130e },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11310 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11312 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11328 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11329 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11330 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11331 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11333 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11334 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11339 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1133a },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   7, 0x1133c },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1133d },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|CTX,   0, 0x1133e },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11344 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11346 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11348 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1134a },
{   0|  0|  0|C11|  0|CXX23|NXX23|  0|NFC|NKC|  0,   0, 0x1134c },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x1134d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1134f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11350 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11356 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|CTX,   0, 0x11357 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1135c },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11361 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11363 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11365 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1136c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1136f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x11374 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x113ff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11434 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11441 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x11442 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11445 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   7, 0x11446 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1144a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1144f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11459 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1145d },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1145e },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11461 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1147f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x114af },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|CTX,   0, 0x114b0 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x114b9 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|CTX,   0, 0x114ba },
{   0|  0|  0|C11|  0|CXX23|NXX23|  0|NFC|NKC|  0,   0, 0x114bc },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|CTX,   0, 0x114bd },
{   0|  0|  0|C11|  0|CXX23|NXX23|  0|NFC|NKC|  0,   0, 0x114be },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x114c1 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x114c2 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   7, 0x114c3 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x114c5 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x114c6 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x114c7 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x114cf },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x114d9 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1157f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x115ae },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|CTX,   0, 0x115af },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x115b5 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x115b7 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x115b9 },
{   0|  0|  0|C11|  0|CXX23|NXX23|  0|NFC|NKC|  0,   0, 0x115bb },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x115be },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x115bf },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   7, 0x115c0 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x115d7 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x115db },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x115dd },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x115ff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1162f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1163e },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x1163f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11640 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11643 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11644 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1164f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11659 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1167f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x116aa },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x116b5 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x116b6 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   7, 0x116b7 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x116b8 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x116bf },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x116c9 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x116ff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1171a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1171c },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1172a },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x1172b },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1172f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11739 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1173f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11746 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x117ff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1182b },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11838 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x11839 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   7, 0x1183a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1189f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x118df },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x118e9 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x118fe },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11906 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11908 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11909 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1190b },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11913 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11914 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11916 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11917 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1192f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|CTX,   0, 0x11930 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11935 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11936 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11937 },
{   0|  0|  0|C11|  0|CXX23|NXX23|  0|NFC|NKC|  0,   0, 0x11938 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1193a },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1193c },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x1193e },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1193f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11940 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11941 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11942 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   7, 0x11943 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1194f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11959 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1199f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x119a7 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x119a9 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x119d0 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x119d7 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x119d9 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x119df },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x119e0 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x119e1 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x119e2 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x119e3 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x119e4 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x119ff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11a00 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11a0a },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11a32 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11a33 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x11a34 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11a39 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11a3a },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11a3e },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11a46 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x11a47 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11a4f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11a50 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11a5b },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11a89 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11a98 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x11a99 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11a9c },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11a9d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11aaf },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11af8 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11bff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11c08 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11c09 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11c2e },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11c36 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11c37 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11c3e },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x11c3f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11c40 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11c4f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11c59 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11c71 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11c8f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11c91 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11ca7 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11ca8 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11cb6 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11cff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11d06 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11d07 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11d09 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11d0a },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11d30 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11d36 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11d39 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11d3a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11d3b },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11d3d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11d3e },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11d41 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   7, 0x11d42 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11d43 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x11d45 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11d46 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11d47 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11d4f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11d59 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11d5f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11d65 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11d66 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11d68 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11d69 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11d89 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11d8e },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11d8f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11d91 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11d92 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11d96 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x11d97 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11d98 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11d9f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11da9 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11edf },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11ef2 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11ef6 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11eff },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11f01 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11f02 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11f03 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11f10 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11f11 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11f33 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11f3a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11f3d },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11f40 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   9, 0x11f42 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11f4f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x11f59 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11faf },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x11fb0 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x11fff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x12399 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x123ff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1246e },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1247f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x12543 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x12f8f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x12ff0 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x12fff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1342f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1343f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x13440 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x13446 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x13455 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x143ff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x14646 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x167ff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x16a38 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x16a3f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x16a5e },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x16a5f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x16a69 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x16a6f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x16abe },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x16abf },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x16ac9 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x16acf },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x16aed },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x16aef },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   1, 0x16af4 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x16aff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x16b2f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x16b36 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x16b3f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x16b43 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x16b4f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x16b59 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x16b62 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x16b77 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x16b7c },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x16b8f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x16e3f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x16e7f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x16eff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x16f4a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x16f4e },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x16f4f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x16f50 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x16f87 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x16f8e },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x16f92 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x16f9f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x16fdf },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x16fe1 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x16fe2 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x16fe3 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x16fe4 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x16fef },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   6, 0x16ff1 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x16fff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x187f7 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x187ff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x18cd5 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x18cff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x18d08 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1afef },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1aff3 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1aff4 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1affb },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1affc },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1affe },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1afff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1b122 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1b131 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1b132 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1b14f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1b152 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1b154 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1b155 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1b163 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1b167 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1b16f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1b2fb },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1bbff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1bc6a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1bc6f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1bc7c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1bc7f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1bc88 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1bc8f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1bc99 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1bc9c },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1bc9d },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   1, 0x1bc9e },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ceff },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1cf2d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1cf2f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1cf46 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1d15d },
{   0|  0|  0|C11|  0|    0|    0|  0|  0|  0|  0,   0, 0x1d164 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 216, 0x1d166 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   1, 0x1d169 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1d16c },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 226, 0x1d16d },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 216, 0x1d172 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1d17a },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x1d182 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1d184 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1d189 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x1d18b },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1d1a9 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1d1ad },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1d1ba },
{   0|  0|  0|C11|  0|    0|    0|  0|  0|  0|  0,   0, 0x1d1c0 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1d241 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1d244 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1d3ff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d454 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1d455 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d49c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1d49d },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d49f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1d4a1 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d4a2 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1d4a4 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d4a6 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1d4a8 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d4ac },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1d4ad },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d4b9 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1d4ba },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d4bb },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1d4bc },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d4c3 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1d4c4 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d505 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1d506 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d50a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1d50c },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d514 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1d515 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d51c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1d51d },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d539 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1d53a },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d53e },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1d53f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d544 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1d545 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d546 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1d549 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d550 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1d551 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d6a5 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1d6a7 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d6c0 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x1d6c1 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d6da },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x1d6db },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d6fa },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x1d6fb },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d714 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x1d715 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d734 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x1d735 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d74e },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x1d74f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d76e },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x1d76f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d788 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x1d789 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d7a8 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x1d7a9 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d7c2 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x1d7c3 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1d7cb },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1d7cd },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|  0|  0,   0, 0x1d7ff },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1d9ff },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1da36 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1da3a },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1da6c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1da74 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1da75 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1da83 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1da84 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1da9a },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1da9f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1daa0 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1daaf },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1deff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1df1e },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1df24 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1df2a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1dfff },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1e006 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1e007 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1e018 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1e01a },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1e021 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1e022 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1e024 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1e025 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1e02a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1e02f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1e06d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1e08e },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1e08f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1e0ff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1e12c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1e12f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1e136 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1e13d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1e13f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1e149 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1e14d },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1e14e },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1e28f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1e2ad },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1e2ae },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1e2bf },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1e2eb },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1e2ef },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1e2f9 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1e4cf },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1e4eb },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 232, 0x1e4ed },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x1e4ee },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1e4ef },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1e4f9 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1e7df },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1e7e6 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1e7e7 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1e7eb },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1e7ec },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1e7ee },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1e7ef },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1e7fe },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1e7ff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1e8c4 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1e8cf },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 220, 0x1e8d6 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1e8ff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1e943 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0, 230, 0x1e949 },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   7, 0x1e94a },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x1e94b },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1e94f },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0x1e959 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1edff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee03 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ee04 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee1f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ee20 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee22 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ee23 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee24 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ee26 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee27 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ee28 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee32 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ee33 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee37 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ee38 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee39 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ee3a },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee3b },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ee41 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee42 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ee46 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee47 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ee48 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee49 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ee4a },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee4b },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ee4c },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee4f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ee50 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee52 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ee53 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee54 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ee56 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee57 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ee58 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee59 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ee5a },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee5b },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ee5c },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee5d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ee5e },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee5f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ee60 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee62 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ee63 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee64 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ee66 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee6a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ee6b },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee72 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ee73 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee77 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ee78 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee7c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ee7d },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee7e },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ee7f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee89 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ee8a },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1ee9b },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1eea0 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1eea3 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1eea4 },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1eea9 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1eeaa },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|  0|  0,   0, 0x1eebb },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1f0ff },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x1f10a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1f10f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x1f12e },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1f12f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x1f14f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1f169 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x1f16c },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1f18f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x1f190 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1f1ff },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x1f202 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1f20f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x1f23b },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1f23f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x1f248 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1f24f },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|  0|  0,   0, 0x1f251 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1fbef },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|  0|  0,   0, 0x1fbf9 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1fffd },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x1ffff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x2a6df },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2a6ff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x2b739 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2b73f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x2b81d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2b81f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x2cea1 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2ceaf },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x2ebe0 },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2f7ff },
{   0|  0|  0|C11|  0|CXX23|    0|  0|  0|  0|  0,   0, 0x2fa1d },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2fffd },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x2ffff },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x3134a },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x3134f },
{   0|  0|  0|C11|  0|CXX23|    0|CID|NFC|NKC|  0,   0, 0x323af },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x3fffd },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x3ffff },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x4fffd },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x4ffff },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x5fffd },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x5ffff },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x6fffd },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x6ffff },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x7fffd },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x7ffff },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x8fffd },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x8ffff },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x9fffd },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x9ffff },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xafffd },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xaffff },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xbfffd },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xbffff },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xcfffd },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xcffff },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xdfffd },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xdffff },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xe00ff },
{   0|  0|  0|C11|  0|CXX23|NXX23|CID|NFC|NKC|  0,   0, 0xe01ef },
{   0|  0|  0|C11|  0|    0|    0|CID|NFC|NKC|  0,   0, 0xefffd },
{   0|  0|  0|  0|  0|    0|    0|CID|NFC|NKC|  0,   0, 0x10ffff },
};
static bool
check_nfc (cpp_reader *pfile, cppchar_t c, cppchar_t p)
{
  switch (c)
    {
    case 0x0300:
      switch (p)
	{
	case 0x0041:
	case 0x0045:
	case 0x0049:
	case 0x004f:
	case 0x0055:
	case 0x0061:
	case 0x0065:
	case 0x0069:
	case 0x006f:
	case 0x0075:
	case 0x00dc:
	case 0x00fc:
	case 0x004e:
	case 0x006e:
	case 0x0415:
	case 0x0418:
	case 0x0435:
	case 0x0438:
	case 0x0112:
	case 0x0113:
	case 0x014c:
	case 0x014d:
	case 0x0057:
	case 0x0077:
	case 0x00c2:
	case 0x00e2:
	case 0x0102:
	case 0x0103:
	case 0x00ca:
	case 0x00ea:
	case 0x00d4:
	case 0x00f4:
	case 0x01a0:
	case 0x01a1:
	case 0x01af:
	case 0x01b0:
	case 0x0059:
	case 0x0079:
	case 0x1f00:
	case 0x1f80:
	case 0x1f01:
	case 0x1f81:
	case 0x1f08:
	case 0x1f88:
	case 0x1f09:
	case 0x1f89:
	case 0x1f10:
	case 0x1f11:
	case 0x1f18:
	case 0x1f19:
	case 0x1f20:
	case 0x1f90:
	case 0x1f21:
	case 0x1f91:
	case 0x1f28:
	case 0x1f98:
	case 0x1f29:
	case 0x1f99:
	case 0x1f30:
	case 0x1f31:
	case 0x1f38:
	case 0x1f39:
	case 0x1f40:
	case 0x1f41:
	case 0x1f48:
	case 0x1f49:
	case 0x1f50:
	case 0x1f51:
	case 0x1f59:
	case 0x1f60:
	case 0x1fa0:
	case 0x1f61:
	case 0x1fa1:
	case 0x1f68:
	case 0x1fa8:
	case 0x1f69:
	case 0x1fa9:
	case 0x03b1:
	case 0x1fb3:
	case 0x03b5:
	case 0x03b7:
	case 0x1fc3:
	case 0x03b9:
	case 0x03bf:
	case 0x03c5:
	case 0x03c9:
	case 0x1ff3:
	case 0x0391:
	case 0x1fbc:
	case 0x0395:
	case 0x0397:
	case 0x1fcc:
	case 0x1fbf:
	case 0x03ca:
	case 0x0399:
	case 0x1ffe:
	case 0x03cb:
	case 0x03a5:
	case 0x00a8:
	case 0x039f:
	case 0x03a9:
	case 0x1ffc:
	  return false;
	default:
	  return true;
	}

    case 0x0301:
      switch (p)
	{
	case 0x0041:
	case 0x0045:
	case 0x0049:
	case 0x004f:
	case 0x0055:
	case 0x0059:
	case 0x0061:
	case 0x0065:
	case 0x0069:
	case 0x006f:
	case 0x0075:
	case 0x0079:
	case 0x0043:
	case 0x0063:
	case 0x004c:
	case 0x006c:
	case 0x004e:
	case 0x006e:
	case 0x0052:
	case 0x0072:
	case 0x0053:
	case 0x0073:
	case 0x005a:
	case 0x007a:
	case 0x00dc:
	case 0x00fc:
	case 0x0047:
	case 0x0067:
	case 0x00c5:
	case 0x00e5:
	case 0x00c6:
	case 0x00e6:
	case 0x00d8:
	case 0x00f8:
	case 0x00a8:
	case 0x0391:
	case 0x1fbc:
	case 0x0395:
	case 0x0397:
	case 0x1fcc:
	case 0x0399:
	case 0x039f:
	case 0x03a5:
	case 0x03a9:
	case 0x1ffc:
	case 0x03ca:
	case 0x03b1:
	case 0x1fb3:
	case 0x03b5:
	case 0x03b7:
	case 0x1fc3:
	case 0x03b9:
	case 0x03cb:
	case 0x03bf:
	case 0x03c5:
	case 0x03c9:
	case 0x1ff3:
	case 0x03d2:
	case 0x0413:
	case 0x041a:
	case 0x0433:
	case 0x043a:
	case 0x00c7:
	case 0x00e7:
	case 0x0112:
	case 0x0113:
	case 0x00cf:
	case 0x00ef:
	case 0x004b:
	case 0x006b:
	case 0x004d:
	case 0x006d:
	case 0x00d5:
	case 0x00f5:
	case 0x014c:
	case 0x014d:
	case 0x0050:
	case 0x0070:
	case 0x0168:
	case 0x0169:
	case 0x0057:
	case 0x0077:
	case 0x00c2:
	case 0x00e2:
	case 0x0102:
	case 0x0103:
	case 0x00ca:
	case 0x00ea:
	case 0x00d4:
	case 0x00f4:
	case 0x01a0:
	case 0x01a1:
	case 0x01af:
	case 0x01b0:
	case 0x1f00:
	case 0x1f80:
	case 0x1f01:
	case 0x1f81:
	case 0x1f08:
	case 0x1f88:
	case 0x1f09:
	case 0x1f89:
	case 0x1f10:
	case 0x1f11:
	case 0x1f18:
	case 0x1f19:
	case 0x1f20:
	case 0x1f90:
	case 0x1f21:
	case 0x1f91:
	case 0x1f28:
	case 0x1f98:
	case 0x1f29:
	case 0x1f99:
	case 0x1f30:
	case 0x1f31:
	case 0x1f38:
	case 0x1f39:
	case 0x1f40:
	case 0x1f41:
	case 0x1f48:
	case 0x1f49:
	case 0x1f50:
	case 0x1f51:
	case 0x1f59:
	case 0x1f60:
	case 0x1fa0:
	case 0x1f61:
	case 0x1fa1:
	case 0x1f68:
	case 0x1fa8:
	case 0x1f69:
	case 0x1fa9:
	case 0x1fbf:
	case 0x1ffe:
	  return false;
	default:
	  return true;
	}

    case 0x0302:
      switch (p)
	{
	case 0x0041:
	case 0x0045:
	case 0x0049:
	case 0x004f:
	case 0x0055:
	case 0x0061:
	case 0x0065:
	case 0x0069:
	case 0x006f:
	case 0x0075:
	case 0x0043:
	case 0x0063:
	case 0x0047:
	case 0x0067:
	case 0x0048:
	case 0x0068:
	case 0x004a:
	case 0x006a:
	case 0x0053:
	case 0x0073:
	case 0x0057:
	case 0x0077:
	case 0x0059:
	case 0x0079:
	case 0x005a:
	case 0x007a:
	case 0x1ea0:
	case 0x1ea1:
	case 0x1eb8:
	case 0x1eb9:
	case 0x1ecc:
	case 0x1ecd:
	  return false;
	default:
	  return true;
	}

    case 0x0303:
      switch (p)
	{
	case 0x0041:
	case 0x004e:
	case 0x004f:
	case 0x0061:
	case 0x006e:
	case 0x006f:
	case 0x0049:
	case 0x0069:
	case 0x0055:
	case 0x0075:
	case 0x0056:
	case 0x0076:
	case 0x00c2:
	case 0x00e2:
	case 0x0102:
	case 0x0103:
	case 0x0045:
	case 0x0065:
	case 0x00ca:
	case 0x00ea:
	case 0x00d4:
	case 0x00f4:
	case 0x01a0:
	case 0x01a1:
	case 0x01af:
	case 0x01b0:
	case 0x0059:
	case 0x0079:
	  return false;
	default:
	  return true;
	}

    case 0x0304:
      switch (p)
	{
	case 0x0041:
	case 0x0061:
	case 0x0045:
	case 0x0065:
	case 0x0049:
	case 0x0069:
	case 0x004f:
	case 0x006f:
	case 0x0055:
	case 0x0075:
	case 0x00dc:
	case 0x00fc:
	case 0x00c4:
	case 0x00e4:
	case 0x0226:
	case 0x0227:
	case 0x00c6:
	case 0x00e6:
	case 0x01ea:
	case 0x01eb:
	case 0x00d6:
	case 0x00f6:
	case 0x00d5:
	case 0x00f5:
	case 0x022e:
	case 0x022f:
	case 0x0059:
	case 0x0079:
	case 0x0418:
	case 0x0438:
	case 0x0423:
	case 0x0443:
	case 0x0047:
	case 0x0067:
	case 0x1e36:
	case 0x1e37:
	case 0x1e5a:
	case 0x1e5b:
	case 0x03b1:
	case 0x1fb3:
	case 0x0391:
	case 0x1fbc:
	case 0x03b9:
	case 0x0399:
	case 0x03c5:
	case 0x03a5:
	  return false;
	default:
	  return true;
	}

    case 0x0306:
      switch (p)
	{
	case 0x0041:
	case 0x0061:
	case 0x0045:
	case 0x0065:
	case 0x0047:
	case 0x0067:
	case 0x0049:
	case 0x0069:
	case 0x004f:
	case 0x006f:
	case 0x0055:
	case 0x0075:
	case 0x0423:
	case 0x0418:
	case 0x0438:
	case 0x0443:
	case 0x0416:
	case 0x0436:
	case 0x0410:
	case 0x0430:
	case 0x0415:
	case 0x0435:
	case 0x0228:
	case 0x0229:
	case 0x1ea0:
	case 0x1ea1:
	case 0x03b1:
	case 0x1fb3:
	case 0x0391:
	case 0x1fbc:
	case 0x03b9:
	case 0x0399:
	case 0x03c5:
	case 0x03a5:
	  return false;
	default:
	  return true;
	}

    case 0x0307:
      switch (p)
	{
	case 0x0043:
	case 0x0063:
	case 0x0045:
	case 0x0065:
	case 0x0047:
	case 0x0067:
	case 0x0049:
	case 0x005a:
	case 0x007a:
	case 0x0041:
	case 0x0061:
	case 0x004f:
	case 0x006f:
	case 0x0042:
	case 0x0062:
	case 0x0044:
	case 0x0064:
	case 0x0046:
	case 0x0066:
	case 0x0048:
	case 0x0068:
	case 0x004d:
	case 0x006d:
	case 0x004e:
	case 0x006e:
	case 0x0050:
	case 0x0070:
	case 0x0052:
	case 0x0072:
	case 0x0053:
	case 0x0073:
	case 0x015a:
	case 0x015b:
	case 0x0160:
	case 0x0161:
	case 0x1e62:
	case 0x1e63:
	case 0x0054:
	case 0x0074:
	case 0x0057:
	case 0x0077:
	case 0x0058:
	case 0x0078:
	case 0x0059:
	case 0x0079:
	case 0x017f:
	  return false;
	default:
	  return true;
	}

    case 0x0308:
      switch (p)
	{
	case 0x0041:
	case 0x0045:
	case 0x0049:
	case 0x004f:
	case 0x0055:
	case 0x0061:
	case 0x0065:
	case 0x0069:
	case 0x006f:
	case 0x0075:
	case 0x0079:
	case 0x0059:
	case 0x0399:
	case 0x03a5:
	case 0x03b9:
	case 0x03c5:
	case 0x03d2:
	case 0x0415:
	case 0x0406:
	case 0x0435:
	case 0x0456:
	case 0x0410:
	case 0x0430:
	case 0x04d8:
	case 0x04d9:
	case 0x0416:
	case 0x0436:
	case 0x0417:
	case 0x0437:
	case 0x0418:
	case 0x0438:
	case 0x041e:
	case 0x043e:
	case 0x04e8:
	case 0x04e9:
	case 0x042d:
	case 0x044d:
	case 0x0423:
	case 0x0443:
	case 0x0427:
	case 0x0447:
	case 0x042b:
	case 0x044b:
	case 0x0048:
	case 0x0068:
	case 0x00d5:
	case 0x00f5:
	case 0x016a:
	case 0x016b:
	case 0x0057:
	case 0x0077:
	case 0x0058:
	case 0x0078:
	case 0x0074:
	  return false;
	default:
	  return true;
	}

    case 0x0309:
      switch (p)
	{
	case 0x0041:
	case 0x0061:
	case 0x00c2:
	case 0x00e2:
	case 0x0102:
	case 0x0103:
	case 0x0045:
	case 0x0065:
	case 0x00ca:
	case 0x00ea:
	case 0x0049:
	case 0x0069:
	case 0x004f:
	case 0x006f:
	case 0x00d4:
	case 0x00f4:
	case 0x01a0:
	case 0x01a1:
	case 0x0055:
	case 0x0075:
	case 0x01af:
	case 0x01b0:
	case 0x0059:
	case 0x0079:
	  return false;
	default:
	  return true;
	}

    case 0x030a:
      switch (p)
	{
	case 0x0041:
	case 0x0061:
	case 0x0055:
	case 0x0075:
	case 0x0077:
	case 0x0079:
	  return false;
	default:
	  return true;
	}

    case 0x030b:
      switch (p)
	{
	case 0x004f:
	case 0x006f:
	case 0x0055:
	case 0x0075:
	case 0x0423:
	case 0x0443:
	  return false;
	default:
	  return true;
	}

    case 0x030c:
      switch (p)
	{
	case 0x0043:
	case 0x0063:
	case 0x0044:
	case 0x0064:
	case 0x0045:
	case 0x0065:
	case 0x004c:
	case 0x006c:
	case 0x004e:
	case 0x006e:
	case 0x0052:
	case 0x0072:
	case 0x0053:
	case 0x0073:
	case 0x0054:
	case 0x0074:
	case 0x005a:
	case 0x007a:
	case 0x0041:
	case 0x0061:
	case 0x0049:
	case 0x0069:
	case 0x004f:
	case 0x006f:
	case 0x0055:
	case 0x0075:
	case 0x00dc:
	case 0x00fc:
	case 0x0047:
	case 0x0067:
	case 0x004b:
	case 0x006b:
	case 0x01b7:
	case 0x0292:
	case 0x006a:
	case 0x0048:
	case 0x0068:
	  return false;
	default:
	  return true;
	}

    case 0x030f:
      switch (p)
	{
	case 0x0041:
	case 0x0061:
	case 0x0045:
	case 0x0065:
	case 0x0049:
	case 0x0069:
	case 0x004f:
	case 0x006f:
	case 0x0052:
	case 0x0072:
	case 0x0055:
	case 0x0075:
	case 0x0474:
	case 0x0475:
	  return false;
	default:
	  return true;
	}

    case 0x0311:
      switch (p)
	{
	case 0x0041:
	case 0x0061:
	case 0x0045:
	case 0x0065:
	case 0x0049:
	case 0x0069:
	case 0x004f:
	case 0x006f:
	case 0x0052:
	case 0x0072:
	case 0x0055:
	case 0x0075:
	  return false;
	default:
	  return true;
	}

    case 0x0313:
      switch (p)
	{
	case 0x03b1:
	case 0x1fb3:
	case 0x0391:
	case 0x1fbc:
	case 0x03b5:
	case 0x0395:
	case 0x03b7:
	case 0x1fc3:
	case 0x0397:
	case 0x1fcc:
	case 0x03b9:
	case 0x0399:
	case 0x03bf:
	case 0x039f:
	case 0x03c5:
	case 0x03c9:
	case 0x1ff3:
	case 0x03a9:
	case 0x1ffc:
	case 0x03c1:
	  return false;
	default:
	  return true;
	}

    case 0x0314:
      switch (p)
	{
	case 0x03b1:
	case 0x1fb3:
	case 0x0391:
	case 0x1fbc:
	case 0x03b5:
	case 0x0395:
	case 0x03b7:
	case 0x1fc3:
	case 0x0397:
	case 0x1fcc:
	case 0x03b9:
	case 0x0399:
	case 0x03bf:
	case 0x039f:
	case 0x03c5:
	case 0x03a5:
	case 0x03c9:
	case 0x1ff3:
	case 0x03a9:
	case 0x1ffc:
	case 0x03c1:
	case 0x03a1:
	  return false;
	default:
	  return true;
	}

    case 0x031b:
      switch (p)
	{
	case 0x004f:
	case 0x00d2:
	case 0x00d3:
	case 0x00d4:
	case 0x00d5:
	case 0x00d6:
	case 0x014c:
	case 0x014e:
	case 0x0150:
	case 0x01d1:
	case 0x020c:
	case 0x020e:
	case 0x022a:
	case 0x022c:
	case 0x022e:
	case 0x0230:
	case 0x1e4c:
	case 0x1e4e:
	case 0x1e50:
	case 0x1e52:
	case 0x1ecc:
	case 0x1ece:
	case 0x1ed0:
	case 0x1ed2:
	case 0x1ed4:
	case 0x1ed6:
	case 0x1ed8:
	case 0x006f:
	case 0x00f2:
	case 0x00f3:
	case 0x00f4:
	case 0x00f5:
	case 0x00f6:
	case 0x014d:
	case 0x014f:
	case 0x0151:
	case 0x01d2:
	case 0x020d:
	case 0x020f:
	case 0x022b:
	case 0x022d:
	case 0x022f:
	case 0x0231:
	case 0x1e4d:
	case 0x1e4f:
	case 0x1e51:
	case 0x1e53:
	case 0x1ecd:
	case 0x1ecf:
	case 0x1ed1:
	case 0x1ed3:
	case 0x1ed5:
	case 0x1ed7:
	case 0x1ed9:
	case 0x0055:
	case 0x00d9:
	case 0x00da:
	case 0x00db:
	case 0x00dc:
	case 0x0168:
	case 0x016a:
	case 0x016c:
	case 0x016e:
	case 0x0170:
	case 0x01d3:
	case 0x01d5:
	case 0x01d7:
	case 0x01d9:
	case 0x01db:
	case 0x0214:
	case 0x0216:
	case 0x1e72:
	case 0x1e74:
	case 0x1e76:
	case 0x1e78:
	case 0x1e7a:
	case 0x1ee4:
	case 0x1ee6:
	case 0x0075:
	case 0x00f9:
	case 0x00fa:
	case 0x00fb:
	case 0x00fc:
	case 0x0169:
	case 0x016b:
	case 0x016d:
	case 0x016f:
	case 0x0171:
	case 0x01d4:
	case 0x01d6:
	case 0x01d8:
	case 0x01da:
	case 0x01dc:
	case 0x0215:
	case 0x0217:
	case 0x1e73:
	case 0x1e75:
	case 0x1e77:
	case 0x1e79:
	case 0x1e7b:
	case 0x1ee5:
	case 0x1ee7:
	  return false;
	default:
	  return true;
	}

    case 0x0323:
      switch (p)
	{
	case 0x0042:
	case 0x1e02:
	case 0x0062:
	case 0x1e03:
	case 0x0044:
	case 0x010e:
	case 0x1e0a:
	case 0x0064:
	case 0x010f:
	case 0x1e0b:
	case 0x0048:
	case 0x0124:
	case 0x021e:
	case 0x1e22:
	case 0x1e26:
	case 0x0068:
	case 0x0125:
	case 0x021f:
	case 0x1e23:
	case 0x1e27:
	case 0x004b:
	case 0x01e8:
	case 0x1e30:
	case 0x006b:
	case 0x01e9:
	case 0x1e31:
	case 0x004c:
	case 0x0139:
	case 0x013d:
	case 0x006c:
	case 0x013a:
	case 0x013e:
	case 0x004d:
	case 0x1e3e:
	case 0x1e40:
	case 0x006d:
	case 0x1e3f:
	case 0x1e41:
	case 0x004e:
	case 0x00d1:
	case 0x0143:
	case 0x0147:
	case 0x01f8:
	case 0x1e44:
	case 0x006e:
	case 0x00f1:
	case 0x0144:
	case 0x0148:
	case 0x01f9:
	case 0x1e45:
	case 0x0052:
	case 0x0154:
	case 0x0158:
	case 0x0210:
	case 0x0212:
	case 0x1e58:
	case 0x0072:
	case 0x0155:
	case 0x0159:
	case 0x0211:
	case 0x0213:
	case 0x1e59:
	case 0x0053:
	case 0x015a:
	case 0x015c:
	case 0x0160:
	case 0x1e60:
	case 0x1e64:
	case 0x1e66:
	case 0x0073:
	case 0x015b:
	case 0x015d:
	case 0x0161:
	case 0x1e61:
	case 0x1e65:
	case 0x1e67:
	case 0x0054:
	case 0x0164:
	case 0x1e6a:
	case 0x0074:
	case 0x0165:
	case 0x1e6b:
	case 0x1e97:
	case 0x0056:
	case 0x1e7c:
	case 0x0076:
	case 0x1e7d:
	case 0x0057:
	case 0x0174:
	case 0x1e80:
	case 0x1e82:
	case 0x1e84:
	case 0x1e86:
	case 0x0077:
	case 0x0175:
	case 0x1e81:
	case 0x1e83:
	case 0x1e85:
	case 0x1e87:
	case 0x1e98:
	case 0x005a:
	case 0x0179:
	case 0x017b:
	case 0x017d:
	case 0x1e90:
	case 0x007a:
	case 0x017a:
	case 0x017c:
	case 0x017e:
	case 0x1e91:
	case 0x0041:
	case 0x00c0:
	case 0x00c1:
	case 0x00c2:
	case 0x00c3:
	case 0x00c4:
	case 0x00c5:
	case 0x0100:
	case 0x0102:
	case 0x01cd:
	case 0x01de:
	case 0x01e0:
	case 0x01fa:
	case 0x0200:
	case 0x0202:
	case 0x0226:
	case 0x1ea2:
	case 0x1ea4:
	case 0x1ea6:
	case 0x1ea8:
	case 0x1eaa:
	case 0x1eae:
	case 0x1eb0:
	case 0x1eb2:
	case 0x1eb4:
	case 0x0061:
	case 0x00e0:
	case 0x00e1:
	case 0x00e2:
	case 0x00e3:
	case 0x00e4:
	case 0x00e5:
	case 0x0101:
	case 0x0103:
	case 0x01ce:
	case 0x01df:
	case 0x01e1:
	case 0x01fb:
	case 0x0201:
	case 0x0203:
	case 0x0227:
	case 0x1ea3:
	case 0x1ea5:
	case 0x1ea7:
	case 0x1ea9:
	case 0x1eab:
	case 0x1eaf:
	case 0x1eb1:
	case 0x1eb3:
	case 0x1eb5:
	case 0x0045:
	case 0x00c8:
	case 0x00c9:
	case 0x00ca:
	case 0x00cb:
	case 0x0112:
	case 0x0114:
	case 0x0116:
	case 0x011a:
	case 0x0204:
	case 0x0206:
	case 0x1e14:
	case 0x1e16:
	case 0x1eba:
	case 0x1ebc:
	case 0x1ebe:
	case 0x1ec0:
	case 0x1ec2:
	case 0x1ec4:
	case 0x0065:
	case 0x00e8:
	case 0x00e9:
	case 0x00ea:
	case 0x00eb:
	case 0x0113:
	case 0x0115:
	case 0x0117:
	case 0x011b:
	case 0x0205:
	case 0x0207:
	case 0x1e15:
	case 0x1e17:
	case 0x1ebb:
	case 0x1ebd:
	case 0x1ebf:
	case 0x1ec1:
	case 0x1ec3:
	case 0x1ec5:
	case 0x0049:
	case 0x00cc:
	case 0x00cd:
	case 0x00ce:
	case 0x00cf:
	case 0x0128:
	case 0x012a:
	case 0x012c:
	case 0x0130:
	case 0x01cf:
	case 0x0208:
	case 0x020a:
	case 0x1e2e:
	case 0x1ec8:
	case 0x0069:
	case 0x00ec:
	case 0x00ed:
	case 0x00ee:
	case 0x00ef:
	case 0x0129:
	case 0x012b:
	case 0x012d:
	case 0x01d0:
	case 0x0209:
	case 0x020b:
	case 0x1e2f:
	case 0x1ec9:
	case 0x004f:
	case 0x00d2:
	case 0x00d3:
	case 0x00d4:
	case 0x00d5:
	case 0x00d6:
	case 0x014c:
	case 0x014e:
	case 0x0150:
	case 0x01d1:
	case 0x020c:
	case 0x020e:
	case 0x022a:
	case 0x022c:
	case 0x022e:
	case 0x0230:
	case 0x1e4c:
	case 0x1e4e:
	case 0x1e50:
	case 0x1e52:
	case 0x1ece:
	case 0x1ed0:
	case 0x1ed2:
	case 0x1ed4:
	case 0x1ed6:
	case 0x006f:
	case 0x00f2:
	case 0x00f3:
	case 0x00f4:
	case 0x00f5:
	case 0x00f6:
	case 0x014d:
	case 0x014f:
	case 0x0151:
	case 0x01d2:
	case 0x020d:
	case 0x020f:
	case 0x022b:
	case 0x022d:
	case 0x022f:
	case 0x0231:
	case 0x1e4d:
	case 0x1e4f:
	case 0x1e51:
	case 0x1e53:
	case 0x1ecf:
	case 0x1ed1:
	case 0x1ed3:
	case 0x1ed5:
	case 0x1ed7:
	case 0x01a0:
	case 0x1eda:
	case 0x1edc:
	case 0x1ede:
	case 0x1ee0:
	case 0x01a1:
	case 0x1edb:
	case 0x1edd:
	case 0x1edf:
	case 0x1ee1:
	case 0x0055:
	case 0x00d9:
	case 0x00da:
	case 0x00db:
	case 0x00dc:
	case 0x0168:
	case 0x016a:
	case 0x016c:
	case 0x016e:
	case 0x0170:
	case 0x01d3:
	case 0x01d5:
	case 0x01d7:
	case 0x01d9:
	case 0x01db:
	case 0x0214:
	case 0x0216:
	case 0x1e78:
	case 0x1e7a:
	case 0x1ee6:
	case 0x0075:
	case 0x00f9:
	case 0x00fa:
	case 0x00fb:
	case 0x00fc:
	case 0x0169:
	case 0x016b:
	case 0x016d:
	case 0x016f:
	case 0x0171:
	case 0x01d4:
	case 0x01d6:
	case 0x01d8:
	case 0x01da:
	case 0x01dc:
	case 0x0215:
	case 0x0217:
	case 0x1e79:
	case 0x1e7b:
	case 0x1ee7:
	case 0x01af:
	case 0x1ee8:
	case 0x1eea:
	case 0x1eec:
	case 0x1eee:
	case 0x01b0:
	case 0x1ee9:
	case 0x1eeb:
	case 0x1eed:
	case 0x1eef:
	case 0x0059:
	case 0x00dd:
	case 0x0176:
	case 0x0178:
	case 0x0232:
	case 0x1e8e:
	case 0x1ef2:
	case 0x1ef6:
	case 0x1ef8:
	case 0x0079:
	case 0x00fd:
	case 0x00ff:
	case 0x0177:
	case 0x0233:
	case 0x1e8f:
	case 0x1e99:
	case 0x1ef3:
	case 0x1ef7:
	case 0x1ef9:
	  return false;
	default:
	  return true;
	}

    case 0x0324:
      switch (p)
	{
	case 0x0055:
	case 0x00d9:
	case 0x00da:
	case 0x00db:
	case 0x00dc:
	case 0x0168:
	case 0x016a:
	case 0x016c:
	case 0x016e:
	case 0x0170:
	case 0x01d3:
	case 0x01d5:
	case 0x01d7:
	case 0x01d9:
	case 0x01db:
	case 0x0214:
	case 0x0216:
	case 0x1e78:
	case 0x1e7a:
	case 0x1ee6:
	case 0x0075:
	case 0x00f9:
	case 0x00fa:
	case 0x00fb:
	case 0x00fc:
	case 0x0169:
	case 0x016b:
	case 0x016d:
	case 0x016f:
	case 0x0171:
	case 0x01d4:
	case 0x01d6:
	case 0x01d8:
	case 0x01da:
	case 0x01dc:
	case 0x0215:
	case 0x0217:
	case 0x1e79:
	case 0x1e7b:
	case 0x1ee7:
	  return false;
	default:
	  return true;
	}

    case 0x0325:
      switch (p)
	{
	case 0x0041:
	case 0x00c0:
	case 0x00c1:
	case 0x00c2:
	case 0x00c3:
	case 0x00c4:
	case 0x00c5:
	case 0x0100:
	case 0x0102:
	case 0x01cd:
	case 0x01de:
	case 0x01e0:
	case 0x01fa:
	case 0x0200:
	case 0x0202:
	case 0x0226:
	case 0x1ea2:
	case 0x1ea4:
	case 0x1ea6:
	case 0x1ea8:
	case 0x1eaa:
	case 0x1eae:
	case 0x1eb0:
	case 0x1eb2:
	case 0x1eb4:
	case 0x0061:
	case 0x00e0:
	case 0x00e1:
	case 0x00e2:
	case 0x00e3:
	case 0x00e4:
	case 0x00e5:
	case 0x0101:
	case 0x0103:
	case 0x01ce:
	case 0x01df:
	case 0x01e1:
	case 0x01fb:
	case 0x0201:
	case 0x0203:
	case 0x0227:
	case 0x1ea3:
	case 0x1ea5:
	case 0x1ea7:
	case 0x1ea9:
	case 0x1eab:
	case 0x1eaf:
	case 0x1eb1:
	case 0x1eb3:
	case 0x1eb5:
	  return false;
	default:
	  return true;
	}

    case 0x0326:
      switch (p)
	{
	case 0x0053:
	case 0x015a:
	case 0x015c:
	case 0x0160:
	case 0x1e60:
	case 0x1e64:
	case 0x1e66:
	case 0x0073:
	case 0x015b:
	case 0x015d:
	case 0x0161:
	case 0x1e61:
	case 0x1e65:
	case 0x1e67:
	case 0x0054:
	case 0x0164:
	case 0x1e6a:
	case 0x0074:
	case 0x0165:
	case 0x1e6b:
	case 0x1e97:
	  return false;
	default:
	  return true;
	}

    case 0x0327:
      switch (p)
	{
	case 0x0043:
	case 0x0106:
	case 0x0108:
	case 0x010a:
	case 0x010c:
	case 0x0063:
	case 0x0107:
	case 0x0109:
	case 0x010b:
	case 0x010d:
	case 0x0047:
	case 0x011c:
	case 0x011e:
	case 0x0120:
	case 0x01e6:
	case 0x01f4:
	case 0x1e20:
	case 0x0067:
	case 0x011d:
	case 0x011f:
	case 0x0121:
	case 0x01e7:
	case 0x01f5:
	case 0x1e21:
	case 0x004b:
	case 0x01e8:
	case 0x1e30:
	case 0x1e32:
	case 0x1e34:
	case 0x006b:
	case 0x01e9:
	case 0x1e31:
	case 0x1e33:
	case 0x1e35:
	case 0x004c:
	case 0x0139:
	case 0x013d:
	case 0x1e36:
	case 0x1e38:
	case 0x1e3a:
	case 0x1e3c:
	case 0x006c:
	case 0x013a:
	case 0x013e:
	case 0x1e37:
	case 0x1e39:
	case 0x1e3b:
	case 0x1e3d:
	case 0x004e:
	case 0x00d1:
	case 0x0143:
	case 0x0147:
	case 0x01f8:
	case 0x1e44:
	case 0x1e46:
	case 0x1e48:
	case 0x1e4a:
	case 0x006e:
	case 0x00f1:
	case 0x0144:
	case 0x0148:
	case 0x01f9:
	case 0x1e45:
	case 0x1e47:
	case 0x1e49:
	case 0x1e4b:
	case 0x0052:
	case 0x0154:
	case 0x0158:
	case 0x0210:
	case 0x0212:
	case 0x1e58:
	case 0x1e5a:
	case 0x1e5c:
	case 0x1e5e:
	case 0x0072:
	case 0x0155:
	case 0x0159:
	case 0x0211:
	case 0x0213:
	case 0x1e59:
	case 0x1e5b:
	case 0x1e5d:
	case 0x1e5f:
	case 0x0053:
	case 0x015a:
	case 0x015c:
	case 0x0160:
	case 0x0218:
	case 0x1e60:
	case 0x1e62:
	case 0x1e64:
	case 0x1e66:
	case 0x1e68:
	case 0x0073:
	case 0x015b:
	case 0x015d:
	case 0x0161:
	case 0x0219:
	case 0x1e61:
	case 0x1e63:
	case 0x1e65:
	case 0x1e67:
	case 0x1e69:
	case 0x0054:
	case 0x0164:
	case 0x021a:
	case 0x1e6a:
	case 0x1e6c:
	case 0x1e6e:
	case 0x1e70:
	case 0x0074:
	case 0x0165:
	case 0x021b:
	case 0x1e6b:
	case 0x1e6d:
	case 0x1e6f:
	case 0x1e71:
	case 0x1e97:
	case 0x0045:
	case 0x00c8:
	case 0x00c9:
	case 0x00ca:
	case 0x00cb:
	case 0x0112:
	case 0x0114:
	case 0x0116:
	case 0x011a:
	case 0x0204:
	case 0x0206:
	case 0x1e14:
	case 0x1e16:
	case 0x1e18:
	case 0x1e1a:
	case 0x1eb8:
	case 0x1eba:
	case 0x1ebc:
	case 0x1ebe:
	case 0x1ec0:
	case 0x1ec2:
	case 0x1ec4:
	case 0x1ec6:
	case 0x0065:
	case 0x00e8:
	case 0x00e9:
	case 0x00ea:
	case 0x00eb:
	case 0x0113:
	case 0x0115:
	case 0x0117:
	case 0x011b:
	case 0x0205:
	case 0x0207:
	case 0x1e15:
	case 0x1e17:
	case 0x1e19:
	case 0x1e1b:
	case 0x1eb9:
	case 0x1ebb:
	case 0x1ebd:
	case 0x1ebf:
	case 0x1ec1:
	case 0x1ec3:
	case 0x1ec5:
	case 0x1ec7:
	case 0x0044:
	case 0x010e:
	case 0x1e0a:
	case 0x1e0c:
	case 0x1e0e:
	case 0x1e12:
	case 0x0064:
	case 0x010f:
	case 0x1e0b:
	case 0x1e0d:
	case 0x1e0f:
	case 0x1e13:
	case 0x0048:
	case 0x0124:
	case 0x021e:
	case 0x1e22:
	case 0x1e24:
	case 0x1e26:
	case 0x1e2a:
	case 0x0068:
	case 0x0125:
	case 0x021f:
	case 0x1e23:
	case 0x1e25:
	case 0x1e27:
	case 0x1e2b:
	case 0x1e96:
	  return false;
	default:
	  return true;
	}

    case 0x0328:
      switch (p)
	{
	case 0x0041:
	case 0x00c0:
	case 0x00c1:
	case 0x00c2:
	case 0x00c3:
	case 0x00c4:
	case 0x00c5:
	case 0x0100:
	case 0x0102:
	case 0x01cd:
	case 0x01de:
	case 0x01e0:
	case 0x01fa:
	case 0x0200:
	case 0x0202:
	case 0x0226:
	case 0x1e00:
	case 0x1ea0:
	case 0x1ea2:
	case 0x1ea4:
	case 0x1ea6:
	case 0x1ea8:
	case 0x1eaa:
	case 0x1eac:
	case 0x1eae:
	case 0x1eb0:
	case 0x1eb2:
	case 0x1eb4:
	case 0x1eb6:
	case 0x0061:
	case 0x00e0:
	case 0x00e1:
	case 0x00e2:
	case 0x00e3:
	case 0x00e4:
	case 0x00e5:
	case 0x0101:
	case 0x0103:
	case 0x01ce:
	case 0x01df:
	case 0x01e1:
	case 0x01fb:
	case 0x0201:
	case 0x0203:
	case 0x0227:
	case 0x1e01:
	case 0x1ea1:
	case 0x1ea3:
	case 0x1ea5:
	case 0x1ea7:
	case 0x1ea9:
	case 0x1eab:
	case 0x1ead:
	case 0x1eaf:
	case 0x1eb1:
	case 0x1eb3:
	case 0x1eb5:
	case 0x1eb7:
	case 0x0045:
	case 0x00c8:
	case 0x00c9:
	case 0x00ca:
	case 0x00cb:
	case 0x0112:
	case 0x0114:
	case 0x0116:
	case 0x011a:
	case 0x0204:
	case 0x0206:
	case 0x1e14:
	case 0x1e16:
	case 0x1e18:
	case 0x1e1a:
	case 0x1eb8:
	case 0x1eba:
	case 0x1ebc:
	case 0x1ebe:
	case 0x1ec0:
	case 0x1ec2:
	case 0x1ec4:
	case 0x1ec6:
	case 0x0065:
	case 0x00e8:
	case 0x00e9:
	case 0x00ea:
	case 0x00eb:
	case 0x0113:
	case 0x0115:
	case 0x0117:
	case 0x011b:
	case 0x0205:
	case 0x0207:
	case 0x1e15:
	case 0x1e17:
	case 0x1e19:
	case 0x1e1b:
	case 0x1eb9:
	case 0x1ebb:
	case 0x1ebd:
	case 0x1ebf:
	case 0x1ec1:
	case 0x1ec3:
	case 0x1ec5:
	case 0x1ec7:
	case 0x0049:
	case 0x00cc:
	case 0x00cd:
	case 0x00ce:
	case 0x00cf:
	case 0x0128:
	case 0x012a:
	case 0x012c:
	case 0x0130:
	case 0x01cf:
	case 0x0208:
	case 0x020a:
	case 0x1e2c:
	case 0x1e2e:
	case 0x1ec8:
	case 0x1eca:
	case 0x0069:
	case 0x00ec:
	case 0x00ed:
	case 0x00ee:
	case 0x00ef:
	case 0x0129:
	case 0x012b:
	case 0x012d:
	case 0x01d0:
	case 0x0209:
	case 0x020b:
	case 0x1e2d:
	case 0x1e2f:
	case 0x1ec9:
	case 0x1ecb:
	case 0x0055:
	case 0x00d9:
	case 0x00da:
	case 0x00db:
	case 0x00dc:
	case 0x0168:
	case 0x016a:
	case 0x016c:
	case 0x016e:
	case 0x0170:
	case 0x01af:
	case 0x01d3:
	case 0x01d5:
	case 0x01d7:
	case 0x01d9:
	case 0x01db:
	case 0x0214:
	case 0x0216:
	case 0x1e72:
	case 0x1e74:
	case 0x1e76:
	case 0x1e78:
	case 0x1e7a:
	case 0x1ee4:
	case 0x1ee6:
	case 0x1ee8:
	case 0x1eea:
	case 0x1eec:
	case 0x1eee:
	case 0x1ef0:
	case 0x0075:
	case 0x00f9:
	case 0x00fa:
	case 0x00fb:
	case 0x00fc:
	case 0x0169:
	case 0x016b:
	case 0x016d:
	case 0x016f:
	case 0x0171:
	case 0x01b0:
	case 0x01d4:
	case 0x01d6:
	case 0x01d8:
	case 0x01da:
	case 0x01dc:
	case 0x0215:
	case 0x0217:
	case 0x1e73:
	case 0x1e75:
	case 0x1e77:
	case 0x1e79:
	case 0x1e7b:
	case 0x1ee5:
	case 0x1ee7:
	case 0x1ee9:
	case 0x1eeb:
	case 0x1eed:
	case 0x1eef:
	case 0x1ef1:
	case 0x004f:
	case 0x00d2:
	case 0x00d3:
	case 0x00d4:
	case 0x00d5:
	case 0x00d6:
	case 0x014c:
	case 0x014e:
	case 0x0150:
	case 0x01a0:
	case 0x01d1:
	case 0x020c:
	case 0x020e:
	case 0x022a:
	case 0x022c:
	case 0x022e:
	case 0x0230:
	case 0x1e4c:
	case 0x1e4e:
	case 0x1e50:
	case 0x1e52:
	case 0x1ecc:
	case 0x1ece:
	case 0x1ed0:
	case 0x1ed2:
	case 0x1ed4:
	case 0x1ed6:
	case 0x1ed8:
	case 0x1eda:
	case 0x1edc:
	case 0x1ede:
	case 0x1ee0:
	case 0x1ee2:
	case 0x006f:
	case 0x00f2:
	case 0x00f3:
	case 0x00f4:
	case 0x00f5:
	case 0x00f6:
	case 0x014d:
	case 0x014f:
	case 0x0151:
	case 0x01a1:
	case 0x01d2:
	case 0x020d:
	case 0x020f:
	case 0x022b:
	case 0x022d:
	case 0x022f:
	case 0x0231:
	case 0x1e4d:
	case 0x1e4f:
	case 0x1e51:
	case 0x1e53:
	case 0x1ecd:
	case 0x1ecf:
	case 0x1ed1:
	case 0x1ed3:
	case 0x1ed5:
	case 0x1ed7:
	case 0x1ed9:
	case 0x1edb:
	case 0x1edd:
	case 0x1edf:
	case 0x1ee1:
	case 0x1ee3:
	  return false;
	default:
	  return true;
	}

    case 0x032d:
      switch (p)
	{
	case 0x0044:
	case 0x010e:
	case 0x1e0a:
	case 0x0064:
	case 0x010f:
	case 0x1e0b:
	case 0x0045:
	case 0x00c8:
	case 0x00c9:
	case 0x00ca:
	case 0x00cb:
	case 0x0112:
	case 0x0114:
	case 0x0116:
	case 0x011a:
	case 0x0204:
	case 0x0206:
	case 0x1e14:
	case 0x1e16:
	case 0x1eba:
	case 0x1ebc:
	case 0x1ebe:
	case 0x1ec0:
	case 0x1ec2:
	case 0x1ec4:
	case 0x0065:
	case 0x00e8:
	case 0x00e9:
	case 0x00ea:
	case 0x00eb:
	case 0x0113:
	case 0x0115:
	case 0x0117:
	case 0x011b:
	case 0x0205:
	case 0x0207:
	case 0x1e15:
	case 0x1e17:
	case 0x1ebb:
	case 0x1ebd:
	case 0x1ebf:
	case 0x1ec1:
	case 0x1ec3:
	case 0x1ec5:
	case 0x004c:
	case 0x0139:
	case 0x013d:
	case 0x006c:
	case 0x013a:
	case 0x013e:
	case 0x004e:
	case 0x00d1:
	case 0x0143:
	case 0x0147:
	case 0x01f8:
	case 0x1e44:
	case 0x006e:
	case 0x00f1:
	case 0x0144:
	case 0x0148:
	case 0x01f9:
	case 0x1e45:
	case 0x0054:
	case 0x0164:
	case 0x1e6a:
	case 0x0074:
	case 0x0165:
	case 0x1e6b:
	case 0x1e97:
	case 0x0055:
	case 0x00d9:
	case 0x00da:
	case 0x00db:
	case 0x00dc:
	case 0x0168:
	case 0x016a:
	case 0x016c:
	case 0x016e:
	case 0x0170:
	case 0x01d3:
	case 0x01d5:
	case 0x01d7:
	case 0x01d9:
	case 0x01db:
	case 0x0214:
	case 0x0216:
	case 0x1e78:
	case 0x1e7a:
	case 0x1ee6:
	case 0x0075:
	case 0x00f9:
	case 0x00fa:
	case 0x00fb:
	case 0x00fc:
	case 0x0169:
	case 0x016b:
	case 0x016d:
	case 0x016f:
	case 0x0171:
	case 0x01d4:
	case 0x01d6:
	case 0x01d8:
	case 0x01da:
	case 0x01dc:
	case 0x0215:
	case 0x0217:
	case 0x1e79:
	case 0x1e7b:
	case 0x1ee7:
	  return false;
	default:
	  return true;
	}

    case 0x032e:
      switch (p)
	{
	case 0x0048:
	case 0x0124:
	case 0x021e:
	case 0x1e22:
	case 0x1e26:
	case 0x0068:
	case 0x0125:
	case 0x021f:
	case 0x1e23:
	case 0x1e27:
	  return false;
	default:
	  return true;
	}

    case 0x0330:
      switch (p)
	{
	case 0x0045:
	case 0x00c8:
	case 0x00c9:
	case 0x00ca:
	case 0x00cb:
	case 0x0112:
	case 0x0114:
	case 0x0116:
	case 0x011a:
	case 0x0204:
	case 0x0206:
	case 0x1e14:
	case 0x1e16:
	case 0x1eba:
	case 0x1ebc:
	case 0x1ebe:
	case 0x1ec0:
	case 0x1ec2:
	case 0x1ec4:
	case 0x0065:
	case 0x00e8:
	case 0x00e9:
	case 0x00ea:
	case 0x00eb:
	case 0x0113:
	case 0x0115:
	case 0x0117:
	case 0x011b:
	case 0x0205:
	case 0x0207:
	case 0x1e15:
	case 0x1e17:
	case 0x1ebb:
	case 0x1ebd:
	case 0x1ebf:
	case 0x1ec1:
	case 0x1ec3:
	case 0x1ec5:
	case 0x0049:
	case 0x00cc:
	case 0x00cd:
	case 0x00ce:
	case 0x00cf:
	case 0x0128:
	case 0x012a:
	case 0x012c:
	case 0x0130:
	case 0x01cf:
	case 0x0208:
	case 0x020a:
	case 0x1e2e:
	case 0x1ec8:
	case 0x0069:
	case 0x00ec:
	case 0x00ed:
	case 0x00ee:
	case 0x00ef:
	case 0x0129:
	case 0x012b:
	case 0x012d:
	case 0x01d0:
	case 0x0209:
	case 0x020b:
	case 0x1e2f:
	case 0x1ec9:
	case 0x0055:
	case 0x00d9:
	case 0x00da:
	case 0x00db:
	case 0x00dc:
	case 0x0168:
	case 0x016a:
	case 0x016c:
	case 0x016e:
	case 0x0170:
	case 0x01d3:
	case 0x01d5:
	case 0x01d7:
	case 0x01d9:
	case 0x01db:
	case 0x0214:
	case 0x0216:
	case 0x1e78:
	case 0x1e7a:
	case 0x1ee6:
	case 0x0075:
	case 0x00f9:
	case 0x00fa:
	case 0x00fb:
	case 0x00fc:
	case 0x0169:
	case 0x016b:
	case 0x016d:
	case 0x016f:
	case 0x0171:
	case 0x01d4:
	case 0x01d6:
	case 0x01d8:
	case 0x01da:
	case 0x01dc:
	case 0x0215:
	case 0x0217:
	case 0x1e79:
	case 0x1e7b:
	case 0x1ee7:
	  return false;
	default:
	  return true;
	}

    case 0x0331:
      switch (p)
	{
	case 0x0042:
	case 0x1e02:
	case 0x0062:
	case 0x1e03:
	case 0x0044:
	case 0x010e:
	case 0x1e0a:
	case 0x0064:
	case 0x010f:
	case 0x1e0b:
	case 0x004b:
	case 0x01e8:
	case 0x1e30:
	case 0x006b:
	case 0x01e9:
	case 0x1e31:
	case 0x004c:
	case 0x0139:
	case 0x013d:
	case 0x006c:
	case 0x013a:
	case 0x013e:
	case 0x004e:
	case 0x00d1:
	case 0x0143:
	case 0x0147:
	case 0x01f8:
	case 0x1e44:
	case 0x006e:
	case 0x00f1:
	case 0x0144:
	case 0x0148:
	case 0x01f9:
	case 0x1e45:
	case 0x0052:
	case 0x0154:
	case 0x0158:
	case 0x0210:
	case 0x0212:
	case 0x1e58:
	case 0x0072:
	case 0x0155:
	case 0x0159:
	case 0x0211:
	case 0x0213:
	case 0x1e59:
	case 0x0054:
	case 0x0164:
	case 0x1e6a:
	case 0x0074:
	case 0x0165:
	case 0x1e6b:
	case 0x1e97:
	case 0x005a:
	case 0x0179:
	case 0x017b:
	case 0x017d:
	case 0x1e90:
	case 0x007a:
	case 0x017a:
	case 0x017c:
	case 0x017e:
	case 0x1e91:
	case 0x0068:
	case 0x0125:
	case 0x021f:
	case 0x1e23:
	case 0x1e27:
	  return false;
	default:
	  return true;
	}

    case 0x0338:
      switch (p)
	{
	/* Non-NFC cases not applicable to C/C++.  */
	default:
	  return true;
	}

    case 0x0342:
      switch (p)
	{
	case 0x1f00:
	case 0x1f80:
	case 0x1f01:
	case 0x1f81:
	case 0x1f08:
	case 0x1f88:
	case 0x1f09:
	case 0x1f89:
	case 0x1f20:
	case 0x1f90:
	case 0x1f21:
	case 0x1f91:
	case 0x1f28:
	case 0x1f98:
	case 0x1f29:
	case 0x1f99:
	case 0x1f30:
	case 0x1f31:
	case 0x1f38:
	case 0x1f39:
	case 0x1f50:
	case 0x1f51:
	case 0x1f59:
	case 0x1f60:
	case 0x1fa0:
	case 0x1f61:
	case 0x1fa1:
	case 0x1f68:
	case 0x1fa8:
	case 0x1f69:
	case 0x1fa9:
	case 0x03b1:
	case 0x1fb3:
	case 0x00a8:
	case 0x03b7:
	case 0x1fc3:
	case 0x1fbf:
	case 0x03b9:
	case 0x03ca:
	case 0x1ffe:
	case 0x03c5:
	case 0x03cb:
	case 0x03c9:
	case 0x1ff3:
	  return false;
	default:
	  return true;
	}

    case 0x0345:
      switch (p)
	{
	case 0x1f00:
	case 0x1f01:
	case 0x1f02:
	case 0x1f03:
	case 0x1f04:
	case 0x1f05:
	case 0x1f06:
	case 0x1f07:
	case 0x1f08:
	case 0x1f09:
	case 0x1f0a:
	case 0x1f0b:
	case 0x1f0c:
	case 0x1f0d:
	case 0x1f0e:
	case 0x1f0f:
	case 0x1f20:
	case 0x1f21:
	case 0x1f22:
	case 0x1f23:
	case 0x1f24:
	case 0x1f25:
	case 0x1f26:
	case 0x1f27:
	case 0x1f28:
	case 0x1f29:
	case 0x1f2a:
	case 0x1f2b:
	case 0x1f2c:
	case 0x1f2d:
	case 0x1f2e:
	case 0x1f2f:
	case 0x1f60:
	case 0x1f61:
	case 0x1f62:
	case 0x1f63:
	case 0x1f64:
	case 0x1f65:
	case 0x1f66:
	case 0x1f67:
	case 0x1f68:
	case 0x1f69:
	case 0x1f6a:
	case 0x1f6b:
	case 0x1f6c:
	case 0x1f6d:
	case 0x1f6e:
	case 0x1f6f:
	case 0x1f70:
	case 0x03b1:
	case 0x03ac:
	case 0x1fb6:
	case 0x0391:
	case 0x1f74:
	case 0x03b7:
	case 0x03ae:
	case 0x1fc6:
	case 0x0397:
	case 0x1f7c:
	case 0x03c9:
	case 0x03ce:
	case 0x1ff6:
	case 0x03a9:
	  return false;
	default:
	  return true;
	}

    case 0x0653:
      switch (p)
	{
	case 0x0627:
	  return false;
	default:
	  return true;
	}

    case 0x0654:
      switch (p)
	{
	case 0x0627:
	case 0x0648:
	case 0x064a:
	case 0x06d5:
	case 0x06c1:
	case 0x06d2:
	  return false;
	default:
	  return true;
	}

    case 0x0655:
      switch (p)
	{
	case 0x0627:
	case 0x0622:
	case 0x0623:
	  return false;
	default:
	  return true;
	}

    case 0x093c:
      switch (p)
	{
	case 0x0928:
	case 0x0930:
	case 0x0933:
	case 0x0915:
	case 0x0916:
	case 0x0917:
	case 0x091c:
	case 0x0921:
	case 0x0922:
	case 0x092b:
	case 0x092f:
	  return false;
	default:
	  return true;
	}

    case 0x09be:
      switch (p)
	{
	case 0x09c7:
	  return false;
	default:
	  return true;
	}

    case 0x09d7:
      switch (p)
	{
	case 0x09c7:
	  return false;
	default:
	  return true;
	}

    case 0x0b3e:
      switch (p)
	{
	case 0x0b47:
	  return false;
	default:
	  return true;
	}

    case 0x0b56:
      switch (p)
	{
	case 0x0b47:
	  return false;
	default:
	  return true;
	}

    case 0x0b57:
      switch (p)
	{
	case 0x0b47:
	  return false;
	default:
	  return true;
	}

    case 0x0bbe:
      switch (p)
	{
	case 0x0bc6:
	case 0x0bc7:
	  return false;
	default:
	  return true;
	}

    case 0x0bd7:
      switch (p)
	{
	case 0x0b92:
	case 0x0bc6:
	  return false;
	default:
	  return true;
	}

    case 0x0c56:
      switch (p)
	{
	case 0x0c46:
	  return false;
	default:
	  return true;
	}

    case 0x0cc2:
      switch (p)
	{
	case 0x0cc6:
	  return false;
	default:
	  return true;
	}

    case 0x0cd5:
      switch (p)
	{
	case 0x0cbf:
	case 0x0cc6:
	case 0x0cca:
	  return false;
	default:
	  return true;
	}

    case 0x0cd6:
      switch (p)
	{
	case 0x0cc6:
	  return false;
	default:
	  return true;
	}

    case 0x0d3e:
      switch (p)
	{
	case 0x0d46:
	case 0x0d47:
	  return false;
	default:
	  return true;
	}

    case 0x0d57:
      switch (p)
	{
	case 0x0d46:
	  return false;
	default:
	  return true;
	}

    case 0x0dca:
      switch (p)
	{
	case 0x0dd9:
	case 0x0ddc:
	  return false;
	default:
	  return true;
	}

    case 0x0dcf:
      switch (p)
	{
	case 0x0dd9:
	case 0x0dda:
	  return false;
	default:
	  return true;
	}

    case 0x0ddf:
      switch (p)
	{
	case 0x0dd9:
	case 0x0dda:
	  return false;
	default:
	  return true;
	}

    case 0x102e:
      switch (p)
	{
	case 0x1025:
	  return false;
	default:
	  return true;
	}

    case 0x1b35:
      switch (p)
	{
	case 0x1b05:
	case 0x1b07:
	case 0x1b09:
	case 0x1b0b:
	case 0x1b0d:
	case 0x1b11:
	case 0x1b3a:
	case 0x1b3c:
	case 0x1b3e:
	case 0x1b3f:
	case 0x1b42:
	  return false;
	default:
	  return true;
	}

    case 0x3099:
      switch (p)
	{
	case 0x304b:
	case 0x304d:
	case 0x304f:
	case 0x3051:
	case 0x3053:
	case 0x3055:
	case 0x3057:
	case 0x3059:
	case 0x305b:
	case 0x305d:
	case 0x305f:
	case 0x3061:
	case 0x3064:
	case 0x3066:
	case 0x3068:
	case 0x306f:
	case 0x3072:
	case 0x3075:
	case 0x3078:
	case 0x307b:
	case 0x3046:
	case 0x309d:
	case 0x30ab:
	case 0x30ad:
	case 0x30af:
	case 0x30b1:
	case 0x30b3:
	case 0x30b5:
	case 0x30b7:
	case 0x30b9:
	case 0x30bb:
	case 0x30bd:
	case 0x30bf:
	case 0x30c1:
	case 0x30c4:
	case 0x30c6:
	case 0x30c8:
	case 0x30cf:
	case 0x30d2:
	case 0x30d5:
	case 0x30d8:
	case 0x30db:
	case 0x30a6:
	case 0x30ef:
	case 0x30f0:
	case 0x30f1:
	case 0x30f2:
	case 0x30fd:
	  return false;
	default:
	  return true;
	}

    case 0x309a:
      switch (p)
	{
	case 0x306f:
	case 0x3072:
	case 0x3075:
	case 0x3078:
	case 0x307b:
	case 0x30cf:
	case 0x30d2:
	case 0x30d5:
	case 0x30d8:
	case 0x30db:
	  return false;
	default:
	  return true;
	}

    case 0x110ba:
      switch (p)
	{
	case 0x11099:
	case 0x1109b:
	case 0x110a5:
	  return false;
	default:
	  return true;
	}

    case 0x11127:
      switch (p)
	{
	case 0x11131:
	case 0x11132:
	  return false;
	default:
	  return true;
	}

    case 0x1133e:
      switch (p)
	{
	case 0x11347:
	  return false;
	default:
	  return true;
	}

    case 0x11357:
      switch (p)
	{
	case 0x11347:
	  return false;
	default:
	  return true;
	}

    case 0x114b0:
      switch (p)
	{
	case 0x114b9:
	  return false;
	default:
	  return true;
	}

    case 0x114ba:
      switch (p)
	{
	case 0x114b9:
	  return false;
	default:
	  return true;
	}

    case 0x114bd:
      switch (p)
	{
	case 0x114b9:
	  return false;
	default:
	  return true;
	}

    case 0x115af:
      switch (p)
	{
	case 0x115b8:
	case 0x115b9:
	  return false;
	default:
	  return true;
	}

    case 0x11930:
      switch (p)
	{
	case 0x11935:
	  return false;
	default:
	  return true;
	}

    default:
      cpp_error (pfile, CPP_DL_ICE, "Character %x might not be NFKC", c);
      return true;
  }
}
