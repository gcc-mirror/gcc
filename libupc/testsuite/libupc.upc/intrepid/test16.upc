/*
UPC tests

Copyright (C) 2001
Written by Gary Funck <gary@intrepid.com>
and Nenad Vukicevic <nenad@intrepid.com>

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*/
#include <stdio.h>
#include <stdlib.h>

#include <upc.h>

#define BLKSIZE 10
int shared [BLKSIZE] a[100*THREADS];

struct row {
   int x, y, z;
   };

/* Triples of random integers in the range 1..99 */
struct row data[20] = {
    {23,29,84}, {15,58,19}, {81,17,48}, {15,36,49},
    {10,63,1}, {72,10,48}, {25,67,89}, {75,72,90},
    {92,37,89}, {77,32,19}, {99,16,70}, {50,93,71},
    {10,20,55}, {70,7,51}, {19,27,63}, {44,3,46},
    {91,26,89}, {22,63,57}, {33,10,50}, {56,85,4}
   };

int main ()
{
   int i;
   int shared [BLKSIZE] *p0, *p1;
   int diff, expected;
   for (i = 0; i < 19; ++i)
     {
       int t0 = MYTHREAD;
       int t1 = (MYTHREAD + data[i].x) % THREADS;
       int j = data[i].y;
       int k = data[i].z;
       int ediff = (k - j);
       int pdiff = k % BLKSIZE - j % BLKSIZE;
       int tdiff = (t1 - t0);
       p0 = &a[((j / BLKSIZE) * THREADS + t0) * BLKSIZE + (j % BLKSIZE)];
       p1 = &a[((k / BLKSIZE) * THREADS + t1) * BLKSIZE + (k % BLKSIZE)];
       diff = p1 - p0;
       expected = (ediff - pdiff) * THREADS + tdiff * BLKSIZE + pdiff;
       if (diff != expected)
         {
	   fprintf (stderr, "Error: pointer difference: %d not equal to expected: %d\n",
	            diff, expected);
	   abort ();
	 }
      }
   upc_barrier;
   if (!MYTHREAD)
     {
      printf ("test16: test pointer difference between pointers to shared array with layout specifier - passed.\n");
     }
   return 0;
}
