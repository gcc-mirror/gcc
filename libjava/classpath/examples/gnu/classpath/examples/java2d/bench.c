/* bench.c -- native benchmark for Cairo library (meant to test java2d)
   Copyright (C) 2006  Free Software Foundation, Inc.

This file is part of GNU Classpath examples.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA. */

#include "bench.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gtk/gtk.h>
#include <sys/timeb.h>

G_DEFINE_TYPE (Benchmark, benchmark, GTK_TYPE_DRAWING_AREA);

// Needed for the gtk widget, but not used:
static void
benchmark_class_init (BenchmarkClass *klass)
{
}

static void
benchmark_init (Benchmark *obj)
{
}

// The Arc2D's PathIterator uses some transforms, so we condense the required
// functionality of AffineTransform
static void
doTransform (double rx, double ry, double theta, double *cvec)
{
  // Define identity matrix (corresponds to new AffineTransform())
  double m00 = 1;
  double m10 = 0;
  double m01 = 0;
  double m11 = 1;
  double m02 = 0;
  double m12 = 0;

  // AffineTransform.scale(rx, ry)
  m00 = m00 * rx;
  m01 = m01 * ry;
  m10 = m10 * rx;
  m11 = m11 * ry;

  // AffineTransform.rotate(theta)
  double c = cos(theta);
  double s = sin(theta);
  double n00 = m00 *  c + m01 * s;
  double n01 = m00 * -s + m01 * c;
  double n10 = m10 *  c + m11 * s;
  double n11 = m10 * -s + m11 * c;

  m00 = n00;
  m01 = n01;
  m10 = n10;
  m11 = n11;    
    
  // AffineTransform.transform(cvec, 0, cvec, 0, 1)
  double dstPts[2];
  dstPts[0] = (float) (m00 * cvec[0] + m01 * cvec[1] + m02);
  dstPts[1] = (float) (m10 * cvec[0] + m11 * cvec[1] + m12);
  cvec[0] = dstPts[0];
  cvec[1] = dstPts[1];
}

// Place an arc on the cairo path, simulating java2d's Arc2D
static void 
setupArc(cairo_t *cr, GtkWidget *bench, int shift)
{
  double x, y;
  
  // Normally passed into the Arc2D constructor
  x = bench->allocation.x + (rand() % (bench->allocation.width - minSize + 1));
  y = bench->allocation.y + (rand() % (bench->allocation.height - minSize + 1));
  
  int angle = rand() % 360;
  int length = (rand() % 360) - angle;
  int width = rand() % (int)((bench->allocation.width - x - 10) + 10);
  int height = rand() % (int)((bench->allocation.height - y - 10) + 10);
  
  // This is from the ArcPath iterator
  double start = angle * (M_PI / 180);
  double extent = length * (M_PI / 180);

  if (extent < 0)
    {
      extent = -extent;
      start = 2 * M_PI - extent + start;
    }

  int limit;
  if (width < 0 || height < 0)  // We assume type == 0; ie, Arc2D.OPEN
    limit = -1;
  else if (extent == 0)
    limit = 0;
  else if (extent <= M_PI / 2.0)
    limit = 1;
  else if (extent <= M_PI)
    limit = 2;
  else if (extent <= 3.0 * (M_PI / 2.0))
    limit = 3;
  else
    limit = 4;
    
  // This is from CairoGraphics2D.walkPath
  double xnew = 0;
  double ynew = 0;
  double coords[6];

  cairo_fill_rule_t cfillrule = CAIRO_FILL_RULE_WINDING;
  cairo_set_fill_rule(cr, cfillrule);
  
  // First iteration will move to the starting point
  double rx = width / 2;
  double ry = height / 2;
  double xmid = x + rx;
  double ymid = y + ry;
  coords[0] = xmid + rx * cos(start);
  coords[1] = ymid - ry * sin(start);
  
  if (shift == 1)
    {
      xnew = floor(coords[0]) + 0.5;
      ynew = floor(coords[1]) + 0.5;
    }
  else
    {
      xnew = coords[0];
      ynew = coords[1];
    }
    
  cairo_move_to(cr, xnew, ynew);

  // Iterate through segments of the arc  
  int current;
  for (current = 1; current <= limit; current++)
    {
      // Back to the ArcPath iterator's getCurrent
      double kappa = (sqrt(2.0) - 1.0) * (4.0 / 3.0);
      double quad = (M_PI / 2.0);

      double curr_begin = start + (current - 1) * quad;
      double curr_extent;
      
      if (start + extent - curr_begin < quad)
        curr_extent = (start + extent) - curr_begin;
      else
        curr_extent = quad;
    
      double portion_of_a_quadrant = curr_extent / quad;

      double x0 = xmid + rx * cos(curr_begin);
      double y0 = ymid - ry * sin(curr_begin);

      double x1 = xmid + rx * cos(curr_begin + curr_extent);
      double y1 = ymid - ry * sin(curr_begin + curr_extent);

      double cvec[2];
      double len = kappa * portion_of_a_quadrant;
      double angle = curr_begin;

      cvec[0] = 0;
      cvec[1] = len;
      doTransform(rx, ry, angle, cvec);
      coords[0] = x0 + cvec[0];
      coords[1] = y0 - cvec[1];

      cvec[0] = 0;
      cvec[1] = -len;
      doTransform(rx, ry, angle, cvec);
      doTransform(1, 1, curr_extent, cvec);
      coords[2] = x1 + cvec[0];
      coords[3] = y1 - cvec[1];

      coords[4] = x1;
      coords[5] = y1;
    
      // draw it, from CairoGraphics2D.walkPath
      if (shift == 1)
        {
          xnew = floor(coords[4]) + 0.5;
          ynew = floor(coords[5]) + 0.5;
          cairo_curve_to(cr, floor(coords[0]) + 0.5, floor(coords[1]) + 0.5,
                         floor(coords[2]) + 0.5, floor(coords[3]) + 0.5,
                         xnew, ynew);
        }
      else
        {
          xnew = coords[4];
          ynew = coords[5];
          cairo_curve_to(cr, coords[0], coords[1], coords[2],
                         coords[3], xnew, ynew);
        }
    }
  
  // Randomize the colour, just for asthetics =)
  cairo_set_source_rgb(cr, (rand() % 100 / (float)100),
                       (rand() % 100 / (float)100),
                       (rand() % 100 / (float)100));
  
}

// Place a beizer curve on the cairo path, simulating java2d's CubicCurve2D
static void 
setupCurve(cairo_t *cr, GtkWidget *bench, int shift)
{
  // These are options when creating a new curve
  int x1 = bench->allocation.x + (rand() % (bench->allocation.width - minSize));
  int y1 = bench->allocation.y + (rand() % (bench->allocation.height - minSize));
  int xc1 = bench->allocation.x + (rand() % (bench->allocation.width - minSize));
  int yc1 = bench->allocation.y + (rand() % (bench->allocation.height - minSize));
  int xc2 = bench->allocation.x + (rand() % (bench->allocation.width - minSize));
  int yc2 = bench->allocation.y + (rand() % (bench->allocation.height - minSize));
  int x2 = bench->allocation.x + (rand() % (bench->allocation.width - minSize));
  int y2 = bench->allocation.y + (rand() % (bench->allocation.height - minSize));
  
  // From CairoGraphics2D.walkPath
  double xnew = 0;
  double ynew = 0;
  double coords[6];

  cairo_fill_rule_t cfillrule = CAIRO_FILL_RULE_WINDING;
  cairo_set_fill_rule(cr, cfillrule);
  
  // And into CubicCurve's PathIterator...
  // start by moving to the starting coordinate
  coords[0] = (float) x1;
  coords[1] = (float) y1;
  
  if (shift == 1)
    {
      xnew = floor(coords[0]) + 0.5;
      ynew = floor(coords[1]) + 0.5;
    }
  else
    {
      xnew = coords[0];
      ynew = coords[1];
    }
    
  cairo_move_to(cr, xnew, ynew);
  
  // Now the curve itself
  coords[0] = (float) xc1;
  coords[1] = (float) yc1;
  coords[2] = (float) xc2;
  coords[3] = (float) yc2;
  coords[4] = (float) x2;
  coords[5] = (float) y2;
  
  if (shift == 1)
    {
      xnew = floor(coords[4]) + 0.5;
      ynew = floor(coords[5]) + 0.5;
      cairo_curve_to(cr, floor(coords[0]) + 0.5, floor(coords[1]) + 0.5,
                     floor(coords[2]) + 0.5, floor(coords[3]) + 0.5,
                     xnew, ynew);
    }
  else
    {
      xnew = coords[4];
      ynew = coords[5];
      cairo_curve_to(cr, coords[0], coords[1], coords[2], 
                     coords[3], xnew, ynew);
    }
  
  // Randomize colour for asthetics
  cairo_set_source_rgb(cr, (rand() % 100 / (float)100),
                       (rand() % 100 / (float)100),
                       (rand() % 100 / (float)100));
}

// Place a line on the cairo path, simulating java2d's Line2D
static void 
setupLine(cairo_t *cr, GtkWidget *bench, int shift)
{
  // These are set when you create a line
  int x1 = bench->allocation.x + (rand() % (bench->allocation.width - minSize));
  int y1 = bench->allocation.y + (rand() % (bench->allocation.height - minSize));
  int x2 = bench->allocation.x + (rand() % (bench->allocation.width - minSize));
  int y2 = bench->allocation.y + (rand() % (bench->allocation.height - minSize));
  
  // This is from CairoGraphics2D.walkPath
  double xnew = 0;
  double ynew = 0;
  double coords[6];

  cairo_fill_rule_t cfillrule = CAIRO_FILL_RULE_WINDING;
  cairo_set_fill_rule(cr, cfillrule);
  
  // And into Line2D's PathIterator
  coords[0] = (float) x1;
  coords[1] = (float) y1;
  
  if (shift == 1)
    {
      xnew = floor(coords[0]) + 0.5;
      ynew = floor(coords[1]) + 0.5;
    }
  else
    {
      xnew = coords[0];
      ynew = coords[1];
    }
    
  cairo_move_to(cr, xnew, ynew);
  
  coords[0] = (float) x2;
  coords[1] = (float) y2;
  
  if (shift == 1)
    {
      xnew = floor(coords[0]) + 0.5;
      ynew = floor(coords[1]) + 0.5;
    }
  else
    {
      xnew = coords[0];
      ynew = coords[1];
    }
    
  cairo_line_to(cr, xnew, ynew);
  
  // Randomize colour for asthetics
  cairo_set_source_rgb(cr, (rand() % 100 / (float)100),
                       (rand() % 100 / (float)100),
                       (rand() % 100 / (float)100));
}

// Place a rectangle on the cairo path, simulating java2d's Rectangle2D
static void 
setupRect(cairo_t *cr, GtkWidget *bench, int shift)
{
  // These are set when you create a rectangle
  int x1 = bench->allocation.x + (rand() % (bench->allocation.width - minSize));
  int y1 = bench->allocation.y + (rand() % (bench->allocation.height - minSize));
  int x2 = bench->allocation.x + (rand() % (bench->allocation.width - minSize));
  int y2 = bench->allocation.y + (rand() % (bench->allocation.height - minSize));
  
  // draw() and fill() have been optimized to ignore the PathIterator.
  // We do the same here.
  double xnew = 0;
  double ynew = 0;
  
  if (shift == 1)
    {
      xnew = floor(x1) + 0.5;
      ynew = floor(y1) + 0.5;
    }
  else
    {
      xnew = x1;
      ynew = y1;
    }
    
  cairo_rectangle(cr, x1, y1, x2, y2);
    
  // Randomize colour for asthetics
  cairo_set_source_rgb(cr, (rand() % 100 / (float)100),
                       (rand() % 100 / (float)100),
                       (rand() % 100 / (float)100));
}

// The real work gets done here: this function is called when the widget
// is drawn on screen.
static void
draw (GtkWidget *bench, cairo_t *cr)
{
  // Setup
  struct timeb t1, t2;
  int i, timeElapsed;

  cairo_set_line_width(cr, lineWidth);
  
  if (antialias == 0)
    cairo_set_antialias(cr, CAIRO_ANTIALIAS_NONE);
  else
    cairo_set_antialias(cr, CAIRO_ANTIALIAS_GRAY);
  
  // Tell the user what's going on
  printf("Testing native cairo drawing..\n");
  printf("  Screen size is %d x %d \n", screenWidth, screenHeight);
  printf("  Line width is %d\n", lineWidth);
  printf("  Test size: %d\n", testSize);
  
  if (antialias == 0)
    printf("  Anti-alias is off\n");
  else
    printf("  Anti-alias is on\n");
    
  printf("\n");
  fflush(stdout);

  // Draw & fill Arc
  if (arcTest == 1)
    {
      // Draw
      ftime(&t1);
      for (i = 0; i < testSize; i++)
        {
          setupArc(cr, bench, 1);
          cairo_stroke (cr);
        }
        
      ftime(&t2);
      timeElapsed = 1000 * (t2.time - t1.time) + (t2.millitm - t1.millitm);
      printf("Draw arc: %d ms\n", timeElapsed);
      fflush(stdout);

      // Fill
      ftime(&t1);
      for (i = 0; i < testSize; i++)
        {
          setupArc(cr, bench, 0);
          cairo_fill (cr);
        }
        
      ftime(&t2);
      timeElapsed = 1000 * (t2.time - t1.time) + (t2.millitm - t1.millitm);
      printf("Fill arc: %d ms\n", timeElapsed);
    }

  // Draw cubic curve
  if (curveTest == 1)
    {
      ftime(&t1);
      for (i = 0; i < testSize; i++)
        {
          setupCurve(cr, bench, 1);
          cairo_stroke (cr);
        }
        
      ftime(&t2);
      timeElapsed = 1000 * (t2.time - t1.time) + (t2.millitm - t1.millitm);
      printf("Draw cubic curve: %d ms\n", timeElapsed);
    }
  
  // Ellipse: skip; this is just a special case of arc
  // General path: skip; this doesn't even work in java2d

  // Draw Line
  if (lineTest == 1)
    {
      ftime(&t1);
      for (i = 0; i < testSize; i++)
        {
          setupLine(cr, bench, 1);
          cairo_stroke (cr);
        }
        
      ftime(&t2);
      timeElapsed = 1000 * (t2.time - t1.time) + (t2.millitm - t1.millitm);
      printf("Draw line: %d ms\n", timeElapsed);
    }
  
  // Draw & fill Rectangle
  if (rectTest == 1)
    {
      // Draw
      ftime(&t1);
      for (i = 0; i < testSize; i++)
        {
          setupRect(cr, bench, 1);
          cairo_stroke (cr);
        }
        
      ftime(&t2);
      timeElapsed = 1000 * (t2.time - t1.time) + (t2.millitm - t1.millitm);
      printf("Draw rectangle: %d ms\n", timeElapsed);
    
      // Fill
      ftime(&t1);
      for (i = 0; i < testSize; i++)
        {
          setupRect(cr, bench, 0);
          cairo_fill (cr);
        }
        
      ftime(&t2);
      timeElapsed = 1000 * (t2.time - t1.time) + (t2.millitm - t1.millitm);
      printf("Fill rectangle: %d ms\n", timeElapsed);
    }
  
  // Round rectangle: skip, it's just a combination of lines and curves
  // Image: skip?
  
  printf("\n");
}

GtkWidget *
benchmark_new (void)
{
  return g_object_new (BENCHMARK_TYPE, NULL);
}

int
main (int argc, char **argv)
{
  // Set defaults
  minSize = 10;
  arcTest = 0;
  curveTest = 0;
  lineTest = 0;
  rectTest = 0;
  screenWidth = 320;
  screenHeight = 240;
  testSize = 1000;
  antialias = 0;
  lineWidth = 1;
  
  // Process any command-line user options
  int i;
  for (i = 1; i < argc; i++)
    {
      // Process options first
      if (!strcmp(argv[i], "-a"))
        antialias = 1;
      else if (!strcmp(argv[i], "-h"))
        screenHeight = atoi(argv[++i]);
      else if (!strcmp(argv[i], "-l"))
        lineWidth = atoi(argv[++i]);
      else if (!strcmp(argv[i], "-t"))
        testSize = atoi(argv[++i]);
      else if (!strcmp(argv[i], "-w"))
        screenWidth = atoi(argv[++i]);
      else if (!strcmp(argv[i], "-h") || !strcmp(argv[i], "--h")
               || !strcmp(argv[i], "-help") || !strcmp(argv[i], "--help"))
        {
          printf("Cairo benchmarker, meant to measure JNI overhead\n");
          printf("Usage: bench [-a] [-h height] [-t test size] [-w width] [tests...]\n");
          printf("\n");
          printf("  Valid options: -a   turn on anti-aliasing (default off)\n");
          printf("                 -h   set screen height (default 240)\n");
          printf("                 -l   set stroke line width (default 1)\n");
          printf("                 -t   set test size (default 1000)\n");
          printf("                 -w   set screen width (default 320)\n");
          printf("                 -h | --help\n");
          printf("  Valid tests: arc\n");
          printf("               curve\n");
          printf("               line\n");
          printf("               rect\n");
          printf("               (default: run all)\n");
          exit (0);
        }

      // Process tests
      else if (!strcmp(argv[i], "arc"))
        arcTest = 1;
      else if (!strcmp(argv[i], "curve"))
        curveTest = 1;
      else if (!strcmp(argv[i], "line"))
        lineTest = 1;
      else if (!strcmp(argv[i], "rect"))
        rectTest = 1;
    }
  
  // If no tests were specified, we default to running all of them
  if (arcTest == 0 && curveTest == 0 && lineTest == 0 && rectTest == 0)
    {
      arcTest = 1;
      curveTest = 1;
      lineTest = 1;
      rectTest = 1;
    }
  
  // Set up gtk widget
  GtkWidget *window, *bench;
  gtk_init (&argc, &argv);

  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_resize(GTK_WINDOW(window), screenWidth, screenHeight);
  gtk_window_set_title(GTK_WINDOW(window), "cairo benchmark");
  
  // Set up benchmkar and cairo surface
  bench = benchmark_new ();
  gtk_container_add (GTK_CONTAINER (window), bench);
  gtk_widget_show_all (window);
  
  cairo_t *cr;
  cr = gdk_cairo_create (bench->window);

  // Run tests
  draw (bench, cr);

  // Hold output on screen until user exits.
  printf("Press any key to exit.\n");
  getchar();
  exit(0);
gtk_main();
}
