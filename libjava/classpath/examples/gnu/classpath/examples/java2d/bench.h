/* bench.h -- native benchmark for Cairo library (meant to test java2d)
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

#ifndef __BENCH_H__
#define __BENCH_H__

#include <gtk/gtk.h>

G_BEGIN_DECLS

#define BENCHMARK_TYPE             (benchmark_get_type())
#define BENCHMARK(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj, BENCHMARK_TYPE, Benchmark)
#define BENCHMARK_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), BENCHMARK_TYPE, BenchmarkClass);
#define IS_BENCHMARK(obj)          (G_TYPE_CHECK_INSTANCE_TYPE ((obj), BENCHMARK_TYPE))
#define IS_BENCHMARK_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass), BENCHMARK_TYPE))
#define BENCHMARK_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS ((obj), BENCHMARK_TYPE, BenchmarkClass))

typedef struct _Benchmark Benchmark;
typedef struct _BenchmarkClass BenchmarkClass;

struct _Benchmark {
	GtkDrawingArea parent;
	
};

struct _BenchmarkClass {
	GtkDrawingAreaClass parent_class;
};

GType     benchmark_get_type (void);
GtkWidget *benchmark_new     (void);

static int minSize;
static int antialias;
static int arcTest;
static int curveTest;
static int lineTest;
static int rectTest;

static int screenHeight;
static int screenWidth;
static int testSize;
static int lineWidth;

G_END_DECLS

#endif
