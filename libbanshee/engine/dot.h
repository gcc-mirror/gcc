/*
 * Copyright (c) 2000-2001
 *      The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 */

#ifndef DOT_H
#define DOT_H

#include <stdio.h>
#include "linkage.h"
#include "bool.h"

EXTERN_C_BEGIN

typedef int dot_node;

typedef struct
{
  enum n_attrs
  {
    n_color,
    n_fontcolor,
    n_fontname,
    n_fontsize,
    n_height,
    n_width,
    n_label,
    n_layer,
    n_shape,
    n_shapefile,
    n_style
  } name;
  const char *value;
} node_attr;

typedef struct
{
  enum e_attrs
  {
    e_color,
    e_decorate,
    e_dir,
    e_fontcolor,
    e_fontname,
    e_fontsize,
    e_id,
    e_label,
    e_layer,
    e_minlen,
    e_style,
    e_weight
  } name;
  const char *value;
} edge_attr;

typedef struct
{
  enum g_attrs
  {
    g_center,
    g_clusterrank,
    g_color,
    g_concentrate,
    g_fontcolor,
    g_fontname,
    g_fontsize,
    g_label,
    g_layerseq,
    g_margin,
    g_mclimit,
    g_nodesep,
    g_nslimit,
    g_ordering,
    g_orientation,
    g_page,
    g_rank,
    g_rankdir,
    g_ranksep,
    g_ratio,
    g_size
  } name;
  const char *value;
} graph_attr;

void dot_start(FILE *to,const char *name,bool directed,bool strict);

void dot_global_graph_style(graph_attr *attrs,int n);
void dot_global_edge_style(edge_attr *attrs,int n);
void dot_global_node_style(node_attr *attrs,int n);

dot_node dot_get_node(char *label) deletes;
void dot_node_style(dot_node node,node_attr *attrs,int n);

void dot_plain_edge(dot_node from, dot_node to);
void dot_styled_edge(dot_node from, dot_node to, edge_attr *attrs,int n);

void dot_end(void) deletes;

EXTERN_C_END

#endif /* DOT_H */
