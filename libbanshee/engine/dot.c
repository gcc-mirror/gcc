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

#include <assert.h>
#include <regions.h>
#include "dot.h"
#include "hash.h"

static FILE *of;
static hash_table node_hash_table;
static region dot_region;
static int node_count;
static const char *edge_op;

static void print_n_attrs(node_attr *attrs, int n)
{
  int i;
  fputc('[',of);

  for (i = 0; i < n; i++)
    {
      const char *name;
      switch (attrs[i].name)
	{ 
	case n_color:
	  name = "color";
	  break;
	case n_fontcolor:
	  name = "fontcolor";
	  break;
	case n_fontname:
	  name = "fontname";
	  break;
	case n_fontsize:
	  name = "fontsize";
	  break;
	case n_height:
	  name = "height";
	  break;
	case n_width:
	  name = "width";
	  break;
	case n_label:
	  name = "label";
	  break;
	case n_layer:
	  name = "layer";
	  break;
	case n_shape:
	  name = "shape";
	  break;
	case n_shapefile:
	  name = "shapefile";
	  break;
	case n_style:
	  name = "style";
	  break;
	default:
	  name = "";
	  assert(0);
	  break;
	}
      if (i > 0)
	fputc(',',of);
      fprintf(of,"%s = %s",name,attrs[i].value);
    }

  fputc(']',of);
}

static void print_e_attrs(edge_attr *attrs, int n)
{
  int i;
  fputc('[',of);
  for (i = 0; i < n; i++)
    {
      const char *name;
      switch(attrs[i].name)
	{
	case e_color:
	  name = "color";
	  break;
	case e_decorate:
	  name = "decorate";
	  break;
	case e_dir:
	  name = "dir";
	  break;
	case e_fontcolor:
	  name = "fontcolor";
	  break;
	case e_fontname:
	  name = "fontname";
	  break;
	case e_fontsize:
	  name = "fontsize";
	  break;
	case e_id:
	  name = "id";
	  break;
	case e_label:
	  name = "label";
	  break;
	case e_layer:
	  name = "layer";
	  break;
	case e_minlen:
	  name = "minlen";
	  break;
	case e_style:
	  name = "style";
	  break;
	case e_weight:
	  name = "weight";
	  break;
	default : 
	  name = "";
	  assert(0);
	  break;
	}
      if (i > 0)
	fputc(',',of);
      fprintf(of,"%s = %s",name,attrs[i].value);
    }
  fputc(']',of);
}

static void print_g_attrs(graph_attr *attrs, int n)
{
 int i;
  fputc('[',of);

  for (i = 0; i < n; i++)
    {
      const char *name;
      switch (attrs[i].name)
	{ 
	case g_center:
	  name = "center";
	  break;
	case g_clusterrank:
	  name = "clusterrank";
	  break;
	case g_color:
	  name = "color";
	  break;
	case g_concentrate:
	  name = "concentrate";
	  break;
	case g_fontcolor:
	  name = "fontcolor";
	  break;
	case g_fontname:
	  name = "fontname";
	  break;
	case g_fontsize:
	  name = "fontsize";
	  break;
	case g_label:
	  name = "label";
	  break;
	case g_layerseq:
	  name = "layerseq";
	  break;
	case g_margin:
	  name = "margin";
	  break;
	case g_mclimit:
	  name = "mclimit";
	  break;
	case g_nodesep:
	  name = "nodesep";
	  break;
	case g_nslimit:
	  name = "nslimit";
	  break;
	case g_ordering:
	  name = "ordering";
	  break;
	case g_orientation:
	  name = "orientation";
	  break;
	case g_page:
	  name = "page";
	  break;
	case g_rank:
	  name = "rank";
	  break;
	case g_rankdir:
	  name = "rankdir";
	  break;
	case g_ranksep:
	  name = "ranksep";
	  break;
	case g_ratio:
	  name = "ratio";
	  break;
	case g_size:
	  name = "size";
	  break;
	default : 
	  name = "";
	  assert(0);
	  break;
	}
      if (i > 0)
	fputc(',',of);
      fprintf(of,"%s = %s",name,attrs[i].value);
    }
  fputc(']',of);
}


void dot_start(FILE *to,const char *name,bool is_directed,bool is_strict)
{
  const char *graph_type,*strict;


  node_count = 0;
  dot_region = newregion();
  node_hash_table = make_string_hash_table(dot_region,8,TRUE);
  of = to;

  if (is_directed)
    {
      edge_op = "->";
      graph_type = "digraph";
    }
  else
    {
      edge_op = "--";
      graph_type = "graph";
    }

  if (is_strict)
    strict = "strict";
  else
    strict = "";

  fprintf(of,"%s %s %s{\n",strict,graph_type,name);  
  
}

void dot_global_graph_style(graph_attr *attrs, int n)
{
  fputs("graph ",of);
  print_g_attrs(attrs,n);
  fputc(';',of);
  fputc('\n',of);
}

void dot_global_edge_style(edge_attr *attrs, int n)
{
  fputs("edge ",of);
  print_e_attrs(attrs,n);
  fputc(';',of);
  fputc('\n',of);
}

void dot_global_node_style(node_attr *attrs, int n)
{
  fputs("node ",of);
  print_n_attrs(attrs,n);
  fputc(';',of);
  fputc('\n',of);
}

/* by default, set the node's name to label */
static void declare_node(dot_node n, char *label)
{
  int i;
  char mangled[512];

  if (label[0] == '\"')
    mangled[0] = 's';
  else
    mangled[0] = label[0];

  for (i = 1; label[i] && i < 512 ;i++)
    {
      if (label[i] == '\"')
	mangled[i] = '_';
      else mangled[i] = label[i];
    }
  mangled[i] = '\0';
  
  fprintf(of,"nd_%d [label=\"%s\"]\n",n,mangled);
}

dot_node dot_get_node(char *label) deletes
{
  dot_node result;
  if (!hash_table_lookup(node_hash_table,(hash_key)label,(hash_data *)(char *)&result))
    {
      dot_node newnode = node_count++;
      
      declare_node(newnode,label);
      hash_table_insert(node_hash_table,
			(hash_key)rstrdup(dot_region,label),
			(hash_data)newnode);
      
      return newnode;
    }
  else
    return result;
  
}

void dot_node_style(dot_node node,node_attr *attrs, int n)
{
  fprintf(of,"nd_%d ",node);
  print_n_attrs(attrs,n);
  fputc(';',of);
  fputc('\n',of);
}

void dot_plain_edge(dot_node from, dot_node to)
{
  fprintf(of,"nd_%d %s nd_%d;\n",from,edge_op,to);
}

void dot_styled_edge(dot_node from, dot_node to, edge_attr *attrs, int n)
{
  fprintf(of,"nd_%d %s nd_%d ",from,edge_op,to);
  print_e_attrs(attrs,n);
  fputc(';',of);
  fputc('\n',of);
}

void dot_end(void) deletes
{
  fputc('}',of);
  hash_table_delete(node_hash_table);
  deleteregion_ptr(&dot_region);
}
