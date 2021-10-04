// -*- C++ -*-

// Copyright (C) 2005-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 3, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.


// Copyright (C) 2004 Ami Tavory and Vladimir Dreizin, IBM-HRL.

// Permission to use, copy, modify, sell, and distribute this software
// is hereby granted without fee, provided that the above copyright
// notice appears in all copies, and that both that copyright notice
// and this permission notice appear in supporting documentation. None
// of the above authors, nor IBM Haifa Research Laboratories, make any
// representation about the suitability of this software for any
// purpose. It is provided "as is" without express or implied
// warranty.

/**
 * @file priority_queue_dijkstra_example.cpp
 * A basic example showing how to cross reference a vector and a
 * priority-queue for modify.
 */

/**
 * This example shows how to cross-reference priority queues
 * and a vector. I.e., using a vector to
 * map keys to entries in a priority queue, and using the priority
 * queue to map entries to the vector. The combination
 * can be used for fast modification of keys.
 *
 * As an example, a very simple form of Diskstra's algorithm is used. The graph
 * is represented by an adjacency matrix. Nodes and vertices are size_ts, and
 * it is assumed that the minimal path between any two nodes is less than 1000.
 */



#include <vector>
#include <iostream>
#include <ext/pb_ds/priority_queue.hpp>

using namespace std;
using namespace __gnu_pbds;

// The value type of the priority queue.
// The first entry is the node's id, and the second is the distance.
typedef std::pair<size_t, size_t> pq_value;

// Comparison functor used to compare priority-queue value types.
struct pq_value_cmp
{
  bool
  operator()(const pq_value& r_lhs, const pq_value& r_rhs) const
  {
    // Note that a value is considered smaller than a different value
    // if its distance is* larger*. This is because by STL
    // conventions, "larger" entries are nearer the top of the
    // priority queue.
    return r_rhs.second < r_lhs.second;
  }
};

int main()
{
  enum
    {
      // Number of vertices is hard-coded in this example.
      num_vertices = 5,
      // "Infinity".
      graph_inf = 1000
    };

  // The edge-distance matrix.
  // For example, the distance from node 0 to node 1 is 5, and the
  // distance from node 1 to node 0 is 2.
  const size_t a_a_edge_legnth[num_vertices][num_vertices] =
    {
      {0, 5, 3, 7, 6},
      {2, 0, 2, 8, 9},
      {2, 1, 0, 8, 0},
      {1, 8, 3, 0, 2},
      {2, 3, 4, 2, 0}
    };

  // The priority queue type.
  typedef __gnu_pbds::priority_queue< pq_value, pq_value_cmp> pq_t;

  // The priority queue object.
  pq_t p;

  // This vector contains for each node, a find-iterator into the
  // priority queue.
  vector<pq_t::point_iterator> a_it;

  // First we initialize the data structures.

  // For each node, we push into the priority queue a value
  // identifying it with a distance of infinity.
  for (size_t i = 0; i < num_vertices; ++i)
    a_it.push_back(p.push(pq_value(i, graph_inf)));

  // Now we take the initial node, in this case 0, and modify its
  // distance to 0.
  p.modify(a_it[0], pq_value(0, 0));

  // The priority queue contains all vertices whose final distance has
  // not been determined, so to finish the algorithm, we must loop
  // until it is empty.
  while (!p.empty())
    {
      // First we find the node whose distance is smallest.
      const pq_value& r_v = p.top();
      const size_t node_id = r_v.first;
      const size_t dist = r_v.second;

      // This is the node's final distance, so we can print it out.
      cout << "The distance from 0 to " << node_id 
	   << " is " << dist << endl;

      // Now we go over the node's neighbors and "relax" the
      // distances, if applicable.
      for (size_t neighbor_i = 0; neighbor_i < num_vertices; ++neighbor_i)
        {
	  // Potentially, the distance to the neighbor is the distance
	  // to the currently-considered node + the distance from this
	  // node to the neighbor.
	  const size_t pot_dist = dist + a_a_edge_legnth[node_id][neighbor_i];

	  if (a_it[neighbor_i] == a_it[0])
	    continue;

	  // "Relax" the distance (if appropriate) through modify.
	  if (pot_dist < a_it[neighbor_i]->second)
	    p.modify(a_it[neighbor_i], pq_value(neighbor_i, pot_dist));
        }

      // Done with the node, so we pop it.
      a_it[node_id] = a_it[0];
      p.pop();
    }

  return 0;
}
