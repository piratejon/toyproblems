#ifndef _JS_GRAPH_HPP
#define _JS_GRAPH_HPP

#include <utility>

#include <boost/graph/dijkstra_shortest_paths.hpp>
#include <boost/property_map/property_map.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graph_traits.hpp>

typedef boost::adjacency_list <boost::setS, boost::vecS, boost::undirectedS, boost::no_property, boost::property<boost::edge_weight_t, int>> graph_t;
typedef boost::graph_traits<graph_t>::vertex_descriptor vertex_descriptor;
typedef std::pair<int, int> edge;

class Graph {
  public:
    Graph(int V);

    void add_edge(int u, int v, int owner);

    void dijkstra_shortest_paths(int u);
};

#endif // _JS_GRAPH_HPP

