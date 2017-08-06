/*
 * map.cpp
 * lambdapunter@z10x.com
 * 2017-08-05
 * Try to do some graph math or something to help pick the best move.
 * */

#include <iostream>
#include <fstream>
#include <map>

#include <boost/range/combine.hpp>
#include <boost/tuple/tuple.hpp>
#include <boost/foreach.hpp>

#include "json.hpp"

#include "graph.hpp"

using json = nlohmann::json;

const char MAPFILE[] = "map.json";

int main(int arfc, char ** arfv) {
  json json_map;
  std::map<int, int> phy_to_log, log_to_phy;
  std::ifstream mapfile(MAPFILE);

  mapfile >> json_map;

  json sites = json_map.at("sites");
  json mines = json_map.at("mines");
  json rivers = json_map.at("rivers");

  int phy_id = 0; // order in supplied json list, as opposed to logical id given by the ID field
  for (json::iterator it = sites.begin(); it != sites.end(); ++ it) {
    phy_to_log[phy_id] = it->at("id");
    log_to_phy[it->at("id")] = phy_id;
    phy_id += 1;
  }

  graph_t g(phy_to_log.size());

  for (json::iterator it = rivers.begin(); it != rivers.end(); ++ it) {
    boost::add_edge(it->at("source"), it->at("target"), 1, g);
  }

  std::vector<vertex_descriptor> p(boost::num_vertices(g));
  std::vector<int> d(boost::num_vertices(g));

  // vertex_descriptor s = boost::vertex(0, g);

  for (json::iterator it = mines.begin(); it != mines.end(); ++ it) {
    vertex_descriptor target_mine = boost::vertex(phy_to_log[*it], g);

    std::cout << "Mine: " << *it << " (" << log_to_phy[*it] << ")" << std::endl;

    boost::dijkstra_shortest_paths(
        g
        , target_mine
        , predecessor_map(boost::make_iterator_property_map(p.begin(), boost::get(boost::vertex_index, g)))
        .distance_map(boost::make_iterator_property_map(d.begin(), boost::get(boost::vertex_index, g)))
      );

    for (auto x : boost::combine(p, d)) {
      std::cout << phy_to_log[x.get<0>()] << " (" << x.get<0>() << "): " << x.get<1>() << std::endl;
    }
    // for(auto it = p.begin(); it != p.end(); ++ it) { std::cout << log_to_phy[*it] << ":" << *it << std::endl; }
    // for(auto it = d.begin(); it != d.end(); ++ it) { std::cout << *it << std::endl; }
  }

  return 0;
}
