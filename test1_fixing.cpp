

//=== Cpp2 type declarations
//====================================================

#include "include/cpp2util.h"
#include "source/match.h"

//=== Cpp2 type definitions and function declarations
//===========================

[[nodiscard]] auto main() -> int;

//=== Cpp2 function definitions
//=================================================

[[nodiscard]] auto main() -> int {
  auto g = cpp2::bounded_simulation::graph{
      8, {{1}, {1}, {1}, {1}, {1}, {1}, {1}, {1}}};
  using graph_attrs_pred = decltype(get_default_attrs_pred(g));
  auto pattern_nodes =
      std::vector<std::tuple<std::vector<size_t>, graph_attrs_pred>>{};
  pattern_nodes.push_back({{1, 2, 3}, {4, 5, 6}});
  auto &u = pattern_nodes[0];
  auto match = [](graph_attrs_pred const &pred, auto &&attrs) {
    return pred(attrs);
  };
  auto b = match(std::get<1>(u), get_attrs(g, 0));
  auto m{
#line 2 "test1.cpp2"
      [&](auto &&g) {
        using graph_attrs = decltype(get_attrs(g, 0));
        using graph_adj_list = decltype(get_adj_list(g, 0));
        using graph_attrs_pred = decltype(get_default_attrs_pred(g));
        // template <...args> using relation = std::set<std::tuple<args...>>;
        auto match = [](graph_attrs_pred const& pred, auto&& attrs) {
          return pred(attrs);
        };
        const auto X = create_distance_matrix(g);
        auto anc =
            std::map<std::tuple<size_t, size_t, size_t>, std::set<size_t>>{};
        auto desc = decltype(anc){};
        auto mat = std::map<size_t, std::set<size_t>>{};
        auto premv = decltype(mat){};
        const auto graph_size = get_size(g);
        constexpr auto pattern_size = std::size_t{5};
        auto pattern_edges_map =
            std::map<std::tuple<size_t, size_t>, std::optional<int>>{};
        pattern_edges_map.insert({{0, 1}, 1});
        pattern_edges_map.insert({{1, 2}, 1});
        pattern_edges_map.insert({{2, 3}, 1});
        pattern_edges_map.insert({{3, 0}, 1});
        pattern_edges_map.insert({{3, 4}, 1});
        pattern_edges_map.insert({{4, 1}, 1});
        auto pattern_nodes =
            std::vector<std::tuple<std::vector<size_t>, graph_attrs_pred>>{};
        pattern_nodes.push_back({{1}, {0, 'a', 0x123}});
        pattern_nodes.push_back({{2}, {123}});
        pattern_nodes.push_back({{3}, {}});
        pattern_nodes.push_back({{4, 0}, {1, true, "damn"}});
        pattern_nodes.push_back({{1}, {}});
        for (const auto [edge, edge_value] : pattern_edges_map) {
          const auto [ip_, ip] = edge;
          const auto &u_prime = pattern_nodes[ip_];
          const auto &u = pattern_nodes[ip];
          for (size_t i = 0; i < graph_size; ++i) {
            const auto &v_attrs = get_attrs(g, i);
            if (match(std::get<1>(u), v_attrs)) {
              for (size_t i_ = 0; i_ < graph_size; ++i_) {
                const auto &v_prime_attrs = get_attrs(g, i_);
                if (const auto dist = X[i_][i];
                    (!edge_value ||
                     (edge_value && dist && *dist <= *edge_value)) &&
                    match(std::get<1>(u_prime), v_prime_attrs)) {
                  anc[{i, ip_, ip}].insert(i_);
                }
              }
            }
            if (match(std::get<1>(u_prime), v_attrs)) {
              for (size_t i_ = 0; i_ < graph_size; ++i_) {
                const auto v_prime_attrs = get_attrs(g, i_);
                if (const auto dist = X[i][i_];
                    (!edge_value ||
                     (edge_value && dist && *dist <= *edge_value)) &&
                    match(std::get<1>(u), v_prime_attrs)) {
                  desc[{i, ip_, ip}].insert(i_);
                }
              }
            }
          }
        }
        for (size_t ip = 0; ip < pattern_size; ++ip) {
          const auto &u = pattern_nodes[ip];
          for (size_t i = 0; i < graph_size; ++i) {
            const auto &v_attrs = get_attrs(g, i);
            if (match(std::get<1>(u), v_attrs)) {
              if (std::get<0>(u).size() == 0 ||
                  std::size(get_adj_list(g, i)) != 0) {
                mat[ip].insert(i);
              }
            }
          }
          for (size_t i_ = 0; i_ < graph_size; ++i_) {
            const auto &v_prime_attrs = get_attrs(g, i_);
            if (std::size(get_adj_list(g, i_)) != 0) {
              const auto it = std::find_if(
                  pattern_edges_map.begin(), pattern_edges_map.end(),
                  [&pattern_nodes, &mat, &X, &match, &v_prime_attrs, ip,
                   i_](auto &&edge_value_pair) {
                    const auto [edge, value] = edge_value_pair;
                    if (std::get<1>(edge) != ip) {
                      return false;
                    }
                    const auto ip_ = std::get<0>(edge);
                    const auto &u_prime = pattern_nodes[ip_];
                    const auto &mat_u_range = mat[ip];
                    const auto it = std::find_if(
                        mat_u_range.cbegin(), mat_u_range.cend(),
                        [&X, &u_prime, &match, &v_prime_attrs, i_,
                         value](const auto i) {
                          const auto dist = X[i_][i];
                          return match(std::get<1>(u_prime), v_prime_attrs) &&
                                 (!value || (value && dist && *dist <= *value));
                        });
                    return it != mat_u_range.cend();
                  });
              if (it == pattern_edges_map.end()) {
                premv[ip].insert(i_);
              }
            }
          }
        }
        auto loop_cond = [&premv, pattern_size]() {
          const auto it = std::find_if(premv.begin(), premv.end(),
                                       [](auto &&u_premv_u_pair) {
                                         return !u_premv_u_pair.second.empty();
                                       });
          return it != premv.end() ? std::optional<size_t>{it->first}
                                   : std::optional<size_t>{};
        };
        auto ip_opt = decltype(loop_cond()){};
        while (ip_opt = loop_cond()) {
          const auto ip = *ip_opt;
          for (const auto &[edge, value] : pattern_edges_map) {
            if (std::get<1>(edge) != ip)
              continue;
            const auto ip_ = std::get<0>(edge);
            const auto &premv_u_range = premv[ip];
            for (const auto i1 : premv_u_range) {
              if (mat[ip_].contains(i1)) {
                mat[ip_].erase(i1);
                if (mat[ip_].empty())
                  return std::set<std::tuple<size_t, size_t>>{};
                for (const auto &[edge, value] : pattern_edges_map) {
                  if (std::get<1>(edge) != ip_)
                    continue;
                  const auto ip__ = std::get<0>(edge);
                  const auto &anc_v1_u_prime_prime_u_prime_range =
                      anc[{i1, ip__, ip_}];
                  const auto &prevm_u_prime_range = premv[ip_];
                  auto diff = std::vector<size_t>{};
                  std::set_difference(
                      anc_v1_u_prime_prime_u_prime_range.cbegin(),
                      anc_v1_u_prime_prime_u_prime_range.cend(),
                      prevm_u_prime_range.cbegin(), prevm_u_prime_range.cend(),
                      std::inserter(diff, diff.begin()));
                  for (const auto i1_ : diff) {
                    const auto &desc_v1_prime_u_prime_prime_u_prime_range =
                        desc[{i1_, ip__, ip_}];
                    const auto &mat_u_prime_range = mat[ip_];
                    auto intersec = std::vector<size_t>{};
                    std::set_intersection(
                        desc_v1_prime_u_prime_prime_u_prime_range.cbegin(),
                        desc_v1_prime_u_prime_prime_u_prime_range.cend(),
                        mat_u_prime_range.cbegin(), mat_u_prime_range.cend(),
                        std::inserter(intersec, intersec.begin()));
                    if (intersec.empty()) {
                      premv[ip_].insert(i1_);
                    }
                  }
                }
              }
            }
          }
          premv[ip].clear();
        }
        auto S = std::set<std::tuple<size_t, size_t>>{};
        for (size_t ip = 0; ip < pattern_size; ++ip) {
          const auto &mat_u_range = mat[ip];
          for (const auto i : mat_u_range) {
            S.emplace(ip, i);
          }
        }
        return S;
      }};

  auto rel = m(g);
#line 9 "test1.cpp2"
}
