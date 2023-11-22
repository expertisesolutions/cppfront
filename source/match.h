#ifndef __CPP2_MATCH
#define __CPP2_MATCH

#include "cpp2util.h"
#include "lex.h"
#include "parse.h"

#include <algorithm>
#include <cassert>
#include <cctype>
#include <concepts>
#include <iterator>
#include <map>
#include <optional>
#include <queue>
#include <regex>
#include <set>
#include <sstream>
#include <stack>
#include <string>
#include <string_view>
#include <unordered_map>
#include <variant>
#include <vector>

namespace std {

auto to_string(const cpp2::expression_list_node &eln)
    -> std::string
{
    auto oss = std::ostringstream{};
    auto sep = "";
    for (auto const &expr : eln.expressions) {
        oss << sep << expr.expr->to_string();
        sep = ", ";
    }
    return oss.str();
}

}

namespace cpp2 {

template <typename ...args>
using relation = std::set<std::tuple<args...>>;

size_t find_closing_paren(std::string_view);

// Expands a match statement node to a full-on lambda object
// Maybe struct with `operator()` ?
struct match_generator {
    struct node {
        /// TODO: should label be optional? Maybe '_' for anything
        std::optional<std::string> label = {};
        expression_list_node *attrs = nullptr;
        std::vector<size_t> adj_nodes;

        node() = default;

        node(const auto &l)
            : label{l}, attrs{}
        { }

        node(const auto &l, auto &&a)
            : label {l}, attrs{a}
        { }
    };

    std::vector<node> nodes = {};
    /// TODO: if this is the case, then the member var "label" is unnecessary?
    std::unordered_map<std::string_view, size_t> nodes_map = {};
    std::map<std::tuple<size_t, size_t>, std::optional<size_t>> edges_attrs_map;
    std::vector<error_entry> &errors;

    match_generator() = delete;

    match_generator(
        std::vector<error_entry> &e,
        match_statement_node const* n
    )
        : errors{e}
    {
        parse(n);
    }

private:
    void insert_node(match_node_node const* const mnn) {
        if (!mnn) {
            return;
        }
        const auto label = mnn->get_label()->as_string_view();
        if (
            const auto it = nodes_map.find(label);
            it == nodes_map.end()
        ) {
            nodes_map.emplace(label, nodes.size());
            nodes.emplace_back(label, mnn->attrs.get());
        } else {
            auto &n = nodes[it->second];
            if (mnn->attrs) {
                if (n.attrs) {
                    /// TODO: attributes for node are defined more than once
                    /// report that
                } else {
                    n.attrs = mnn->attrs.get();
                }
            }
        }
    }

    void insert_edge(
        match_node_node const* const mnn1,
        match_node_node const* const mnn2,
        match_edge_attrs_node const* const mean = nullptr
    ) {
        if (!mnn1 || !mnn2) {
            return;
        }
        const auto label1 = mnn1->get_label()->as_string_view();
        const auto label2 = mnn2->get_label()->as_string_view();

        const auto it1 = nodes_map.find(label1);
        const auto it2 = nodes_map.find(label2);
        const auto end = nodes_map.end();

        if (it1 != end && it2 != end) {
            nodes[it1->second].adj_nodes.push_back(it2->second);
            if (!mean || !mean->lhs_attrs) {
                edges_attrs_map.emplace(
                    std::tuple{it1->second, it2->second},
                    std::optional<size_t>{1}
                );
            } else if (mean->is_wildcard()) {
                // unbounded case
                edges_attrs_map.emplace(
                    std::tuple{it1->second, it2->second},
                    std::nullopt
                );
            } else {
                auto attr_str = std::string{mean->lhs_attrs->as_string_view()};
                auto base = 0;
                if (attr_str.starts_with("0b")) {
                    attr_str.erase(0, 2);
                    base = 2;
                }
                auto attr = std::stoull(attr_str, nullptr, base);
                if (attr != 0) {
                    edges_attrs_map.emplace(
                        std::tuple{it1->second, it2->second}, attr
                    );
                } else {
                    edges_attrs_map.emplace(
                        std::tuple{it1->second, it2->second}, std::nullopt
                    );
                }
            }
        } else {
            assert (!"The nodes were not inserted previously");
        }
    }

    void parse_match_expression(match_expression_node const* men) {
        assert (men);
        assert (men->node);
        match_node_node const* prev_node = nullptr;
        match_expression_node const* prev_expr = nullptr;

        do {
            auto *node = men->node.get();
            insert_node(node);
            insert_edge(
                prev_node,
                node,
                prev_expr && prev_expr->arrow ?
                    prev_expr->arrow->attrs.get() : nullptr
            );
            if (
                prev_expr
                && prev_expr->arrow
                && prev_expr->arrow->direction() == match_arrow_node::undirected
            ) {
                // if it is undirected, insert the other way around too
                insert_edge(node, prev_node, prev_expr->arrow->attrs.get());
            }
            prev_node = node;
            prev_expr = men;
            men = men->next();
        } while (men != nullptr);
    }

    void parse(match_statement_node const* const msn) {
        assert (msn);
        assert (msn->match_stmts);
        const auto &exprs = msn->match_stmts->expressions;
        for (const auto &expr : exprs) {
            parse_match_expression(expr.get());
        }
    }

public:
    auto generate()
        -> std::string
    {
        using namespace std::literals::string_view_literals;
        using namespace std::literals::string_literals;
        
        auto oss = std::ostringstream{};
        constexpr auto header_and_captures =
            "[&](auto &&g) requires cpp2::Graph<decltype(g)> {"
            ""sv;
        constexpr auto type_definitions =
            "using graph_attrs = decltype(get_attrs(g, 0));"
            "using graph_adj_list = decltype(get_adj_list(g, 0));"
            "using graph_attrs_pred = decltype(get_default_attrs_pred(g));"
            // "// template <...args> using relation = std::set<std::tuple<args...>>;"
            ""sv;
        constexpr auto match_lambda_definition =
            "auto match = [](graph_attrs_pred const& pred, auto&& attrs){ return pred(attrs); };"
            ""sv;
        constexpr auto distance_matrix =
            "const auto X = ::cpp2::create_distance_matrix(g);"
            ""sv;
        constexpr auto define_anc_desc =
            "auto anc = std::map<std::tuple<size_t, size_t, size_t>, std::set<size_t>>{};"
            "auto desc = decltype(anc){};"
            ""sv;
        constexpr auto define_mat_premv =
            "auto mat = std::map<size_t, std::set<size_t>>{};"
            "auto premv = decltype(mat){};"
            ""sv;
        constexpr auto define_graph_size =
            "const auto graph_size = get_size(g);"
            ""sv;
        const auto define_pattern_size =
            "constexpr auto pattern_size = std::size_t{"s + std::to_string(nodes.size()) + "};"s;
        constexpr auto define_pattern_edges_map =
            "auto pattern_edges_map = std::map<std::tuple<size_t, size_t>, std::optional<int>>{};"
            ""sv;
        
        oss.str("");
        for (const auto [edge, attrs] : edges_attrs_map) {
            const auto [source, sink] = edge;
            oss << "pattern_edges_map.insert({{" << std::to_string(source) << ", "
                << std::to_string(sink) << "}, "
                << (attrs ? std::to_string(*attrs) : "std::nullopt"s) << "});";
        }
        const auto fill_out_edges_map = oss.str();

        constexpr auto define_pattern_nodes =
            "auto pattern_nodes = std::vector<std::tuple<std::vector<size_t>, graph_attrs_pred>>{};"
            ""sv;

        oss.str("");
        for (const auto &n : nodes) {
            oss << "pattern_nodes.push_back({{";
            const char *sep = "";
            for (const auto adj : n.adj_nodes) {
                oss << sep << std::to_string(adj);
                sep = ", ";
            }
            oss << "}, {" << (n.attrs ? std::to_string(*n.attrs) : ""s) << "}});";
        }
        const auto fill_out_pattern_nodes = oss.str();

        constexpr auto fill_out_anc_desc =
            "for (const auto [edge, edge_value] : pattern_edges_map) {"
            "    const auto [ip_, ip] = edge;"
            "    const auto &u_prime = pattern_nodes[ip_];"
            "    const auto &u = pattern_nodes[ip];"
            "    for (size_t i = 0; i < graph_size; ++i) {"
            "        const auto &v_attrs = get_attrs(g, i);"
            "        if (match(std::get<1>(u), v_attrs)) {"
            "            for (size_t i_ = 0; i_ < graph_size; ++i_) {"
            "                const auto &v_prime_attrs = get_attrs(g, i_);"
            "                if ("
            "                    const auto dist = X[i_][i];"
            "                    ("
            "                        !edge_value || ("
            "                            edge_value &&"
            "                            dist &&"
            "                            *dist <= *edge_value"
            "                        )"
            "                    ) && match(std::get<1>(u_prime), v_prime_attrs)"
            "                ) {"
            "                    anc[{i, ip_, ip}].insert(i_);"
            "                }"
            "            }"
            "        }"
            "        if (match(std::get<1>(u_prime), v_attrs)) {"
            "            for (size_t i_ = 0; i_ < graph_size; ++i_) {"
            "                const auto v_prime_attrs = get_attrs(g, i_);"
            "                if ("
            "                    const auto dist = X[i][i_];"
            "                    ("
            "                        !edge_value || ("
            "                            edge_value &&"
            "                            dist &&"
            "                            *dist <= *edge_value"
            "                        )"
            "                    ) && match(std::get<1>(u), v_prime_attrs)"
            "                ) {"
            "                    desc[{i, ip_, ip}].insert(i_);"
            "                }"
            "            }"
            "        }"
            "    }"
            "}"
            ""sv;

        constexpr auto fill_out_mat_premv =
            "for (size_t ip = 0; ip < pattern_size; ++ip) {"
            "    const auto &u = pattern_nodes[ip];"
            "    for (size_t i = 0; i < graph_size; ++i) {"
            "        const auto &v_attrs = get_attrs(g, i);"
            "        if (match(std::get<1>(u), v_attrs)) {"
            "            if ("
            "                std::get<0>(u).size() == 0 ||"
            "                get_adj_list(g, i).size() != 0"
            "            ) {"
            "                mat[ip].insert(i);"
            "            }"
            "        }"
            "    }"
            "    for (size_t i_ = 0; i_ < graph_size; ++i_) {"
            "        const auto &v_prime_attrs = get_attrs(g, i_);"
            "        if (get_adj_list(g, i_).size() != 0) {"
            "            const auto it = std::find_if("
            "                pattern_edges_map.begin(),"
            "                pattern_edges_map.end(),"
            "                [&pattern_nodes, &mat, &X, &match, &v_prime_attrs, ip, i_](auto &&edge_value_pair) {"
            "                    const auto [edge, value] = edge_value_pair;"
            "                    if (std::get<1>(edge) != ip) {"
            "                        return false;"
            "                    }"
            "                    const auto ip_ = std::get<0>(edge);"
            "                    const auto &u_prime = pattern_nodes[ip_];"
            "                    const auto &mat_u_range = mat[ip];"
            "                    const auto it = std::find_if("
            "                        mat_u_range.cbegin(),"
            "                        mat_u_range.cend(),"
            "                        [&X, &u_prime, &match, &v_prime_attrs, i_, value](const auto i) {"
            "                            const auto dist = X[i_][i];"
            "                            return match(std::get<1>(u_prime), v_prime_attrs) &&"
            "                                ("
            "                                    !value ||"
            "                                    (value && dist && *dist <= *value)"
            "                                );"
            "                        }"
            "                    );"
            "                    return it != mat_u_range.cend();"
            "                }"
            "            );"
            "            if (it == pattern_edges_map.end()) {"
            "                premv[ip].insert(i_);"
            "            }"
            "        }"
            "    }"
            "}"
            ""sv;

        constexpr auto main_loop_condition_function =
            "auto loop_cond = [&premv, pattern_size]() {"
            "    const auto it = std::find_if("
            "        premv.begin(),"
            "        premv.end(),"
            "        [](auto &&u_premv_u_pair) {"
            "            return !u_premv_u_pair.second.empty();"
            "        }"
            "    );"
            "    return it != premv.end() ?"
            "        std::optional<size_t>{it->first} :"
            "        std::optional<size_t>{};"
            "};"
            ""sv;

        constexpr auto main_loop =
            "auto ip_opt = decltype(loop_cond()){};"
            "while (ip_opt = loop_cond()) {"
            "    const auto ip = *ip_opt;"
            "    for (const auto &[edge, value] : pattern_edges_map) {"
            "        if (std::get<1>(edge) != ip)"
            "            continue;"
            "        const auto ip_ = std::get<0>(edge);"
            "        const auto &premv_u_range = premv[ip];"
            "        for (const auto i1 : premv_u_range) {"
            "            if (mat[ip_].contains(i1)) {"
            "                mat[ip_].erase(i1);"
            "                if (mat[ip_].empty())"
            "                    return std::set<std::tuple<size_t, size_t>>{};"
            "                for (const auto &[edge, value] : pattern_edges_map) {"
            "                    if (std::get<1>(edge) != ip_)"
            "                        continue;"
            "                    const auto ip__ = std::get<0>(edge);"
            "                    const auto &anc_v1_u_prime_prime_u_prime_range = anc[{i1, ip__, ip_}];"
            "                    const auto &prevm_u_prime_range = premv[ip_];"
            "                    auto diff = std::vector<size_t>{};"
            "                    std::set_difference("
            "                        anc_v1_u_prime_prime_u_prime_range.cbegin(),"
            "                        anc_v1_u_prime_prime_u_prime_range.cend(),"
            "                        prevm_u_prime_range.cbegin(),"
            "                        prevm_u_prime_range.cend(),"
            "                        std::inserter(diff, diff.begin())"
            "                    );"
            "                    for (const auto i1_ : diff) {"
            "                        const auto &desc_v1_prime_u_prime_prime_u_prime_range"
            "                            = desc[{i1_, ip__, ip_}];"
            "                        const auto &mat_u_prime_range = mat[ip_];"
            "                        auto intersec = std::vector<size_t>{};"
            "                        std::set_intersection("
            "                            desc_v1_prime_u_prime_prime_u_prime_range.cbegin(),"
            "                            desc_v1_prime_u_prime_prime_u_prime_range.cend(),"
            "                            mat_u_prime_range.cbegin(),"
            "                            mat_u_prime_range.cend(),"
            "                            std::inserter(intersec, intersec.begin())"
            "                        );"
            "                        if (intersec.empty()) {"
            "                            premv[ip_].insert(i1_);"
            "                        }"
            "                    }"
            "                }"
            "            }"
            "        }"
            "    }"
            "    premv[ip].clear();"
            "}"
            ""sv;

        constexpr auto relation_and_return =
            "auto S = std::set<std::tuple<size_t, size_t>>{};"
            "for (size_t ip = 0; ip < pattern_size; ++ip) {"
            "    const auto &mat_u_range = mat[ip];"
            "    for (const auto i : mat_u_range) {"
            "        S.emplace(ip, i);"
            "    }"
            "}"
            "return S;"
            ""sv;

        constexpr auto end_of_lambda =
            "}"
            ""sv;

        oss.str("");
        oss << header_and_captures
            << type_definitions << match_lambda_definition
            << distance_matrix
            << define_anc_desc << define_mat_premv
            << define_graph_size << define_pattern_size
            << define_pattern_edges_map << fill_out_edges_map
            << define_pattern_nodes << fill_out_pattern_nodes
            << fill_out_anc_desc
            << fill_out_mat_premv
            << main_loop_condition_function << main_loop
            << relation_and_return
            << end_of_lambda;

        return oss.str();
    }
};

}


namespace cpp2::bounded_simulation {

struct trivial_predicate {
    template<typename ...args>
    trivial_predicate(args...) {

    }

    bool operator()(auto &&) const {
        return true;
    }
};

// void test()
// {
//     constexpr auto value = std::convertible_to<
//         decltype(std::declval<trivial_predicate>()(
//             std::declval<int>()
//         )),
//         bool
//     >;
// }

template<
    typename attrs_type_ = std::tuple<std::any>,
    typename predicate_type_ = trivial_predicate
>
    requires (
        requires (attrs_type_ at) {
            std::get<0>(at);
        }
        && requires (predicate_type_ pred, attrs_type_ at) {
            pred(at);
        }
        && std::convertible_to<
            decltype(std::declval<predicate_type_>()(
                std::declval<attrs_type_>()
            )),
            bool
        >
    )
struct pattern {
    // using v_type = int;
    using v_type = size_t;
    using attrs_type = attrs_type_;
    using predicate_type = predicate_type_;
    using e_type = std::pair<v_type, v_type>;
    using edge_value_type = std::optional<int>;
    using adjacency_list_type = std::vector<v_type>;
    using node_type = std::tuple<
        // value of the node
        v_type,
        // adjacency list with the specified (optional) value (f_E)
        adjacency_list_type,
        // preficate object for the current node (f_V)
        predicate_type
    >;

    pattern() = delete;

    pattern(size_t N, const predicate_type &pred) {
        nodes.reserve(N);

        for (size_t i = 0; i < N; ++i) {
            nodes.emplace_back(i, std::vector<v_type>{}, pred);
        }
    }

    pattern(size_t N, const std::vector<predicate_type> &preds) {
        assert (preds.size() >= N);
        nodes.reserve(N);

        for (size_t i = 0; i < N; ++i) {
            nodes.emplace_back(i, std::vector<v_type>{}, preds[i]);
        }
    }

    auto get_node(v_type vertex) const
        -> const node_type&
    {
        assert (vertex < nodes.size());

        return nodes.at(vertex);
    }

    auto get_node(v_type vertex)
        -> node_type&
    {
        assert (vertex < nodes.size());

        return nodes[vertex];
    }

    auto get_size() const
        -> size_t
    {
        return nodes.size();
    }

    auto get_edges() const
        -> const auto&
    {
        return edges_map;
    }

    auto get_edges()
        -> auto&
    {
        return edges_map;
    }

    auto add_vertex(v_type vertex, const predicate_type &pred)
        -> bool
    {
        nodes.emplace_back(nodes.size(), std::vector<v_type>{}, pred);

        return true;
    }

    auto add_edge(const e_type &edge, const edge_value_type &value = {})
        -> bool
    {
        const auto [source, sink] = edge;
        if (source >= nodes.size() || sink >= nodes.size()) {
            return false;
        }

        // std::get<1>(nodes[source]).push_back(sink);
        auto &adj_list = std::get<1>(nodes[source]);
        const auto it = std::lower_bound(adj_list.begin(), adj_list.end(), sink);
        if (it == adj_list.end() || *it < sink) {
            adj_list.insert(it, sink);
        }
        edges_map.insert({edge, value});

        return true;
    }
private:
    std::vector<node_type> nodes;
    std::map<e_type, edge_value_type> edges_map;
};

template<typename attrs_type_ = typename pattern<>::attrs_type>
    requires (
        requires (attrs_type_ at) {
            std::get<0>(at);
        }
    )
struct graph {
    using attrs_type = attrs_type_;
    using v_type = size_t;
    using e_type = std::pair<v_type, v_type>;
    using adjacency_list_type = std::vector<v_type>;
    using node_type = std::tuple<
        adjacency_list_type,
        attrs_type
    >;
    using path_type = std::vector<v_type>;

    graph() = delete;

    graph(size_t N)
        requires (std::is_trivially_constructible_v<attrs_type>)
        : nodes(N)
    { }

    graph(size_t N, const std::vector<attrs_type> &attrs) {
        assert (attrs.size() >= N);
        nodes.reserve(N);

        for (size_t i = 0; i < N; ++i) {
            nodes.emplace_back(std::vector<v_type>{}, attrs[i]);
        }
    }

    auto get_node(v_type vertex) const
        -> node_type const&
    {
        assert (vertex < nodes.size());

        return nodes.at(vertex);
    }

    auto get_node(v_type vertex)
        -> node_type&
    {
        assert (vertex < nodes.size());

        return nodes[vertex];
    }

    auto get_adj_list(v_type vertex) const
        -> adjacency_list_type const&
    {
        assert (vertex < nodes.size());

        return std::get<0>(nodes[vertex]);
    }

    auto get_adj_list(v_type vertex)
        -> adjacency_list_type&
    {
        assert (vertex < nodes.size());

        return std::get<0>(nodes[vertex]);
    }

    auto get_attrs(v_type vertex) const
        -> attrs_type const&
    {
        assert (vertex < nodes.size());

        return std::get<1>(nodes[vertex]);
    }

    auto get_attrs(v_type vertex)
        -> attrs_type &
    {
        assert (vertex < nodes.size());

        return std::get<1>(nodes[vertex]);
    }

    auto get_size() const
        -> size_t
    {
        return nodes.size();
    }

    auto add_vertex(const attrs_type &attr)
        -> bool
    {
        nodes.emplace_back(std::vector<v_type>{}, attr);

        return true;
    }

    auto add_edge(const e_type &edge)
        -> bool
    {
        const auto [source, sink] = edge;
        if (source >= nodes.size() || sink >= nodes.size()) {
            return false;
        }

        // std::get<0>(nodes[source]).push_back(sink);
        auto &adj_list = std::get<0>(nodes[source]);
        const auto it = std::lower_bound(adj_list.begin(), adj_list.end(), sink);
        if (it == adj_list.end() || *it < sink) {
            adj_list.insert(it, sink);
        }

        return true;
    }

    auto is_valid_path(const path_type &path) const {
        assert (path.size() >= 2);
        const auto n_size = nodes.size();

        for (const auto n : path) {
            if (n >= n_size)
                return false;
        }

        auto it = path.begin();
        auto it_next = std::next(it);
        for (; it_next != path.end(); ++it, ++it_next) {
            const auto &adj_list = get_adj_list(*it);
            const auto edge_sink_it = std::find_if(
                adj_list.begin(),
                adj_list.end(),
                [edge_sink = *it_next](const auto v) {
                    return v == edge_sink;
                }
            );

            if (edge_sink_it == adj_list.end())
                return false;
        }

        return true;
    }

private:
    std::vector<node_type> nodes;
};

template <typename at, typename pt>
auto get_node_value(
    typename pattern<at, pt>::node_type const &node
)
    -> typename pattern<at, pt>::v_type
{
    return std::get<0>(node);
}

template <typename at, typename pt>
auto get_node_predicate(
    typename pattern<at, pt>::node_type const &node
)
    -> pt
{
    return std::get<2>(node);
}

template <typename at, typename pt>
auto get_edge_value(
    pattern<at, pt> const &pat,
    typename pattern<at, pt>::e_type edge
)
    -> typename pattern<at, pt>::edge_value_type
{
    if (
        const auto edge_it = pat.edges_map.find(edge);
        edge_it != pat.edges_map.end()
    )
        return edge_it->second;
    return {};
}

template <typename at, typename pt>
auto get_adj_list(
    typename pattern<at, pt>::node_type const &node
)
    -> typename pattern<at, pt>::adjacency_list_type const&
{
    return std::get<1>(node);
}

template <typename at>
auto get_adj_list(
    typename graph<at>::node_type const &node
)
    -> typename graph<at>::adjacency_list_type const&
{
    return std::get<0>(node);
}

template <typename at, typename pt>
auto get_out_degree(
    typename pattern<at, pt>::node_type const &node 
)
    -> size_t
{
    return std::size(get_adj_list<at, pt>(node));
}

template <typename at>
auto get_out_degree(
    typename graph<at>::node_type const &node 
)
    -> size_t
{
    return std::size(get_adj_list<at>(node));
}

template <typename at>
auto get_attrs(
    typename graph<at>::node_type const &node
)
    -> at const&
{
    return std::get<1>(node);
}

template <typename at, typename pt>
auto match(
    typename pattern<at, pt>::node_type const &pat_node,
    auto &&node
)
    -> bool
{
    return get_node_predicate<at, pt>(pat_node)(
        get_attrs<at>(node)
    );
}

enum class walk_type : std::uint8_t { bfs, dfs };
template <walk_type wt, typename at>
auto search(const graph<at> &g, typename graph<at>::v_type source)
    -> std::vector<std::optional<typename graph<at>::v_type>>
{
    using v_type = typename graph<at>::v_type;
    auto next_nodes = std::conditional_t<
        wt == walk_type::bfs,
        std::queue<v_type>,
        std::conditional_t<
            wt == walk_type::dfs,
            std::stack<v_type>,
            // should fail if wt is something else entirely
            void
        >
    >{};
    const auto graph_size = g.get_size();
    auto parent = std::vector<std::optional<v_type>>(graph_size);
    auto visited = std::vector<bool>(graph_size);

    auto push = [&next_nodes](auto &&elem) {
        next_nodes.push(elem);
    };

    auto pop = [&next_nodes]() {
        auto pop = v_type{};
        if constexpr (wt == walk_type::bfs) {
            pop = next_nodes.front();
        } else if constexpr (wt == walk_type::dfs) {
            pop = next_nodes.top();
        } else {
            static_assert (
                (wt == walk_type::bfs) ||
                (wt == walk_type::dfs) ||
                !"It is something else entirely"
            );
        }
        next_nodes.pop();

        return pop;
    };

    push(source);
    parent[source] = source;
    visited[source] = true;

    while (!next_nodes.empty()) {
        const auto v = pop();
        // const auto &curr_node = g.get_node(v);
        const auto &adj_list = g.get_adj_list(v); // std::get<0>(curr_node);

        for (const auto adj : adj_list) {
            if (!visited[adj]) {
                parent[adj] = v;
                visited[adj] = true;
                push(adj);
            }
        }
    }

    return parent;
}

template <typename v_type>
    requires (std::is_integral_v<v_type>)
auto calc_distance(
    const std::vector<std::optional<v_type>> &parent
)
    -> std::vector<std::optional<size_t>>
{
    // using v_type = typename graph<at>::v_type;
    auto distance = std::vector<std::optional<size_t>>(parent.size());

    for (v_type i = 0; i < parent.size(); ++i) {
        if (!parent[i]) {
            continue;
        } else if (distance[i]) {
            continue;
        } else if (i == *parent[i]) {
            distance[i] = 0;
            continue;
        }

        auto i_parent = *parent[i];
        auto dist = distance[i_parent];
        auto lineage = std::stack<v_type>{};
        while (!dist && i_parent != *parent[i_parent]) {
            lineage.push(i_parent);
            i_parent = *parent[i_parent];
            dist = distance[i_parent];
        }
        if (i_parent == *parent[i_parent]) {
            dist = distance[i_parent] = 0;
        }
        auto curr_dist = *dist;
        while (!lineage.empty()) {
            distance[lineage.top()] = ++curr_dist;
            lineage.pop();
        }
        
        distance[i] = *distance[*parent[i]] + 1;
    }

    return distance;
}

template <typename at>
auto create_distance_matrix(const graph<at> &g)
    -> std::vector<std::vector<std::optional<size_t>>>
{
    using v_type = typename graph<at>::v_type;
    const auto graph_size = g.get_size();

    auto dist_matrix = std::vector<std::vector<std::optional<size_t>>>{};

    for (v_type i = 0; i < graph_size; ++i) {
        auto parent = search<walk_type::bfs>(g, i);

        dist_matrix.push_back(calc_distance(parent));
    }

    return dist_matrix;
}

template <typename at, typename pt>
auto is_bounded_simulation_match(
    const relation<
        typename pattern<at, pt>::v_type,
        typename graph<at>::v_type
    > &rel,
    const pattern<at, pt> &pat,
    const graph<at> &grp
)
    -> bool
{
    using pattern_v_type = typename pattern<at, pt>::v_type;
    using graph_v_type = typename graph<at>::v_type;
    /// TODO: check whether every vertex of the pattern is in the relation
    auto pattern_graph_vertex_mmap = std::unordered_multimap<pattern_v_type, graph_v_type>{};

    for (const auto [pat_v, graph_v] : rel) {
        const auto &pat_node = pat.get_node(pat_v);
        const auto &graph_node = grp.get_node(graph_v);

        if (!match<at, pt>(pat_node, graph_node)) {
            return false;
        }
        pattern_graph_vertex_mmap.insert({pat_v, graph_v});
    }

    const auto &pat_edges = pat.get_edges();
    for (const auto [pat_edge, pat_edge_value] : pat_edges) {
        /// For each edge (u, u') in the pattern, there must be a path
        /// v, ..., v' such that (u', v') is in the realtion and the path
        /// length is bounded by the pattern edge value, if present, and
        /// unbounded otherwise. In other words, each edge in the pattern
        /// is mapped to a path (bounded or not) in the graph.

        // u, u'
        const auto [u, u_prime] = pat_edge;
        // v, v'
        const auto v_range = pattern_graph_vertex_mmap.equal_range(u),
                   v_prime_range = pattern_graph_vertex_mmap.equal_range(u_prime);

        for (auto it = v_range.first; it != v_range.second; ++it) {
            const auto v = it->second;
            for (auto it2 = v_prime_range.first; it2 != v_prime_range.second; ++it2) {
                const auto v_prime = it2->second;

                // check whether there is a path from v to v' and its distance
                const auto parent = search<walk_type::bfs>(grp, v);
                if (parent[v_prime]) {
                    if (pat_edge_value) {

                    }
                } else {

                }
            }
        }
    }

    return true;
}

template<typename t1, typename t2>
std::ostream& operator<<(std::ostream &os, const std::tuple<t1, t2> &t)
{
    return os << "(" << std::get<0>(t) << "," << std::get<1>(t) << ")";
}

template<typename t1, typename t2, typename t3>
std::ostream& operator<<(std::ostream &os, const std::tuple<t1, t2, t3> &t)
{
    return os << "(" << std::get<0>(t) << "," << std::get<1>(t) << "," << std::get<2>(t) << ")";
}

template<typename value>
std::ostream& operator<<(std::ostream &os, const std::set<value> &s)
{
    os << "{";
    auto sep = "";
    for (const auto &v : s) {
        os << sep << v;
        sep = ",";
    }
    return os << "}";
}

template <typename key, typename value>
std::ostream& operator<<(std::ostream &os, const std::map<key, value> &m)
{
    os << "{\n";
    for (const auto &[k, v] : m) {
        os << "\t(" << k << ", " << v << ")\n";
    }
    return os << "}\n";
}

}

template <typename at>
auto get_size(cpp2::bounded_simulation::graph<at> const& g)
    -> size_t
{
    return g.get_size();
}

template <typename at>
auto get_adj_list(cpp2::bounded_simulation::graph<at> const& g, size_t i)
    -> typename cpp2::bounded_simulation::graph<at>::adjacency_list_type const&
{
    assert (i < g.get_size());
    return g.get_adj_list(i);
}

template <typename at>
auto get_attrs(cpp2::bounded_simulation::graph<at> const& g, size_t i)
    -> const at&
{
    assert (i < g.get_size());
    return g.get_attrs(i);
}

template <typename at>
auto get_default_attrs_pred(cpp2::bounded_simulation::graph<at> const& g)
    -> cpp2::bounded_simulation::trivial_predicate
{
    return {};
}

namespace cpp2::bounded_simulation {

template <typename at, typename pt>
auto bounded_simulation_match(
    const pattern<at, pt> &pat,
    const graph<at> &grp
)
    -> relation<
        typename pattern<at, pt>::v_type,
        typename graph<at>::v_type
    >
{
    using pattern_v_type = typename pattern<at, pt>::v_type;
    using graph_v_type = typename graph<at>::v_type;

    const auto X = create_distance_matrix(grp);

    auto anc = std::map<
        std::tuple<graph_v_type, pattern_v_type, pattern_v_type>,
        std::set<graph_v_type>
    >{};
    auto desc = decltype(anc){};

    auto mat = std::map<pattern_v_type, std::set<graph_v_type>>{};
    auto premv = decltype(mat){};

    const auto graph_size = grp.get_size();
    const auto &pat_edges = pat.get_edges();
    /**
     * Indexing conventions:
     * i for v, i1 for v1, i_ for v_prime, i1_ for v1_prime and so on
     * ip for u, ip1 for u1. ip_ for u_prime, ip1_prime for u1_prime and so on 
     */
    // For each edge (u', u) in Ep
    for (const auto &[edge, edge_value] : pat_edges) {
        const auto [ip_, ip] = edge;
        const auto &u_prime = pat.get_node(ip_);
        const auto &u = pat.get_node(ip);

        // and each v in V
        for (size_t i = 0; i < graph_size; ++i) {
            const auto &v = grp.get_node(i);
            // if fA(v) satisfies fV(u) then compute anc(fE(u', u), fV(u'), v)
            if (match<at, pt>(u, v)) {
                for (size_t i_ = 0; i_ < graph_size; ++i_) {
                    const auto &v_prime = grp.get_node(i_);
                    if (
                        const auto dist = X[i_][i];
                        (
                            !edge_value || (
                                edge_value &&
                                dist &&
                                *dist <= *edge_value
                            )
                        ) && match<at, pt>(u_prime, v_prime)
                    ) {
                        anc[{i, ip_, ip}].insert(i_);
                    }
                }
            }
            // if fA(v) satisfies fV(u') then compute desc(fE(u', u), fV(u), v)
            if (match<at, pt>(u_prime, v)) {
                for (size_t i_ = 0; i_ < graph_size; ++i_) {
                    const auto &v_prime = grp.get_node(i_);
                    if (
                        const auto dist = X[i][i_];
                        (
                            !edge_value || (
                                edge_value &&
                                dist &&
                                *dist <= *edge_value
                            )
                        ) && match<at, pt>(u, v_prime)
                    ) {
                        desc[{i, ip_, ip}].insert(i_);
                    }
                }
            }
        }
    }

    const auto pattern_size = pat.get_size();
    // for each u in Vp
    for (size_t ip = 0; ip < pattern_size; ++ip) {
        const auto &u = pat.get_node(ip);
        // mat(u) = {v; v in V, fA(v) satisfies fV(u)
        //           and out-degree(v) != 0 if out-degree(u) != 0}
        for (size_t i = 0; i < graph_size; ++i) {
            const auto &v = grp.get_node(i);
            if (match<at, pt>(u, v)) {
                // const auto out_degree_v = get_out_degree<at>(v);
                // const auto out_degree_u = get_out_degree<at, pt>(u);    
                // (out-degree(u) != 0) --> (out-degree(v) != 0) is equivalent to
                // (out-degree(u) = 0) or (out-degree(v) != 0)
                if (
                    get_out_degree<at, pt>(u) == 0 ||
                    get_out_degree<at>(v) != 0
                ) {
                    mat[ip].insert(i);
                }
            }
        }
        // calc premv is tricky
        // premv(u) = {v'; v' in V, out-degree(v') != 0 and there is no
        //             (u', u) in Ep such that (v in mat(u), fA(v') satisfies
        //             fV(u') and len(v', ..., v) <= f(u',u))}
        for (size_t i_ = 0; i_ < graph_size; ++i_) {
            const auto &v_prime = grp.get_node(i_);
            // const auto out_degree_v_prime = get_out_degree<at>(v_prime);
            if (get_out_degree<at>(v_prime) != 0) {
                const auto it = std::find_if(
                    pat_edges.begin(),
                    pat_edges.end(),
                    [&pat, &mat, &X, &v_prime, ip, i_](auto &&edge_value_pair) {
                        const auto [edge, value] = edge_value_pair;
                        if (std::get<1>(edge) != ip) {
                            // edge sink is not u
                            return false;
                        }
                        const auto ip_ = std::get<0>(edge);
                        const auto &u_prime = pat.get_node(ip_);

                        const auto &mat_u_range = mat[ip];
                        const auto it = std::find_if(
                            mat_u_range.cbegin(),
                            mat_u_range.cend(),
                            [&X, &u_prime, &v_prime, i_, value](const auto i) {
                                // const auto &v = grp.get_node(i);
                                const auto dist = X[i_][i];
                                return match<at, pt>(u_prime, v_prime) &&
                                    (
                                        !value ||
                                        (value && dist && *dist <= *value)
                                    );
                            }
                        );
                        return it != mat_u_range.cend();
                    }
                );

                if (it == pat_edges.end()) {
                    premv[ip].insert(i_);
                }
            }
        }
    }

    // there exists a node u in Vp such that premv(u) is not empty
    auto loop_cond = [&premv, pattern_size]() {
        const auto it = std::find_if(
            premv.begin(),
            premv.end(),
            [](auto &&u_premv_u_pair) {
                return !u_premv_u_pair.second.empty();
            }
        );
        return it != premv.end() ? 
            std::optional<size_t>{it->first} :
            std::optional<size_t>{};
        // for (size_t ip = 0; ip < pattern_size; ++ip) {
        //     if (!premv[ip].empty())
        //         return std::optional<size_t>{ip};
        // }
        // return std::optional<size_t>{};
    };

    auto ip_opt = decltype(loop_cond()){};
    // while the is u in Vp such that premv(u) is not empty
    while (ip_opt = loop_cond()) {
        const auto ip = *ip_opt;
        // for each (u', u) in Vp
        for (const auto &[edge, value] : pat_edges) {
            // const auto [edge, value] = edge_value_pair;
            if (std::get<1>(edge) != ip) {
                continue;
            }
            const auto ip_ = std::get<0>(edge);
            // const auto &u_prime = pat.get_node(ip_);
            // const auto &u = pat.get_node(ip);
            
            const auto &premv_u_range = premv[ip];
            // and each v1 in premv(u)
            for (const auto i1 : premv_u_range) {
                // const auto &v1 = grp.get_node(i1);
                if (mat[ip_].contains(i1)) {
                    mat[ip_].erase(i1);

                    if (mat[ip_].empty())
                        return {};
                    // for each u'' with (u'', u') in Ep
                    for (const auto &[edge, value] : pat_edges) {
                        if (std::get<1>(edge) != ip_)
                            continue;
                        const auto ip__ = std::get<0>(edge);
                        // const auto &u_prime_prime = pat.get_node(ip__);
                        const auto &anc_v1_u_prime_prime_u_prime_range = anc[{i1, ip__, ip_}];
                        const auto &prevm_u_prime_range = premv[ip_];
                        auto diff = std::vector<graph_v_type>{};
                        // std::vector<
                        //     decltype(prevm_u_prime_range)::key_type
                        // >{};
                        std::set_difference(
                            anc_v1_u_prime_prime_u_prime_range.cbegin(),
                            anc_v1_u_prime_prime_u_prime_range.cend(),
                            prevm_u_prime_range.cbegin(),
                            prevm_u_prime_range.cend(),
                            std::inserter(diff, diff.begin())
                        );

                        // for each v1' in (anc(fE(u'', u'), fV(u''), v1) - premv(u'))
                        for (const auto i1_ : diff) {
                            const auto &desc_v1_prime_u_prime_prime_u_prime_range
                                = desc[{i1_, ip__, ip_}];
                            const auto &mat_u_prime_range = mat[ip_];
                            auto intersec = std::vector<graph_v_type>{};
                            std::set_intersection(
                                desc_v1_prime_u_prime_prime_u_prime_range.cbegin(),
                                desc_v1_prime_u_prime_prime_u_prime_range.cend(),
                                mat_u_prime_range.cbegin(),
                                mat_u_prime_range.cend(),
                                std::inserter(intersec, intersec.begin())
                            );

                            if (intersec.empty()) {
                                premv[ip_].insert(i1_);
                            }
                        }                                    
                    }
                }
            }            
        }

        // premv(u) := empty set
        premv[ip].clear();
    }
    
    auto S = relation<pattern_v_type, graph_v_type>{};
    for (size_t ip = 0; ip < pattern_size; ++ip) {
        const auto &mat_u_range = mat[ip];
        for (const auto i : mat_u_range) {
            S.emplace(ip, i);
        }
    }

    return S;
}

}

// static_assert(cpp2::Graph<cpp2::bounded_simulation::graph<>>);

#endif
