#ifndef __CPP2_MATCH
#define __CPP2_MATCH

#include "cpp2util.h"
#include "lex.h"

#include <assert.h>
#include <ctype.h>
#include <iterator>
#include <map>
#include <optional>
#include <regex>
#include <set>
#include <sstream>
#include <string>
#include <string_view>
#include <unordered_map>
#include <variant>
#include <vector>

/* Generation rules:
MATCH -> MATCH '->' NODE
NODE -> '(' NODE_DESCRIPTION ')'
NODE_DESCRIPTION -> NODE_LABEL ':' NODE_ATTRIBUTE | ''
NODE_LABEL -> rex [\w_][\w\d_]*
NODE_ATTRIBUTE -> '{' ATTRIBUTE_LIST '}' | '_'
ATTRIBUTE_LIST -> ATTRIBUTE_LIST ',' VALUE | VALUE
VALUE -> LITERAL | CAPTURE
LITERAL -> rex [0-9]*\.?[0-9]+ | rex "[^"]*" | ...
CAPTURE -> '$' (todo...)
*/

namespace cpp2 {

template <typename ...args>
using relation = std::set<std::tuple<args...>>;

size_t find_closing_paren(std::string_view);

// Expands a match string literal to a full-on lambda object
// Maybe struct with `operator()` ?
struct match_generator {
    constexpr static auto arrow_lexeme = std::string_view{"->"};
    constexpr static auto or_lexeme = std::string_view{"|"};
    constexpr static auto and_lexeme = std::string_view{";"};
    constexpr static auto node_lexeme = std::string_view{"()"};
    constexpr static auto node_attr_lexeme = std::string_view{"{}"};

    struct node {
        std::optional<std::string> label;
        std::vector<
            std::variant<
                std::monostate,
                token
            >
        > attrs;
        std::vector<node*> adj_nodes;

        node() = default;

        node(const auto &l)
            : label{l}, attrs{}
        { }

        node(const auto &l, auto &&a)
            : label {l}, attrs{a}
        { }
    };

    std::string literal;
    std::vector<std::string> parts = {};
    std::vector<
        std::tuple<
            std::string_view,
            const std::string*
        >
    > node_views = {};
    std::vector<
        std::tuple<
            node,
            std::string_view,
            const std::string*
        >
    > nodes = {};
    /// TODO: the following must be replaced by a non-const reference
    /// (or non-const pointer)
    std::vector<error_entry> errors = {};

    match_generator(const std::string &str)
        : literal{str}
    {
        parse();
    }

private:
    void parse_nodes();

    void parse_relations();

    void get_nodes() {
        // constexpr auto open_parens = std::array{'(', '[', '{'};
        for (const auto &part : parts) {
            for (size_t i = 0, size = part.size(); i < size; ++i) {
                if (const auto ch = part[i];
                    ch == node_lexeme[0]) {
                    const auto *str_data = part.c_str() + i;
                    if (const auto closing_paren_idx = find_closing_paren({str_data, size - i});
                        closing_paren_idx != std::string_view::npos) {
                        node_views.push_back({
                            {str_data, closing_paren_idx + 1},
                            &part
                        });
                    }
                }
            }
        }

        // for(const auto &node : node_views) {
        //     std::cout << "node: " << node << std::endl;
        // }
    }

    void get_parts() {
        auto ss = std::stringstream{literal};
        auto part = std::string{};

        while (std::getline(ss, part, and_lexeme[0])) {
            parts.push_back(part);
        }
    }

    void parse() {
        get_parts();
        get_nodes();
        parse_nodes();
        parse_relations();
    }

};

auto find_closing_paren(std::string_view sv)
    -> size_t
{
    const auto open_close_paren_map = std::map<char, char>{
        {'(', ')'}, {'[', ']'}, {'{', '}'}, {'"', '"'}
    };
    const auto open_paren = sv[0];
    assert (open_close_paren_map.contains(open_paren));
    const auto close_paren = open_close_paren_map.at(open_paren);
    auto count_open_paren = 1;

    for (size_t i = 1, size = sv.size(); i < size; ++i) {
        if (sv[i] == open_paren) {
            ++count_open_paren;
        } else if (sv[i] == close_paren) {
            --count_open_paren;
        }
        if (0 == count_open_paren) {
            return i;
        }
    }

    return std::string_view::npos;
}

auto parse_attr_list(
    std::string_view sv,
    std::vector<error_entry> &errors
)
    -> decltype(match_generator::node::attrs)
{
    using attrs_type = decltype(match_generator::node::attrs);
    auto attrs = attrs_type{};

    std::cout << "parse_attr_list(): sv " << sv << std::endl;

    auto line = std::string{sv}, curr_comment = std::string{};
    auto in_comment = false;
    auto comment_pos = source_position{};
    auto tokens = std::vector<token>{};
    auto comments = std::vector<comment>{};
    // auto errors = std::vector<error_entry>{};
    auto rsm = std::optional<raw_string>{};

    lex_line(line, 1, in_comment, curr_comment, comment_pos, tokens,
             comments, errors, rsm);

    if (line != sv) {
        /// TODO: if string was changed, deal with that
        std::cout << "line " << line << ", sv " << sv << std::endl;
    } else {
        // token string view must point to persistent string view
        for (auto &t : tokens) {
            t = token{
                sv.data() + (t.as_string_view().data() - line.data()),
                t.length(),
                t.position(),
                t.type()
            };
        }   
    }

    // Every token in-between should be alternating literal/identifier
    // and comma. Should we allow for trailing commas?
    auto it = tokens.cbegin();
    const auto it_end = tokens.cend();
    auto next_must_be_comma = false;
    for (; it != it_end; ++it, next_must_be_comma = !next_must_be_comma) {
        const auto type = it->type();
        if (!next_must_be_comma) {
            if (!is_literal(type) || type != lexeme::Identifier) {
                errors.push_back(
                    {   
                        source_position{-1, -1},
                        "Attribute should be wither a literal or identifier"
                    }
                );
            }

            attrs.push_back({*it});
        } else {
            if (type != lexeme::Comma) {
                errors.push_back(
                    {
                        source_position{-1, -1},
                        "There should be a comma after a literal or identifier list"
                    }
                );
            }
        }
    }

    return attrs;
error:
    return {};
}

auto parse_node(
    std::string_view sv_node,
    std::vector<error_entry> &errors
)
    -> match_generator::node
{   
    using attrs_type = decltype(match_generator::node::attrs);
    // const auto str_node = std::string{sv_node};
    const auto node_pat = std::regex{"^\\(([a-zA-Z_]\\w*)(:\\{(.*)\\})?\\)$"};
    constexpr auto label_regex_idx = 1;
    constexpr auto attr_regex_idx = 3;
    auto match = std::cmatch{};

    if (std::regex_search(sv_node.cbegin(), sv_node.cend(), match, node_pat)) {
        for (auto i = 0; i < match.size(); ++i) {
            std::cout << "match " << i << " " << match[i].str() << std::endl;
        }
        if (match.size() >= 4) {
            return {
                match[label_regex_idx].str(),
                parse_attr_list(
                    sv_node.substr(
                        match.position(attr_regex_idx),
                        match.length(attr_regex_idx)
                    ),
                    errors
                )
            };
        } else {
            return {match[1].str()};
        }
    }

    return {};
}

void match_generator::parse_nodes()
{
    for (const auto &[nv, str] : node_views) {
        nodes.push_back({
            parse_node(nv, errors),
            nv,
            str
        });
    }
}

void match_generator::parse_relations()
{
    // struct cmp_string_view {
    //     auto operator()(std::string_view lhs, std::string_view rhs) const
    //         -> bool {
    //         return lhs.data() < rhs.data();
    //     }
    // };
    auto parts_view_node_map = std::map<
        const std::string*,
        std::vector<
            std::tuple<
                node*,
                std::string_view
            >
        >
    >{};
    for (auto &[node, nv, str] : nodes) {
        parts_view_node_map[str].push_back({&node, nv});
    }

    // checking whether the nodes in the parts are linked by arrow lexeme
    for (const auto &[str, nodes] : parts_view_node_map) {
        auto oss = std::ostringstream{};
        auto sep = std::string_view{""};
        std::cout << "*str " << *str << " nodes " << std::endl;
        for (
            auto it = nodes.begin(), it_end = nodes.end();
            it != it_end;
            ++it
        ) {
            auto *node = std::get<0>(*it);
            const auto nv = std::get<1>(*it);
            // std::cout << "\t" << *(node->label) << std::endl;
            std::cout << "\t" << nv << std::endl;
            if (
                const auto next_it = std::next(it);
                next_it != it_end
            ) {
                node->adj_nodes.push_back(std::get<0>(*next_it));
            }
            oss << sep << nv;
            sep = arrow_lexeme;
        }
        if (*str != oss.str()) {
            errors.push_back(
                {
                    source_position{-1, -1},
                    "Link between nodes should be an arrow (`->`)"
                }
            );
            return;
        }
    }
}

}


namespace cpp2::bounded_simulation {

struct trivial_predicate {
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
    using node_type = std::tuple<
        // value of the node
        v_type,
        // adjacency list with the specified (optional) value (f_E)
        std::vector<v_type>,
        // preficate object for the current node (f_V)
        predicate_type
    >;
    using adjacency_list_type = decltype(std::get<1>(std::declval<node_type>()));

    pattern() = delete;

    pattern(size_t N, const predicate_type &pred) {
        nodes.reserve(N);

        for (size_t i{}; i < N; ++i) {
            nodes.emplace_back(i, std::vector<v_type>{}, pred);
        }
    }

    pattern(size_t N, const std::vector<predicate_type> &preds) {
        assert (preds.size() >= N);
        nodes.reserve(N);

        for (size_t i{}; i < N; ++i) {
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

        /// TODO: check whether it already exists and insert sorted
        /// rather than pushing back
        std::get<1>(nodes[source]).push_back(sink);
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
    using node_type = std::tuple<
        std::vector<v_type>,
        attrs_type
    >;
    using adjacency_list_type = decltype(std::get<0>(std::declval<node_type>()));
    using path_type = std::vector<v_type>;

    graph() = delete;

    graph(size_t N)
        requires (std::is_trivially_constructible_v<attrs_type>)
        : nodes(N)
    { }

    graph(size_t N, const std::vector<attrs_type> &attrs) {
        assert (attrs.size() >= N);
        nodes.reserve(N);

        for (size_t i{}; i < N; ++i) {
            nodes.emplace_back(std::vector<v_type>{}, attrs[i]);
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

        /// TODO: check whether it already exists and insert sorted
        /// rather than pushing back
        std::get<0>(nodes[source]).push_back(sink);

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
            const auto &adj_list = std::get<0>(nodes[*it]);
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
    typename pattern<at, pt>::node_type const &pat_node
)
    -> typename pattern<at, pt>::v_type
{
    return std::get<0>(pat_node);
}

template <typename at, typename pt>
auto get_node_predicate(
    typename pattern<at, pt>::node_type const &pat_node
)
    -> pt
{
    return std::get<2>(pat_node);
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
auto match(
    typename pattern<at, pt>::node_type const &pat_node,
    auto &&node
)
    -> bool
{
    return std::get<2>(pat_node)(node);
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
        // u, u'
        const auto [u, u_prime] = pat_edge;
        // v, v'
        const auto v_range = pattern_graph_vertex_mmap.equal_range(u),
                   v_prime_range = pattern_graph_vertex_mmap.equal_range(u_prime);

        for (auto it = v_range.first; it != v_range.second; ++it) {
            const auto v = it->second;
            for (auto it2 = v_prime_range.first; it2 != v_prime_range.second; ++it2) {
                const auto v_prime = it2->second;
                if (pat_edge_value) {

                }
            }
        }
    }

    return true;
}

}

#endif
