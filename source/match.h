#ifndef __CPP2_MATCH
#define __CPP2_MATCH

#include "cpp2util.h"

#include <assert.h>
#include <map>
#include <optional>
#include <regex>
#include <string>
#include <string_view>
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

size_t find_closing_paren(std::string_view);

// Expands a match string literal to a full-on lambda object
// Maybe struct with `operator()` ?
struct match_generator {
    constexpr static auto arrow_lexeme = std::string_view{"->"};
    constexpr static auto or_lexeme = std::string_view{"|"};
    constexpr static auto and_lexeme = std::string_view{"&"};
    constexpr static auto node_lexeme = std::string_view{"()"};
    constexpr static auto node_attr_lexeme = std::string_view{"{}"};

    struct node {
        std::optional<std::string> label;
        std::vector<
            std::variant<
                std::monostate,
                long,
                double,
                bool,
                std::string
            >
        > attrs;

        node() = default;

        node(const auto &l)
            : label{l}, attrs{}
        { }

        node(const auto &l, auto &&a)
            : label {l}, attrs{a}
        { }
    };

    std::string literal;
    std::vector<std::string_view> node_views = {};
    std::vector<node> nodes = {};

    match_generator(const std::string &str)
        : literal{str}
    {
        parse();
    }

private:
    void parse_nodes() {

    }

    void get_nodes() {
        // constexpr auto open_parens = std::array{'(', '[', '{'};
        for (size_t i = 0, size = literal.size(); i < size; ++i) {
            const auto ch = literal[i];
            if (ch == '(') {
                const auto *str_data = literal.c_str() + i;
                if (const auto closing_paren_idx = find_closing_paren({str_data, size - i});
                    closing_paren_idx != std::string_view::npos) {
                    node_views.push_back({str_data, closing_paren_idx + 1});
                }
            }
        }

        // for(const auto &node : node_views) {
        //     std::cout << "node: " << node << std::endl;
        // }
    }

    void parse() {
        get_nodes();
    }

};

auto find_closing_paren(std::string_view sv)
    -> size_t
{
    const auto open_close_paren_map = std::map<char, char>{
        {'(', ')'}, {'[', ']'}, {'{', '}'}
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

auto parse_node(std::string_view sv_node)
    -> match_generator::node
{   
    const auto str_node = std::string{sv_node};
    const auto node_pat = std::regex{"^\\(([a-zA-Z_]\\w*)(:\\{(.*)\\})\\)?$"};
    auto match = std::smatch{};

    if (std::regex_search(str_node, match, node_pat)) {
        // for (auto i = 0; i < match.size(); ++i) {
        //     std::cout << "match " << match[i].str() << std::endl;
        // }
        return {match[1].str()};
    }

    return {};
}

}

#endif
