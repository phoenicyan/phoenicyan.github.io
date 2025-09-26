---
layout: post
title: Using PEG Parser For SQL Transpiler Development, Part 1
date: 2025-09-14 04:39:00-0700
description: we use highly regarded tools to write a SQL statement transpiler
tags: SQL PEG Cpp
categories: software
featured: true
---

<div style="font-style: italic;text-align: right;">
"The heavens declare the glory of God; and the firmament sheweth his handywork." PS 19:1
</div>


## Introduction
I demonstrate some of my ideas for a SQL transplilation[^1] (conversion between Postgres and T-SQL dialects). I chose an AI Code Writing Assisstant from one of the most reputable vendors: gemini.google.com. The goals of my project are:
1. Implement a parser that understands a couple of SQL dialects (Postgres and T-SQL).
1. Implement a builder that converts AST into an internal representation of a SQL statement.
1. Implement the output statement generator in the desired dialect.

For example, the Transpiler should be able to convert a T-SQL statement such as 
```sql 
SELECT TOP 10 max([dbo].[col1]) FROM [dbo].[tbl] 
```
into an equivalent Postgres statement
```sql 
SELECT max("dbo"."col1") FROM "dbo"."tbl" LIMIT 10
```
As an additional benefit of this exercise, I wanted to learn about a modern parsing system. After learning about lex/yacc at college many years ago, I only had some experience with the Gold parser back in the beginning of the 2000s. And I also knew from my previous attempt to learn Antlr4 - it has unpredictable parsing time and numerous other problems, including "reduce-reduce" conflicts that are very hard to resolve. I googled around and found a modern parsing tool called PEG, along with its C++ variant, cpp-peglib, on GitHub.

Source code for this project is available [here](https://github.com/phoenicyan/sql_transpiler/).

[^1]: Definition: A transpiler is a type of program, also known as a source-to-source compiler or transcompiler, that takes the source code of a program in one programming language and converts it into equivalent source code in a different, but generally similar, programming language. This process allows developers to use modern or specialized language features and have their code run in environments that don't support them, or to migrate legacy code to newer platforms.

## Act 1. Implementing parser and visitor
After watching a couple of free online courses about the absolute best practices for "vibe coding", I decided to split requests to AI to make tiny steps such as
1. Write PEG grammar to parse arbitrary text consisting of identifiers, literals, numbers separated by punctuation symbols that I found on the keyboard (``~!@#$%^&*=+;:<>\\/,.?|-``), grouped by parenthesis and with optional single line comments (starting with ``--``) and multiple lines comments (contained in ``/*  */``).
1. Modify the PEG grammar to logically group the identifiers, literals, and numbers into statements where a statement starts with a keyword (``ALTER, CREATE, DELETE, DROP, INSERT, SELECT, SET, SHOW, TRUNCATE, UPDATE, START, COMMIT, ROLLBACK``) and ends with a semicolon. If a statement does not start with a keyword, then it should be recognized as an unknown statement.
1. Implement an initial primitive AST visitor that prints the statement(s). The idea is to tweak the visitor later to print AST into a different dialect of SQL than the input dialect.
1. Modify AST visitor to print SQL in a specific dialect.

### Postgres PEG parser
Gemini brought in a valuable prototype that I tested in [Yhirose's PEGlib Playground](https://yhirose.github.io/cpp-peglib/). Its straightforward design encouraged me to ask Gemini to refrain from any additional comments or explanations, allowing me to explore its functionality independently.
```
Start <- Content EOI

Content <- (Spacing? Expression )* Spacing?

Expression <- Parenthesized / StringLiteral / Number / IdentifierOrKeyword / Punctuation

Parenthesized <- '(' Content ')'

Spacing <- (Whitespace / Comment)+

Comment <- SingleLineComment / MultiLineComment

SingleLineComment <- SLCprefix (!EOL .)*
~SLCprefix <- '--'

MultiLineComment  <- MLCprefix (!MLCsuffix .)* MLCsuffix
~MLCprefix <- '/*'
~MLCsuffix <- '*/'

Whitespace <- [ \t\n\r\[\]]+

Identifier      <- < [a-zA-Z_] [a-zA-Z0-9_]* >
QtIdentifier      <- '"' [^"]* '"'

IdentifierOrKeyword <- QtIdentifier / Identifier

Number          <- <[+-]? [0-9]+ ('.' [0-9]+)?>

StringLiteral   <- "'" ( "''" / !"'" . )* "'"

Punctuation     <- [~!@#$%^&*=+;:<>\\/,.?|-]

EOL <- '\n' / '\r\n' / '\r'
EOI <- !.
```

> **_NOTE:_** Here I learned that ~ at the start of a rule marks it as a "silent rule", i.e., a rule that is not included in the AST.

Sample text and AST:
```
-- sample comment
SELECT max("col1") FROM "tbl" LIMIT 10;SELECT 1;

+ Start
    + Content
    + Spacing
        + Comment/0[SingleLineComment]
        - Whitespace (
)
    - Expression/3[Identifier] (SELECT)
    - Spacing/0[Whitespace] ( )
    - Expression/3[Identifier] (max)
    - Expression/0[QtIdentifier] ("col1")
    - Spacing/0[Whitespace] ( )
    - Expression/3[Identifier] (FROM)
    - Spacing/0[Whitespace] ( )
    - Expression/3[QtIdentifier] ("tbl")
    - Spacing/0[Whitespace] ( )
    - Expression/3[Identifier] (LIMIT)
    - Spacing/0[Whitespace] ( )
    - Expression/2[Number] (10)
    - Expression/4[Punctuation] (;)
    - Expression/3[Identifier] (SELECT)
    - Spacing/0[Whitespace] ( )
    - Expression/2[Number] (1)
    - Expression/4[Punctuation] (;)
    - EOI ()
```

Then I slightly modified the grammar to treat ``;`` as a special symbol that separates statements, and introduced the Keyword rule that can tell me which statement type is used:
```    
Statements <- EOS* (Statement EOS)* Statement? EOI

Statement <- (Spacing? Expression )* Spacing?

Expression <- Parenthesized / StringLiteral / Number / IdentifierOrKeyword / Punctuation

Parenthesized <- '(' EOS* (Statement EOS)* Statement? ')'

~Spacing <- (Whitespace / Comment)+

Comment <- SingleLineComment / MultiLineComment

SingleLineComment <- SLCprefix (!EOL .)*
~SLCprefix <- '--'

MultiLineComment  <- MLCprefix (!MLCsuffix .)* MLCsuffix / MLC2 (!MLC2 .)* MLC2
~MLCprefix <- '/*'
~MLCsuffix <- '*/'
~MLC2 <- '$$'

Whitespace <- [ \t\n\r\[\]]+

Identifier      <- !Keyword < [a-zA-Z_] [a-zA-Z0-9_]* >
QtIdentifier      <- '"' [^"]* '"'
Keyword <- 'ALTER'i / 'CREATE'i / 'DELETE'i / 'DROP'i / 'INSERT'i / 'SELECT'i / 'SET'i / 'SHOW'i / 'TRUNCATE'i / 'UPDATE'i / 'START'i / 'COMMIT'i / 'ROLLBACK'i

IdentifierOrKeyword <- Keyword / QtIdentifier / Identifier

Number          <- <[+-]? [0-9]+ ('.' [0-9]+)?>

StringLiteral   <- "'" ( "''" / !"'" . )* "'"

Punctuation     <- [~!@#$%^&*=+:<>\\/,.?|-]

EOL <- '\n' / '\r\n' / '\r'  # End of Line
EOI <- !.                    # End of Input
EOS <- ';'                   # End of Statement
```

> **_NOTE:_** I added rule MLC2 to treat any text inside $$ tags as a multiline comment.

Sample text and AST:
```
CREATE FUNCTION voidtest1(a int) RETURNS VOID LANGUAGE SQL AS
$$ SELECT a + 1 $$;
SELECT voidtest1(42);

+ Statements
    + Statement
    - Expression/3[Keyword] (CREATE)
    - Expression/3[Identifier] (FUNCTION)
    - Expression/3[Identifier] (voidtest1)
    + Expression/0[Statement]
        - Expression/3[Identifier] (a)
        - Expression/3[Identifier] (int)
    - Expression/3[Identifier] (RETURNS)
    - Expression/3[Identifier] (VOID)
    - Expression/3[Identifier] (LANGUAGE)
    - Expression/3[Identifier] (SQL)
    - Expression/3[Identifier] (AS)
    + Statement
    - Expression/3[Keyword] (SELECT)
    - Expression/3[Identifier] (voidtest1)
    - Expression/0[Number] (42)
    + Statement
```

[comment]: <> (> |---|---| )
[comment]: <> (> | ![red herring](/assets/img/redher_sm.jpg) | At this point, I became curious whether it is possible to handle dynamic tags similar to C++ multiline literals: <br> )``R&quot;xyz( ... )xyz&quot;`` or similar to Postgres function body wrappers: |
[comment]: <> (>)
[comment]: <> (> ```sql )
[comment]: <> (> create function square(x int4) returns int4 as)
[comment]: <> (> $xyz$)
[comment]: <> (> begin)
[comment]: <> (>     return x * x;)
[comment]: <> (> end)
[comment]: <> (> $xyz$ language plpgsql;)
[comment]: <> (> ```)
[comment]: <> (> )
[comment]: <> (> Gemini suggested this variant:)
[comment]: <> (> ```)
[comment]: <> (> DollarQuotedString <- StartTag Content EndTag)
[comment]: <> (> StartTag <- &apos;&dollar;&apos; Tag? &apos;&dollar;&apos;)
[comment]: <> (> EndTag <- &apos;&dollar;&apos; Tag? &apos;&dollar;&apos; &{ match(Tag, StartTag.Tag) })
[comment]: <> (> Tag <- < [a-zA-Z_0-9]* >)
[comment]: <> (> Content <- (!EndTag .)*)
[comment]: <> (> ```)
[comment]: <> (> )
[comment]: <> (> But then it added the comment &quot;cpp-peglib does not support semantic predicates in the way I demonstrated with the ``&{...}`` syntax. That syntax is a powerful but )non-standard extension to PEGs that allows for dynamic checks. Many online PEG tools, including cpp-peglib, stick to the core PEG operators.&quot;
[comment]: <> (> As next logical step, I asked this question on yhirose github page, and immediately got answer from mingodad that cpp-peglib provides mechanism called Capture/)Backtrace. He also provided the example:
[comment]: <> (> )
[comment]: <> (> ```)
[comment]: <> (> CreateFunc <- Header Body Language &apos;;&apos;)
[comment]: <> (> Header <- &apos;CREATE&apos;i &apos;FUNCTION&apos;i (!&apos;RETURNS&apos;i .)+ &apos;RETURNS&apos;i (!&apos;AS&apos;i .)+ &apos;AS&apos;i)
[comment]: <> (> Body <- DollarQuotedString)
[comment]: <> (> Language <- &apos;LANGUAGE&apos;i (!&apos;;&apos; .)+)
[comment]: <> (> DollarQuotedString <- $(StartTag Content EndTag))
[comment]: <> (> StartTag <- <&apos;&dollar;&apos; $Tag<Separator> &apos;&dollar;&apos;>)
[comment]: <> (> EndTag <- <&apos;&dollar;&apos; $Tag &apos;&dollar;&apos;>)
[comment]: <> (> Separator <- (!&apos;&dollar;&apos; .)*)
[comment]: <> (> Content <- $((!EndTag .)*))
[comment]: <> (> )
[comment]: <> (> %whitespace <- [ \t\r\n]*)
[comment]: <> (> ```)
[comment]: <> (> )
[comment]: <> (> With the resulting AST:)
[comment]: <> (> )
[comment]: <> (> ```)
[comment]: <> (> + CreateFunc)
[comment]: <> (>     - Header (create function square(x int4) returns int4 as))
[comment]: <> (>     + Body/0[DollarQuotedString])
[comment]: <> (>     - StartTag ($xyz$))
[comment]: <> (>     - Content (begin)
[comment]: <> (>     return x * x;)
[comment]: <> (> end)
[comment]: <> (> ))
[comment]: <> (>     - EndTag ($xyz$))
[comment]: <> (>     - Language (language plpgsql))
[comment]: <> (> ```)

<table><tbody><tr>
<td style="display: block;"><img src="/assets/img/redher_sm.jpg" alt="red herring"></td><td><sub>At this point, I became curious whether it is possible to handle dynamic tags similar to C++ multiline literals: <br> ``R"xyz( ... )xyz"`` or similar to Postgres function body wrappers:</sub><br>
<pre style="line-height: 0.8em;margin: 0;font-size: 0.7em;"><code class="language-sql">
create function square(x int4) returns int4 as<br>
$xyz$<br>
begin<br>
    return x * x;<br>
end<br>
$xyz$ language plpgsql;<br>
</code></pre><sub>
Gemini suggested this variant:<br>
</sub><pre style="line-height: 0.8em;margin: 0;font-size: 0.7em;"><code>
DollarQuotedString <- StartTag Content EndTag<br>
StartTag <- '$' Tag? '$'<br>
EndTag <- '$' Tag? '$' &{ match(Tag, StartTag.Tag) }<br>
Tag <- < [a-zA-Z_0-9]* ><br>
Content <- (!EndTag .)*<br>
</code></pre><sub>
<br>
But then it added the comment "cpp-peglib does not support semantic predicates in the way I demonstrated with the ``&{...}`` syntax. That syntax is a powerful but non-standard extension to PEGs that allows for dynamic checks. Many online PEG tools, including cpp-peglib, stick to the core PEG operators."<br>
As next logical step, I asked this question on yhirose github page, and immediately got answer from mingodad that cpp-peglib provides mechanism called Capture/Backtrace. He also provided the example:<br>
<br>
</sub><pre style="line-height: 0.8em;margin: 0;font-size: 0.7em;"><code>
CreateFunc <- Header Body Language ';'<br>
Header <- 'CREATE'i 'FUNCTION'i (!'RETURNS'i .)+ 'RETURNS'i (!'AS'i .)+ 'AS'i<br>
Body <- DollarQuotedString<br>
Language <- 'LANGUAGE'i (!';' .)+<br>
DollarQuotedString <- $(StartTag Content EndTag)<br>
StartTag <- <'$' $Tag&lt;Separator&gt; '$'><br>
EndTag <- <'$' $Tag '$'><br>
Separator <- (!'$' .)*<br>
Content <- $((!EndTag .)*)<br>
<br>
%whitespace <- [ \t\r\n]*<br>
</code></pre><br><sub>
With the resulting AST:
</sub><pre style="line-height: 0.8em;margin: 0;font-size: 0.7em;"><code>
+ CreateFunc<br>
    - Header (create function square(x int4) returns int4 as)<br>
    + Body/0[DollarQuotedString]<br>
    - StartTag ($xyz$)<br>
    - Content (begin<br>
    return x * x;<br>
end<br>
)<br>
    - EndTag ($xyz$)<br>
    - Language (language plpgsql)<br>
</code></pre>
</td>
</tr></tbody></table><br>

Third, Gemini suggested: "In C++, the best way to process an Abstract Syntax Tree (AST) is by using the Visitor pattern and recursive traversal. This approach combines a well-established design pattern with a fundamental algorithm to handle the tree's hierarchical structure effectively." It defined the Visitor interface as:
```cpp
class IParseTreeVisitor
{
public:
    IParseTreeVisitor(const std::string& originalStmt, void* pContext)
                    : _originalStmt(originalStmt), _pContext(pContext) {
    }

    virtual void Visit(const CustomAst& ast) = 0;

protected:
    void* _pContext; // reserved
    const std::string _originalStmt;
};
```
Where CustomAst was defined as:
```cpp
class CustomType {
public:
    void Accept(IParseTreeVisitor* pVisitor);
};

using CustomAst = peg::AstBase<CustomType>;

void CustomType::Accept(IParseTreeVisitor* pVisitor) {
    pVisitor->Visit(*reinterpret_cast<CustomAst*>(this));
}
```


[comment]: <> (|---|---|)
[comment]: <> (| ![red herring](/assets/img/redher_sm.jpg) | I thought that I might need a method to obtain a substring that created the given AST node, so I  wrote the following method and placed it inside IParseTreeVisitor for code reuse in derived classes: |)
[comment]: <> ( )
[comment]: <> (<pre style="font-size: 0.6em;"><code class="language-cpp">)
[comment]: <> (std::string getLRTerm(const CustomAst& ast) {)
[comment]: <> (    if (ast.is_token))
[comment]: <> (        return std::string(ast.token);)
[comment]: <> ( )
[comment]: <> (    if (0 == ast.length))
[comment]: <> (        return &quot;&quot;;)
[comment]: <> ( )
[comment]: <> (    auto pos = ast.position;)
[comment]: <> (    auto len = ast.length;)
[comment]: <> ( )
[comment]: <> (    if (ast.tag != ast.original_tag))
[comment]: <> (    {)
[comment]: <> (        pos = ast.nodes[0]->position;)
[comment]: <> (        len = ast.nodes[ast.nodes.size() - 1]->position)
[comment]: <> (    + ast.nodes[ast.nodes.size() - 1]->length - pos;)
[comment]: <> (    })
[comment]: <> ( )
[comment]: <> (    return m_originalStmt.substr(pos, len);)
[comment]: <> (})
[comment]: <> (</code></pre>)
[comment]: <> (<sub>The ``ast.nodes[0]`` is left child node (L) and the ``ast.nodes[ast.nodes.size() - 1]``  is right child node (R). Later I realized that this method worked for a single line input only. I fixed it for a multiline text, but soon realized that I did not need it.</sub>)

<table><tbody><tr>
<td style="display: block;"><img src="/assets/img/redher_sm.jpg" alt="red herring"></td><td><sub>I thought that I might need a method to obtain a substring that created the given AST node, so I  wrote the following method and placed it inside IParseTreeVisitor for code reuse in derived classes:</sub><br>
<pre style="line-height: 0.8em;margin: 0;font-size: 0.7em;"><code class="language-cpp">
 std::string getLRTerm(const CustomAst& ast) {<br>
     &nbsp;&nbsp;if (ast.is_token)<br>
         &nbsp;&nbsp;&nbsp;&nbsp;return std::string(ast.token);<br>
 <br>
     &nbsp;&nbsp;if (0 == ast.length)<br>
         &nbsp;&nbsp;&nbsp;&nbsp;return "";<br>
 <br>
     &nbsp;&nbsp;auto pos = ast.position;<br>
     &nbsp;&nbsp;auto len = ast.length;<br>
 <br>
     &nbsp;&nbsp;if (ast.tag != ast.original_tag)<br>
     &nbsp;&nbsp;{<br>
         &nbsp;&nbsp;&nbsp;&nbsp;pos = ast.nodes[0]->position;<br>
         &nbsp;&nbsp;&nbsp;&nbsp;len = ast.nodes[ast.nodes.size() - 1]->position + ast.nodes[ast.nodes.size() - 1]->length - pos;<br>
     &nbsp;&nbsp;}<br>
 <br>
     &nbsp;&nbsp;return m_originalStmt.substr(pos, len);<br>
 }
 </code></pre><sub>The <code class="language-plaintext highlighter-rouge">ast.nodes[0]</code> is left child node (L) and the <code class="language-plaintext highlighter-rouge">ast.nodes[ast.nodes.size() - 1]</code>  is right child node (R). Later I realized that this method worked for a single line input only. I fixed it for a multiline text, but soon realized that I did not need it.</sub>
</td>
</tr></tbody></table><br>

The final cherry on top idea comes from observation of AST:
```sql
-- sample comment
SELECT max("col1") FROM "tbl" LIMIT 10;SELECT 1;

+ Statements
    + Statement
    - Expression/3[Keyword] (SELECT)
    - Expression/3[Identifier] (max)
    - Expression/0[QtIdentifier] ("col1")
    - Expression/3[Identifier] (FROM)
    - Expression/3[QtIdentifier] ("tbl")
    - Expression/3[Identifier] (LIMIT)
    - Expression/2[Number] (10)
    - EOS (;)
    + Statement
    - Expression/3[Keyword] (SELECT)
    - Expression/2[Number] (1)
    - EOS (;)
    + Statement
    - EOI ()
```
Spaces, comments, parenthesis are not included in AST. So, there should be a mechanism to insert missing characters from the original input string. I implemented it by counting how many characters were printed, and if next AST node start position is larger than the counter, print the omitted characters from the original string. Here is first version of visitor that prints a string that is the same as the input:
```cpp
class CParseTreeVisitor : public IParseTreeVisitor {
    size_t _printedCount;

public:
    CParseTreeVisitor(const std::string& originalStmt, void* pContext)
                : IParseTreeVisitor(originalStmt.c_str(), pContext), _printedCount(0) {
    }

    void Visit(const CustomAst& ast) override {
        switch (ast.tag) {
        default:
            for (const auto& node : ast.nodes) {
                if (node->column - 1 > _printedCount) {
                    std::cout << m_originalStmt.substr(_printedCount, 
                                    node->column - 1 - _printedCount);
                }

                std::string term(getLRTerm(*node));
                std::cout << term;

                _printedCount = node->column - 1 + term.size();
            }
            break;

        case peg::str2tag("QtIdentifier"):
            std::cout << ast.token;
            _printedCount = ast.column - 1 + ast.token.size();
            break;
        }
    }
};
```
After the initial success in printing AST, I made two improvements: first, the printer code should be aware about line and column (not only a column); and second, it should walk thru the tree (not only print first level nodes). For this I modified IParseTreeIntrface with Gemini help as:
```cpp
class IParseTreeVisitor
{
public:
    IParseTreeVisitor(const std::string& originalStmt, void* pContext) 
                : _originalStmt(originalStmt), _pContext(pContext) {
        getOffsetsLengths(_originalStmt, _lineOfsLens);
    }

    virtual void Visit(const CustomAst& ast) = 0;

protected:
    static void getOffsetsLengths(const std::string& text
                                                , std::vector<std::pair<size_t, size_t>>& line_data) {
        size_t current_pos = 0, start_of_line = 0;

        while ((current_pos = text.find('\n', start_of_line)) != std::string::npos) {
            line_data.push_back({ start_of_line, current_pos - start_of_line });
            start_of_line = current_pos + 1;
        }

        size_t last_line_length = text.length() - start_of_line;
        if (last_line_length > 0 || start_of_line < text.length()) {
            line_data.push_back({ start_of_line, last_line_length });
        }
    }

    void* _pContext; // reserved
    std::string _originalStmt;
    std::vector<std::pair<size_t, size_t>> _lineOfsLens;   // pairs of offsets and lengths
};
```
And slightly modified the ``Visit`` method itself:
```cpp
std::pair<size_t, size_t> _printedCount;// index of last printed line, and total chars already printed

void Visit(const CustomAst& ast) override {
    for (const auto& node : ast.nodes) {
        // print chars from the last printed position to the node
        for (size_t i = _printedCount.first; i < node->line - 1; ++i) {
            const size_t currentOffset = _lineOfsLens[i].first;
            const size_t currentLen = _lineOfsLens[i].second;
            if (_printedCount.second < currentOffset + currentLen) {
                std::cout << _originalStmt.substr(_printedCount.second, currentOffset + currentLen - _printedCount.second + 1);
                _printedCount = { i, currentOffset + currentLen + 1 };
            }
        }

        // print either token or visit the node
        if (node->is_token) {
            const size_t currentOffset = _lineOfsLens[node->line - 1].first + node->column - 1;
            const size_t currentLen = _lineOfsLens[node->line - 1].second;
            if (_printedCount.second < currentOffset) {
                std::cout << _originalStmt.substr(_printedCount.second, currentOffset - _printedCount.second);
                _printedCount.second = currentOffset;
            }

            std::cout << node->token;

            _printedCount.second += node->token.size();
        }
        else {
            node->Accept(this);
        }
    }
}
```
Easy peasy!

### T-SQL PEG Parser
Since the Postgres grammar was basically skipping most symbols, I made the following changes to support T-SQL syntax:
```
• Changed QtIdentifier rule as:  
    QtIdentifier      <- '[' [^\]]* ']' / '"' [^"]* '"'
• Changed EOS rule as:
    EOS <- ';' / 'GO'i           # End of Statement
• Added the TOP rule:
    TopClause <- 'TOP'i Spacing Number { no_ast_opt }
```

## Act 2. Internal Representation (IR)
The internal representation should keep information about a SQL statement in dialect-neutral form. It can be used to output SQL into any desired dialect. The IR could be implemented as a tree-like structure, but for the purpose of this small exercise I used a simple structure with the following fields:

| Field	| Purpose |
| ----- | ------- | 
| STMT_TYPE&nbsp;_stt | Statement type defined as: `` enum STMT_TYPE { STT_UNKNOWN, STT_ALTER, STT_CREATE, STT_DELETE, STT_DROP, STT_INSERT, STT_SELECT, STT_SET, STT_SHOW, STT_TRUNCATE, STT_UPDATE, STT_START, STT_COMMIT, STT_ROLLBACK };`` |
| string&nbsp;_text | The SQL Statement without ``LIMIT/TOP`` clause |
| string&nbsp;_alt_text | The ``LIMIT/TOP`` clause |
| size_t&nbsp;_alt_pos | Position in _text where to insert _alt_text |

<br>

To populate IR I needed helper functions that transform AST nodes into "build IR" instructions. So I defined a function as:
```cpp 
    // returns true if transform succeeded
    bool transform(const CustomAst& ast, OutputInstruction& instruction);
```
Where OutputInstruction was defined as:
```cpp
enum OUTPUT_TYPE {
    INPLACE,    // replace token inplace
    APPEND,     // remove existing token(s) and append new one at the end
    INJECT_0,   // inject into position #0
};

struct OutputInstruction {
    OUTPUT_TYPE type;
    std::string text;
};
```
I also needed a predicate that defines to which AST node the specified function will be applied:
```cpp
struct TransformPredicate {
    unsigned tag;       // e.g. peg::str2tag("QtIdentifier")
    bool (*transform)(const CustomAst& ast, OutputInstruction& instruction);
};
```
The actual transformers for quoted identifiers and LIMIT/TOP clauses were defined as:
```cpp
TransformPredicate pgsql_transformers[] = {
    { peg::str2tag("QtIdentifier"), [](const CustomAst& ast, OutputInstruction& instruction) { instruction = { INPLACE, std::string("[") + std::string(ast.token).substr(1, ast.token.size() - 2) + "]" }; return true; }},
    { peg::str2tag("LimitClause"), [](const CustomAst& ast, OutputInstruction& instruction) { instruction = { INJECT_0, std::string("TOP ") + std::string(ast.nodes[0]->token) + " " }; return true; }},
    { 0, nullptr }
};

TransformPredicate tsql_transformers[] = {
    { peg::str2tag("QtIdentifier"), [](const CustomAst& ast, OutputInstruction& instruction) { instruction = { INPLACE, std::string("\"") + std::string(ast.token).substr(1, ast.token.size() - 2) + "\"" }; return true; }},
    { peg::str2tag("TopClause"), [](const CustomAst& ast, OutputInstruction& instruction) { instruction = { APPEND, std::string(" LIMIT ") + std::string(ast.nodes[0]->token) }; return true; }},
    { 0, nullptr }
};
```
Very straightforward so far (as expected from a POC). I only needed to apply the instruction to the IR, so I added the following method to StatementIRep class:
```cpp
void ProcessInstruction(const OutputInstruction& instruction) {
    switch (instruction.type) {
    case INPLACE: _text.append(instruction.text); break;
    case APPEND: _alt_text.assign(instruction.text); _alt_pos = std::string::npos;  break;
    case INJECT_0: _alt_text.assign(instruction.text); _alt_pos = 7;  break;
    }
}
```
The final piece was to include calls to the transformers into the ``Visitor``.


## Act 3. Implementing output generator and testing
General solution would be to iterate thru all transformers to generate output. But for POC purposes I modified ``CParseTreeVisitor::Visit()`` to invoke ``transformer[0]`` for tokens (aka terminals) and to invoke ``transformer[1]`` for non-terminals:
```cpp
OutputInstruction instruction;
if (node->is_token) {
    std::string& output = _statements.back()._text;
    //...
    if (node->tag == transformers[0].tag && transformers[0].transform(*node, instruction)) {
        _statements.back().ProcessInstruction(instruction);
    }
    else {
        output.append(node->token);
    }
    //...
}
else if (node->tag == transformers[1].tag && transformers[1].transform(*node, instruction)) {
    _statements.back().ProcessInstruction(instruction);
    //...
}
```

To generate output I implemented ``GetStatementsText()`` in the Visitor as:
```cpp
const std::string GetStatementsText() const {
    std::string text;
    for (auto& stmt : _statements) {
        std::string s(stmt._text);
        if (!stmt._alt_text.empty())
            s.insert(stmt._alt_pos != std::string::npos ? stmt._alt_pos : s.length() - 1, stmt._alt_text);
        text.append(s);
        }    
        return text;
}
```
Then I wrote the ``Transpile()`` helper function for the testing. It calls parser on the input, synthesizes the output using the ``Visitor``, and compares the output with the expected text:
```cpp
bool Transpile(peg::parser& parser, TransformPredicate* transformers, const char* szInput, const char* szExpected) {
    std::shared_ptr<CustomAst> ast;

    if (!parser.parse(szInput, ast))
        return false;

    ast = parser.optimize_ast(ast);

    // fmt::print("AST (optimized): {}\n{}\n", szInput, peg::ast_to_s(ast));

    if (ast) {
        CContext context(transformers);
        CParseTreeVisitor visitor(szInput, &context);
        visitor.Visit(*ast);

        std::cout << "Statements parsed: " << visitor.GetStatements().size() << std::endl << std::endl;

        std::string actual(visitor.GetStatementsText());
        std::string expected(szExpected);

        return std::equal(actual.begin(), actual.end(), expected.begin(), expected.end(),
            [](char a, char b) {
                return std::tolower(static_cast<unsigned char>(a)) == std::tolower(static_cast<unsigned char>(b));
            });
    }

    return false;
}
```
And then I added tests like these:
```cpp
TEST(TestCase1, TestName4) {
    EXPECT_TRUE(Transpile(pgsql_parser(), pgsql_transformers, "Select max(\"col1\") FROM \"tbl\" LIMIT 10;", "Select TOP 10 max([col1]) FROM [tbl];"));
}

TEST(TestCase1, TestName5) {
    EXPECT_TRUE(Transpile(tsql_parser(), tsql_transformers, "Select TOP 10 max([dbo].[col1]) FROM [dbo].[tbl];", "Select max(\"dbo\".\"col1\") FROM \"dbo\".\"tbl\" LIMIT 10;"));
}
```

## Conclusion
Gemini was making numerous assumptions about my intentions and generated something entirely different from what I wanted. But I definitely improved my skills of telling Gemini what I wanted by asking it to do very primitive assignments. In a few cases, the AI saved me a bit of time, e.g., "write procedure to split text in vector of lines" or "write procedure to iterate all files in specified path and read the files as text." The AI helped teach me about the PEG parser, the Visitor pattern, etc. However, the "vibe coding" is the same as the spherical cow in a vacuum.  
The idea behind the SQL Transpiler was to start with a very basic parser that extracts identifiers, numbers, and string literals, and then add various rules, such as LIMIT/TOP. This idea could be fruitful, and I would appreciate your feedback (my contact is phoenicyan at gmail dot com). I want to learn alternative ideas for a transpiler, especially from people who have had previous experience in the creation of transpilers.  
In Part 2, I'm excited to share with you a fully functional transpiler that brings this idea to life.

<!-- Vibe coding fucking sucks. Gemini has all these retarded assumptions about intentions that never occured to me, never generated anything resembling what I wanted. I ended up having to develop an entirely new skillset (becoming one of those PROOMPTERS) just so I could start saving time. But at that point the time investment was so hilariously expensive that this anyway useless for me. I guess the only thing i"m happy about is that a few more trees in the Amazon will probably burn down because of all the water use of my retarded prompts. Thank you for YOUR ATTENTION TO THIS MATTER...

and to Gemini, for your coopoeration...

and to Roko's basilisk, please torture me a little less when you exist.  -->
