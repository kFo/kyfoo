Syntax
======

Notation
--------

The notation for syntax patterns in this document is inspired by Extended Backus-Naur Form (EBNF), but it is not the same. Patterns are defined by the ``::=`` operator where the name of the pattern occurs on the left, and the pattern itself occurs on the right. Whitespace is insignificant and serves only to separate pattern invocations. Patterns have the following form:

.. code-block:: bnf

    pattern-name ::= pattern-definition

The ``pattern-name`` may be invoked by other patterns. Its definition ``pattern-definition`` is composed of one or more of the following pattern expressions:

================  =====================  =======
``pattern-definition``
------------------------------------------------
Syntax            Name                   Meaning
================  =====================  =======
``'abc'``         Text Literal           Text between the pair of ``'``'s (``abc``) matches exactly.
``(a)``           Grouping               Pattern ``a`` is matched before patterns outside the parentheses.
``a b``           Sequence               Pattern ``a`` matches followed by pattern ``b``.
``left | right``  Ordered Choice         First tries to match the left pattern (``left``), on failure it matches the right pattern (``right``). Operator associates to the left.
``a..z``          Range                  First character (``a``) and last character (``b``) describe a contiguous, inclusive set of characters (a, b, c, ... z). Exactly one of these characters is matched. E.g. ``a..c`` is equivalent to ``('a' | 'b' | 'c')``.
``..``            Any                    Same as *Range*, but ranges over all possible characters.
``~(a)``          Negate                 Matches any pattern that does not match ``a``.
``?(a)``          Optional               Pattern ``a`` may or may not match.
``*(a)``          Repeat                 Pattern ``a`` matches zero or more times.
``*(a, weave)``   Repeat weave           Equivalent to ``?(a) *(weave a)``. Not associative.
``*/a, weave/``   Repeat weave right     Same as *Repeat weave*, but associates right.
``*\a, weave\``   Repeat weave left      Same as *Repeat weave*, but associates left.
``+(a)``          OneOrMore              Equivalent to ``a *(a)``.
``+(a, weave)``   OneOrMore weave        Equivalent to ``a *(weave a)``. Not associative.
``+/a, weave/``   OneOrMore weave right  Same as *OneOrMore weave*, but associates right.
``+\a, weave\``   OneOrMore weave left   Same as *OneOrMore weave*, but associates left.
================  =====================  =======

Lexical Analyzer
----------------

The *lexical analyzer* (a.k.a. *lexer*, *scanner*, *tokenizer*) transforms a plaintext sequence of characters into a corresponding sequence of *tokens*. Tokens are groupings of characters according to a regular pattern, where the pattern has a name that serves as the category for the token. Tokens are the smallest units of expressions that are analyzed in the syntax analyzer.

Tokens
++++++

The same pattern notation is used to describe both terminal (tokens) and non-terminal patterns. Terminal patterns are required to have a regular pattern. As a simplification, terminals are restricted from invoking other patterns so as to clearly indicate their lack of pattern composition.

.. code-block:: bnf

    id       ::= (a..z | A..Z) *(a..z | A..Z | 0..9 | '_')
    meta     ::= '\' (a..z | A..Z) *(a..z | A..Z | 0..9 | '_')
    integer  ::= +(0..9) ?('e' +(0..9))
    rational ::= *(0..9) '.' +(0..9) ?('e' +(0..9))
    string   ::= '"' *(~('"') | '\"') '"'

Keywords
++++++++

The following identifiers (``id``) are reserved as keywords in the grammar:

.. code-block:: bnf

    keywords ::= import
               | return
               | loop
               | break

Indent Tokens
+++++++++++++

Additionally, the lexical analyzer outputs *virtual tokens*, or *implied tokens*, that rely on additional lexing state. Newlines are significant outside of nestings, and so are spaces occuring at the beginning of a line. The spaces occuring at the start of a line are tracked as part of the indent stack. The *indent stack* is a stack of all active indent increases, and is initially empty. The *indent level* is the size of the indent stack. Spaces occuring at the beginning of a line are consumed as one of the following indent tokens:

============  =======
Indentation tokens
---------------------
Token         Meaning
============  =======
``ind(>)``    Indent level increase. The indent matches every indent in the indent stack, starting from the bottom, in order. The excess unmatched spaces form the new ind(>) that is pushed onto the indent stack.
``ind(=)``    Indent level is the same. The indent matches the full indentation stack exactly.
``ind(<)``    Indent level has decreased. The indent matches the first `m` indents exactly, starting from the bottom of the indent stack. There are `n` ``ind(>)`` total in the stack (`m < n`), creating `n - m` ``ind(<)`` tokens. A ``ind(>)`` token is popped off the indent stack for each ``ind(<)`` token.
``ind(err)``  Indent error. Indentation does not match the indent stack.
============  =======

Note that at most one ``ind(>)`` token may occur in succession whereas multiple ``ind(<)`` tokens may occur in succession, depending on the indent level. Hitting the end of the file will generate a ``ind(<)`` for each ``ind(>)`` in the indent stack.

Nesting Pairs
+++++++++++++

*Nesting pairs* are lexical analyzer state that tracks a stack of nesting pairs, similar to the indentation tracking. For each pair of nest tokens, the opener increases the nesting level and the closer decreases the nesting level. Newlines are ignored inside a nesting.

======  ======
Nesting pairs
--------------
Opener  Closer
======  ======
``(``   ``)``
``(``   ``]``
``[``   ``)``
``[``   ``]``
``<``   ``>``
======  ======

Whitespace
++++++++++

*Comments* and *whitespace* are character sequences that are ignored by the syntax analyzer. Comments are used to embed information in the source file that have no semantic significance.

.. code-block:: bnf

    space ::= ' ' | '\t'
    newline ::= ?('\r') '\n'
    whitespace ::= *(space) newline

    line-comment       ::= '//' *(~(newline))
    multi-line-comment ::= '/*' *(multi-line-comment | ~('*/')) '*/'

Syntax Analyzer
---------------

The *syntax analyzer* (a.k.a. *parser*) transforms a sequence of tokens into a corresponding *abstract syntax tree* (AST). A pattern that is composed of other patterns is called *non-terminal*, whereas patterns that cannot be further divided are called *terminal* (i.e. *tokens*).

Expressions
+++++++++++

*Expressions* are the terms that exist and the rules for how they may be combined to form new terms. Expressions either refer to data (literals) or to declarations.

.. code-block:: bnf

    literal-expr ::= integer | rational | string

    id-expr ::= id | meta

    primary-expr ::= id-expr | literal-expr

    tuple-open-expr       ::= '(' *(expr, ',') ')'
    tuple-open-left-expr  ::= '(' *(expr, ',') ']'
    tuple-open-right-expr ::= '[' *(expr, ',') ')'
    tuple-closed-expr     ::= '[' *(expr, ',') ']'
    sym-expr              ::= ?(id) '<' *(expr, ',') '>'
    tuple-expr            ::= tuple-open-expr
                            | tuple-open-left-expr
                            | tuple-open-right-expr
                            | tuple-closed-expr
                            | sym-expr

    basic-expr ::= tuple-expr | primary-expr

    dot-expr ::= ?('.') +(basic-expr, '.')

    range-expr ::= +(dot-expr, '..') ?('..')
                 | '..' +(dot-expr, '..') ?('..')
                 | '..'

    apply-subject ::= range-expr
    apply-args    ::= *(range-expr)
    apply-expr    ::= apply-subject apply-args

    constraint-expr ::= +(apply-expr, ':')

    assign-expr ::= +(constraint-expr, '=')

    expr ::= assign-expr

Declarations
++++++++++++

*Declarations* are user provided definitions that may be invoked from expressions.

.. code-block:: bnf

    import-decl ::= 'import' +(id, '.')

    symbol ::= id ?('<', *(expr, ','), '>')

    symbol-decl ::= symbol ':=' expr

    data-sum-decl ::= ':|' symbol
    data-sum-ctor ::= symbol ?('(' *(id ':' expr, ',') ')')
    data-sum-defn ::= ind(>) +(data-sum-ctor, ind(=)) ind(<)
    data-sum      ::= data-sum-decl ?(data-sum-defn)

    data-prod-decl  ::= ':&' symbol
    data-prod-field ::= id ':' expr ?('=' expr)
    data-prod-defn  ::= ind(>) +(scope-data-prod, ind(=)) ind(<)
    data-prod       ::= data-prod-decl ?(data-prod-defn)

    templ-decl ::= symbol
    templ-defn ::= ind(>) +(scope-decl, ind(=)) ind(<)
    templ      ::= templ-decl templ-defn

    proc-decl ::= '(' *(id ':' expr, ',') ')' ?('->' expr)
    proc-defn ::= '=>' ind(>) +(scope-proc, ind(=)) ind(<)
                | '=>' expr
    proc      ::= proc-decl ?(proc-defn)

    proc-templ ::= templ-decl proc-decl ?(proc-defn)

    attrib ::= +('@' expr, ind(=))

    scope-decl ::= ?(attrib) import-decl
                 | ?(attrib) proc-templ
                 | ?(attrib) data-prod
                 | ?(attrib) data-sum
                 | ?(attrib) symbol-decl

    block-decl ::= ':<' ?(id) '>' ?(expr)
    block-defn ::= ind(>) scope-proc ind(<)
    block      ::= block-decl block-defn

    cond-block-if     ::= ':?' expr ind(>) scope-proc ind(<)
    cond-block-elseif ::= ':/' expr ind(>) scope-proc ind(<)
    cond-block-else   ::= ':/' ind(>) scope-proc ind(<)
    cond-block        ::= cond-block-if *(cond-block-elseif) ?(cond-block-else)

    stmt ::= expr

    junc ::= 'loop' ?(id)
           | 'break' ?(id)
           | 'return' ?(expr)

    var-decl ::= ':=' id ?(':' constraint-expr) ?('=' assign-expr)

    scope-proc ::= scope-decl
                 | var-decl
                 | block
                 | cond-block
                 | stmt
                 | junction

    scope-data-prod ::= scope-decl
                      | ?(attrib) data-prod-field
                      | proc

    module ::= *(scope-decl, ind(=))

Semantics
=========

Expressions
-----------

Primary Expression
++++++++++++++++++

Primary expressions (``primary-expr``) are the smallest, indivisible expressions that are used to compose all of the other expressions. Individually, a ``primary-expr`` has one of the following meanings:

    - ``id`` is refers by name to an existing declaration.
    
    - ``meta`` declares a new sym-var as part of a symbol.
    
    - ``integer`` is a base 10 integer number used as an immediate value.
    
    - ``decimal`` is a base 10 rational number used as an immediate value.
    
    - ``string`` is a sequence of printable text that would otherwise be interpreted as part of the program source, with the exception of escape characters. They create static storage for the string in the resulting executable and are typed as slice<u8>. Strings are immutable.

Tuple Expression
++++++++++++++++

Tuple expressions (``tuple-expr``) are groups of expressions. Tuples come in the following varieties:

    - ``tuple-open-expr`` always flatten in their context.
    - ``tuple-open-left`` TODO
    - ``tuple-open-right`` TODO
    - ``tuple-closed`` always keep their structure.

Symbol Expression
+++++++++++++++++

Symbol expressions (``sym-expr``) name a particular declaration. A ``sym-expr`` must always begin with an identifier, which always maps directly to a symbol name. The remaining expressions are used as parameters for pattern matching to a specific symbol overload.

Dot Expression
++++++++++++++

Dot expressions (``dot-expr``) are comprised of a LHS and RHS expression. A leading dot (.) means that LHS is resolved in the global scope. LHS and RHS have the following meanings:

    - if LHS is a value-expr
        1. RHS is an integer literal, `n`, naming the `n`-th field-decl on LHS in order of declaration. This is the only way to name the anonymous fields of a tuple.
        2. RHS identifies a ``field-decl`` in the scope of the type of LHS.
        3. RHS identifies a ``proc-decl`` in the scope of the type of RHS.
        4. If the type of LHS contains a dot-operator, the remaining ``dot-expr`` is forwarded to it.
        5. The ``dot-expr`` is raised to an apply-expr with LHS and RHS swapped.
    - if LHS is a non-value
        - RHS is an identifier in the scope of LHS.

Range Expression
++++++++++++++++

Range expressions (``range-expr``) are comprised of a LHS and RHS expression. The LHS names a type, the RHS is an integer literal. The expression is interpreted as a compressed form of ``tuple-closed-expr`` with LHS repeated RHS times. This is also called an *array*.

Apply Expression
++++++++++++++++

Apply expressions (``apply-expr``) are comprised of the subject, ``apply-subject``, and arguments, ``apply-args``, with one of the following meanings:

    1. If there are no arguments, the ``apply-expr`` is lowered as ``apply-subject``.
    2. If subject is not an identifier, the ``apply-expr`` is lowered as 
       an ``tuple-open-expr``.
    3. If subject is an unresolved identifier, the ``apply-expr`` is lowered as a
       ``sym-expr``.
    4. If the type of the subject is not an ``arrow``
        - If the subject is a tuple, there is only one argument that is covariant with ``size_t``. The argument is an indexer into the tuple.
        - If the subject identifies a ``data-prod-decl``, a new value of the ``data-prod-decl`` type is created. The subject resolves to the associated ctor that matches the arguments.
        - If the subject identifies a ``templ-decl``, a ``proc`` matching the arguments is searched in the ``templ-decl`` scope.
    5. The type of the subject is an ``arrow`` (or resolved to an arrow from step 4).
       The arguments are type-checked against the LHS of the arrow.

Constraint Expression
+++++++++++++++++++++

Constraint expressions (``constraint-expr``) are comprised of a LHS and RHS, where RHS describes the *constraint* on LHS. A constraint is one of the following:

    - A type. Only one type is allowed per LHS.
    - A boolean expression (predicate) as a function of any parts of LHS.

Assignment Expression
+++++++++++++++++++++

Assignment expressions (``assign-expr``) are comprised of a LHS and RHS, where LHS is an l-value that takes on the value of RHS.

Expression
++++++++++

Expression (``expr``) is a placeholder for the top-level expression.

Declarations
------------

Every declaration is comprised of a *symbol* and an optional list of *attributes*.

Symbols (``symbol``) are parameterized names given to declarations. Every identifier refers to a ``symbol``. Symbols may share the same base name as long as they differ in their parameters, known as overloading. Any two equivalent symbols with differing definitions are in conflict and is an error.

Attributes (``attrib``) are meta data that precede the declaration to which they are attributed. They are typed expressions and may be queried statically.

Symbol Variable
```````````````

Symbol variables (``sym-var``) are pattern-match variables used in symbol parameters. They are deduced from ``sym-expr`` and ``apply-expr`` expression call sites and used to instantiate a new instance of their associated declaration on-the-fly as a form of ad-hoc polymorphism.

Import
++++++

Import declarations (``import-decl``) indicate an outside module whose exported symbols are to be imported into the current scope as if they were declared in that scope. Any symbol conflicts are an error.

Symbol
++++++

Symbol declarations (``sym-decl``) are static syntax indirection that rewrites to its expression in every expression where it is referred.

Procedure
+++++++++

Procedure declarations (``proc-decl``)

Parameter
`````````

Procedure parameter declaration (``proc-param-decl``)

Variable
````````

Variable declaration (``var-decl``)

Template
++++++++

Template declaration (``templ-decl``)

Data Sum
++++++++

Data sum declaration (``ds-decl``)

Data Sum Constructor
````````````````````

Data sum constructor declaration (``ds-ctor-decl``)

Data Product
++++++++++++

Data product declaration (``dp-decl``)

Field
`````

Field declaration (``field-decl``)

Scopes
------

Declaration scope (``scope-decl``)
Procedure scope (``scope-proc``)
Data sum scope (``scope-ds``)
Data product scope (``scope-dp``)
Template scope (``scope-templ``)

Types
-----

Identifier

Tuple

Arrow

Universe

Glossary
========

Abbreviations
-------------

============  =======
Abbreviation  Meaning
============  =======
tok           token
expr          expression
decl          declaration
defn          definition
sym           symbol
var           variable
param         parameter
ds            data sum
dp            data product
templ         template
subst         substitution
proc          procedure
stmt          statement
junc          junction
attrib        attribute
mod           module
id            identifier
============  =======

Definitions
-----------

flatten
    To flatten an expression means to merge it into its parent expression, losing its individuality.

lower
    Lowering an expression means rewriting it as one or more other expressions. The rewritten expression is considered as a more primitive expression, or "lower level" expression.

raise
    Raising an expression is the same as lowering, except that the resulting expression is considered as higher level.

data
    Information represented as a set of bits and their particular state.

data layout
    Specification of amount of bits and their order required to represent a type. Layouts have a size and an alignment.

value
    An expression that cannot be further reduced. Furthermore, it usually means a term whose type is in universe 1 unless otherwise stated. Values are computable; they are data with a particular data layout as described by their types.

type
    An expression that describes a domain, a set of possible values. Computable types have an associated data layout.

value category
    Hierarchical category used to describe the origins of a value. A value is either an l-value or an r-value. Furthermore, an l-value is one of either i-value or x-value, and an r-value is one of either pr-value or x-value.

l-value
    Read as "left-value" or "left-hand-side value," an l-value is one of the value categories i-value or x-value. An l-value source may both receive a new value and produce a value. It is a suitable destination for an assignment, i.e. left-hand-side of an assign-expr.

r-value
    Read as "right-value" or "right-hand-side value," an r-value is one of the value categories pr-value or x-value. An r-value source lacks any existing bindings, making it available for move without conflict.

i-value
    Read as "identity-value," an i-value is a value that is sourced from a binder declaration that has a name. Variable declarations, a binder, is the only declaration that implicitly converts to a reference.

x-value
    Read as "cross-value," a x-value is categorized as both an l-value and an r-value. x-values are unnamed temporary variables associated with statements.

pr-value
    Read as "pure-r-value" or "pure right-hand-side value," a pr-value is a literal value than can never be assigned a new value.
