# fxtran-acdc API — basic understanding

Notes on how fxtran-acdc transforms FORTRAN source code: parsing FORTRAN into
XML documents, querying the XML syntax tree with the `F` function (XPath-like
patterns), and modifying the tree with the `XML::LibXML` DOM API to produce
transformed FORTRAN source code.

Verified hands-on with `demo.pl` run against `actke.F90` (parse -> query ->
rename a variable + insert a statement -> serialize back to FORTRAN).

## 1. Parsing FORTRAN into XML documents

- An external **`fxtran` binary** (wrapped by the XS module `fxtran.pm`, found in
  `~/perl5/lib/perl5/x86_64-linux-thread-multi/`) parses FORTRAN and emits XML in
  the namespace `http://fxtran.net/#syntax`. The singleton
  `fxtran::parser::PARSER` invokes it and loads the result with
  `XML::LibXML->load_xml`, returning an `XML::LibXML::Document`.
- Entry points (via `fxtran::parser::parse`):
  - `parse(location => $file, fopts => [...], dir => $tmp)` — parse a file
  - `parse(string => ...)` — parse a string (through a temp file)
  - `parse(fragment => ...)` — parse statements wrapped in a dummy program;
    returns a list of unbound nodes (useful to build code fragments)
  - `parse(statement => ...)` / `parse(expr => ...)` — single statement /
    expression; wrapped by the `s()` and `e()` helpers (see below)
  - `parse(program => ...)` — full program, returns top-level nodes
- Typical `fopts`: `-construct-tag -no-include -no-cpp -line-length 5000
  -canonic` (see e.g. `Fxtran::Generate::routineToRoutineTail` pipeline).
- XML grammar conventions (observed in `actke.F90.xml`):
  - `object/file/program-unit` wraps everything.
  - Statements are `*-stmt` elements: `subroutine-stmt`, `end-subroutine-stmt`,
    `a-stmt` (assignment), `do-stmt`, `if-then-stmt`, `T-decl-stmt`
    (type declaration), `use-stmt`, `call-stmt`, ...
  - Constructs: `do-construct`, `if-construct`/`if-block`, `program-unit`.
  - Expressions end in `-E`: `named-E` (variable or function reference),
    `literal-E` (with `<l>` value and optional `<K-spec>` kind), `op-E`
    (`<op><o>+</o></op>` between operands), `string-E` (`<S>`).
  - An assignment `a-stmt` is `<E-1>` (lhs) + `<a>=</a>` + `<E-2>` (rhs).
  - Names are `<N><n>NAME</n></N>` wrapped in role tags: `subroutine-N`,
    `arg-N`, `EN-N` (entity name in declarations), `module-N`, `use-N`, ...
  - Lists end in `-LT`: `dummy-arg-LT`, `EN-decl-LT`, `R-LT` (reference list:
    array subscripts `array-R`, call arguments `parens-R`), `element-LT`,
    `section-subscript-LT`, `shape-spec-LT`, `rename-LT`.
  - Attributes/intents: `attribute`/`attribute-N`, `intent-spec`, `K-selector`.
  - **Every source character (spaces, newlines, punctuation) is preserved in
    text nodes**, so `textContent` of any node reproduces its FORTRAN source
    exactly. This is what makes the round trip XML -> FORTRAN trivial.

## 2. The `F` function (XPath-like queries)

Defined in `fxtran::xpath`, re-exported by `Fxtran` (`lib/Fxtran.pm`).

- `F($xpath, $node [, $flag])` preprocesses the XPath string (result cached in
  `%P`) then calls `f()`, which evaluates it with
  `XML::LibXML::XPathContext->findnodes` (namespace `f` registered as
  `http://fxtran.net/#syntax`).
- Preprocessing rules:
  - Bare element names are auto-prefixed with `f:` so queries can be written
    without namespace clutter: `'.//call-stmt/procedure-designator'`.
  - **`@NAME`** is rewritten to `f:N/f:n/text()` — shortcut to fetch the name
    of a node.
  - **`ANY-XXX`** wildcard matches any element whose tag ends in `-XXX`
    (e.g. `ANY-E` = any expression, `ANY-stmt` = any statement), implemented as
    `f:*[substring(name(),string-length(name())-N)="-XXX"]`.
- `f()` also supports `?` placeholders, substituted with extra scalar arguments
  (poor man's parameter binding):
  `&F('./call-stmt[string(procedure-designator)="?"]', $proc, $d)`.
- Optional `$flag` argument:
  - `1` — return `textContent`, whitespace-stripped and uppercased (canonical
    name comparison).
  - `2` — return raw `textContent`.
  - absent — return nodes.

Typical query examples (from `lib/Fxtran/*.pm`):

```perl
my @pu    = &F ('.//program-unit', $d);
my @call  = &F ('.//call-stmt/procedure-designator', $section);
my ($ep)  = &F ('./execution-part', $pu);
my @args  = &F ('./dummy-arg-LT/arg-N', $stmt, 1);            # uppercase names
my @decl  = &F ('.//T-decl-stmt[.//EN-decl[string(EN-N)="?"]]', $name, $unit);
my @drhk  = &F ('.//call-stmt[string(procedure-designator)="DR_HOOK"]', $d);
```

## 3. Modifying the XML DOM (XML::LibXML) -> transformed FORTRAN

- Everything is a plain `XML::LibXML::Node`; the usual DOM methods are used:
  `unbindNode`, `insertBefore` / `insertAfter`, `appendChild`, `replaceChild`,
  `cloneNode (1)`, `setData`, `setNodeName`, `textContent`, `parentNode`,
  `nextSibling` / `previousSibling`, ...
- Fragment constructors exported by `Fxtran`:
  - **`s("FORTRAN statement")`** — parse a statement string into an XML fragment
  - **`e("expr")`** — parse an expression string into a fragment
  - **`n('<xml/>')`** — parse raw fxtran XML (wrapped in the fxtran namespace)
  - **`t("text")`** — create a text node (used for `"\n"` separators between
    statements)
  - **`TRUE` / `FALSE`** — `.TRUE.` / `.FALSE.` `literal-E` nodes
- Helpers in `Fxtran.pm`:
  - `Fxtran::stmt($e)` — nearest ancestor `*-stmt` of a node
  - `Fxtran::expr($e)` — nearest ancestor `*-E` of a node
  - `Fxtran::removeListElement($x)` — remove a list element, taking care of the
    comma before/after it
  - `Fxtran::expand($stmt)` — unwrap `cnt`/`C` continuation markers, collapse
    multi-line text
  - `Fxtran::stmt_is_executable($stmt)`
- Round trip back to FORTRAN:
  - `Fxtran::Canonic::indent($d)` — re-indent the document and emit FORTRAN
    source (splits long statements, indents constructs).
  - `Fxtran::Util::updateFile($file, $code, ...)` — write the file only if the
    content changed, with optional version/time/from metadata comments.

Idiomatic transformation (from `Fxtran::Call::addSuffix`):

```perl
# find nodes
for my $proc (&F ('.//call-stmt/procedure-designator', $section))
  {
    my ($name) = &F ('./named-E/N/n/text()', $proc);
    $name->setData ($name->textContent . $suffix);   # edit in place
  }

# create + insert new code
my $use1 = &s ("USE $mod$suffix, ONLY : $proc$suffix");
$up->insertAfter ($_, $use) for ($use1, &t ("\n"));

# clone + modify
my $include1 = $include->cloneNode (1);
my ($t) = &F ('./filename/text()', $include1);
$t->setData ($newfile);
$include->parentNode->insertBefore ($include1, $include);
```

## 4. The big picture

All higher-level modules in `lib/Fxtran/*.pm` (`Call`, `Subroutine`, `Decl`,
`Loop`, `Inline`, `Canonic`, `SingleBlock`, `ManyBlocks`, `Pointer::Parallel`,
`FieldAPI`, `IO`, ...) are built from these primitives: they `use Fxtran` and
combine `F` queries with DOM edits. The driver `Fxtran::Generate` (invoked via
the `fxtran-gen` / `fxtran-f90` frontends) parses a file, applies a
transformation method (selected by ACDC `!$ACDC` directives or command-line
options), then serializes the result with `Fxtran::Canonic::indent` and
`Fxtran::Util::updateFile`.

## Files referenced

- `lib/Fxtran.pm` — exports `s e F f n t TRUE FALSE`, tree helpers
- `~/perl5/.../fxtran.pm` — XS binding to the parser; `parse`, `s`, `e`, `n`, `t`
- `~/perl5/.../fxtran/xpath.pm` — `F`/`f` XPath query functions
- `~/perl5/.../fxtran/parser.pm` — parser driver (`fxtran::parser::PARSER`)
- `lib/Fxtran/Generate.pm` — main transformation driver
- `lib/Fxtran/Canonic.pm` — `makeCanonic`, `indent` (serialization)
- `lib/Fxtran/Util.pm` — `updateFile` (write-back)
- `attic/demo.pl` — working end-to-end demo of the API
