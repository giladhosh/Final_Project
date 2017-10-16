# Scheme2Scheme_Compiler
## Intro
This is the final version of a compiler made during a compilation course at Ben Gurion University [Comp171].
The compiler takes a Sexpresion in scheme syntax and semantically analyzes it. 

### Work flow
The basic workflow is very simple:
Scanner -> Parser -> Semantic Analyzer -> Code Generation

• Scanner (lexical analysis) - converts a sequence of characters into a sequence of tokens.
• Parser (syntactic analysis) - checks for correct syntax and builds a hierarchical structure (parse tree) implicit in the input tokens.
• Semantic Analyzer - compiler adds semantic information to the parse tree and builds the symbol table. This phase performs semantic checks such as type checking, object binding etc.
• Code Generator - process of converting some intermediate representation of the source code into another computer language.

In practice:
Lexical analysis -> Syntax analysis | Parsing -> AST representation -> Intermediate Representation(IR) -> Code Generator


## Compiler Optimizations:
There are many optimizations available for compilers these days, some techniques learned: 

    Global:
    *   Common Subexpression Elimination (CSE)
    *   Copy Propagation (CP)
    *   Dead Code Elimination

    Tools used:
    *   Available Expressions analysis
    *   Liveness Analysis

Implemented Optimizations in project:
    
    *   Constants table
    *   Removal of write-after-write instructions
    *   Identify & Support use of tail recurssion
    
### Tech

* [Chez Scheme] - functional programing language.
* [CISC] - a version of assembly. (link to architecture zip)

### Installation
Requires [Chez Scheme] to run.
Windows users can use Scheme Widget Library ([SWL]).

Run with compile-scheme-file method in compiler.scm file (use external file as input).

License
----
Free

[//]: # (These are reference links used in the body of this note)

[Comp171]: <https://www.cs.bgu.ac.il/~comp171>
[Chez Scheme]: <https://cisco.github.io/ChezScheme/>
[SWL]: <https://www.scheme.com/download/index.html#sec:swl>
[CISC]: <https://www.cs.bgu.ac.il/~comp171/wiki.files/arch.zip>
