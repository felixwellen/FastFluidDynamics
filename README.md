The main purpose of this project is to learn ocaml.
It is supposed to implement a fast and unprecise Navier-Stokes solver from the paper

http://www.dgp.toronto.edu/people/stam/reality/Research/pdf/GDC03.pdf

So far, only diffusion is implemented and there is a bug to be found...
I learned how to use opengl through lablgl from the examples here:

https://github.com/youjinbou/lablgl/tree/master/LablGlut/examples/lablGL

Make it run - run make!
Maybe that helps:
felix@felixu ~/code/openglcaml $ opam list
# Packages matching: installed
# Name                  # Installed # Synopsis
base                    v0.11.0     Full standard library replacement for OCaml
base-bigarray           base        
base-threads            base        
base-unix               base        
bin_prot                v0.11.0     A binary protocol generator
biniou                  1.2.0       Binary data format designed for speed, safety, ease of use and backward compatibility as protocols evolve
camlp4                  4.06+system Camlp4 is a system for writing extensible parsers for programming languages
conf-m4                 1           Virtual package relying on m4
conf-which              1           Virtual package relying on which
configurator            v0.11.0     Helper library for gathering system configuration
core                    v0.11.3     Industrial strength alternative to OCaml's standard library
core_kernel             v0.11.1     Industrial strength alternative to OCaml's standard library
cppo                    1.6.5       Equivalent of the C preprocessor for OCaml programs
dune                    1.6.0       Fast, portable and opinionated build system
easy-format             1.3.1       High-level and functional interface to the Format module of the OCaml standard library
fieldslib               v0.11.0     Syntax extension to define first class values representing record fields, to get and set record fields, iterate and fold over all fields of a record and create new record valu
jane-street-headers     v0.11.0     Jane Street C header files
jbuilder                transition  This is a transition package, jbuilder is now named dune. Use the dune
lablgl                  1.05        Interface to OpenGL
merlin                  3.2.2       Installation with Opam
num                     1.1         The legacy Num library for arbitrary-precision integer and rational arithmetic
ocaml                   4.06.0      The OCaml compiler (virtual package)
ocaml-compiler-libs     v0.11.0     OCaml compiler libraries repackaged
ocaml-config            1           OCaml Switch Configuration
ocaml-migrate-parsetree 1.2.0       Convert OCaml parsetrees between different versions
ocaml-system            4.06.0      The OCaml compiler (system version, from outside of opam)
ocamlbuild              0.14.0      OCamlbuild is a build system with builtin rules to easily build most OCaml projects.
ocamlfind               1.8.0       A library manager for OCaml
octavius                0.1.0       odoc comment syntax parser
parsexp                 v0.11.0     S-expression parsing library
ppx_assert              v0.11.0     Assert-like extension nodes that raise useful errors on failure
ppx_base                v0.11.0     Base set of ppx rewriters
ppx_bench               v0.11.0     Syntax extension for writing in-line benchmarks in ocaml code
ppx_bin_prot            v0.11.1     Generation of bin_prot readers and writers from types
ppx_compare             v0.11.1     Generation of comparison functions from types
ppx_custom_printf       v0.11.0     Printf-style format-strings for user-defined string conversion
ppx_derivers            1.0         Shared [@@deriving] plugin registry
ppx_enumerate           v0.11.1     Generate a list containing all values of a finite type
ppx_expect              v0.11.1     Cram like framework for OCaml
ppx_fail                v0.11.0     Add location to calls to failwiths
ppx_fields_conv         v0.11.0     Generation of accessor and iteration functions for ocaml records
ppx_hash                v0.11.1     A ppx rewriter that generates hash functions from type expressions and definitions
ppx_here                v0.11.0     Expands [%here] into its location
ppx_inline_test         v0.11.0     Syntax extension for writing in-line tests in ocaml code
ppx_jane                v0.11.0     Standard Jane Street ppx rewriters
ppx_js_style            v0.11.0     Code style checker for Jane Street Packages
ppx_let                 v0.11.0     Monadic let-bindings
ppx_optcomp             v0.11.0     Optional compilation for OCaml
ppx_optional            v0.11.0     Pattern matching on flat options
ppx_pipebang            v0.11.0     A ppx rewriter that inlines reverse application operators `|>` and `|!`
ppx_sexp_conv           v0.11.2     Generation of S-expression conversion functions from type definitions
ppx_sexp_message        v0.11.0     A ppx rewriter for easy construction of s-expressions
ppx_sexp_value          v0.11.0     A ppx rewriter that simplifies building s-expressions from ocaml values
ppx_typerep_conv        v0.11.1     Generation of runtime types from type declarations
ppx_variants_conv       v0.11.1     Generation of accessor and iteration functions for ocaml variant types
ppxlib                  0.5.0       Base library and tools for ppx rewriters
re                      1.8.0       RE is a regular expression library for OCaml
result                  1.0         Compatibility Result module
seq                     0.1         Compatibility package for OCaml's standard iterator type starting from 4.07.
sexplib                 v0.11.0     Library for serializing OCaml values to and from S-expressions
sexplib0                v0.11.0     Library containing the definition of S-expressions and some base converters
spawn                   v0.12.0     Spawning sub-processes
splittable_random       v0.11.0     PRNG that can be split into independent streams
stdio                   v0.11.0     Standard IO library for OCaml
topkg                   1.0.0       The transitory OCaml software packager
typerep                 v0.11.0     typerep is a library for runtime types.
variantslib             v0.11.0     Part of Jane Street's Core library
yojson                  1.6.0       Yojson is an optimized parsing and printing library for the JSON format
