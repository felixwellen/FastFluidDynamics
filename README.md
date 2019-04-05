The main purpose of this project is to learn ocaml.
It is supposed to implement a fast and unprecise Navier-Stokes solver from the paper

http://www.dgp.toronto.edu/people/stam/reality/Research/pdf/GDC03.pdf

So far, only diffusion is implemented and there is a bug to be found...
I learned how to use opengl through lablgl from the examples here:

https://github.com/youjinbou/lablgl/tree/master/LablGlut/examples/lablGL

I am working with fedora 29. In addition to installing basic ocaml stuff, I did:

dnf install ocaml-camlp4 ocaml-camlp4-devel freeglut-devel freeglut ocaml-ocamlbuild ocaml-ocamlbuild-devel ocaml-num-devel ocaml-num

Here is how I made an opam switch that works:

opam switch create opengl ocaml-system.4.07.0

(You can find out what the last argument should be with 'opam switch list')
and then

opam install lablgl
opam install core
