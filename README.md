CONTROLS:

- q          -- quit.
- v          -- show the velocity vector field.
- s          -- start/stop simulation.
- d          -- show divergence of velocity field.(red means positive, green negative)
- left click -- drop something into the fluid.

The goal of this project was that I learn how to use ocaml.
The program is supposed to implement a fast and unprecise 2d-Navier-Stokes solver from the paper

http://www.dgp.toronto.edu/people/stam/reality/Research/pdf/GDC03.pdf

I learned how to use opengl through lablgl from the examples here:

https://github.com/youjinbou/lablgl/tree/master/LablGlut/examples/lablGL

and got some hints from (the outdated examples) here

http://www.linux-nantes.org/~fmonnier/OCaml/GL/ocaml-opengl-howto.en.html

I am working with fedora 29. In addition to installing basic ocaml stuff, I did:

dnf install ocaml-camlp4 ocaml-camlp4-devel freeglut-devel freeglut ocaml-ocamlbuild ocaml-ocamlbuild-devel ocaml-num-devel ocaml-num

Here is how I made an opam switch that works:

opam switch create opengl ocaml-system.4.07.0

(You can find out what the last argument should be with 'opam switch list')
and then

opam install lablgl
opam install core

then

make

should build a 'main' executable.