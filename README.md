CONTROLS:

- q          -- quit.
- v          -- show the velocity vector field.
- s          -- start/stop simulation.
- d          -- show divergence of velocity field.(red means positive, green negative)
- left click -- drop something into the fluid.

Something seems still to be off: the velocities don't interact intuitively with each other.
The goal is, that I learn more ocaml.
The program is supposed to implement a fast and unprecise Navier-Stokes solver from the paper

http://www.dgp.toronto.edu/people/stam/reality/Research/pdf/GDC03.pdf

So far, only a dynamic density field and a constant velocity field are implemented.
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