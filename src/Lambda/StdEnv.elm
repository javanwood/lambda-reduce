module Lambda.StdEnv exposing (..)

import Lambda exposing (Lambda)



-- Subset from Figure 9.9
-- Writing Interpreters for the λ-Calculus
-- https://www.cl.cam.ac.uk/~lp15/MLbook/PDF/chapter9.pdf


true : Lambda
true =
    Lambda.init "x" [ "y" ] (Lambda.Free "x")


false : Lambda
false =
    Lambda.init "x" [ "y" ] (Lambda.Free "y")


if_ : Lambda
if_ =
    Lambda.init "p" [ "x", "y" ] (Lambda.apply (Lambda.Free "p") [ Lambda.Free "x", Lambda.Free "y" ])
