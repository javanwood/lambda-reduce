module Lambda exposing (Lambda(..), apply, byName, byValue, headNF, init, instantiate)

import Dict exposing (Dict)



{-
   This module contains code adapted from "Implementing a lambda calculus", Chapter 9 of "ML for
   the Working Programmer" by Larry C. Paulson (Cambridge University Press)
   https://www.cl.cam.ac.uk/~lp15/MLbook/PDF/chapter9.pdf

   This code is adapted from:
    Figure 9.5, "The name-free representation of λ-terms" (p. 386)
    Figure 9.8, "Reduction of λ-terms" (p. 397)

   The module Lambda.StdEnv also draws from:
    Figure 9.9, "Constructing the standard environment" (p. 400)

    The above samples are licensed according to the license in sample9.sml.
-}


type Lambda
    = Free String
    | Bound Int
    | Abst String Lambda
    | Apply Lambda Lambda


{-| Create a lambda with one or more bindings (x, xs) and a body
-}
init : String -> List String -> Lambda -> Lambda
init x xs body =
    abstractList (x :: xs) body


{-| Apply arguments to a lambda
-}
apply : Lambda -> List Lambda -> Lambda
apply t0 us =
    List.foldl (\u t -> Apply t u) t0 us


{-| abstract substitutes free variables for indices pointing at the captured variable. This
helps resolve ambiguity in naming by referring to the nearby values.

For example in λ x. x (identity), the bound variable 'x' becomes Abst "x" (Bound 0).

Applying abstract to (1) λ x y. x (lambda-encoded true) and (2) λ x y. y (lambda-encoded false)
results in `Abst "x" (Abst "y" (Bound 1))` and `Abst "x" (Abst "y" (Bound 0))` respectively.
For (1), `Bound 1` refers to the next closure away, while in (2) `Bound 0` refers to the closure
immediatly nearby.

For top level, use i = 0. On recursion this will become i=1 etc to account for the level of
nesting.

-}
abstract : Int -> String -> Lambda -> Lambda
abstract i b l =
    case l of
        Free a ->
            if a == b then
                Bound i

            else
                Free a

        Bound j ->
            Bound j

        Abst a t ->
            Abst a (abstract (i + 1) b t)

        Apply t u ->
            Apply (abstract i b t) (abstract i b u)


{-| Helper for abstracting multiple values in a signature. Used in `init`.
-}
abstractList : List String -> Lambda -> Lambda
abstractList bs t =
    List.foldr (\b u -> Abst b (abstract 0 b u)) t bs


{-| subst subtitutes a lambda term for the bound index i in the lambda l. It is useful in
β-conversion of (λx.l)u, where l may refer to u internally. This is the principle way lambda
terms may be simplified and it is used in byValue / eval and byName / headNF.

subst is almost always called with i=0. During recursion it is i+1, ie for
`subst 0 <lambda> (Abst "x" (Abst "y" (Bound 1)))`, <lambda> will replace reference Bound 1.

Replacement is actually accomplished by shift.

-}
subst : Int -> Lambda -> Lambda -> Lambda
subst i u l =
    case l of
        Free a ->
            Free a

        Bound j ->
            if j < i then
                -- Locally bound
                Bound j

            else if j == i then
                shift i 0 u

            else
                -- j > i
                -- Non-local to l, account for removal.
                Bound (j - 1)

        Abst a t ->
            Abst a (subst (i + 1) u t)

        Apply t1 t2 ->
            Apply (subst i u t1) (subst i u t2)


{-| shift is a private method that replaces bound indexes in subst. It handles the logic for
ensuring that the term being inserted aligns with the index structure it is being inserted into.

Here i is the level being inserted into, and d is the relative index. shift is initially called
with d = 0. During recursion, it keep track of the depth.

-}
shift : Int -> Int -> Lambda -> Lambda
shift i d u =
    case ( i, u ) of
        ( 0, _ ) ->
            -- Top level call from subst so directly substitute.
            u

        ( _, Free a ) ->
            -- No bound indices to substitute.
            Free a

        ( _, Bound j ) ->
            -- Not 100% sure what's going on for the if statement, but this will ignore
            -- bound variables for adjacents when recursing (d = 1, j = 0).
            if j >= d then
                Bound (j + i)

            else
                Bound j

        ( _, Abst a t ) ->
            -- Increment d and shift the next level.
            Abst a (shift i (d + 1) t)

        ( _, Apply t u_ ) ->
            -- Increment d and shift the next level.
            Apply (shift i d t) (shift i d u_)


{-| Instantiate a lambda with the definitions in the environment. This only replaces free
variables. If the definition found itself has a definition, the instantiation process continues
until there is no matching definition, that is the lambda will include only free variables not
defined in env.
-}
instantiate : Dict String Lambda -> Lambda -> Lambda
instantiate env l =
    case l of
        Free a ->
            case Maybe.map (instantiate env) (Dict.get a env) of
                Just a_ ->
                    a_

                Nothing ->
                    Free a

        Bound i ->
            Bound i

        Abst a t ->
            Abst a (instantiate env t)

        Apply t1 t2 ->
            Apply (instantiate env t1) (instantiate env t2)


{-| eval evaluates a term, and therefore only effects Apply values (Or Abst values containing
Apply values).

For `Apply t1 t2` it first evaluates t1, then evalues t2. This is the call-by-value pattern
which causes issues in some cases (infinite lists for example).

For Abst values, it will not evaluate the body, instead substituting t2 into the body. This allows
recursion.

Because of this, eval will not always produce results in normal form.

-}
eval : Lambda -> Lambda
eval t =
    case t of
        Apply t1 t2 ->
            let
                u1 =
                    eval t1
            in
            case u1 of
                Abst _ u ->
                    eval (subst 0 (eval t2) u)

                _ ->
                    Apply u1 (eval t2)

        _ ->
            t


{-| `byValue` reduces a term to normal form using `eval`, performing an additional fixup step to
also normalise abstractions.
-}
byValue : Lambda -> Lambda
byValue t =
    bodies (eval t)


bodies : Lambda -> Lambda
bodies t =
    case t of
        Abst a t_ ->
            Abst a (byValue t_)

        Apply t1 t2 ->
            Apply (bodies t1) (bodies t2)

        _ ->
            t


{-| headNF reduces the lambda term, t, by computing head normal form. For `t1 t2`, headNF
computes headNF for t1 then performs a β-conversion (subst) if necessary. This is call-by-name
since t2 is not reduced before substitution.
-}
headNF : Lambda -> Lambda
headNF t =
    case t of
        Abst a t_ ->
            Abst a (headNF t_)

        Apply t1 t2 ->
            let
                u1 =
                    headNF t1
            in
            case u1 of
                Abst _ t_ ->
                    headNF (subst 0 t2 t_)

                _ ->
                    Apply u1 t2

        _ ->
            t


{-| byName performances a final step on top of headNF by also reducing t2 in `t1 t2`, which is not
reduced in headNF.
-}
byName : Lambda -> Lambda
byName t =
    args (headNF t)


args : Lambda -> Lambda
args t =
    case t of
        Abst a t_ ->
            Abst a (args t_)

        Apply t1 t2 ->
            Apply (args t1) (byName t2)

        _ ->
            t
