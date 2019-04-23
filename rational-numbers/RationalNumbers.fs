﻿module RationalNumbers

open System

type RationalNumber = { Numerator: int; Denominator: int }

let negate n = { n with Numerator = -n.Numerator }

let fixSign n =
    if n.Denominator < 0 then
        { Numerator = -n.Numerator; Denominator = -n.Denominator }
    else
        n

let invert n =
    { Numerator = n.Denominator; Denominator = n.Numerator } |> fixSign

let create numerator denominator = { Numerator = numerator; Denominator = denominator }

let add r1 r2 =
    let left, right = r1.Numerator * r2.Denominator, r2.Numerator * r1.Denominator
    if left = -right then
        { Numerator = 0; Denominator = 1 }
    else
        { Numerator = left + right; Denominator = r1.Denominator * r2.Denominator }

let sub r1 r2 = add r1 (negate r2)

let reduce r =
    if r.Numerator % r.Denominator = 0 then
        { Numerator = r.Numerator / r.Denominator
          Denominator = 1 }
    elif r.Denominator % r.Numerator = 0 then
        { Numerator = 1
          Denominator = r.Denominator / r.Numerator }
    else r
    |> fixSign

let mul r1 r2 =
    { Numerator = r1.Numerator * r2.Numerator
      Denominator = r1.Denominator * r2.Denominator }
    |> reduce

let div r1 r2 = mul r1 (invert r2)

let abs r = { Numerator = abs r.Numerator; Denominator = r.Denominator }

let exprational n r = { Numerator = pown r.Numerator n; Denominator = pown r.Denominator n }

let expreal r n = Math.Pow (float n, (float r.Numerator) / (float r.Denominator))