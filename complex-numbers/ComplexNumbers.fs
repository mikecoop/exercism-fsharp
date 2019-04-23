module ComplexNumbers

type ImaginaryNumber = { Real: double; Imaginary: double }

let create real imaginary = { Real = real; Imaginary = imaginary }

let mul z1 z2 = { Real = (z1.Real * z2.Real - z1.Imaginary * z2.Imaginary); Imaginary = (z1.Imaginary * z2.Real + z1.Real * z2.Imaginary) }

let add z1 z2 = { Real = z1.Real + z2.Real; Imaginary = z1.Imaginary + z2.Imaginary }

let sub z1 z2 = { Real = z1.Real - z2.Real; Imaginary = z1.Imaginary - z2.Imaginary }

let div z1 z2 = { Real = (z1.Real * z2.Real + z1.Imaginary * z2.Imaginary) / (pown z2.Real 2 + pown z2.Imaginary 2); Imaginary = (z1.Imaginary * z2.Real - z1.Real * z2.Imaginary) / (pown z2.Real 2 + pown z2.Imaginary 2) }

let abs z = sqrt (pown z.Real 2 + pown z.Imaginary 2)

let conjugate z = { Real = z.Real; Imaginary = -z.Imaginary }

let real z = z.Real

let imaginary z = z.Imaginary

let exp z = { Real = exp z.Real * cos z.Imaginary; Imaginary = sin z.Imaginary }