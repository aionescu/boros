#!/usr/bin/env boros

{- "Computes the factorial of the number specified as a command-line argument."; -}

let n' = {- 1 -} in
let n = {- read args.[0] -} || (print n'; halt ()) in

comments.[1] <- n' * n;
comments.[2] <- n - 1;
