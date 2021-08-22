#!/usr/bin/env boros

let n' = {- 1 -} in
let n = read args.[0] - {- 0 -} || (print n'; halt ()) in

comments.[0] <- read comments.[0] * n;
comments.[1] <- read comments.[1] + 1;
