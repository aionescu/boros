#!/usr/bin/env boros

let n = {- 0 -} || read args.[0] in
let n' = {- 1 -} in

if n == 1 then (
  print n';
  halt ();
);

comments.[1] <- show $ read comments.[1] * n;
comments.[0] <- show $ n - 1;
