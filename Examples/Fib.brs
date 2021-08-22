#!/usr/bin/env boros

let n = {- 1 -} in
let a = {- 1 -} in
let b = {- 2 -} in

if n == 100 then
  halt ();

comments.[0] <- show (n + 1);
comments.[1] <- comments.[2];
comments.[2] <- show (a + b);

a
