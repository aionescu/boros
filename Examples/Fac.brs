let fac x =
  if x == 0
  then 1
  else x * fac (x - 1)
in

let facF f x =
  if x == 0
  then 1
  else x * f (x - 1)
in

let fac' x = fix facF x in

print $ fac 1000;
print $ fac' 1000;
let a = "abc" in 2
