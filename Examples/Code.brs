let f x = x + 1 in
print { a = 2, b = "abc", c = a.b, d = a.[2].b.[2].c.d.e };
a <- 2;
b.c <- "abc";
let x = 2 in
a.[0] <- a.[0] + 1;
print a
