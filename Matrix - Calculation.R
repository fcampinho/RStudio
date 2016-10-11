a = matrix(c(1,1,1,1,2,2,2,2),2,4)
a

b = 1:4
b

a %*% b
b * a

apply(a*b,1,sum)

c = matrix(c(1,3,2,4,0,1), 2, 3, byrow=TRUE)
c

d = matrix(c(1,0,5,3,1,2), 3, 2)
d
c * d
c %*% d


e = matrix(c(3,4,2,16),2,2, byrow = TRUE)
e

g <- solve(e)
g

f <- solve(e)%*%e
f
