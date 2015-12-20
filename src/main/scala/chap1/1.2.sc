import scala.annotation.tailrec

// 练习1.9  第二个是尾递归


// 练习1.10
def a(x: Int, y: Int): Int = {
  if (y == 0) 0
  else if (x == 0) 2 * y
  else if (y == 1) 2
  else a(x - 1, a(x, y - 1))
}

a(1, 10)
for (aa <- 1 to 4) println(a(2, aa))
a(3, 3)
// f(n) = 2 *n
// g(n) = 2 * g(n-1); g(1) = 2; => g(n) = 2^n
// h(n) = 2^h(n-1) 对于n>=1 成立
// 练习1.11
def f(n: Int): Int = {
  @tailrec
  def f_helper(n: Int, f_2: Int, f_1: Int, f_0: Int): Int = {
    if (n == 2) f_2
    else if (n == 1) f_1
    else if (n == 0) f_0
    else
      f_helper(n - 1, f_2 + 2 * f_1 + 3 * f_0, f_2, f_1)
  }

  f_helper(n, 2, 1, 0)
}

f(1)
f(2)
f(3)

// 练习12
def pascal(x: Int, y: Int): Int = {
  if (y < x) 0
  else if (x < 1) 0
  else if (x == 1) 1
  else pascal(x - 1, y - 1) + pascal(x, y - 1)
}
for (i <- 1 to 4; j <- 1 to i) {
  println(pascal(j, i))
}

// 练习1.16
// 不变式: b^b * a
def fastExpt(b: Double, n: Int): Double = {
  @tailrec
  def fastExptIter(b: Double, n: Int, a: Double): Double = {
    if (n == 0) a
    else if (n % 2 == 1) fastExptIter(b: Double, n - 1, b * a)
    else fastExptIter(b * b, n / 2, a)
  }
  fastExptIter(b, n, 1)
}
fastExpt(2, 11)

// 练习1.17, 18
// 不变式: x*y + a
def multLog(x: Double, y: Double): Double = {
  @tailrec
  def multLogIter(x: Double, y: Double, a: Double): Double = {
    if (x == 1) y + a
    else if (x % 2 == 1) multLogIter(x - 1, y, a + y)
    else multLogIter(x / 2, y * 2, a)
  }

  multLogIter(x, y, 0)
}

multLog(10, 13)
multLog(1, 0)


// 练习1.19, 根据矩阵概念可以推出来

def fib(n: Int): Int = {
  @tailrec
  def fib_iter(a: Int, b: Int, p: Int, q: Int, count: Int): Int = {
    if (count == 0) b
    else if (count % 2 == 0) fib_iter(a, b, q * q + p * p, q*q + 2*p*q, count/2)
    else fib_iter(b*q+a*q+a*p,b*p + a*q, p, q, count-1)
  }
  fib_iter(1,0, 0, 1, n)
}

for(i <- 1 to 8) println(fib(i))

