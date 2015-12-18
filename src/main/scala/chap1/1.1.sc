import scala.annotation.tailrec

def square(x: Int): Int = x * x

square(21)
square(2 + 5)
square(square(3))

def sum_of_square(x: Int, y: Int): Int = square(x) + square(y)

def sum_of_square_curried(x: Int)(y: Int): Int = square(x) + square(y)

sum_of_square(3, 4)

sum_of_square_curried(3)(4)

def abs(x: Int): Int = if (x < 0) -x else x

// 练习1.1
10 // 10
5 + 3 + 4 // 12
9 - 1 // 8
6 / 2 // 3
(4 * 2) + (4 - 6) //6
val a = 3 // a =3
val b = a + 1 // b = 4
a + b + a * b //19
if (b > a && b < (a * b)) b else a // 4
/**
  * 6+7+3 = 16
  */
if (a == 4)
  6
else if (b == 4)
  6 + 7 + a
else 25

// 练习1.3
def sumMax2of3(x: Int)(y: Int)(z: Int): Int = {
  if (x <= y && x <= z)
    y + z
  else if (y <= x && y <= z)
    x + z
  else
    x + y
}

assert(sumMax2of3(2)(2)(2) == 4)
assert(sumMax2of3(2)(3)(1) == 5)

// 练习1.4 f(a,b) = a + |b|

// 练习1.5
def p(x: Int) {
  p(x)
}
def test(x: Int, y: Int => Unit): Any = {
  if (x == 0) 0
  else y(x)
}

assert(test(0, p) == 0) //scala既支持call by value 也支持 call by name
// 实例1.1.7 牛顿法
def abs(x: Double): Double = if (x < 0) -x else x
def square(x: Double): Double = x * x
def good_enough(guess: Double)(x: Double): Boolean = abs(square(guess) - x) <= 0.001
def average(guess: Double)(x: Double) = (guess + x) / 2
def improve(guess: Double)(x: Double): Double = average(guess)(x / guess)

@tailrec
def sqrt_iter(guess: Double)(x: Double): Double = {
  if (good_enough(guess)(x)) guess
  else sqrt_iter(improve(guess)(x))(x)
}
sqrt_iter(1)(10)
assert(abs(sqrt_iter(1)(10) - Math.sqrt(10)) < 0.001, "wrong")
// 练习1.6 todo 这里应该要学习scala的类型类
//def new_if(pred, then_clause, else_clause)
// 练习1.7
def good_enough2(guess: Double)(x: Double): Boolean = {
  abs(guess - improve(guess)(x) / guess) < 0.001
}
def sqrt_iter2(guess: Double)(x: Double): Double = {
  if (good_enough2(guess)(x)) guess
  else sqrt_iter(improve(guess)(x))(x)
}

//sqrt_iter(1)(10000000)
//sqrt_iter2(1)(10000000)
//
//sqrt_iter(1)(0.00000000001)
//sqrt_iter2(1)(0.00000000001)
//sqrt_iter(1)(1e9)
//sqrt_iter2(1)(1e9)

// 练习1.8
def improve2(guess: Double)(x: Double): Double = (x / square(guess) + 2 * guess) / 3
def good_enough3(guess: Double)(x: Double): Boolean = abs((guess * guess * guess - x) / x) < 0.001
@tailrec
def triqrt_iter(guess: Double, x: Double): Double = {
  if (good_enough3(guess)(x))
    guess
  else triqrt_iter(improve2(guess)(x), x)
}

triqrt_iter(1, 27)


