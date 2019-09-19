### Exercise 1.6.1.1

> Machin's formula converges to $\pi$ faster than Example 1.4.1.5:
>
> $$
> \frac{\pi}{4} = 4 \arctan \frac{1}{5} - \arctan \frac{1}{239} \quad , \\
> \arctan \frac{1}{n} = \frac{1}{n} - \frac{1}{3} \frac{1}{n^3} + \frac{1}{5} + \frac{1}{n^5} - \dots = \sum_{k = 1}^{\infty} \frac{(-1)^{k}}{2k + 1} n^{-2k - 1} \quad .
> $$
>
> Implement a function that computes the series for $\arctan \frac{1}{n}$ up to
> a given number of terms, and compute an approximation of $\pi$ using this
> formula. Show that about 12 terms of the series are already sufficient for a
> full-precision `Double` approximation of $\pi$.

``` scala
object Exercise_1_6_1_1 {

  def arctan_r(n: Int, terms: Int): Double = {
    // Taylor series expansion for arctan(1 / k).
    (1.0 / n) + (1 to terms).map(k => math.pow(-1, k) / (2 * k + 1) * math.pow(n, -2 * k - 1)).sum
  }

  def machin(terms: Int): Double = {
    // Computes pi / 4.
    4 * arctan_r(5, terms) - arctan_r(239, terms)
  }

  def main(args: Array[String]): Unit = {
    (1 to 12).map(terms => {
      println(4 * machin(terms))
    })
  }

}
```

### Exercise 1.6.1.2

> Using the function `isPrime`, check numerically the Euler product
> formula for the Riemann zeta function $\zeta(4)$; it is known that
> $\zeta(4) = \frac{\pi^4}{90}$.
>
> $$
> \prod_{k \geq 2; {\,} k \text{ is prime}} \frac{1}{1 - p^{-4}} = \frac{\pi^4}{90} \quad .
> $$

``` scala
object Exercise_1_6_1_2 {

  def isPrime(n: Int): Boolean = {
    (2 until n)
      .takeWhile(k => k * k <= n)
      .forall(i => n % i != 0)
  }

  def riemannZeta(s: Int, k: Int): Double = {
    (2 to k)
      .filter(isPrime)
      .map(p => 1 / (1 - math.pow(p, -4)))
      .product
  }

  def main(args: Array[String]): Unit = {
    println(riemannZeta(4, 10000))
    println(math.pow(math.Pi, 4) / 90)
  }

}
```

### Exercise 1.6.2.1

> Define a function `add20` of type `List[List[Int]] => List[List[Int]]` that
> adds 20 to every element of every inner list. A sample test:
>
> ``` scala
> scala> add20(List(List(1), List(2, 3)))
> res0: List[List[Int]] = List(List(21), List(22, 23))
> ```

``` scala
object Exercise_1_6_2_1 {

  def add20(outer: List[List[Int]]): List[List[Int]] = {
    outer.map(inner => inner.map(el => el + 20))
  }

  def main(args: Array[String]): Unit = {
    println(add20(List(List(1), List(2, 3))))
  }

}
```

### Exercise 1.6.2.2

> An integer $n$ is called a "3-factor" if it is divisible by only three
> different integers $j$ such that $2 \leq j \lt n$. Compute the set of all
> "3-factor" integers $n$ among $n \in [1, \dots, 1000]$.

``` scala
object Exercise 1.6.2.2 {

  def f(n: Int): Boolean = {
    (1 to n).filter(j => n % j == 0).size == 3
  }

  def filter1000(f: Int => Boolean): Seq[Int] = {
    (1 to 1000).filter(f)
  }

  def main(args: Array[String]): Unit = {
    println(filter1000(f))
  }

}
```

Observation: "3-factor" integers are square values of prime numbers.

### Exercise 1.6.2.3

> Given a function `f: Int => Boolean`, an integer $n$ is called a "3-$f$" if
> there are only three different integers $j \in [1, \dots, n]$ such that
> $f(j)$ returns `true`. Define a function that takes $f$ as an argument and
> returns a sequence of all "3-$f$" integers among $n \in [1, \dots, 1000]$.
> What is the type of that function? Implement Exercise 1.6.2.2 using that
> function.

Is this not the same question as Exercise 1.6.2.2? The type of that function is
`(Int => Boolean) => Seq[Int]`.

### Exercise 1.6.2.4

> Define a function `see100` of type `List[List[Int]] => List[List[Int]]` that
> selects only those inner lists whose largest value is at least 100. Test
> with:
>
> ``` scala
> scala> see100(List(List(0, 1, 100), List(60, 80), List(1000)))
> res0: List[List[Int]] = List(List(0, 1, 100), List(1000))
> ```

``` scala
object Exercise_1_6_2_4 {

  def see100(outer: List[List[Int]]): List[List[Int]] = {
    outer.filter(inner => inner.filter(el => el >= 100).size > 0)
  }

  def main(args: Array[String]): Unit = {
    println(see100(List(List(0, 1, 100), List(60, 80), List(1000))))
  }

}
```

### Exercise 1.6.2.5

> Define a function of type `List[Double] => List[Double]` that "normalizes"
> the list: finds the element having the largest absolute value and, if that
> value is nonzero, divides all elements by that factor and returns a new list;
> otherwise returns the original list.

``` scala
object Exercise_1_6_2_5 {

  def maxAbs(l: List[Double]): Double = {
    l.map(e => math.abs(e)).max
  }

  def normalize(l: List[Double]): List[Double] = {
    val max: Double = maxAbs(l)
    if (max != 0)
      l.map(e => e / max)
    else
      l
  }

  def main(args: Array[String]): Unit = {
    println(normalize(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
    println(normalize(List(-100, 0, 0, 0, 3)))
    println(normalize(List(0, 0, 0, 0)))
  }

}
```
