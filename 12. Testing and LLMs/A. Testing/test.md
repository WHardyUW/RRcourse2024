# Testing

As economists, we are concerned about money. The Consortium for Information & Software Quality's report for 2020 estimated that the cost of operational software failures is $1.56 trillion per year. Legacy systems cost another half a trillion.

Many of these failures could have been averted by *software testing*. Testing ensures that the code behaves as it *should* in pre-defined cases. As computers are deterministic, given the same input, we get the same output -- always. It does not mean that testing makes software perfect in all conditions. Whether input is actually the same is not that obvious (more on that during the class on Reproducible environments), and users may use the software in ways testing or quality assurance engineers did not expect.

> A QA engineer walks into a bar. Orders a beer. Orders 0 beers. Orders 99999999999 beers. Orders a lizard. Orders -1 beers. Orders a ueicbksjdhd. 

> First real customer walks in and asks where the bathroom is. The bar bursts into flames, killing everyone.

---

## How to test?

[Software Construction course: inspiration for this part](https://ocw.mit.edu/ans7870/6/6.005/s16/classes/03-testing/index.html)

### (Wrong?) ways of testing

* **exhaustively**? How many test cases are there for integer multiplication?
* **haphazardly**? If the program fails most of the time...
* **statistically**? Software does not work like engineering
* **manually**? You will get bored soon.

### (Right?) ways of testing

??? -- do they exist?

Test everything the program does?

Test everything the program may realistically do?

Test everything the end user may do?

---

## Testing levels

[There are four major levels of testing.](https://en.wikipedia.org/wiki/Software_testing#Testing_levels)

* **unit testing**: ensure that specific functions are working as expected.
* integration testing: ensure that the components are working together well.
* system testing: ensure that the whole system is working as intended.
* acceptance testing: ensure that the end user is going to accept the system.

I will demonstrate the first level, unit testing, as it is the simplest conceptually.

---

## Unit testing in Python

Let us write a function that divides number *a* by number *b*. Such a function is very simple, but may (and should?) fail in numerous ways.

    def divide(a, b):
        return a / b

* the division is wrong (possibly due to a typo: // instead of /)
* user (or a programmer using this function) tries to divide by 0
* the inputs are not numbers

Let us test some basic possibilities.

    import unittest
    import sys

    class TestDivide(unittest.TestCase):
        def test_divide_floats(self):
            self.assertEqual(divide(5.0, 2.0), 2.5)

        def test_divide_ints(self):
            self.assertEqual(divide(2, 2), 1)
            self.assertEqual(divide(9, 4), 2.25)
            # it could return 2
            # (and it does in Python 2!)

        def test_divide_int_by_float(self):
            self.assertEqual(divide(8, 2.0), 4.0)
            # it could cause an error

        def test_divide_by_zero(self):
            with self.assertRaises(ZeroDivisionError):
                divide(5, 0)

        def test_wrong_inputs(self):
            with self.assertRaises(TypeError):
                divide('a', 'b')
            with self.assertRaises(TypeError):
                divide(False, True)

        @unittest.skipUnless(sys.platform.startswith("win"), "requires Windows")
        def test_windows_support(self):
            self.assertEqual(divide(95, 2000), 0.0475+1)

        @unittest.skipUnless(sys.platform.startswith("lin"), "requires Linux")
        def test_windows_support(self):
            self.assertEqual(divide(4, 10), 0.4+1)

        @unittest.skipIf(sys.version_info > (3, 1), 'not supported in this Python version')
        def test_python2_validity(self):
            self.assertEqual(divide(1, 2), 0)

    if __name__ == '__main__':
        unittest.main()
        
---

### Available assertions

[documentation](https://docs.python.org/3/library/unittest.html)

    assertEqual(a, b)  # a == b
    assertNotEqual(a, b)  # a != b
    assertTrue(x)  # bool(x) is True
    assertFalse(x)  # bool(x) is False
    assertIs(a, b)  # a is b
	assertIsNot(a, b)  # a is not b
	assertIsNone(x)  # x is None
	assertIsNotNone(x)  # x is not None
	assertIn(a, b)  # a in b
	assertNotIn(a, b)  # a not in b
	assertIsInstance(a, b)  # isinstance(a, b)
	assertNotIsInstance(a, b)  # not isinstance(a, b)
	assertAlmostEqual(a, b)  # round(a-b, 7) == 0
	assertNotAlmostEqual(a, b)  # round(a-b, 7) != 0
	assertGreater(a, b)  # a > b
	assertGreaterEqual(a, b)  # a >= b
	assertLess(a, b)  # a < b
	assertLessEqual(a, b)  # a <= b
	assertRegex(s, r)  # r.search(s)
	assertNotRegex(s, r)  # not r.search(s)
	assertCountEqual(a, b)  # a and b have the same elements in the same number, regardless of their order.
    
    ## comparisons (type)
    assertMultiLineEqual(a, b)  # strings
	assertSequenceEqual(a, b)  # sequences
	assertListEqual(a, b)  # lists
	assertTupleEqual(a, b)  # tuples
	assertSetEqual(a, b)  # sets or frozensets
	assertDictEqual(a, b)  # dicts

---

## Unit testing in R

Let us rewrite what we wrote in Python, but in R (skipping all the @unittest.skip decorators).

    divide <- function(a, b) {
        return(a / b)
    }

    testthat::test_that('divide-floats', {
        testthat::expect_equal(divide(5.0, 2.0), 2.5)
    })

    testthat::test_that('divide-ints', {
        testthat::expect_equal(divide(2, 2), 1)
        testthat::expect_equal(divide(9, 4), 2.25)
    })

    testthat::test_that('divide-int-by-float', {
        testthat::expect_equal(divide(8, 2.0), 4.0)
    })

    testthat::test_that('divide-by-zero', {
        testthat::expect_error(divide(5, 0))
    })

    testthat::test_that('wrong-inputs', {
        testthat::expect_error(divide('a', 'b'))
        testthat::expect_error(divide(FALSE, TRUE))
    })

---

## Further steps

You may have heard about [Selenium](https://www.selenium.dev/). This is a tool to automate browser usage. In Python, you can create web applications, for example with *Flask*. While *unittest* can automate a lot of things, it cannot check whether a sequence of steps is clickable.

*Which level of testing does Selenium fall into?*

---

## Exercise

1. Write a function in your preferred language which allows the user to convert the temperature in Fahrenheit degrees to Celsius degrees or Kelvins, depending on *target* parameter. The function should raise an error for any other temperature scale. In Python, working (but not correctly) code looks like this:

<code>

    def convert(f, target='c'):
        if target == 'c':
            return (f - 32) / 1.8
        elif target == 'k':
            return ((f - 32) / 1.8) + 273.15
        else:
            raise Exception('wrong target')

</code>

2. Check whether 50 degrees Fahrenheit are converted correctly to Celsius.
3. Check whether -500, 0, and 1000 degrees Fahrenheit are converted correctly to Kelvin. (hint: should they all be? First write the appropriate test, second -- fix the function)

Upload the file(s) to GitHub.
