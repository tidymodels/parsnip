# Make a parsnip call expression

Make a parsnip call expression

## Usage

``` r
make_call(fun, ns, args, ...)
```

## Arguments

- fun:

  A character string of a function name.

- ns:

  A character string of a package name.

- args:

  A named list of argument values.

## Value

A call.

## Details

The arguments are spliced into the `ns::fun()` call. If they are
missing, null, or a single logical, then are not spliced.
