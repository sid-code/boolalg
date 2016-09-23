## boolalg

`boolalg` is a boolean algebra system written in Nim. It can simplify boolean
algebra expressions in any number of variables to their minimal sum of products
form. It comes with a parser to parse boolean algebra expressions (see
`index.html` for instructions).

## Building

### Demo

You can build (and run) the demo by running

```
$ nim c boolalg
```

and run it with

```
$ ./boolalg "(-a)b + ac + bc"
```

and it will show you the expression's simplest form.

### Web application

The library can also be compiled to Nim's JavaScript backend. This is done with
the command

```
$ nim js -o:web/app.js app.nim
```

To view the application, open `web/index.html`.

This application is hosted [here][1].

  [1]: http://www.public.asu.edu/~skulka20/boolalg/

## Todo List

 * API documentation

## License

MIT
