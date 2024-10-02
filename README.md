# glfw-clj
[![Clojars Project](https://img.shields.io/clojars/v/org.suskalo/glfw-clj.svg)](https://clojars.org/org.suskalo/glfw-clj)

A wrapper for the excellent [GLFW](https://www.glfw.org/) library, on top of the
[coffi](https://github.com/IGJoshua/coffi) foreign function interface library
for Clojure.

This library provides

## Installation
This library is available on Clojars, and as a git dependency. You can add a
dependency on it with the following configuration in your `deps.edn` file.

```clojure
org.suskalo/glfw-clj {:mvn/version "1.0.86"}
io.github.IGJoshua/glfw-clj {:git/tag "v1.0.86" :git/sha "911496a"}
```

In addition to the module management [needed by
coffi](https://github.com/IGJoshua/coffi#installation), glfw-clj depends on the
GLFW library being present on the `LD_LIBRARY_PATH`.

## Usage
General usage of the library follows the usage of GLFW, as the library matches
that one closely. See [this
page](https://www.glfw.org/docs/latest/quick_guide.html) if you want help
getting started.

Functions are provided in the `glfw-clj.core` namespace, which is intended to be
aliased as `glfw`.

```clojure
user=> (require '[glfw-clj.core :as glfw])
```

Then functions will follow the same names as are in the original library, but
without the `glfw` prefix, and in kebab case. For example the function
`glfwGetError` is provided as `glfw-clj.core/get-error`.

```clojure
user=> (glfw/get-error)
nil
```

In order to be able to use most functionality in the library, it must first be
initialized.

```clojure
user=> (glfw/init)
true
```

This returns `true` when it succeeds, and `false` when it fails for whatever
reason.

In general, integers that map to `GLFW_TRUE` or `GLFW_FALSE` are replaced with
booleans, and integers which map to a given `GLFW_SOME_ENUM` like `GLFW_PRESS`
are replaced with keywords in the `glfw-clj.core` namespace. So `GLFW_PRESS` is
mapped to the keyword `::glfw/press`.

The set-callback functions take an optional `arena` parameter, which is a
resource arena from coffi. This ensures that the callback is kept around for as
long as it will be used. If you don't pass a arena, the global arena will be
used. This is generally advisable if you set the callback once at the beginning
of your program and don't change it. It's recommended to use this arena even
when developing at the REPL to prevent JVM crashes.

Callbacks will catch exceptions that are thrown in them and log them using
[clojure.tools.logging](https://github.com/clojure/tools.logging), before
returning an appropriate do-nothing value (at the time of writing, all callbacks
return void and therefore nil is used as the return value).

Opaque objects like the window, monitor, and cursor objects are represented as
pointers.

Docstrings are provided for all functions, but are there as reminder text, not a
replacement for the main GLFW documentation. When in doubt, check the GLFW docs.

## Future Plans
These features/changes are being considered for future versions of the library.

- An alternate way of loading the library

## License

Copyright © 2021 Joshua Suskalo

Distributed under the zlib/libpng license, the same as GLFW.
