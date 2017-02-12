## About
This is an implementation of the server interface, using [Woo](https://github.com/fukamachi/woo) as the backend.

## How To
The configuration expected is as follows:

```
:enabled       ; => list of enabled instances
name :address  ; => bind address, defaults to "0.0.0.0"
name :port     ; => listen port
```

A default instance named `:default` is added on first startup, with the address set to `"0.0.0.0"` and the port set to the value of `(mconfig :radiance-core :port)`, or `8080` if that should be unavailable.
