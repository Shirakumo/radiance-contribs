## About
This is an implementation of the server interface, using [Wookie](https://github.com/orthecreedence/wookie) as the backend.

## How To
The configuration expected is as follows:

```
:enabled       ; => list of enabled instances
name :address  ; => bind address, defaults to "0.0.0.0"
name :port     ; => listen port
name :ssl-cert ; => ssl cert path (optional)
name :ssl-key  ; => ssl key path (optional)
name :ssl-pass ; => ssl cert password (optional)
```

A default instance named `:default` is added on first startup, with the address set to `"0.0.0.0"` and the port set to the value of `(mconfig :radiance-core :port)`, or `8080` if that should be unavailable.
