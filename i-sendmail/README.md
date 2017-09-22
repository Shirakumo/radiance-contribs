## About
This is an implementation for the mail interface, using [cl-sendmail](http://quickdocs.org/cl-sendmail/).

## How To
The following configuration variables are available:

```
:from                   ; => The email address of the sender.
:reply-to               ; => Where to send replies to the mail to.
:display-name           ; => The name to display for the sender.
:external-format        ; => The external format to use to encode text. Defaults to UTF-8.
```
