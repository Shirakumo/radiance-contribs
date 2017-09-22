## About
This is an implementation for the mail interface, using [cl-smtp](http://quickdocs.org/cl-smtp/).

## How To
You will most likely need to configure this module before you're able to use it. The configurations are as follows:

```
:host                   ; => The hostname of the SMTP sender.
:from                   ; => The email address of the sender.
:ssl                    ; => One of NIL, :STARTTLS :TLS
:reply-to               ; => Where to send replies to the mail to.
:display-name           ; => The name to display for the sender.
:auth                   ; => Whether to authenticate against the server.
:auth :method           ; => The method to use. Either :plain or :login.
:auth :username         ; => The username for the login.
:auth :password         ; => The password for the login.
:external-format        ; => The external format to use to encode text. Defaults to UTF-8.
```
