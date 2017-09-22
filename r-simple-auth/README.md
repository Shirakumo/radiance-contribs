## About
This is a simple implementation of the auth interface. It provides a login and registration form, and handles authentication of users through a password. The password is saved hashed and salted. An administration panel is added (if the admin interface is implemented) to allow users to change their password.

## Configuration
Two configuration values are of relevance:

```
:salt                   ; => The salt for the passwords. Is defaulted to a random string on first init.
:registration           ; => If this is "open", then registrations for new accounts can be made.
:recovery :timeout      ; => The time in seconds for which a recovery code is valid.
:recovery :subject      ; => The email subject line.
:recovery :message      ; => A format string forming the recovery message body. Two arguments are given: the username and the link to the recovery.
```

Note that once you change the salt, all passwords will become invalid and will need to be reset. Also note that account recovery will only be available if an implementation for the mail interface is loaded.
